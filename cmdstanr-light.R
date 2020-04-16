## that the first dimension of each list entry is the iteration.
extract_draw <- function(sims, draw) lapply(sims, asub, idx=draw, dim=1)

## applies a function over each entry of the posterior if
## vectorized=FALSE; for vectorized=TRUE the function is assumed to
## perform the simulation in a single sweep. Note that all arguments
## to the function are automatically deduced from it's formals and
## that all arguments which are not in the sims list are searched in
## the global environment.
posterior_simulate <- function(sims, fun, vectorized=FALSE, res_type, envir) {
    require(abind)
    args <- setdiff(names(formals(fun)), "seed")

    from_draw <- intersect(args, names(sims))
    from_env  <- setdiff(args, names(sims))

    if (missing(envir))
        envir <- parent.frame()

    sims <- sims[from_draw]
    aux <- mget(from_env, envir=envir)

    if(!vectorized) {
        S <- NROW(sims[[1]])
        calc_draw <- function(i) do.call(fun, c(aux, extract_draw(sims, i)))
        if(missing(res_type))
            res_type <- calc_draw(1)
        res <- vapply(1:S, calc_draw, res_type)
        nd <- length(dim(res))
        if(2 > (nd - 1))
            aux_ind <- c()
        else
            aux_ind <- 2:(nd-1)
        return(aperm(res, c( nd, aux_ind, 1)))
    } else {
        return(do.call(fun, c(sims, aux)))
    }
}

stan_generate_model <- function(file, ..., postfix="_generated", enable_stan_next=FALSE) {
    library(tools)
    generated_file <- gsub(".stan$", paste0(postfix, ".stan"), file)
    md5 <- md5sum(file)
    ## in case the file already exists, check if there is a MD5 hint
    if(file.exists(generated_file)) {
        stan_source_gen <- readLines(generated_file)
        md5_line <- grepl("^//MD5:", stan_source_gen)
        if(sum(md5_line) == 1) {
            md5_last <- gsub("^//MD5:", "", stan_source_gen[md5_line])
            if (md5_last == md5) {
                cat("Using existing file.\n")
                return(generated_file)
            }
        }
    }
    stan_model <- stanc_builder(file, ...)
    cat(stan_model$model_code, file=generated_file)
    if(enable_stan_next)
        system(paste0("sed -i_orig 's#\\/\\/stan_next:##g' ", generated_file))
    cat(paste0("\n\n//MD5:",md5, "\n\n"), file=generated_file, append=TRUE)
    generated_file
}


cmdstan <- function(stan_model,
                    num_warmup=100, save_warmup=0, num_samples=100,
                    algorithm="hmc",
                    adapt_delta=0.9,
                    stepsize=0.1,
                    init=1,
                    seed=1,
                    chains=1,
                    chain_id=1,
                    data="stan_data.R",
                    init_buffer,
                    term_buffer,
                    window,
                    inv_metric,
                    engaged,
                    cores=getOption("mc.cores", 1L),
                    output_dir=getOption("cmdstan_tmp", tempfile("cmdstan-")),
                    cmdstan=getOption("cmdstan_home"),
                    quiet=FALSE,
                    clean=TRUE,
                    compress=FALSE,
                    check_dirty,
                    enable_stan_next=FALSE) {
    ## first check if we need to auto-generate a stan file without any includes
    if(sum(grepl("#include", readLines(stan_model)) > 0)) {
        stan_model_generated <- gsub("\\.stan$", "_generated.stan", stan_model)
        if(file.info(stan_model_generated)$mtime < file.info(stan_model)$mtime) {
            if(!quiet)
                cat("Info: Generating Stan model without includes:", stan_model_generated, "\n")
            stan_generate_model(stan_model, postfix="_generated", enable_stan_next=enable_stan_next)
        }
        stan_model <- stan_model_generated
    }
    stan_model_path <- normalizePath(stan_model)
    stan_model_bin <- gsub(".stan$", "", stan_model_path)
    stan_model_name <- basename(stan_model_bin)
    stdout <- stderr <- ifelse(quiet, TRUE, "")
    if(missing(check_dirty)) {
        check_dirty <- TRUE
        if(file.exists(stan_model_bin)) {
            check_dirty <- file.info(stan_model_bin)$mtime < file.info(stan_model_path)$mtime
        }
    }
    if(check_dirty) {
        cwd <- getwd()
        setwd(cmdstan)
        system2("make", args=stan_model_bin, stdout=stdout, stderr=stderr)
        setwd(cwd)
    }
    epaste <- function(name, arg, ...) {
        if(!is.null(arg))
            return(paste(name, arg, ..., sep="="))
        return("")
    }
    created_dir <- FALSE
    if(is.function(output_dir))
        output_dir <- output_dir()
    if(!dir.exists(output_dir)) {
        created_dir <- TRUE
        dir.create(output_dir, showWarnings=FALSE)
    }
    created_data <- FALSE
    if(is.list(data)) {
        data_file <- file.path(output_dir, paste0(stan_model_name, "_data.R"))
        stan_rdump(names(data), data_file, envir=list2env(data))
        created_data <- TRUE
    } else {
        data_file <- data
    }
    created_init <- FALSE
    if(is.list(init) | is.function(init))
        created_init <- TRUE
    if(missing(term_buffer))
        term_buffer  <- NULL
    if(missing(init_buffer))
        init_buffer  <- NULL
    if(missing(window))
        window  <- NULL
    if(!missing(inv_metric)) {
        metric_file <- file.path(output_dir, paste0(stan_model_name, "_metric.data.R"))
        stan_rdump("inv_metric", metric_file)
        created_metric <- TRUE
    } else {
        metric_file <- NULL
        created_metric <- FALSE
    }
    if(missing(engaged)) {
        if(num_warmup == 0 && !missing(inv_metric)) {
            message("Disabling engaged adaptation.")
            engaged <- 0
        } else {
            engaged <- 1
        }
    }
    get_init <- function(chain_id) {
        if (is.numeric(init))
            return(init)
        init_file <- file.path(output_dir, paste0(stan_model_name, "_init-", chain_id,".R"))
        if (is.function(init)) {
            ## in case the init is a function with one argument, then
            ## pass in the chain id
            if(length(formals(init)) == 1)
                init_chain <- init(chain_id)
            else
                init_chain <- init()
        } else if (is.list(init)) {
            init_chain <- init[[chain_id]]
        } else {
            stop("Init not recognized.")
        }
        stan_rdump(names(init_chain), init_file, envir=list2env(init_chain))
        init_file
    }
    run_chain <- function(chain_id) {
        if(!quiet)
            cat("Running chain", chain_id, "...\n")
        output_file <- file.path(output_dir, paste0("chain-", chain_id, "-", seed, ".csv"))
        stan_args <- c(
            "sample",
            epaste("num_warmup", num_warmup),
            epaste("save_warmup", save_warmup),
            epaste("num_samples", num_samples),
            epaste("adapt delta", adapt_delta),
            epaste("engaged", engaged),
            epaste("init_buffer", init_buffer),
            epaste("term_buffer", term_buffer),
            epaste("window", window),
            epaste("algorithm", algorithm),
            epaste("stepsize", stepsize),
            epaste("metric_file", metric_file),
            epaste("init", get_init(chain_id)),
            epaste("random seed", seed),
            epaste("id", chain_id),
            epaste("data file", data_file),
            epaste("output file" , output_file)
        )
        if(!quiet)
            cat("Stan call:", stan_model_bin, stan_args, "\n")
        msgs  <- system2(stan_model_bin, args=stan_args,
                         stdout=stdout, stderr=stderr
                         )
        if(compress) {
            system2("bzip2", args=c("-9", output_file))
            return(paste0(output_file, ".bz2"))
        }
        output_file
    }
    chain_set <- chain_id + (1:chains) - 1
    if(cores > 1) {
        library(parallel)
        outputs <- mclapply(chain_set, run_chain, mc.cores=min(cores, chains))
    } else {
        outputs <- lapply(chain_set, run_chain)
    }
    outputs <- unlist(outputs)
    fit <- read_stan_csv(outputs)
    if(clean) {
        if(!quiet)
            cat("Removing", paste(basename(outputs), collapse=", "), ".\n")
        do.call(file.remove, as.list(outputs))
        if(created_data)
            file.remove(data_file)
        if(created_init) {
            init_files <- file.path(output_dir, paste0(stan_model_name, "_init-", 1:chains,".R"))
            if(!quiet)
                cat("Removing", paste(basename(init_files), sep=", "), ".\n")
            file.remove(init_files)
        }
        if(created_metric) {
            file.remove(metric_file)
        }
        if(created_dir)
            file.remove(output_dir)
    }
    fit
}

