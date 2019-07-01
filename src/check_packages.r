# Check if specified packages are installed, if not, install them:

# OLD
#check_packages = function(packages){
#  installed_packages = installed.packages()[, "Package"]
#  missing = !packages %in% installed_packages
#  if( any(missing) ){
#    missing_text = paste0(packages[missing], collapse=" ")
#    stop("Missing packages: ", missing_text, call.=FALSE)
#  }
#}


missing_packages = function(packages){
    installed_packages = installed.packages()[, "Package"]
    missing = !packages %in% installed_packages
    res = list(
        missing = packages[missing],
        present = packages[!missing]
        )
    res
    }


main = function(packages, install, silent){
    packages = missing_packages(packages)

    if(!silent){
        message("Present packages: ", paste0(packages$present, collapse=", "))
        message("Missing packages: ", paste0(packages$missing, collapse=", "))
        }

    if(install && length(packages$missing > 0)){
        err = tryCatch(
            install.packages(packages$missing),
            error = function(e) e,
            warning = function(w) w
            )
        if(is(err) == "simpleWarning" || is(err) == "simpleError") stop(err$message, call.=FALSE)
        }
    }


parse_args = function(){
    argv = commandArgs(TRUE)
    options = list(
            install = FALSE,
            silent = FALSE,
            args = NULL
            )
    flags = parse_flags(argv)
    if("-h" %in% flags || "--help" %in% flags) {help_message(); quit()}
    if("-i" %in% flags || "--install" %in% flags) options$install=TRUE
    if("-s" %in% flags || "--silent" %in% flags) options$silent=TRUE
    args = parse_nonflags(argv)
 
    options$args = args
    options
    }


parse_flags = function(argv){
    pos = grep("^--?", argv)
    
    if(!identical(pos, seq_along(pos))){
        help_message()
        stop("Misspecified arguments.", call.=FALSE)
        }
    argv[pos]
    }


parse_nonflags = function(argv){
    pos = grep("^--?", argv, invert=TRUE)
    if(length(pos) < 1){
        help_message()
        stop("Must specify at least one package.", call.=FALSE)
        }
    argv[pos]
    }


help_message = function(){
    cat(
        "Usage: check_packages.r [OPTION]... [PACKAGE]...\n",
        "Checks if specified R packages are installed.\n\n",
        "Arguments:\n",
        "  -i, --install  install missing packages\n",
        "  -s, --silent   will not print messages\n",
        "  -h, --help     prints this help and exits\n\n"
        )
    }


if(!interactive()){
    args = parse_args()
    main(args$args, args$install, args$silent)
    }
