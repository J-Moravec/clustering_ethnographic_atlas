library("magrittr")
library("argparser")
# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/utils.r", read_named_vector)
import::from("lib/utils.r", mkdir)
import::from("lib/purity.r", purity_proces_methods)
setwd(workdir)

main = function(
    input,
    output,
    folder,
    residences,
    kmax=300,
    penalization=c(2,1,1/2,1/4)
    ){
    # prepare inputs
    clustered = readRDS(input)
    residences = read_named_vector(residences)

    # set up folders:
    images = file.path(folder, "figures")
    tables = file.path(folder, "tables")

    purities = process_clustering_methods(
        clustered,
        residences,
        images,
        tables,
        kmax,
        penalization
        ) 
    saveRDS(purities, output)
    }


# process_clustering_methods
# calculate every purity method for every clustering method
process_clustering_methods = function(clustered, residences, images, tables, kmax, penalization){
    methods = names(clustered)
    result = list()
    
    for(method in methods){
        imagepath = file.path(images, method)
        tablepath = file.path(tables, method)
        result[[method]] = purity_proces_methods(
            clustered[[method]],
            residences,
            imagepath,
            tablepath,
            kmax,
            penalization
            )
        }
    result
    }


args_parser = function(){
    parser = arg_parser("Calculate purity functions for outcome from clustering methods.")
    parser = add_argument(
        parser, "input", type="character",
        help = "Path to rds file containing clustering outcomes."
        )
    parser = add_argument(
        parser, "output", type="character",
        help = "Rds file with purity outcomes"
        )
    parser = add_argument(
        parser, "--folder", type="character",
        help = "Folder for tables and images from all methods."
        )
    parser = add_argument(
        parser, "--residences", type="character",
        help = "File with society names and their residences."
        )
    parser = add_argument(
        parser, "--kmax", type="numeric", default=300,
        help = "Maximum number of bifurcations explored."
        )
    parser = add_argument(
        parser, "--penalization", type="numeric", nargs="Inf", default=c(2, 1, 1/2, 1/4),
        help = "Penalization values for a larger number of clusters."
        )
    args = parse_args(parser)

    if(is.na(args$folder)) stop("Folder parameter is required")
    if(is.na(args$residences)) stop("Residences parameter is required")
    args
    }


if(!interactive()){
    args = args_parser()
    main(
        args$input,
        args$output,
        args$folder,
        args$residences,
        args$kmax,
        args$penalization
        )
    }
