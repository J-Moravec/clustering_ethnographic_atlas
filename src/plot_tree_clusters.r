# plot_tree_clusters.r
#
# This script will plot a tree with from clustering cut to specifid number of clusters
# with residence composition of such clusters.
# The same is done in mass in "optimal clusters".


library("argparser")
library("magrittr")
library("ape")
library("phytools", quietly=TRUE)

# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/utils.r", write_table)
import::from("lib/utils.r", mkdir)
import::from("lib/utils.r", read_named_vector)
import::from("lib/plot_utils.r", plot_pdf)
import::from("lib/plot_tree.r", plot_collapsed_tree)
setwd(workdir)


args_parser = function(){
    parser = arg_parser(
        "Plot tree divided into cluster with their residence composition."
        )
    parser = add_argument(
        parser, "clustered", type="character",
        help = "path to rds file containing output from clustering"
        )
    parser = add_argument(
        parser, "output", type="character",
        help = "output image file"
        )
    parser = add_argument(
        parser, "--residences", type="character",
        help = "path to file with residence information"
        )
    parser = add_argument(
        parser, "--method", type="character",
        help = "tree from which clustering method should be plotted"
        )

    parser = add_argument(
        parser, "--k", type="character",
        help = "into how many clusters should be tree divided"
        )

    args = parse_args(parser)
    args
    }



main = function(clustered, residences, method, k, output){
    clustered = readRDS(clustered)
    residences = read_named_vector(residences)

    clustering_methods = names(clustered)
    if(!method %in% clustering_methods){
        stop(
            "Please use one of the followig clustering methods: ",
            paste(clustering_methods, collapse=", ")
            )
        }

        plot_pdf(
            filename = output,
            width = 12,
            height = 14,
            plot_fun = plot_collapsed_tree,
            clustered = clustered[[method]],
            k = k,
            residences = residences,
            offset = 2
            )
    }


if(!interactive()){
    args = args_parser()
    args$opts = NULL
    if(args %>% is.na %>% any){
        stop("All parameters are required")
        }
    main(
        clustered = args$clustered,
        residences = args$residences,
        method = args$method,
        k = args$k,
        output = args$output
        )
    }
