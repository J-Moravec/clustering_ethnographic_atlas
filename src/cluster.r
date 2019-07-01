# Performs hierarchical clustering
library("magrittr")
library("argparser")
library("ape")


# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
Rcpp::sourceCpp("lib/similarity.cpp")
import::from("lib/utils.r", "read_named_vector")
import::from("lib/utils.r", "mkdir")
import::from("lib/plot_utils.r", "plot_png")
setwd(workdir)

main = function(input, output, methods, residence, mca, figures){
    EA = readRDS(input)
    distance_matrix = get_distance_matrix(EA, mca)
    clustered  = cluster_all(distance_matrix, methods)
    residences = read_named_vector(residence, na_string="")

    if(!is.na(figures)){
        mkdir(figures)
        plot_tree_figures(clustered, residences=residences, folder=figures)
        }

    if(!is.na(output)){
        saveRDS(clustered, file=output)
        }
    }


args_parser = function(){
    parser = arg_parser("Perform hierarchical clustering of Ethnographic Atlas")
    parser = add_argument(
        parser, "input", type="character",
        help="Path to rds file containing Ethnographic Atlas"
        )
    parser = add_argument(
        parser, "--methods", type="character", default="all", nargs=Inf,
        help=paste0("One or multiple clustering methods or \"all\" string (default).",
            " These correspond to the clustering methods in hclust function:",
            " single, complete, average, median, ward.D and ward.D2.",
            " See ?hclust in R for more details."
            )
        )
    parser = add_argument(
        parser, "--residence", type="character",
        help=paste0("File with society names and their residences.")
        )
    parser = add_argument(
        parser, "--mca", type="character",
        help=paste0("Path to rds file containing results from MCA analysis.",
            " If this is provided, clustering will be performed on the MCA results",
            " instead on the Ethnographic Atlas itself."
            )
        )
    parser = add_argument(
        parser, "--output", type="character",
        help=paste0("Clustering outcome is stored as rds object in specified path.",
            " This object is a list containing clustering for specified methods."
            )
        )
    parser = add_argument(
        parser, "--figures", type="character",
        help="Output folder for the tree figures of clustering outcomes."
        )
    args = parse_args(parser)
    args$methods = parse_methods(args$methods)
    args
    }

################################################################################
# Implementation
################################################################################

parse_methods = function(arg){
    methods = c("single", "complete", "average", "median", "ward.D", "ward.D2")
    # check all
    if(arg == "all"){
        return(methods)
        }
    # check invalid
    invalid = !arg %in% methods
    if(any(invalid)){
        stop("Invalid methods: ", paste0(invalid, collapse=", "))
        }
    arg
    }


get_distance_matrix = function(EA, mca){
    if(is.na(mca)){
        distance_matrix = distance_matrix(EA)
        } else {
        mca_result = readRDS(mca)
        distance_matrix = dist(mca_result$objscores)
        }
    distance_matrix
    }


distance_matrix = function(x){
    1 - similarity_matrix(x)
    }


similarity_matrix = function(x){
    names = rownames(x)
    y = as.matrix(x)
    mode(y) = "integer"
    y = cpp_similarity_matrix(y)
    rownames(y) = names
    colnames(y) = names
    return(y)
    }


cluster_all = function(distance_matrix, methods){
    clustered = list()
    for(method in methods){
        clustered[[method]] = cluster(distance_matrix, method=method)
        }
    clustered
    }


cluster = function(x, ...){
    dist = as.dist(x)
    y = hclust(dist, ...)
    return(y)
    }


plot_tree_figures = function(clustered, residences, folder){
    for(method in names(clustered)){
        filename = paste0("tree_", method, ".png")
        filepath = file.path(folder, filename)
        plot_tree_figure(clustered[[method]], residences, filepath)
        }
    }


plot_tree_figure = function(data, residences, name){
    plot_png(
        filename = name,
        width = 1024,
        height = 10240,
        plot_fun = plot_tree,
        clustered = data,
        residences = residences
        )
    }

plot_tree = function(clustered, residences){
    par(mar = c(0,0,0,0))
    tree = as.phylo(clustered)
    tip_names = tree$tip.label
    legend = names(residence_colors)
    legend_colors = residence_colors
    tip_colors = residence_to_color(residences)
    branch_colors = "grey"
    font_size = 0.8

    .plot_tree = function(do_plot=FALSE, offset=0, show=FALSE, xlim=NULL){
        plot.phylo(
            tree,
            edge.width = 3,
            edge.color = branch_colors,
            tip.color = tip_colors,
            font = 1,
            plot = do_plot,
            no.margin = TRUE,
            x.lim = xlim,
            align.tip.label = TRUE,
            underscore = TRUE,
            show.tip.label = show,
            label.offset = offset,
            cex = font_size
            )
        }

    .plot_tree()
    max_label_width = max(strwidth(tip_names))
    offset = strwidth("m")*1
    current_xlim = par("usr")[2]
    new_xlim = max_label_width + current_xlim + offset
    .plot_tree(TRUE, offset, show=TRUE, xlim=new_xlim)

    tiplabels(col=tip_colors, pch=21, bg=tip_colors)
    plot_legend(legend=legend, colors=legend_colors)
    }


.plot_phylo_collapsed = function(
    x, do_plot=FALSE, offset=0, show=FALSE, xlim=NULL, ylim=NULL,
    edge_width = 3    
    ){
    plot.phylo(
        x,
        edge.width = edge_width,
        edge.color = "darkgrey",
        font = 1,
        plot = do_plot,
        no.margin = TRUE,
        x.lim = xlim,
        y.lim = ylim,
        align.tip.label = TRUE,
        show.tip.label = show,
        label.offset = offset,
        cex = 2,
        )
    }


residence_colors = c(
    "Matrilocal" = "#E21E26",
    "Patrilocal" = "#1F9DD9",
    "Ambilocal" = "#0A9748",
    "Neolocal" = "#942581",
    "NA" = "black"
    )


residence_to_color = function(residences){
    y = residence_colors[residences]
    y[is.na(y)] = "black"
    return(y)
    }


plot_legend = function(legend_pos = "topleft", legend, colors, cex=3){
    par(mar=c(0,0,0,0))
    plot.window( c(0,100), c(0,100) )
    legend(
        legend_pos,
        pch = 16,
        col = colors,
        legend = legend,
        bty = "n",
        border = NA,
        bg = "transparent",
        xpd = TRUE,
        cex = cex
        )
    }


if(!interactive()){
    args = args_parser()
    main(
        input = args$input,
        output = args$output,
        methods = args$methods,
        residence = args$residence,
        mca = args$mca,
        figures = args$figures
        )
    }
