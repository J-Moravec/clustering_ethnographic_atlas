# optimal_clusters.r
#
# Finds the optimal number of clusters according to various purity methods.
# Optimal number of clusters are those that are found at least twice for various number of
# penalisations and purity methods.
#
# Outputs:
# -- human-readable list of optimal clusters
# -- optimal clusters and their purity values
# -- similarity between optimal clusters of various clustering methods
# -- tree figures of each optimal cluster
library("argparser")
library("magrittr")
library("jsonlite")
library("entropy")
library("ape")
library("phytools")

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
import::from("lib/clusters.r", get_sizes)
import::from("lib/clusters.r", get_clusters)
import::from("lib/plot_tree.r", plot_collapsed_tree)
setwd(workdir)


args_parser = function(){
    parser = arg_parser(
        "Calculate optimal cluster sizes from purity information"
        )
    parser = add_argument(
        parser, "purity", type="character",
        help = "path to rds file containing calculated purities"
        )
    parser = add_argument(
        parser, "clustering", type="character",
        help = "path to rds file containing output from clustering"
        )
    parser = add_argument(
        parser, "optimal", type="character",
        help = "an output JSON file with calculated optimal cluster sizes"
        )
    parser = add_argument(
        parser, "--tables", type="character",
        help = paste0(
            "folder for tables of purity values of optimal clusters and a table similarity",
            " between clusters"
            )
        )
    parser = add_argument(
        parser, "--figures", type="character",
        help = "folder for tree visualization of clustering outcomes with the optimal number of cluster"
        )
    parser = add_argument(
        parser, "--residences", type="character",
        help = "path to file with residence information"
        )
    args = parse_args(parser)
    args
    }


main = function(purity, clustering, optimal, tables, figures, residences){
    if(is.na(residences) && !is.na(figures)){
        stop("ERROR: Residences must be specified for plotting trees!")
        }


    purities = readRDS(purity)
    clustered = readRDS(clustering)
    residences = read_named_vector(residences)
    mkdir(figures)
    mkdir(tables)

    optimal_clusters = get_optimal_clusters(purities)

    write_json(optimal_clusters, optimal, pretty=TRUE)

    if(!is.na(tables)){
        make_purity_tables(optimal_clusters, purities, tables)
        make_similarity_table(optimal_clusters, clustered, tables)
        }
    if(!is.na(figures)){
        make_optimal_cluster_trees(optimal_clusters, clustered, residences, figures)
        }
    }


# ------------------------------------------------------------------------------------------------ #
# Optimal clusters
get_optimal_clusters = function(x){
    result = list()
    for(method in names(x)){
        subtab = getElement(x, method) %>% max_purity_method
        optimal_clusters = optimal_clusters(subtab)
        if(length(optimal_clusters) > 0){
            result[[method]] = optimal_clusters
            }
        }
    result
    }


max_purity_method = function(x){
    lapply(x, function(y) apply(y, 1, which.max))
    }


optimal_clusters = function(clusters){
    clusters = unlist(clusters) %>% subset(.>1) %>% subset(.<100)
    optimal_clusters = table(clusters) %>% subset(.>1) %>% names %>% as.numeric
    optimal_clusters
    }

# ------------------------------------------------------------------------------------------------ #
# Purity tables
make_purity_tables = function(optimal_clusters, purities, path){
    purity_methods = purities %>% getElement(1) %>% names
    for(purity_method in purity_methods){
        filename = paste0("optimal_clusters_", purity_method, ".tex")
        filename = file.path(path, filename)
        purity_table = purity_table(optimal_clusters, purity_method, purities)
        write_table(purity_table, filename)
        }
    }


purity_table = function(optimal_clusters, purity_method, purities){
    clustering_methods = names(optimal_clusters)
    result = list()
    for(clustering_method in clustering_methods){
        clusters = optimal_clusters[[clustering_method]]
        subtable = extract_purity_values(clustering_method, purity_method, purities, clusters)
        subtable = formatC(subtable, digits=2, format="f")
        nrows = nrow(subtable)
        if(nrows == 1){
            method_name = clustering_method
            } else {
            method_name = c(clustering_method, rep("", nrows-1))
            }
        subtable = cbind(method_name, clusters, subtable)
        result[[clustering_method]] = subtable
        }
    do.call(rbind, result)
    }


extract_purity_values = function(clustering_method, purity_method, purities, clusters){
    order = c("0", "0.25", "0.5", "1", "2")
    res = purities[[clustering_method]][[purity_method]][,clusters]
    if(is.matrix(res)){
        res = t(res)
        res = res[,order]
        } else {
        res = res[order]
        }
    if(!is.matrix(res)){
        res = matrix(res, nrow=1, ncol=length(res))
        }
    res
    }


# ------------------------------------------------------------------------------------------------ #
# Optimal cluster trees
#make_optimal_cluster_trees = function(optimal_clusters, clustered, residences, filepath){
#    clustering_methods = names(optimal_clusters)
#    for(clustering_method in clustering_methods){
#        for(k in optimal_clusters[[clustering_method]]){
#            filename = paste0(clustering_method, "_", k, ".png")
#            plot_png(
#                filename = file.path(filepath, filename),
#                width = 1024,
#                height = 1024,
#                plot_fun = plot_collapsed_tree,
#                clustered = clustered[[clustering_method]],
#                k = k,
#                residences = residences,
#                offset = 5
#                )
#            }
#        }
#    }

make_optimal_cluster_trees = function(optimal_clusters, clustered, residences, filepath){
    clustering_methods = names(optimal_clusters)
    for(clustering_method in clustering_methods){
        for(k in optimal_clusters[[clustering_method]]){
            filename = paste0(clustering_method, "_", k, ".pdf")
            plot_pdf(
                filename = file.path(filepath, filename),
                width = 12,
                height = 14,
                plot_fun = plot_collapsed_tree,
                clustered = clustered[[clustering_method]],
                k = k,
                residences = residences,
                offset = 5
                )
            }
        }
    }


# ------------------------------------------------------------------------------------------------ #
# make similarity table
make_similarity_table = function(optimal_clusters, clustered, filepath){
    filename = "cluster_similarity_table.tex"
    filepath = file.path(filepath, filename)
    similarity_table = cluster_similarity_matrix(optimal_clusters, clustered)
    similarity_table = formatC(similarity_table, digits=2, format="f")
    write_table(similarity_table, filepath)
    }


unlist_optimal_clusters = function(optimal_clusters){
    vecs = c()
    meths = c()
    for(method in names(optimal_clusters)){
        vec = optimal_clusters[[method]]
        name_vec = paste0(method, " k", vec)
        names(vec) = name_vec
        meths = c(meths, rep(method, length(vec)) )
        vecs = c(vecs, vec)
        }
    list("clusters"=vecs, "methods"=meths)
    }


cluster_similarity_matrix = function(optimal_clusters, clustered){
    uoc = unlist_optimal_clusters(optimal_clusters)
    oc = uoc$clusters
    methods = uoc$methods
    csm = matrix(0, nrow = length(oc), ncol = length(oc))
    colnames(csm) = names(oc)
    rownames(csm) = names(oc)
    for(i in 1:length(oc)){
        for(j in i:length(oc)){
            method_i = methods[i]
            method_j = methods[j]
            clustered_i = clustered[[method_i]]
            clustered_j = clustered[[method_j]]
            k_i = oc[i]
            k_j = oc[j]
            clusters_i = get_clusters(clustered_i, k_i)
            clusters_j = get_clusters(clustered_j, k_j)
            similarity = clusters_similarity(clusters_i, clusters_j)
            csm[i, j] = similarity
            csm[j, i] = similarity
            }
        }
    return(csm)
    }


cluster_comparison = function(cluster, clusters){
    sapply(clusters, function(x){ sum(cluster %in% x)/length(cluster) })
    }

clusters_comparison = function(x, y){
    xlen = length(x)
    xres = list()
    for(i in 1:xlen){
        xres[[i]] = cluster_comparison(x[[i]], y)
        }
    xres
    }


entropy = function(x, sizes){
    k = length(x[[1]])
    y = sapply(x, entropy::entropy, unit="log2")
    y = sapply(y, function(x) 1 - x/k)
    y = y*sizes
    sum(y)/sum(sizes)
    }


clusters_similarity = function(x, y){
    comp1 = clusters_comparison(x, y)
    comp2 = clusters_comparison(y, x)
    sizes1 = get_sizes(x)
    sizes2 = get_sizes(y)
    sim1 = entropy(comp1, sizes1)
    sim2 = entropy(comp2, sizes2)
    mean(c(sim1,sim2))
    }


####################################################################################################
if(!interactive()){
    args = args_parser()
    main(args$purity, args$clustering, args$optimal, args$tables, args$figures, args$residences)
    }
