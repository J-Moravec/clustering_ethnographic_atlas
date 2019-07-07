# compare_variables.r
#
# Compare clusters and their variable compositions:

library("magrittr")
library("argparser")
library("ape")
library("phytools", quietly=TRUE)

scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/clusters.r", get_clusters)
import::from("lib/plot_tree.r", collapse_tree)
import::from("lib/ea_variables.r", ea_classes_vnamed)
import::from("lib/utils.r", write_table)
setwd(workdir)



args_parser = function(){
    parser = arg_parser(
        "Compare variables between clusters"
        )
    parser = add_argument(
        parser, "clustered", type="character",
        help = "path to rds file containing output from clustering"
        )

    parser = add_argument(
        parser, "--method", type="character",
        help = "clustering method to be extracted"
        )
    parser = add_argument(
        parser, "--k", type="character",
        help = "number of clusters to be extracted"
        )
    parser = add_argument(
        parser, "--ea", type="character",
        help = "ethnographic atlas with ethnographical variables"
        )
    parser = add_argument(
        parser, "--count_table", type="character",
        help = "output count matrix of all variables"
        )
    parser = add_argument(
        parser, "--category_table", type="character",
        help = "output count matrix, but variables are grouped into categories"
        )
    args = parse_args(parser)
    if(is.na(args$method)) stop("Method parameter required")
    if(is.na(args$k)) stop("Parameter k required")
    if(is.na(args$ea)) stop("Parameter ea required")
    args
    }



main = function(clustered, method, k, ea, count_table, category_table){
    clustered = readRDS(clustered)
    clustered = clustered[[method]]
    data = readRDS(ea)

    clusters = get_clusters(clustered, k)
    tree = collapse_tree(clustered, clusters)
    edges = rename_edges(tree$edge, tree$tip.label)
    clusters = prepare_clusters(edges, clusters)
    comparisons = get_comparisons(edges)

    variable_comparisons = make_comparisons(comparisons, clusters, data)
    comparison_matrix = process_comparisons(variable_comparisons)
    count_matrix = make_count_matrix(comparison_matrix) %>% add_percentage_difference

    count_matrix_categories = count_matrix_categories(count_matrix)
    count_matrix_summed = sum_categories(count_matrix_categories) %>% add_percentage_difference

    if(!is.na(count_table)){
        make_count_table(count_matrix_categories, count_table)
        }
    if(!is.na(category_table)){
        make_count_table(count_matrix_summed, category_table)
        }
    }


rename_edges = function(edges, labels){
    labels = as.numeric(labels)
    new_edges = edges
    for(i in seq_along(labels)){
        new_edges[edges[,2] == i, 2] = labels[i]
        }
    new_edges
    }



# ------------------------------------------------------------------------------------------------ #
# prepare clusters and bifurcations that will be compared
prepare_clusters = function(edges, tip_clusters){
    clusters = vector(mode="list", length=max(edges))
    
    # populate cluster with tips
    clusters[seq_along(tip_clusters)] = tip_clusters

    # calculate for others:
    from = length(tip_clusters) + 1
    to = length(clusters)

    for(i in from:to){
        clusters[[i]] = get_cluster(i, edges, clusters)
        }

    clusters
    }

# recursive function for getting clusters:
get_cluster = function(i, edges, clusters){
    cluster = clusters[[i]]
    
    if(is.null(cluster)){
        childrens = get_children(i, edges)
        children_clusters = lapply(childrens, get_cluster, edges=edges, clusters=clusters)
        cluster = merge_clusters(children_clusters)
        }
            
    cluster
    }


get_children = function(i, edges){
    edges[edges[,1] == i, 2]
    }


merge_clusters = function(clusters){
    do.call(c, clusters)
    }


get_comparisons = function(edges){
    unique(edges[,1]) %>% lapply(., get_children, edges=edges)
    }

# ------------------------------------------------------------------------------------------------ #
# compare data between clusters
cluster_freq = function(x){
    apply(x, 2, table, deparse.level=0)
    }


cluster_data = function(cluster, data){
    dat = data[cluster,]
    #dat[is.na(dat)] = "NA"
    dat
    }

cluster_diff = function(x, y, data){
    # take categories from whole dataset, rather than from individual clusters
    categories = cluster_freq(data)
    categories = lapply(categories, names)
    x_dat = cluster_data(x, data)
    y_dat = cluster_data(y, data)
    x_freq = cluster_freq(x_dat)
    y_freq = cluster_freq(y_dat)
    y_freq = y_freq[names(x_freq)]
    mapply(var_diff, x_freq, y_freq, categories)
    }


named_vec = function(names){
    setNames(rep(0, length(names)), names)
    }


category_vec = function(x, cat){
    xcat = names(x)
    new = named_vec(cat)
    new[xcat] = x
    new
    }


AICcorrection = function(n, k){
    if(n-1 < k){
        return(Inf)
        } else {
        return ( (2*k^2 + 2*k)/(n - k - 1) )
        }
    }


AIC = function(L, k){
    2*k - 2 * log2(L)
    }


# performs test according to multinomial distribution:
# Variables are different if:
# Probability probability of x and y having their own distribution is significantly
# greater than x and y following the same distribution.
# Pr(x|f_x) * Pr(y, f_y) > Pr(x|g) * Pr(y|g)
# where Pr(x | f_x) is dmultinom
# Additionally penalized using AIC
# Here we use the MLE to estimate the frequencies f_x, f_y and g:
# f_x = x/sum(x), f_y = y/sum(y) and g = x+y / sum(x) + sum(y)
# We should use GLOBAL categories rather than LOCAL categories.
var_diff = function(x, y, cat){
    # if x or y doesn't have samples: (all unknown), return zero
    if( sum(x) == 0 || sum(y) == 0 ){
        return(0)
        }

    # unify categories:
    x = category_vec(x, cat)
    y = category_vec(y, cat)

    nx = sum(x)
    ny = sum(y)

    fx = x/nx
    fy = y/ny
    g = (x+y) / ( ny + nx )

    kx = length(fx)
    ky = length(fy)
    k1 = kx + ky
    k2 = length(g)

    Pr1 = dmultinom(x, nx, fx) * dmultinom(y, ny, fy)
    Pr2 = dmultinom(x, nx, g) * dmultinom(y, ny, g)    
    # AICc = AIC + 2k^2 + 2k / n - k - 1
    # AIC = 2k - 2ln(L)
    AICc1 = AIC(Pr1, k1) + AICcorrection(nx, kx) + AICcorrection(ny, ky)
    AICc2 = AIC(Pr2, k2) + AICcorrection(nx + ny, k2)
    AICc1 - AICc2
    }

make_comparison = function(comparison, clusters, data){
        x = clusters[[comparison[1]]]
        y = clusters[[comparison[2]]]
        cluster_diff(x, y, data)
    }

make_comparisons = function(comparisons, clusters, data){
    lapply(comparisons, make_comparison, clusters=clusters, data=data)   
    }


# ------------------------------------------------------------------------------------------------ #
# Once comparisons are finished, we are processing them in various ways
binarize_matrix = function(mat){
    new_mat = mat
    new_mat[mat > 2] = 1
    new_mat[mat < -2] = -1
    new_mat[mat < 2 & mat > -2] = 0
    new_mat
    }


process_comparisons = function(comparisons){
    comparison_matrix = do.call(cbind, comparisons)
    names = rownames(comparison_matrix)
    names_order = order(nchar(names), names)
    comparison_matrix = comparison_matrix[names_order, ]
    # and binarize
    binarized_matrix = binarize_matrix(comparison_matrix)
    binarized_matrix
    }


make_count_matrix = function(comparison_matrix){
    difference = rowSums(comparison_matrix == -1)
    no_evidence = rowSums(comparison_matrix == 0)
    same = rowSums(comparison_matrix == 1)
    count_matrix = cbind(same, no_evidence, difference)
    count_matrix
    }


unlist_categories = function(list){
    res = list()
    for(category in names(list)){
        res[[category]] = cbind(category, list[[category]])
        }
    do.call(rbind, res)
    }


category_vector = function(){
    category_vector = unlist_categories(ea_classes_vnamed)
    rownames(category_vector) = category_vector[, 2]
    category_vector = category_vector[, 1]
    category_vector
    }


count_matrix_categories = function(count_matrix){
    category_vector = category_vector()
    category_vector = category_vector[rownames(count_matrix)]

    count_matrix = as.data.frame(count_matrix, stringsAsFactors=FALSE)
    count_matrix = cbind(count_matrix, "category"=category_vector, stringsAsFactors=FALSE)

    count_matrix
    }


sum_categories = function(count_matrix){
    types = unique(count_matrix$category)
    summed_categories = lapply(types, sum_category_type, count_matrix=count_matrix)
    summed_categories = do.call(rbind, summed_categories)
    rownames(summed_categories) = types
    summed_categories = summed_categories[sort(types),]
    summed_categories
    }


sum_category_type = function(type, count_matrix){
    subset = subset(count_matrix, category==type)
    colSums(subset[, 1:3])
    }



add_percentage_difference = function(count_matrix){
    diff_perc = count_matrix[,3] / rowSums(count_matrix[,1:3])
    diff_perc = round(diff_perc, 2)
    cbind(count_matrix, "different_p"=diff_perc)
    }

# ------------------------------------------------------------------------------------------------ #
# output:
make_count_table = function(count_matrix, file){
    names = rownames(count_matrix)
    table = cbind(names, count_matrix)
    write_table(table, file)
    }

# ------------------------------------------------------------------------------------------------ #
# runner:
if(!interactive()){
    args = args_parser()
    main(
        clustered = args$clustered,
        method = args$method,
        k = args$k,
        ea = args$ea,
        count_table = args$count_table,
        category_table = args$category_table
        )
    }
