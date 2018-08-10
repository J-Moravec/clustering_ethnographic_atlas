library("ape")
library("phytools")
library("magrittr")
library("entropy")

cluster = function(x, ...){
    dist = as.dist(x)
    y = hclust(dist, ...)
    return(y)
    }


get_clusters = function(x, k){
    cut = cutree(x, k)
    clusters = vector(mode="list", length=k)
    for(i in 1:k){
        clusters[[i]] = cut[cut == i] %>% names
        }
    return(clusters)
    }


collapse_tree = function(x, clusters){
    n = length(clusters)
    tree = as.phylo(x)
    for(i in 1:n){
        tree = drop.clade(tree, clusters[[i]])
        tree$tip.label[tree$tip.label == "NA"] = i
        }    
    return(tree)
    }


clusters_res_freq = function(clusters, residences){
    lapply(clusters, cluster_res_freq, residences=residences)
    }


cluster_res_freq = function(cluster, residences){
    res = residences[cluster]
    tab = factor(res, levels = unique(residences))
    tab = table(tab)
    
    tab = tab/sum(tab)
    return(tab)
    }


clusters_res_freq_table = function(clusters, residences){
    res_freq = clusters_res_freq(clusters, residences)
    res_freq_mat = do.call(rbind, res_freq)
    return(res_freq_mat)
    }
