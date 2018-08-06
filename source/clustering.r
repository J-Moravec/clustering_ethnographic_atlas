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



purity_max_mean = function(x){
    mean(sapply(x, max))
    }


purity_max_threshold = function(x, threshold=0.9){
    sum(sapply(x, max) > threshold)
    }

purity_entropy_sum = function(x){
    sum(sapply(x, entropy, unit="log2"))
    }


overall_purity = function(clustered, residences, k_max, funct_purity=purity_max_mean, ...){
    k_max = min(length(clustered$labels), k_max)
    purity = vector(mode="numeric", length=k_max)
    for(k in 1:k_max){
        clusters = get_clusters(clustered, k)
        res_freq = clusters_res_freq(clusters, residences)
        purity[k] = funct_purity(res_freq, ...)
        cat(k, "/", k_max, "\r", sep="")
        }
    cat("\n", sep="")
    return(purity)
    }
