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


get_sizes = function(clusters){
    sapply(clusters, length)
    }


cluster_diff = function(x, y, data){
    x_dat = cluster_data(x, data)
    y_dat = cluster_data(y, data)
    x_freq = cluster_freq(x_dat)
    y_freq = cluster_freq(y_dat)
    y_freq = y_freq[names(x_freq)]
    mapply(var_diff, x_freq, y_freq)
    }


named_vec = function(names){
    setNames(rep(0, length(names)), names)
    }

var_diff = function(x, y){
    x_cat = names(x)
    y_cat = names(y)
    new_x = x/sum(x)
    new_y = y/sum(y)
    var_cat = sort(union(x_cat, y_cat))
    var = named_vec(var_cat)
    var[x_cat] = var[x_cat] + new_x
    var[y_cat] = var[y_cat] - new_y
    sum(abs(var))/2
    }

cluster_freq = function(x){
    apply(x, 2, table, deparse.level=0)
    }


cluster_data = function(cluster, data){
    dat = data[cluster,]
    dat[is.na(dat)] = "NA"
    dat
    }
