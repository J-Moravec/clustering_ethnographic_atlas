# set of functions to operate with clusters
library("magrittr")
library("phytools")


get_clusters = function(x, k){
    cut = cutree(x, k)
    clusters = vector(mode="list", length=k)
    for(i in 1:k){
        clusters[[i]] = cut[cut == i] %>% names
        }
    return(clusters)
    }


get_sizes = function(clusters){
    sapply(clusters, length)
    }


merge_clusters = function(clusters, vec){
    do.call(c, clusters[vec])
    }


clusters_res_freq = function(clusters, residences, na.rm=TRUE){
    lapply(clusters, cluster_res_freq, residences=residences, na.rm=na.rm)
    }


cluster_res_freq = function(cluster, residences, na.rm=TRUE){
    # Problems with NA
    # -- size of clusters is used irregardles of NA
    # ---- For example, if all but one value are NA, cluster seems to be completely pure
    # ---- For example, if all values are NA, this cannot be even calculated!
    # -- leaving out NA is problematic regarding entropy
    # For this reason, NA is assumed to have a perfect entropy
    # -- For example, with 2 states, the NA value would turn into vector (1/2, 1/2)
    # This stabilizes entropy and other purity values
    #
    # For tree however, "NA" is turned into text and evaluated as a separate state
    if(!na.rm){
        residences[is.na(residences)] = "NA"
        }

    res = residences[cluster]
    na_count = is.na(res) %>% sum
    tab = factor(res, levels = unique(residences))
    len = length(levels(tab))
    na_vec = rep(1/len, len)
    tab = table(tab) + na_count * na_vec
    tab = tab/sum(tab)
    return(tab)
    }


clusters_res_freq_table = function(clusters, residences){
    res_freq = clusters_res_freq(clusters, residences, na.rm=FALSE)
    res_freq_mat = do.call(rbind, res_freq)
    return(res_freq_mat)
    }
