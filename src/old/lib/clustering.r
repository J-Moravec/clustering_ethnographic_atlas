library("ape")
library("phytools")
library("magrittr")
source("module.r")
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
        if(length(clusters[[i]]) != 1){
            tree = drop.clade(tree, clusters[[i]])
            tree$tip.label[tree$tip.label == "NA"] = i
            } else {
            tree$tip.label[tree$tip.label == clusters[[i]] ] = i
            }
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


merge_clusters = function(clusters, vec){
    do.call(c, clusters[vec])
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




cluster_freq = function(x){
    apply(x, 2, table, deparse.level=0)
    }


cluster_data = function(cluster, data){
    dat = data[cluster,]
    #dat[is.na(dat)] = "NA"
    dat
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

# Unify categories:
#    x_cat = names(x)
#    y_cat = names(y)
#    new_x = x/sum(x)
#    new_y = y/sum(y)
#    var_cat = sort(union(x_cat, y_cat))
#    var_x = var_y = named_vec(var_cat)
#    var_x[x_cat] = new_x
#    var_y[y_cat] = new_y
