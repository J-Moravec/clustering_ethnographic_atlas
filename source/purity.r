# Set of purity funcions and their specification
#
# Also, set of helper functions for plotting purity (not the main functions)


source("module.r")

plotting = module("plotting.r")
clustering = module("clustering.r")

# plurity$plot
# a plotting$plot_png with some useful defaults for purity plots
plot_purity = function(purity, filename){
    plotting$plot_png(
        filename = filename,
        width = 1024,
        height = 1024,
        plot_fun = plotting$plot_purity,
        purity = purity
        )
    }

# penalization:
# a*b*(k/n)
# a -- scales importance of penalization
#   -- one of 2, 1, 1/2, 1/4, s
# b -- scales penalization to same units as purity
# k -- number of clusters
# n -- total number of societies
#   -- k/n scales penalization to range 0 to 1
penalization = function(purity, a, b, n){
    from = 1
    to = length(purity)
    penalization = a * b * (from:to)/n
    penalization
    }


# purity$plot_penalized
# plots series of penalization of purity vector
plot_penalized = function(purity, name, a=c(2, 1, 1/2, 1/4), b=1, n){
    for(i in a){
        filename = paste0(
            name, "_", format(i, digits=4), ".png"
            )
        filepath = file.path("figures", filename)
        penalized = purity + penalization(purity, i, b, n)
        plot_purity(penalized, filepath)
        }
    }


# range: 0 to 1, maximum is best
purity_max_mean = function(x, sizes){
    weighted.mean(sapply(x, max), w=sizes)
    }


# range: 0 to 1, maximum is best
purity_max_threshold = function(x, sizes, threshold=0.9){
    y = sapply(x, max) > threshold
    y = sizes[y]
    sum(y)/sum(sizes)
    }


# range: 0 to -log2(1/m) -> m=5 -> 0 to 2.321928
# -sum_i^m 1/m log2(1/m) -> -m*(1/m) log2(1/m) where m is number of categories
# minimum is best
purity_entropy = function(x, sizes){
    y = sapply(x, entropy, unit="log2")
    y = y*sizes
    sum(y)/sum(sizes)
    }


# range 0 to 1-sum( 1/m^2) -> m=5 -> 0 to 4/5
# 1-sum(1/m^2) = 1 - m * 1/m^2 = 1 - 1/m = (m-1)/m
# in fact, this is gini-simpson index and not just gini-index
# minimum is best
purity_gini = function(x, sizes){
    y = sapply(x, function(x) 1 - sum(x^2))
    y = y*sizes
    sum(y)/sum(sizes)
    }


get_sizes = function(clusters){
    sapply(clusters, length)
    }


overall_purity = function(clustered, residences, k_max, funct_purity=purity_max_mean, ...){
    k_max = min(length(clustered$labels), k_max)
    purity = vector(mode="numeric", length=k_max)
    for(k in 1:k_max){
        clusters = clustering$get_clusters(clustered, k)
        sizes = get_sizes(clusters)
        res_freq = clustering$clusters_res_freq(clusters, residences)
        purity[k] = funct_purity(res_freq, sizes, ...)
        cat(k, "/", k_max, "\r", sep="")
        }
    cat("\n", sep="")
    return(purity)
    }
