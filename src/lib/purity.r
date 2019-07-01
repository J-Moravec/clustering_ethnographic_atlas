# Set of function for calculation of purity (or non-diversity)
# code might be a bit dirty

library("entropy")


import::from("utils.r", mkdir)
import::from("clusters.r", get_clusters, get_sizes, clusters_res_freq)
import::from("plot_utils.r", plot_png)

purity_proces_methods = function(
    clustered,
    residences,
    imagepath,
    tablepath,
    kmax = 300,
    a = c(2, 1, 1/2, 1/4)
    ){
    mkdir(imagepath)
    mkdir(tablepath)
    results = list()

    for(method in names(methods)){
        cat("Processing purity method:", method, "\n")
        results[[method]] = purity_proces_method(
            clustered,
            residences,
            funct_purity = methods[[method]],
            method,
            imagepath,
            tablepath,
            kmax,
            a
            )
        }
    results
    }

# Process a single specified purity method:
purity_proces_method = function(
    clustered,
    residences,
    funct_purity,
    name,
    imagepath="figures",
    tablepath="processed",
    kmax=300,
    a = c(2,1,1/2,1/4)
    ){
    n = length(residences)
    purity = overall_purity(clustered, residences, kmax, funct_purity)
    penalized = penalize_purities(purity, a, n)

    # plotting purity
    filename = file.path(imagepath, paste0(name, ".png"))
    plot_purity_png(purity, filename)

    # plotting penalized purities:
    plot_penalized(penalized, name, imagepath)

    # create output list:
    result = list("purity" = purity, "penalized" = penalized)
    result_tab = purity_res_to_table(result)
    # and save the output to table:
    filename = file.path(tablepath, paste0(name, ".txt"))
    write.table(result_tab, file=filename, col.names=TRUE, row.names=TRUE)
    return(result_tab)
    }


purity_res_to_table = function(purity_res){
    tab = rbind(
        "0"=purity_res$purity,
        do.call(rbind, purity_res$penalized)
        )
    tab
    }


# purity$plot_purity
# a plotting$plot_png with some useful defaults for purity plots
plot_purity_png = function(purity, filename){
    plot_png(
        filename = filename,
        width = 1024,
        height = 1024,
        plot_fun = plot_purity,
        purity = purity
        )
    }


plot_purity = function(purity){
    plot(purity, pch=19, frame.plot=FALSE)
    lines(purity, col="red", lwd=2, lty=2)
    }

# purity$plot_penalized
# plots series of penalization of purity vector
plot_penalized = function(penalized, name, path){
    for(i in names(penalized)){
        filename = paste0(name, "_", i, ".png")
        filepath = file.path(path, filename)
        plot_purity_png(penalized[[i]], filepath)
        }
    }


purity_penalize = function(purity, a, n){
    purity = purity - penalization(purity, a, n)
    purity
    }


penalize_purities = function(purity, a=c(2,1,1/2,1/4), n){
    penalized = lapply(a, function(x) purity_penalize(purity, x, n))
    names(penalized) = as.character(a)
    penalized
    }


# penalization:
# a*(k/n)
# a -- scales importance of penalization
#   -- one of 2, 1, 1/2, 1/4, s
# k -- number of clusters
# n -- total number of societies
#   -- k/n scales penalization to range 0 to 1
penalization = function(purity, a, n){
    from = 1
    to = length(purity)
    penalization = a * (from:to)/n
    penalization
    }


purity_max = function(x, sizes){
    weighted.mean(sapply(x, max), w=sizes)
    }


purity_threshold = function(x, sizes, threshold=0.9){
    y = sapply(x, max) > threshold
    y = sizes[y]
    sum(y)/sum(sizes)
    }


# Shannon's entropy
purity_entropy = function(x, sizes){
    k = length(x[[1]])
    y = sapply(x, entropy::entropy, unit="log2")
    y = sapply(y, function(x) 1 - x/k)
    y = y*sizes
    sum(y)/sum(sizes)
    }


# normalized gini-simpson index/diversity
# NGS = (1 - sum(x^2))/ ( 1 - 1/k) where k is number of classes
# Purity = 1 - NGS
purity_gini_simpson = function(x, sizes){
    k = length(x[[1]])
    y = sapply(x, function(x) (1 - sum(x^2)) / (1-1/k) )
    y = sapply(y, function(x) 1 - x)
    y = y*sizes
    sum(y)/sum(sizes)
    }


methods = list(
    "max" = purity_max,
    "threshold" = purity_threshold,
    "entropy" = purity_entropy,
    "gini_simpson" = purity_gini_simpson
    )


# If one wants to improve performance, this (and associated purity functions) is an obvious choice.
# Still, one run of this takes about 30 seconds, so it is not bad.
overall_purity = function(clustered, residences, k_max, funct_purity=purity_max, ...){
    k_max = min(length(clustered$labels), k_max)
    purity = vector(mode="numeric", length=k_max)
    for(k in 1:k_max){
        clusters = get_clusters(clustered, k)
        sizes = get_sizes(clusters)
        res_freq = clusters_res_freq(clusters, residences)
        purity[k] = funct_purity(res_freq, sizes, ...)
        cat(k, "/", k_max, "\r", sep="")
        }
    cat("\n", sep="")
    return(purity)
    }
