cluster = function(x, ...){
    dist = as.dist(x)
    y = hclust(dist, ...)
    return(y)
    }
