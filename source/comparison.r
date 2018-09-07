source("module.r")

clustering = module("clustering.r")

max_purity_method = function(x){
    lapply(x, function(y) apply(y, 1, which.max))
    }


max_purity_table = function(x){
    tab = list()
    for(method in names(x)){
        y = x[[method]]
        subtab = max_purity_method(y)
        purity_names = names(subtab)
        subtab = do.call(rbind, subtab)
        # reorder:
        subtab = subtab[,c("0","0.25","0.5","1","2")]
        nrows = nrow(subtab)
        if(nrows == 1){
            method_name = method
            } else {
            method_name = c(method, rep("", nrows-1))
            }
        subtab = cbind(method_name, purity_names, subtab)
        tab[[method]] = subtab
        }
    do.call(rbind, tab)
    }

extract_purity_values = function(results, cluster, puritymethod, clustermethod){
    order = c("0", "0.25", "0.5", "1", "2")
    res = results[[clustermethod]][[puritymethod]][,cluster]
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


optimal_clusters_table = function(optimal, results, purity){
    res = list()
    for(method in names(optimal)){
        clusters = optimal[[method]]
        subtab = extract_purity_values(results, clusters, purity, method)
        subtab = formatC(subtab, digits=2, format="f")
        nrows = nrow(subtab)
        if(nrows == 1){
            method_name = method
            } else {
            method_name = c(method, rep("", nrows-1))
            }
        subtab = cbind(method_name, clusters, subtab)
        res[[method]] = subtab
        }
    do.call(rbind, res)
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
            clusters_i = clustering$get_clusters(clustered_i, k_i)
            clusters_j = clustering$get_clusters(clustered_j, k_j)
            similarity = clustering$clusters_similarity(clusters_i, clusters_j)
            csm[i, j] = similarity
            csm[j, i] = similarity
            }
        }
    return(csm)
    }


binarize_matrix = function(mat){
    new_mat = mat
    new_mat[mat > 2] = 1
    new_mat[mat < -2] = -1
    new_mat[mat < 2 & mat > -2] = 0
    new_mat
    }


variable_comparison_table = function(tab){
    table = tab
    table[tab == -1] = "\\cellcolor{myred} -"
    table[tab == 0 ] = 0
    table[tab == 1 ] = "\\cellcolor{myblue} +"
    write.table(table,
        file="processed/var_comparison_ward_d2k6.tex",
        quote = FALSE,
        row.names = TRUE,
        col.names = TRUE,
        sep = " & ",
        eol = "\\\\\n"
        )
    }


unlist_categories = function(list){
    res = list()
    for(category in names(list)){
        res[[category]] = cbind(category, list[[category]])
        }
    do.call(rbind, res)
    }
