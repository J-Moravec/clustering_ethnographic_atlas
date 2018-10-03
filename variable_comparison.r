source("source/module.r")

filtering = module("source/filtering.r")
residence = module("source/residence.r")
clustering = module("source/clustering.r")
comparison = module("source/comparison.r")

load("processed/cluster_results.Rdata")
load("data/EA.Rdata")
# modify slavery variable
EA = filtering$modify_slavery(EA)
# filter EA
fEA = filtering$filter(EA, filter_bad=TRUE)
residences = residence$get_residences(EA)

var_comparison_wardd2_6 = function(){
    # Comparison between clusters of tree 6
    clustered = tree_results[["ward.D2"]]
    clusters = clustering$get_clusters(clustered, 6)
    tree = clustering$collapse_tree(clustered, clusters)
    tree$edge.length = NULL
    write.tree(tree, "") # "((4,(3,2)),(1,(5,6)));"
    # Not enough time so I don't have a way to do this computationally,
    # but there are following bipartitions:
    # 3 vs 2
    # 5 vs 6
    # 4 vs 3+2
    # 1 vs 5+6
    # 4+3+2 vs 1+5+6
    # We will compare these bipartitions:
    res = list()
    # some shortcuts
    cl_1 = clusters[[1]]
    cl_2 = clusters[[2]]
    cl_3 = clusters[[3]]
    cl_4 = clusters[[4]]
    cl_5 = clusters[[5]]
    cl_6 = clusters[[6]]
    cl_2_3 = clustering$merge_clusters(clusters, c(2, 3))
    cl_5_6 = clustering$merge_clusters(clusters, c(5, 6))
    cl_2_3_4 = clustering$merge_clusters(clusters, c(2, 3, 4))
    cl_1_5_6 = clustering$merge_clusters(clusters, c(1, 5, 6))
    
    res[["2 vs 3"]] = clustering$cluster_diff(cl_2, cl_3, fEA)
    res[["5 vs 6"]] = clustering$cluster_diff(cl_5, cl_6, fEA)
    res[["4 vs 2+3"]] = clustering$cluster_diff(cl_4, cl_2_3, fEA)
    res[["1 vs 5+6"]] = clustering$cluster_diff(cl_1, cl_5_6, fEA)
    res[["2+3+4 vs 1+5+6"]] = clustering$cluster_diff(cl_2_3_4, cl_1_5_6, fEA)
    res = do.call(cbind, res)
    names = rownames(res)
    names_order = order(nchar(names), names)
    res = res[names_order, ]
    # and binarize
    res = comparison$binarize_matrix(res)
    res
    }

var_comparison = var_comparison_wardd2_6()
var_comparison_colored = comparison$color_cells(var_comparison)
variable_cats = comparison$unlist_categories(filtering$ea_classes)
rownames(variable_cats) = variable_cats[,2]
variable_cats = variable_cats[,1]
variable_cats = variable_cats[rownames(var_comparison)]
var_comparison_colored = cbind("category" = variable_cats, var_comparison_colored)
    write.table(var_comparison_colored,
        file="processed/variable_comparison.tex",
        quote = FALSE,
        row.names = TRUE,
        col.names = TRUE,
        sep = " & ",
        eol = "\\\\\n"
        )

types = unique(variable_cats)
res = list()
for(type in types){
    subdata = var_comparison[variable_cats == type,, drop=FALSE]
    res[[type]] = c(colMeans(subdata), nrow(subdata))
    }
res = do.call(rbind, res)

n_cats = res[,ncol(res)]
res = res[, -ncol(res)]
means = colMeans(var_comparison)
res = rbind(res, means)
res = formatC(res, digits=2, format="f")
n_cats = c(formatC(n_cats), "")
res = cbind("mean"=n_cats, res)
write.table(
    res,
    file = "processed/var_comparison_categories.tex",
    quote = FALSE,
    row.names = TRUE,
    col.names = TRUE,
    sep = " & ",
    eol = "\\\\\n"
    )

#var_means = rowMeans(var_comparison)
##  v1   v2   v3   v4   v5   v6   v7   v8   v9  v15  v16  v17  v19  v21  v23  v24 
##-0.2 -1.0 -1.0 -1.0 -0.6 -1.0 -0.8 -1.0 -0.6 -1.0 -1.0 -1.0 -1.0 -0.6 -1.0 -1.0 
## v25  v26  v27  v28  v29  v30  v31  v32  v33  v34  v35  v36  v37  v38  v40  v41 
##-0.8 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -0.8 -0.8 -0.6 -0.2 -0.2  0.2 -1.0 -1.0 -1.0 
## v42  v43  v44  v45  v46  v47  v48  v49  v50  v51  v52  v53  v54  v55  v57  v66 
##-1.0 -1.0 -0.6 -0.6 -0.4 -0.2 -0.6 -0.4 -0.2 -0.4 -0.8 -0.6 -1.0 -0.6 -1.0 -1.0 
## v68  v70  v71  v72  v73  v74  v75  v76  v77  v78  v79  v80  v81  v82  v83  v84 
##-1.0 -1.0 -0.4 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -0.6 -1.0 -0.8 -1.0 -1.0 -0.8  0.2 
## v85  v86  v87  v88  v90  v94  v95  v96 v112 
##-0.2 -0.2  0.2 -0.2 -0.2 -0.2 -0.2  1.0 -0.2
#length(var_means) # 73
#table(var_means)
##  -1 -0.8 -0.6 -0.4 -0.2  0.2    1 
##  36    7   10    4   12    3    1 

#sort(var_means)
#  v2   v3   v4   v6   v8  v15  v16  v17  v19  v23  v24  v26  v27  v28  v29  v30 
#-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 
# v31  v38  v40  v41  v42  v43  v54  v57  v66  v68  v70  v72  v73  v74  v75  v76 
#-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 
# v77  v79  v81  v82   v7  v25  v32  v33  v52  v80  v83   v5   v9  v21  v34  v44 
#-1.0 -1.0 -1.0 -1.0 -0.8 -0.8 -0.8 -0.8 -0.8 -0.8 -0.8 -0.6 -0.6 -0.6 -0.6 -0.6 
# v45  v48  v53  v55  v78  v46  v49  v51  v71   v1  v35  v36  v47  v50  v85  v86 
#-0.6 -0.6 -0.6 -0.6 -0.6 -0.4 -0.4 -0.4 -0.4 -0.2 -0.2 -0.2 -0.2 -0.2 -0.2 -0.2 
# v88  v90  v94  v95 v112  v37  v84  v87  v96 
#-0.2 -0.2 -0.2 -0.2 -0.2  0.2  0.2  0.2  1.0
