library("magrittr")

source("source/module.r")

comparison = module("source/comparison.r")
clustering = module("source/clustering.r")
utils = module("source/utils.r")
plotting = module("source/plotting.r")
residence = module("source/residence.r")
tanglegram = module("source/tanglegram.r")
filtering = module("source/filtering.r")
# here we compare results of clustering.

load("processed/cluster_results.Rdata")
load("data/EA.Rdata")
# modify slavery variable
EA = filtering$modify_slavery(EA)
# filter EA
fEA = filtering$filter(EA, filter_bad=TRUE)
residences = residence$get_residences(EA)



max_purity_table = comparison$max_purity_table(cluster_results)

# With this, only header and caption is required
# see standalne folder for a standalone PDF table
write.table(max_purity_table,
    file="processed/max_purity_table.tex",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    sep = " & ",
    eol = "\\\\\n"
    )

# From table, we extract following number of clusters:
# We require at least two repeats
# We prefere smaller number for comparison
optimal_clusters = list(
    # single failed
    "complete" = c(4, 24), 
    "average" = c(8),
    # median failed
    "ward.D" = c(7, 32),
    "ward.D2" = c(54, 6)
    )

# We extract purity value of optimal number of clusters accross methods:
# It is not practical to have purity method in single table, so I have divided
# them. Only one method, lets say entropy, will be in text.
for(purity in c("max", "threshold", "entropy", "gini_simpson")){
    optimal_clusters_table = comparison$optimal_clusters_table(
        optimal_clusters, cluster_results, "entropy"
        )
    write.table(
        optimal_clusters_table,
        file = paste0("processed/optimal_clusters_", purity, ".tex"),
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE,
        sep = " & ",
        eol = "\\\\\n"
        )
    }


# we can also plot trees:
for(method in names(optimal_clusters)){
    filepath = paste0("figures/comparison/", method)
    utils$mkdir(filepath)
    for(k in optimal_clusters[[method]]){
        filename = paste0("tree_k", k, ".png")
        plotting$plot_png(
            filename = file.path(filepath, filename),
            width = 1024,
            height = 1024,
            plot_fun = plotting$plot_collapsed,
            # arguments for plot_collapsed:
            clustered = tree_results[[method]],
            k = k,
            residences = residences,
            offset = 5
            )
        }
    }


# and tanglegrams showing comparison between trees. First, for the same method:
for(method in names(optimal_clusters)){
    filepath = paste0("figures/comparison/", method)
    utils$mkdir(filepath)
    clustered = tree_results[[method]]
    ks = optimal_clusters[[method]]
        if(length(ks) > 1){
        filename = paste0("tanglegram_k", ks[1], "_k", ks[2], ".png")
        clust1 = clustering$get_clusters(clustered, ks[1])
        clust2 = clustering$get_clusters(clustered, ks[2])
        tree1 = clustering$collapse_tree(clustered, clust1)
        tree2 = clustering$collapse_tree(clustered, clust2)
        text1 = paste0(method, " k", ks[1])
        text2 = paste0(method, " k", ks[2])
        plotting$plot_png(
            filename = file.path(filepath, filename),
            width = 1024,
            height = 1024,
            plot_fun = tanglegram$tanglegram,
            # arguments for tanglegram,
            tree1 = tree1,
            tree2 = tree2,
            cluster1 = clust1,
            cluster2 = clust2,
            text1 = text1,
            text2 = text2
            )
        }
    }


# tanglegrams between methods:
filepath = paste0("figures/comparison/tanglegrams")
utils$mkdir(filepath)
for(method1 in names(optimal_clusters)){
    for(method2 in names(optimal_clusters)){
        if(method1 == method2){
            #nothing
            } else {
            clustered1 = tree_results[[method1]]
            clustered2 = tree_results[[method2]]
            kis = optimal_clusters[[method1]]
            kjs = optimal_clusters[[method2]]
            for(ki in kis){
                for(kj in kjs){
                    filename = paste0(
                        "tanglegram_",
                        method1, "_k", ki, "__",
                        method2, "_", kj, ".png"
                        )
                    clust1 = clustering$get_clusters(clustered1, ki)
                    clust2 = clustering$get_clusters(clustered2, kj)
                    tree1 = clustering$collapse_tree(clustered1, clust1)
                    tree2 = clustering$collapse_tree(clustered2, clust2)
                    text1 = paste0(method1, " k", ki)
                    text2 = paste0(method2, " k", kj)
                    plotting$plot_png(
                        filename = file.path(filepath, filename),
                        width = 1024,
                        height = 1024,
                        plot_fun = tanglegram$tanglegram,
                        tree1 = tree1,
                        tree2 = tree2,
                        cluster1 = clust1,
                        cluster2 = clust2,
                        text1 = text1,
                        text2 = text2
                        )
                    }
                }
            }
        }
    }


# appart of tanglegrams, we want table of similarity:
similarity_table = comparison$cluster_similarity_matrix(
    optimal_clusters, tree_results
    )
similarity_table = formatC(similarity_table, digits=2, format="f")
write.table(similarity_table,
    file="processed/cluster_similarity_table.tex",
    quote = FALSE,
    row.names = TRUE,
    col.names = TRUE,
    sep = " & ",
    eol = "\\\\\n"
    )


