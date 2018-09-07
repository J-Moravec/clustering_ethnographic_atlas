source("source/module.r")

clustering = module("source/clustering.r")

load("processed/cluster_mca_results.Rdata")
load("processed/cluster_results.Rdata")

# similarity between:
# non-mca ward.D2 k6
# mca ward.D k139

clustered = tree_results[["ward.D2"]]
clusters = clustering$get_clusters(clustered, 6)

clustered_mca = tree_mca_results[["ward.D"]]
clusters_mca = clustering$get_clusters(clustered_mca, 139)

clustering$clusters_similarity(clusters, clusters_mca)
