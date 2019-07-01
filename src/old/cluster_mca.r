# This script will cluster the results of MCA

library("homals")
source("source/module.r")

filtering = module("source/filtering.r")

similarity = module("source/similarity.r")

clustering = module("source/clustering.r")

plotting = module("source/plotting.r")

residence = module("source/residence.r")

purity = module("source/purity.r")

utils = module("source/utils.r")

# get result from MCA:
load("processed/mca_result.Rdata")
load("data/EA.Rdata")

# distance matrix is just euclidean distance, the default for dist in R:
if(!exists("distance_matrix")){
    distance_matrix = dist(mca_res)
    }


methods = c("single", "complete", "average", "median", "ward.D", "ward.D2")
residences = residence$get_residences(EA)
kmax = 300
a = c(2, 1, 1/2, 1/4)
cluster_results = list()
tree_results = list()
# distinguish non-MCA and MCA
path = "cluster_mca"

# From now on, the same approach as non-mca:
for(method in methods){
    cat("Processing:", method, "\n")
    # create folder for outputs, one for images and another for tables:
    imagepath = file.path("figures", path, method)
    tablepath = file.path("processed", path, method)

    utils$mkdir(imagepath)
    utils$mkdir(tablepath)

   # cluster according to chosen method
    clustered = clustering$cluster(distance_matrix, method=method)
    tree_results[[method]] = clustered
    # plot the whole clustered EA, note the large dimension of image:
    plotting$plot_png(
        filename = file.path(imagepath, "cluster.png"),
        width = 1024,
        height = 10240,
        plot_fun = plotting$plot_tree,
        # arguments for plot_tree:
        clustered = clustered,
        residences = residences
        )

    # process all purity methods:
    # originally, each of them were done manually
    # but this save a lot of writing
    cluster_results[[method]] = purity$purity_process_methods(
        clustered,
        residences,
        imagepath,
        tablepath,
        kmax,
        a
        )
    }

cluster_mca_results = cluster_results
tree_mca_results = tree_results
# and save the result object for future comparison
# under different name and variables to non-MCA
save(cluster_mca_results, tree_mca_results, file="processed/cluster_mca_results.Rdata")
