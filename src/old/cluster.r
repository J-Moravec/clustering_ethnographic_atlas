library("magrittr")

source("source/module.r")

filtering = module("source/filtering.r")

similarity = module("source/similarity.r")

clustering = module("source/clustering.r")

plotting = module("source/plotting.r")

residence = module("source/residence.r")

purity = module("source/purity.r")

utils = module("source/utils.r")

# Ethnographic atlas is in variable EA
load("data/EA.Rdata")

# modify slavery variable
EA = filtering$modify_slavery(EA)
# filter EA
fEA = filtering$filter(EA, filter_bad=TRUE)

similarity_matrix = similarity$similarity_matrix(fEA)
distance_matrix = 1 - similarity_matrix

# various clustering methods:
methods = c("single", "complete", "average", "median", "ward.D", "ward.D2")

# We will test all these methods.
# First, we need to identify the number of clusters according to purity.
# This cannot be done automatically, since we don't know what is the ideal
# penalization for number of clusters.

residences = residence$get_residences(EA)
n_societies = nrow(fEA) # total number of societies
kmax = 300 # maximum number of clusters that will be explored.
a = c(2, 1, 1/2, 1/4) # different values for penalization coeficient
cluster_results = list()
tree_results = list()
# so I can distinguish non-MCA and MCA
path = "cluster"


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


# and save the result object for future comparison
save(cluster_results, tree_results, file="processed/cluster_results.Rdata")
