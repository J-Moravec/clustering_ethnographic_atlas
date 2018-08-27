library("magrittr")

source("source/module.r")

filtering = module("source/filtering.r")

similarity = module("source/similarity.r")

clustering = module("source/clustering.r")

plotting = module("source/plotting.r")

residence = module("source/residence.r")

purity = module("source/purity.r")


# Ethnographic atlas is in variable EA

load("data/EA.Rdata")


# modify slavery variable
EA = filtering$modify_slavery(EA)

# filter EA
fEA = filtering$filter(EA)

similarity_matrix = similarity$similarity_matrix(fEA)
distance_matrix = 1 - similarity_matrix

# clustering
clustered = clustering$cluster(distance_matrix)

# Plotting:
residences = residence$get_residences(EA)
plotting$plot_png(
    filename = "figures/cluster.png",
    width = 1024,
    height = 10240,
    plot_fun = plotting$plot_tree,
    # arguments for plot_tree:
    clustered = clustered,
    residences = residences
    )



# number of societies in total:
n_societies = nrow(fEA)

# how many clusters should be taken at maximum
kmax = 300

# purity_max_mean
# range: 0 to 1
# maximum is best
purity_max_mean = purity$overall_purity(
    clustered, residences, kmax, funct_purity=purity$purity_max_mean
    )
purity$plot_purity(purity_max_mean, "figures/purity_max_mean.png")
purity_max_mean_penalized = purity$plot_penalized(
    purity_max_mean, "purity_max_mean_pen", b=-1, n=n_societies
    )


# purity_max_threshold
# range: 0 to 1
# maximum is best
purity_max_threshold = purity$overall_purity(
    clustered, residences, kmax, funct_purity=purity$purity_max_threshold
    )
purity$plot_purity(purity_max_threshold, "figures/purity_max_threshold.png")
purity_max_threshold_penalized = purity$plot_penalized(
    purity_max_threshold, "purity_max_threshold_pen", b=-1, n=n_societies
    )

# purity_entropy
# range: 0 to 2.321928
# range: 0 to -log2(1/m)
# m = 5 (4 residences + NA)
# minimum is best
m = length(unique(residences))
entropy_max = -log2(1/m)
purity_entropy = purity$overall_purity(
    clustered, residences, kmax, funct_purity=purity$purity_entropy
    )
purity$plot_purity(purity_entropy, "figures/purity_entropy.png")
purity_entropy_penalized = purity$plot_penalized(
    purity_entropy, "purity_entropy_pen", b=entropy_max, n=n_societies
    )


# purity_gini (gini-simpson)
# range: 0 to 4/5
# range: 0 to 1-1/m
# m = 5 (4 residences + NA
# minimum is best
m = length(unique(residences))
gini_max = 1-1/m
purity_gini = purity$overall_purity(
    clustered, residences, kmax, funct_purity=purity$purity_gini
    )
purity$plot_purity(purity_gini, "figures/purity_gini.png")
purity_gini_penalized = purity$plot_penalized(
    purity_gini, "purity_gini_pen", b=gini_max, n=n_societies
    )


plotting$plot_png(
    filename = "tree_k19.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_collapsed,
    # arguments for plot_collapsed
    clustered = clustered,
    k = 19,
    residences = residences,
    offset = 5
    )


plotting$plot_png(
    filename = "tree_k24.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_collapsed,
    # arguments for plot_collapsed
    clustered = clustered,
    k = 24,
    residences = residences,
    offset = 5
    )

