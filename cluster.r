library("magrittr")

source("source/module.r")

filtering = module("source/filtering.r")

similarity = module("source/similarity.r")

clustering = module("source/clustering.r")

plotting = module("source/plotting.r")

residence = module("source/residence.r")

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
    filename = "cluster.png",
    width = 1024,
    height = 10240,
    plot_fun = plotting$plot_tree,
    # arguments for plot_tree:
    clustered = clustered,
    residences = residences
    )


# calculating overall purity with several purity functions
purity_max_mean = clustering$overall_purity(clustered, residences, 300)

plotting$plot_png(
    filename = "purity_max_mean.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_max_mean
    )    



plotting$plot_png(
    filename = "tree_k2.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_collapsed,
    # arguments for plot_collapsed
    clustered = clustered,
    k = 2,
    residences = residences,
    offset = 5
    )


plotting$plot_png(
    filename = "tree_k15.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_collapsed,
    # arguments for plot_collapsed
    clustered = clustered,
    k = 15,
    residences = residences,
    offset = 5
    )


purity_max_threshold = clustering$overall_purity(
    clustered, residences, 300, funct_purity=clustering$purity_max_threshold
    )

purity_max_threshold_penal_lin = purity_max_threshold - 1:300
purity_max_threshold_penal_perc = purity_max_threshold/1:300

plotting$plot_png(
    filename = "purity_max_threshold.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_max_threshold
    )    


plotting$plot_png(
    filename = "purity_max_threshold_penal_lin.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_max_threshold_penal_lin
    )    



plotting$plot_png(
    filename = "purity_max_threshold_penal_perc.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_max_threshold_penal_perc
    )    


purity_entropy_sum = clustering$overall_purity(
    clustered, residences, 300, funct_purity=clustering$purity_entropy_sum
    )

purity_entropy_sum_penal_lin = purity_entropy_sum - 1:300
purity_entropy_sum_penal_avg = purity_entropy_sum/1:300

plotting$plot_png(
    filename = "purity_entropy_sum.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_entropy_sum
    )


plotting$plot_png(
    filename = "purity_entropy_sum_penal_lin.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_entropy_sum_penal_lin
    )    


plotting$plot_png(
    filename = "purity_entropy_sum_penal_avg.png",
    width = 1024,
    height = 1024,
    plot_fun = plotting$plot_purity,
    # arguments for plot_purity
    purity = purity_entropy_sum_penal_avg
    )    
