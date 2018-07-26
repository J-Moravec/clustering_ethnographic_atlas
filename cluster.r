library("magrittr")


module = function(file){
    env = new.env()
    sys.source(file, env , TRUE)
    return(env)
    }


filtering = module("source/filtering.r")

similarity = module("source/similarity.r")

clustering = module("source/clustering.r")

plotting = module("source/plotting.r")



# Ethnographic atlas is in variable EA

load("data/EA.Rdata")


# modify slavery variable
EA = filtering$modify_slavery(EA)

# filter EA
fEA = filtering$filter(EA)

similarity_matrix = similarity$similarity_matrix(fEA)
distance_matrix = 1 - similarity_matrix

# clustering
cluster = clustering$cluster(distance_matrix)

# Plotting:
residences = EA$v12
plotting$plot_png(cluster, residences)
