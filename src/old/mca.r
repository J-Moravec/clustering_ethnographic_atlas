# In this script, MCA is performed on filtered Ethnographic Atlas
#

library("homals")
library("magrittr")
source("source/module.r")

filtering = module("source/filtering.r")
plotting = module("source/plotting.r")

# read filtered EA from previous thing
load("data/EA.Rdata")
EA = filtering$modify_slavery(EA)
fEA = filtering$filter(EA, filter_bad=TRUE)

nvars = ncol(fEA)

# Prepare variable type variable:
vartype = rep("nominal", nvars)
names(vartype) = colnames(fEA)
vartype[filtering$ordinal] = "ordinal"

ncat = filtering$ncat(fEA)

# this code won't repeat if result already exist
# Homals can handle only 98 dimensions out of 524
# this is due to numbers being too small in weighted Gram-Schmidt Orthonormalization
#
if(!exists("result")){
    result = homals(fEA, ndim=98, level = vartype)
    }

variability = cumsum(result$eigenvalues/sum(result$eigenvalues))
names(variability) = seq_along(variability)

plotting$plot_png(
    "figures/mca/eigen_var.png",
    height = 480,
    width = 1024,
    plot_fun = plotting$plot_barplot,
    x = variability,
    names = FALSE,
    axisnames = TRUE,
    tr = NA,
    ylab = "percentage of explained variability",
    xlab = "eigenvalue",
    space = 0,
    sort=FALSE
    )

# three types of plots for dimension 1 to 5
maxdim = 5
xylim_load = sapply(result$loadings, function(x) x[1, 1:maxdim])
xylim_load = c(min(xylim_load, 0), max(xylim_load, 0))*1.1
xylim_object = range(result$objscores[, 1:maxdim])*1.1
xylim_discrim = range(result$discrim[, 1:maxdim])*1.1

for(i in 1:maxdim){
    for(j in i:maxdim){
        if(i != j){
            plotting$plot_png(
                paste0("figures/mca/loads/loadplot_dim_", i, "_", j, ".png"),
                height = 480,
                width = 480,
                plot_fun = plotting$loadplot,
                x = result, xlim=xylim_load, ylim=xylim_load,
                dim1=i, dim2=j
                )
            plotting$plot_png(
                paste0("figures/mca/objects/objectplot_dim_", i, "_", j, ".png"),
                height = 480,
                width = 480,
                plot_fun = plotting$objectplot,
                x = result, xlim=xylim_object, ylim=xylim_object,
                dim1=i, dim2=j
                )
            plotting$plot_png(
                paste0("figures/mca/discrim/discrimplot_dim_", i, "_", j, ".png"),
                height = 480,
                width = 480,
                plot_fun = plotting$discrimplot,
                x = result, xlim=xylim_discrim, ylim=xylim_discrim,
                dim1=i, dim2=j
                )
            }
        }
    }

# now, save the results as R object so we can work with it:
mca_res = result$objscores
save(mca_res, file="processed/mca_result.Rdata")
