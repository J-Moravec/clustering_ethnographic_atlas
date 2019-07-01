# In this script, Ethnographic Atlas is explored and variables are filtered
# for the use in MCA

library("magrittr")
source("source/module.r")

filtering = module("source/filtering.r")

plotting = module("source/plotting.r")

load("data/EA.Rdata")

# modify slavery variable:
EA = filtering$modify_slavery(EA)

# filter EA to get chosen variables: (filtered EA)
fEA = filtering$filter(EA)

# dimension of filtered ethnographic atlas:
dim(fEA)

# percentage of unknown values:
n_unknown = sum(is.na(fEA)) / (nrow(fEA) * ncol(fEA))
# around 29.15% of data in fEA
cat("Unknown before:", n_unknown, "\n")
# exploration of diversity of chosen variables:
freqs = sapply(fEA, table)
vars = names(freqs)
n_cat = length(unlist(freqs)) # 568
cat("Categories before:", n_cat, "\n")

# We can summarize categories across variables according to following statistics:
# normalized gini simpson diversity
diversity_normalized = sapply(freqs, filtering$gini_simpson_normalized)
# amount of mass in greatest and smallest category:
max_mass_cat = sapply(freqs, function(x) max(x/sum(x)))
min_mass_cat = sapply(freqs, function(x) min(x/sum(x)))

# percentage of unknown category (i.e., NA)
na_mass = sapply(fEA, function(x) sum(is.na(x))/length(x) )

# This gives us an idea how are values and categories distributed in fEA.
# For MCA, some authors suggest to remove low-frequency categories, since they
# will become dimensions in MCA and can squew the results, the problematic
# variables are these:

bad = c()
# variables with very low diversity
bad = c(bad, vars[diversity_normalized < 0.1])
# variables where the total mass is monopolized by a single category
bad = c(bad, vars[max_mass_cat > 0.9])
# variables with very high amount of unknown values:
bad = c(bad, vars[na_mass > 0.9])
# Variables with a low count are more problematic, we will deal with them lated.
bad = sort(unique(bad))
# Following 15 variables are to be removed:
# v20 v22 v61 v62 v64 v65 v67 v69 v18 v56 v58 v59 v60 v63 v113
# These variables code:
# v18 Largest Patrilineal Exogamous Group (If Different from Variable v17)
# v20 Largest Matrilineal Exogamous Group (If Different from Variable v19)
# v22 Secondary Cognatic Kin Group: Kindreds and Ramages
# v56 Age or Occupational Specialization: Weaving
# v58 Age or Occupational Specialization: Pottery Making
# v59 Age or Occupational Specialization: Boat Building
# v60 Age or Occupational Specialization: House Construction
# v61 Age or Occupational Specialization: Gathering
# v62 Age or Occupational Specialization: Hunting
# v63 Age or Occupational Specialization: Fishing
# v64 Age or Occupational Specialization: Animal Husbandry
# v65 Age or Occupational Specialization: Agriculture
# v67 Class Stratification, Secondary Features
# v69 Class Stratification (Endogamy), Secondary Type
# v113 Societal Rigidity

# We will now look at low-frequency variables:
rel_freqs = sapply(freqs, function(x) x/sum(x))
rel_freqs_all = sort(unlist(rel_freqs))
names(rel_freqs_all) = seq_along(rel_freqs_all)




# Plot of images:
plotting$plot_png(
    "figures/filtering/gini_simpson_norm.png",
    height = 1024,
    width = 2048,
    # plotting function:
    plot_fun = plotting$plot_barplot,
    x = diversity_normalized,
    ylab="normalized Gini-Simpson diversity"
    )


plotting$plot_png(
    "figures/filtering/smallest_category_perc.png",
    height = 1024,
    width = 2048,
    plot_fun = plotting$plot_barplot,
    x = min_mass_cat,
    tr = 0.01,
    ylab = "relative frequency"
    )


plotting$plot_png(
    "figures/filtering/largest_category_perc.png",
    height = 1024,
    width = 2048,
    plot_fun = plotting$plot_barplot,
    x = max_mass_cat,
    tr = 0.9,
    ylab = "relative frequency"
    )


plotting$plot_png(
    "figures/filtering/unknown_values_perc.png",
    height = 1024,
    width = 2048,
    plot_fun = plotting$plot_barplot,
    x = na_mass,
    tr = 0.9,
    ylab = "percentage"
    )

plotting$plot_png(
    "figures/filtering/low_frequency_categories.png",
    height = 1024,
    width = 2048,
    plot_fun = plotting$plot_barplot,
    x = rel_freqs_all,
    names = FALSE,
    axisnames = TRUE,
    tr = NA,
    ylab = "relative frequency",
    xlab = "order",
    space = -0.01
    )

# Without bad variables, the new fEA looks in a following way:
fEA = filtering$filter(EA, filter_bad=TRUE)
dim(fEA) # 1267 73
n_unknown = sum(is.na(fEA)) / (nrow(fEA) * ncol(fEA)) # 28.62%
freqs = sapply(fEA, table)
n_cat = length(unlist(freqs)) # 524
cat("Unknown after:", n_unknown, "\n")
cat("Categories after:", n_cat, "\n")
