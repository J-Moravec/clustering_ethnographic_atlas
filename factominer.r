

library("FactoMineR")
library("missMDA")
library("FactoInvestigate")
library("magrittr")
source("source/module.r")

filtering = module("source/filtering.r")
plotting = module("source/plotting.r")

# read filtered EA from previous thing
load("data/EA.Rdata")
EA = filtering$modify_slavery(EA)
fEA = filtering$filter(EA, filter_bad=TRUE)

# turn into factors:
ffEA = fEA
ffEA[] = lapply(fEA[], factor)

# Imputing missing values: this unfortunatelly fails:(
# from http://factominer.free.fr/course/missing.html
# nb = estim_ncpMCA(ffEA) # fails :(
# imputed = imputeMCA(ffEA, 100) # impute from first 100 dimensions, fails :(
mca = MCA(ffEA, ncp=524, graph=FALSE)
Investigate(mca)
