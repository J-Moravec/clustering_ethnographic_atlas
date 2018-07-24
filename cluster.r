library("magrittr")

source("cluster_functions.r")

# read data
load("data/EA.Rdata")

# Ethnographic atlas is in variable EA
dim(EA)
# 1267 societies and 156 variables

# For more details on variables, see: selection.txt
#
# There are these basic categories:
# Subsistence v1--v5, v28--v29, v40--v42
# Sex differences: v44--v54
# Housing: v79--v88
# Descent: v17--v22, v27, v43
# Marriage: v6--v9, v10---v14 (PMR), v15--v16, v23--v26
# Linguistic affiliation: v97--v99
# Geographical area: v91--v92, v104, v106
# Climate: v95, v96
# Age and occupational specialization: v55--v65
# Class stratification and slavery: v66--v71
# Settlement pattern and size: v30--v31
# Belief and religion: v34, v112
# Political organization: v32--v33, v72--v73, v90, v94
# Inheritance of property: v74--v77
# Sex related taboos and traditions: v36--v38, v78
# Games: v35
# Societal rigidity: v113
#
# and useless category for various reasons: v89, v93, v107
#
################################################################################
# Step 1: Modifying of slavery
################################################################################
# First, we will modify slavery v71 variable:
# Slavery v71 have codes:
# NA missing data
# 1 absent or exists currently and in past
# 2 formerly present, but not currently existing
#
# Obviously, for clustering (or similarity), this is unusable.
# This would mean that societies without slavery or with slavery are more similar
# than societies that had slavery in past. That is nonsense.
# New coding should be instead:
# NA missing data
# 1 absent currently and in past
# 2 formerly present, but not currently existing
# 3 exists currently and in past
# This can be recovered from v70 "absence or near absence" of slavery.
# Following transformation needs to be done:
new_v71 = EA$v71
new_v71[EA$v70 == 1 & EA$v71 == 1] = 1
new_v71[EA$v70 != 1 & EA$v71 == 1] = 3
EA$v71 = new_v71

# list of classes with column names:
ea_classes = list(
    "subsistence" = c(1:5, 28:29, 40:42),
    "sex_differences" = c(44:54),
    "housing" = c(79:88),
    "descent" = c(17:22, 27, 43),
    "marriage" = c(6:9, 10:14, 15:16, 23:26),
    "linguistic_affiliation" = c(97:99),
    "geographical_area" = c(91:92, 104, 106),
    "climate" = c(95:96),
    "age_and_occupational_specialization" = c(55:65),
    "class_stratification_and_slavery" = c(66:71),
    "settlement_pattern_and_size" = c(30:31),
    "belief_and_religion" = c(34, 112),
    "political_organization" = c(32:33, 72:73, 90, 94),
    "inheritance_of_property" = c(74:77),
    "sex_related_taboos_and_traditions" = c(36:38, 78),
    "games" = c(35),
    "societal_rigidity" = c(113)
    )

ea_classes = sapply(ea_classes, function(x) paste0("v", x) )

classes_choice = c(
    "subsistence",
    "sex_differences",
    "housing",
    "descent",
    "marriage",
    #"linguistic_affiliation",
    #"geographical_area",
    "climate",
    "age_and_occupational_specialization",
    "class_stratification_and_slavery",
    "settlement_pattern_and_size",
    "belief_and_religion",
    "political_organization",
    "inheritance_of_property",
    "sex_related_taboos_and_traditions",
    "games",
    "societal_rigidity"
    )

chosen_variables = ea_classes[classes_choice] %>% unlist
# remove PMR (v10--v14)
PMR = paste0("v", 10:14)
chosen_variables = chosen_variables[!chosen_variables %in% PMR]
EA_filtered = EA[, chosen_variables]
rownames(EA_filtered) = EA$society

# calculate a distance matrix
distance_matrix = distance_matrix(EA_filtered)
