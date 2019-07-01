# ea_categories.r
# Function and variables that describe data in Ethnographic Atlas

ea_classes = list(
    "subsistence" = c(1:5, 28:29, 39:42),
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

ea_classes_vnamed = sapply(ea_classes, function(x) paste0("v", x) )


ea_classes_selected = c(
    "subsistence",
    "sex_differences",
    "housing",
    "descent",
    "marriage",
    #"linguistic_affiliation",
    #"geographical_area",
    #"climate", -- not in dplace
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


ea_ordinal_variables = paste0(
    "v",
    c(1:5, 28, 30, 31:34, 36, 41, 71)
    )


ea_selected_variables = function(){
    vars = ea_classes_vnamed[ea_classes_selected]
    vars = unlist(vars)
    vars
    }
