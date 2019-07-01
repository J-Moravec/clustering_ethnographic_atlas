import::from("ea_variables.r", "ea_selected_variables")

# modify_slavery
# Slavery v71 have codes:
#  NA missing data
# 1 absent or exists currently and in past
# 2 formerly present, but not currently existing
#
# Turning into:
# NA missing data
# 1 absent currently and in past
# 2 formerly present, but noc currently existing
# 3 exist currently and in past
# This can be done through v70 "absence or near absence" of slavery
modify_slavery = function(EA){
    new_v71 = EA$v71
    new_v71[EA$v70 == 1 & EA$v71 == 1] = 1
    new_v71[EA$v70 != 1 & EA$v71 == 1] = 3
    EA$v71 = new_v71
    EA
    }


# removes PMR from v-named variables
# currently unused?
remove_PMR = function(EA){
    PMR = paste0("v", 10:14)
    EA = EA[!names(EA) %in% PMR]
    EA
    }


# select variables for further processing
# seee ea"variables.r for more information
select_variables = function(EA){
    EA = EA[, ea_selected_variables()]
    EA
    }


# get residence from the Ethnographic Atlas
# variables must be already v-named
get_residence = function(EA){
    residence = residence_mapping[EA$v12]
    residence[is.na(residence)] = "NA" # -- this helps during color codding in graphs
    names(residence) = rownames(EA)
    residence
    }


residence_mapping = c(
    "Avunculocal" = "Matrilocal",
    "Ambilocal" = "Ambilocal",
    "Optionally uxorilocal or avunculocal" = "Matrilocal",
    "Optionally patrilocal" = "Patrilocal",
    "Matrilocal" = "Matrilocal",
    "Neolocal" = "Neolocal",
    "No common residence" = "Neolocal",
    "Patrilocal" = "Patrilocal",
    "Uxorilocal" = "Matrilocal",
    "Virilocal" = "Patrilocal"
    )
