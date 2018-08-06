major_residence_mapping = c(
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


get_residences = function(EA){
    y = major_residence_mapping[EA$v12]
    y[is.na(y)] = "NA"
    names(y) = EA$society
    return(y)
    }
