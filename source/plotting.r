

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

major_residences_colors = c(
    "Matrilocal" = "red",
    "Patrilocal" = "blue",
    "Ambilocal" = "orange",
    "Neolocal" = "green"
    )

plot = function(x){
    

    }


major_residence_to_color = function(x){
    y = major_residence_mapping[x]
    y = major_residences_colors[y]
    y[is.na(y)] = "black"
    return(y)
    }

residence_mapping = function(){
    
