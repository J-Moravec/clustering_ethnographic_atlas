# get and transform post-marital residence data
# from the original Ethnographic Atlas
# TODO no longer required, moved to "prepare.r", delete?"

library("argparser")
import::from("src/lib/utils.r", "write_named_vector")


main = function(input, output){
    load(input)
    residence = get_residence(EA)
    write_named_vector(residence, output)
    }


args_parser = function(){
    parser = arg_parser("Extract residence from Ethnographic Atlas")
    parser = add_argument(
        parser, "input", type="character",
        help="EA.Rdata file with Ethnographic Atlas as EA variable."
        )
    parser = add_argument(
        parser, "output", type="character",
        help="Output file for named vector of residences."
        )

    args = parse_args(parser)
    args
    }


get_residence = function(EA){
    residence = residence_mapping[EA$v12]
    residence[is.na(residence)] = "NA" # -- this helps during color codding in graphs
    names(residence) = EA$society
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


if(!interactive()){
    args = args_parser()
    main(args$input, args$output)
    }
