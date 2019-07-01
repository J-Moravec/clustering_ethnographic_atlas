# prepare_ea.r
# Filters and prepare R's version of Ethnographic Atlas
# 1. Fixes some errors in EA coding
# 2. Modify slavery variable
# 3. selection of variables of interest
library("argparser")
import::from("src/lib/prepare.r", "modify_slavery")
import::from("src/lib/prepare.r", "select_variables")
import::from("src/lib/prepare.r", "get_residence")
import::from("src/lib/utils.r", "write_named_vector")


args_parser = function(){
    parser = arg_parser(
        "Parse and filter R's EA.Rdata obtained from pydplace API"
        )
    parser = add_argument(
        parser, "input", type="character",
        help = "path to dplace csv file",
        )
    parser = add_argument(
        parser, "output", type="character",
        help = "output file"
        )
    parser = add_argument(
        parser, "--residence", flag=TRUE,
        help = "if specified, only residence information is parsed"
        )
    args = parse_args(parser)
    args
    }


main = function(input, output, residence){
    load(input)
    names = EA$society
    rownames(EA) = names

    if(residence){
        residences = get_residence(EA)
        write_named_vector(residences, output)
        } else {

        EA = remove_wrong_na(EA)
        EA = modify_slavery(EA)
        EA = select_variables(EA)
        saveRDS(EA, file=output)
        }
    }


# remove_wrong_na
# some variables are incorrectly coded as 0 instead of NA
remove_wrong_na = function(EA){
    wrong_na = c("v34","v81","v86","v90","v94","v95","v96")
    EA[wrong_na][EA[wrong_na] == 0] == NA
    EA
    }


if(!interactive()){
	args = args_parser()
	main(args$input, args$output, args$residence)
	}
