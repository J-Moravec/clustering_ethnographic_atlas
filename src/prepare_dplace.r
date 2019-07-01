# prepare_dplace.r
# Filters and prepares dplace's version of Ethnographic Atlas
# data from dplace have different format then data from R version of ethnographic atlas
# this code unifies their forms so that both can be processed at once
# 1. Converts Dplace formatting to R's formatting of EA
# 2. Modify slavery variable
# 3. Selects variables of interests
library("argparser")
library("magrittr")
import::from("src/lib/prepare.r", "modify_slavery")
import::from("src/lib/prepare.r", "select_variables")
import::from("src/lib/prepare.r", "get_residence")
import::from("src/lib/utils.r", "write_named_vector")


args_parser = function(){
    parser = arg_parser(
        "Parse and filter dplace.csv output obtained from pydplace API"
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
    data = read_data(input)
    EA = parse_data(data)

    if(residence){
        residences = get_residence(EA)
        #print(residences)
        write_named_vector(residences, output)
        } else {
        EA = modify_slavery(EA)
        EA = select_variables(EA)
        saveRDS(EA, file=output)
        }
     }


read_data = function(filename){
    data = read.table(
        filename, header=TRUE, sep=",",
        stringsAsFactors=FALSE, quote="\""
        )
    data
    }


get_variable = function(data, variable){
    subset = data[data$Variable == variable,]
    variable = subset$Value
    names(variable) = subset$Name
    variable
    }


rename_variables = function(names){
    # rename variables from EAXXX format to vX format
    names = sub("EA", "", names) %>% as.numeric
    names = paste0("v", names)
    names
    }


parse_data = function(data){
    names = unique(data$Name)
    variables = unique(data$Variable)
    EA = data.frame(row.names=names)
    
    for(variable in variables){
        subset = get_variable(data, variable)
        subset = subset[names] # reorder accoring to names
        EA[[variable]] = subset
        }

    colnames(EA) = rename_variables(colnames(EA))
    EA
    }


if(!interactive()){
    args = args_parser()
    main(args$input, args$output, args$residence)
    }
