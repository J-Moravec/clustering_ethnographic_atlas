# maximal_purity.r
#
# Gets maximal purity table
# which should which cluster sizes maximize particular purities
library("argparser")
library("magrittr")

# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/utils.r", write_table)
setwd(workdir)



args_parser = function(){
    parser = arg_parser("Get maximal purity table")
    parser = add_argument(
        parser, "input", type="character",
        help="Path to rds file containing calculated purity functions"
        )
    parser = add_argument(
        parser, "output", type="character",
        help="Output file"
        )
    args = parse_args(parser)
    args
    }


main = function(input, output){
    purities = readRDS(input)
    table = max_purity_table(purities)
    write_table(table, output)
    }


max_purity_method = function(x){
    lapply(x, function(y) apply(y, 1, which.max))
    }


max_purity_table = function(x){
    tab = list()
    for(method in names(x)){
        y = x[[method]]
        subtab = max_purity_method(y)
        purity_names = names(subtab)
        subtab = do.call(rbind, subtab)
        # reorder:
        subtab = subtab[,c("0","0.25","0.5","1","2")]
        nrows = nrow(subtab)
        if(nrows == 1){
            method_name = method
            } else {
            method_name = c(method, rep("", nrows-1))
            }
        subtab = cbind(method_name, purity_names, subtab)
        tab[[method]] = subtab
        }
    do.call(rbind, tab)
    }



if(!interactive()){
    args = args_parser()
    main(args$input, args$output)
    }
