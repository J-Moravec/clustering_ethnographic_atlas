# Some utility functions

# shorthand for mkdir -p
# dir.create(showWarning=FALSE) could work as well, but it has a few problems
# notably that it doesn't show any other warnings
mkdir = function(path){
    if(!dir.exists(path)){
        dir.create(path, recursive=TRUE)
        }
    }


# Read and write named vector.
# using rds files instead would mean that these functions are not required and rds are already
# used for storing complex objects (lists, data.frames), but simple text file is much more friendly
write_named_vector = function(named_vector, file, column_names=NA){
    if(is.na(column_names)){
        column_names = c("name", "value")
        }

    matrix = cbind(names(named_vector), named_vector)
    colnames(matrix) = column_names
    rownames(matrix) = NULL

    write.table(matrix, file=file, row.names=FALSE, col.names=TRUE, quote=TRUE)
    }


read_named_vector = function(file, na_string="NA"){
    table = read.table(
        file, header=TRUE, stringsAsFactors=FALSE,
        check.names=FALSE, na.strings=na_string
        )
    named_vector = table[,2]
    names(named_vector) = table[,1]
    named_vector
    }


write_table = function(table, file){
    write.table(table,
        file=file,
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE,
        sep = " & ",
        eol = "\\\\\n"
        )
    }
