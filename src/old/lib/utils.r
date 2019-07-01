# Some utility functions

# shorthand for mkdir -p (kind of)
# dir.create(showWarning=FALSE) could work as well, but it has a few problems
# To not repeat myself all the time
mkdir = function(path){
    if(!dir.exists(path)){
        dir.create(path, recursive=TRUE)
        }
    }
