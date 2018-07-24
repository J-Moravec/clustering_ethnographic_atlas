library("Rcpp")
Rcpp::sourceCpp("cluster_functions.cpp")

#similarity = function(x, y){
#    if( is.na(x) || is.na(y) ){
#        return(NA)
#        }

#    if( x == y ){
#        return(1)
#        } else {
#        return(0)
#        }
#    }


#similarity_vec = Vectorize(similarity)

#similarity_vec_norm = function(x, y){
#    sim = similarity_vec(x, y)
#    total = sum(sim, na.rm=TRUE)
#    norm = sum(!is.na(sim))
#    return(total/norm)
#    }


#similarity_matrix = function(x){
#    # Create a matrix (a similarity matrix)
#    n = nrow(x)
#    mat = matrix(NA, n, n)
#    colnames(mat) = x$society
#    rownames(mat) = x$society


#    # calculate similarities for all variables
#    # lets start with a for cycle:
#    for(i in 1:n){
#        cat("Processing: ", i, "/", n, sep="")
#        cat("\r")
#        for(j in i:n){

#            dist = similarity_vec_norm( x[i, ], x[j, ])
#            mat[i, j] = dist
#            mat[j, i] = dist
#            }
#        }
#    return(mat)
#    }


distance_matrix = function(x){
    names = rownames(x)
    matrix = as.matrix(x)
    mode(matrix) = "integer"
    
    similarity = cpp_similarity_matrix(matrix)
    distance = 1 - similarity
    rownames(distance) = names
    colnames(distance) = names
    }
