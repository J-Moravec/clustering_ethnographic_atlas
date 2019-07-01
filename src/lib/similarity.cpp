#include <Rcpp.h>
using namespace Rcpp;



int similarity_pair(int x, int y){
    if( x == NA_INTEGER  || y == NA_INTEGER ){
        return(NA_INTEGER);
        }

    if( x == y ){
        return(1);
        } else {
        return(0);
        }
    }


IntegerVector similarity_vec(IntegerVector x, IntegerVector y){
    int n = x.size();
    IntegerVector similarity(n);

    for(int i = 0; i < n; i++){
        similarity[i] = similarity_pair(x[i], y[i]);
        }
    return(similarity);
    }


double similarity_norm(IntegerVector x, IntegerVector y){
    int n = x.size();
    int ones = 0;
    int zeros = 0;
    IntegerVector similarity = similarity_vec(x, y);

    for(int i = 0; i < n; i++){
        if(similarity[i] == 1){
            ones += 1;
            } else if(similarity[i] == 0){
            zeros += 1;
            }
        }
    return( (double) ones / (ones + zeros) );
    }


// [[Rcpp::export]]
NumericMatrix cpp_similarity_matrix(IntegerMatrix x){
    int n = x.nrow();
    double dist;
    NumericMatrix mat(n, n);

    for(int i = 0; i < n; i++){
        for(int j = i; j < n; j++){
            dist = similarity_norm( x(i, _), x(j, _) );
            mat(i, j) = dist;
            mat(j, i) = dist;
            }
        }
    return(mat);
    }
