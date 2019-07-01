library("magrittr")

source("source/module.r")

clustering = module("source/clustering.r")
tanglegram = module("source/residence.r")

load("processed/cluster_results.Rdata")

n_ward = 32
n_complete = 24

clust_ward = tree_results[["ward.D"]]
clust_complete = tree_results[["complete"]]

c_ward = clustering$get_clusters(clust_ward, n_ward)
c_complete = clustering$get_clusters(clust_complete, n_complete)
tree_ward = clustering$collapse_tree(clust_ward, c_ward)
tree_complete = clustering$collapse_tree(clust_complete, c_complete)

sim_list = clustering$clusters_comparison(c_ward, c_complete)
sim_mat = do.call(rbind, sim_list)
rownames(sim_mat) = as.character(1:nrow(sim_mat))
colnames(sim_mat) = as.character(1:ncol(sim_mat))

plotting_order = function(tree){
    n_tips = length(tree$tip.label)
    tree$edge[tree$edge[,2] <= n_tips, 2]
    }

order_ward = plotting_order(tree_ward)
order_complete = plotting_order(tree_complete)

sim_mat_order = sim_mat[order_ward, order_complete]


cross_distance = function(matrix){
    n_row = nrow(matrix)
    n_col = ncol(matrix)
    max = max(n_col, n_row) - 1
    min = min(n_col, n_row) - 1

    values = round(min * (0:max) / max) + 1

    if(n_row > n_col){
        rows = 1:n_row
        cols = values
        } else {
        rows = values
        cols = 1:n_col
        }
    correction = table(rows)

    total = 0
    for(i in seq_along(values)){
        row = rows[i]
        col = cols[i]
        total = total + matrix[row, col] / correction[row]
        }
    total
    }
