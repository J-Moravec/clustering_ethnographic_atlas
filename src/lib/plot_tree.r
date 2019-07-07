# plot_tree.r
#
# Function for plotting collapsed trees
library("ape")
library("phytools", quietly=TRUE)

import::from("clusters.r", clusters_res_freq_table)
import::from("clusters.r", get_clusters)
import::from("clusters.r", get_sizes)


plot_collapsed_tree = function(
    clustered, k, residences, offset=2.5, piecex=2,
    edge_width=5, show_label=TRUE, x_min = 0){
    # prepare data
    clusters = get_clusters(clustered, k)
    # scale letter pie and letter size according to number of clusters
    piecex = 0.1 + 20 / (length(clusters) + 2)
    tipcex = 0.1 + 40 / (length(clusters) + 2)
    legendcex = 2.5 + 10/(length(clusters) + 2)
    x_min = -7/(10*length(clusters))

    res_freq_table = clusters_res_freq_table(clusters, residences)
    collapsed_tree = collapse_tree(clustered, clusters)
    res_freq_table = res_freq_table[collapsed_tree$tip.label %>% as.numeric,]
    res_freq_colors = residence_colors[colnames(res_freq_table)]
    sizes = get_sizes(clusters)
    sizes = sizes[collapsed_tree$tip.label %>% as.numeric]


    .plot_phylo_collapsed(collapsed_tree, cex=tipcex)
    max_label_width = max(strwidth(collapsed_tree$tip.label))
    .offset = strwidth("m") * tipcex * offset + abs(x_min)*tipcex*0.01
    current_xlim = par("usr")[2]
    current_ylim = c(par("usr")[3:4])
    new_xlim = max_label_width + current_xlim + .offset*1.5
    new_ylim = current_ylim + c(-1,1)*strwidth("m")*tipcex
    .plot_phylo_collapsed(collapsed_tree, TRUE, .offset, show=show_label, xlim=c(x_min, new_xlim), ylim=new_ylim, edge_width = edge_width, cex=tipcex)
    par("fg" = NA) # color for piegraphs
    tiplabels(pie=res_freq_table, piecol=res_freq_colors, cex=piecex,)
    par("fg" = "black")
    tiplabels(text=sizes, frame="none", col="white", font=2, cex=piecex*0.8)

    legend = names(residence_colors)
    legend_colors = residence_colors
    plot_legend(legend=legend, colors=legend_colors, cex=legendcex) 
    }


.plot_phylo_collapsed = function(
    x, do_plot=FALSE, offset=0, show=FALSE, xlim=NULL, ylim=NULL, cex,
    edge_width = 3    
    ){
    plot.phylo(
        x,
        edge.width = edge_width,
        edge.color = "darkgrey",
        font = 1,
        plot = do_plot,
        no.margin = TRUE,
        x.lim = xlim,
        y.lim = ylim,
        align.tip.label = 0,
        show.tip.label = show,
        label.offset = offset,
        cex = cex
        )
    }


collapse_tree = function(x, clusters){
    n = length(clusters)
    tree = as.phylo(x)
    for(i in 1:n){
        if(length(clusters[[i]]) != 1){
            tree = drop.clade(tree, clusters[[i]])
            tree$tip.label[tree$tip.label == "NA"] = i
            } else {
            tree$tip.label[tree$tip.label == clusters[[i]] ] = i
            }
        }    
    return(tree)
    }


# red, blue, green, purple
# In future, possible to extend by other colors
residence_colors = c(
    "Matrilocal" = "#E21E26",
    "Patrilocal" = "#1F9DD9",
    "Ambilocal" = "#0A9748",
    "Neolocal" = "#942581",
    "NA" = "black"
    )


residence_to_color = function(residences){
    y = residence_colors[residences]
    y[is.na(y)] = "black"
    return(y)
    }


plot_legend = function(legend_pos = "topleft", legend, colors, cex=3){
    par(mar=c(0,0,0,0))
    plot.window( c(0,100), c(0,100) )
    legend(
        legend_pos,
        pch = 16,
        col = colors,
        legend = legend,
        bty = "n",
        border = NA,
        bg = "transparent",
        xpd = TRUE,
        cex = cex
        )
    }
