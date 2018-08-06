library("ape")

source("module.r")

clustering = module("clustering.r")

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



plot_tree = function(clustered, residences){
    par(mar = c(0,0,0,0))
    tree = as.phylo(clustered)
    tip_names = tree$tip.label
    legend = names(residence_colors)
    legend_colors = residence_colors
    tip_colors = residence_to_color(residences)
    branch_colors = "grey"
    font_size = 0.8

    .plot_tree = function(do_plot=FALSE, offset=0, show=FALSE, xlim=NULL){
        plot.phylo(
            tree,
            edge.width = 3,
            edge.color = branch_colors,
            tip.color = tip_colors,
            font = 1,
            plot = do_plot,
            no.margin = TRUE,
            x.lim = xlim,
            align.tip.label = TRUE,
            underscore = TRUE,
            show.tip.label = show,
            label.offset = offset,
            cex = font_size
            )
        }

    .plot_tree()
    max_label_width = max(strwidth(tip_names))
    offset = strwidth("m")*1
    current_xlim = par("usr")[2]
    new_xlim = max_label_width + current_xlim + offset
    .plot_tree(TRUE, offset, show=TRUE, xlim=new_xlim)

    tiplabels(col=tip_colors, pch=21, bg=tip_colors)
    plot_legend(legend=legend, colors=legend_colors)
    }


.plot_phylo_collapsed = function(x, do_plot=FALSE, offset=0, show=FALSE, xlim=NULL, ylim=NULL){
    plot.phylo(
        x,
        edge.width = 3,
        edge.color = "grey",
        font = 1,
        plot = do_plot,
        no.margin = TRUE,
        x.lim = xlim,
        y.lim = ylim,
        align.tip.label = FALSE,
        show.tip.label = show,
        label.offset = offset,
        cex = 2,
        )
    }


plot_collapsed = function(clustered, k, residences, offset=2.5, piecex=2){
    clusters = clustering$get_clusters(clustered, k)
    res_freq_table = clustering$clusters_res_freq_table(clusters, residences)
    collapsed_tree = clustering$collapse_tree(clustered, clusters)
    res_freq_colors = residence_colors[colnames(res_freq_table)]

    .plot_phylo_collapsed(collapsed_tree)
    max_label_width = max(strwidth(collapsed_tree$tip.label))
    .offset = strwidth("m")*offset
    current_xlim = par("usr")[2]
    current_ylim = c(par("usr")[3:4])
    new_xlim = max_label_width + current_xlim + .offset
    new_ylim = current_ylim + c(-1,1)*strwidth("m") * 4
    .plot_phylo_collapsed(collapsed_tree, TRUE, .offset, show=TRUE, xlim=new_xlim, ylim=new_ylim)
    tiplabels(pie=res_freq_table, piecol=res_freq_colors, cex=piecex)

    legend = names(residence_colors)
    legend_colors = residence_colors
    plot_legend(legend=legend, colors=legend_colors, cex=1.5) 
    }


plot_png = function(filename, width, height, plot_fun, ...){
    png(filename=filename, width=width, height=height)
    plot_fun(...)
    invisible(dev.off())
    }


plot_purity = function(purity){
    plot(purity, pch=19, frame.plot=FALSE)
    lines(purity, col="red", lwd=2, lty=2)
    }
