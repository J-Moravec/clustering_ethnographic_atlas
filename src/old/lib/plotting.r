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


.plot_phylo_collapsed = function(
    x, do_plot=FALSE, offset=0, show=FALSE, xlim=NULL, ylim=NULL,
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
        align.tip.label = TRUE,
        show.tip.label = show,
        label.offset = offset,
        cex = 2,
        )
    }


plot_collapsed = function(
    clustered, k, residences, offset=2.5, piecex=2,
    edge_width=3, show_label=TRUE, x_min = 0){
    # prepare data
    clusters = clustering$get_clusters(clustered, k)
    res_freq_table = clustering$clusters_res_freq_table(clusters, residences)
    collapsed_tree = clustering$collapse_tree(clustered, clusters)
    res_freq_table = res_freq_table[collapsed_tree$tip.label %>% as.numeric,]
    res_freq_colors = residence_colors[colnames(res_freq_table)]
    sizes = clustering$get_sizes(clusters)
    sizes = sizes[collapsed_tree$tip.label %>% as.numeric]

    .plot_phylo_collapsed(collapsed_tree)
    max_label_width = max(strwidth(collapsed_tree$tip.label))
    .offset = strwidth("m")*offset
    current_xlim = par("usr")[2]
    current_ylim = c(par("usr")[3:4])
    new_xlim = max_label_width + current_xlim + .offset
    new_ylim = current_ylim + c(-1,1)*strwidth("m") * 4
    .plot_phylo_collapsed(collapsed_tree, TRUE, .offset, show=show_label, xlim=c(x_min, new_xlim), ylim=new_ylim, edge_width = edge_width)
    tiplabels(pie=res_freq_table, piecol=res_freq_colors, cex=piecex)
    tiplabels(text=sizes, frame="none", col="white", font=2, cex=piecex*0.8)

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


plot_barplot = function(
    x, tr=0.1, cex=2, names=TRUE, axisnames=FALSE, sort=TRUE, ...
    ){
    par(mar = c(5, 5.5, 2, 0) + 0.1, mgp=c(3.5,1,0))
    if(sort){
        x = sort(x)
        }
    o = barplot(
        x, col="grey", border=NA, ylim=c(0,1),
        axes=FALSE, axisnames=axisnames, cex.names=cex, cex.lab=cex, ...
        )
    segments(0, tr, max(o)+o[1], tr, lwd=5, col=c("black"), lty=2)
    axis(2, las=1, cex.axis=cex)
    if(!is.na(tr)){
        axis(2, at=tr, las=1, cex.axis=cex, font=2)
        }
    if(names){
        pic1 = seq(1, length(o), 2)
        pic2 = seq(2, length(o), 2)
        mtext(names(x)[pic1], side=1, at=o[pic1], cex=1.5, line=1)
        mtext(names(x)[pic2], side=1, at=o[pic2], cex=1.5, line=3)    
        }
    }



