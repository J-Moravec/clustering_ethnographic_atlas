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


loadplot = function(x, dim1=1, dim2=2, ...){
    par(mar = c(5, 4, 1, 1) + 0.1)
    coords = t(sapply(x$loadings, function(xy) xy[1, c(dim1, dim2)]))
    args = list(...)
    args.default = list(
        xlim = c(min(coords[,1],0), max(coords[,1],0) )*1.2,
        ylim = c(min(coords[,2],0), max(coords[,2],0) )*1.2,
        xlab = paste("Dimension", dim1),
        ylab = paste("Dimension", dim2),
        main = "", axes=FALSE
        )
    args = merge_list(args, args.default)
    args$x=0
    args$type="n"
    do.call(plot, args)
    axis(1, las=1, col=NA, col.ticks="black")
    axis(2, las=1, col=NA, col.ticks="black")
    new_black = adjustcolor("black", alpha.f=0.7)
    abline(h=0,v=0, col=new_black, lty=2, lwd=1.5)
    points(coords, pch=20)
    posvec = apply(coords, 1, sign)[2,] + 2
    nvar = dim(x$dframe)[2]
    for(i in 1:nvar){
        arrows(0, 0, coords[i,1], coords[i,2], length = 0.08)
        }
    text(coords, labels = rownames(coords), pos = posvec, cex = 1)
    }


objectplot = function(x, dim1=1, dim2=2, main="", ...){
    par(mar = c(5, 4, 1, 1) + 0.1)
    coords = t(sapply(x$loadings, function(xy) xy[1, c(dim1, dim2)]))
    args = list(...)
    args.default = list(
        xlim = range(x$objscores[, dim1]),
        ylim = range(x$objscores[, dim2]),
        xlab = paste("Dimension", dim1),
        ylab = paste("Dimension", dim2),
        main = "", axes=FALSE
        )
    args = merge_list(args, args.default)
    args$x=0
    args$type="n"
    do.call(plot, args)
    axis(1, las=1, col=NA, col.ticks="black")
    axis(2, las=1, col=NA, col.ticks="black")
    new_black = adjustcolor("black", alpha.f=0.7)
    abline(h=0,v=0, col=new_black, lty=2, lwd=1.5)
    text(x$objscores[, c(dim1, dim2)], rownames(x$objscores), cex = 1, xpd=TRUE)
    }


merge_list = function(x, y){ # pulled from RCurl
    if (length(x) == 0) return(y)
    if (length(y) == 0) return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i)){
        x[names(y)[which(i)]] = y[which(i)]
        }
    x
    }



discrimplot = function(x, dim1=1, dim2=2, ...){
    par(mar = c(5, 5, 1, 1) + 0.1)
    xylim = range(x$discrim[, c(dim1, dim2)])
    args = list(...)
    args_default = list(
        type = "p", pch = 20, cex = 1.2, main = "",
        xlab = paste("Dimension", dim1),
        ylab = paste("Dimension", dim2),
        xlim = xylim, ylim=xylim, axes=FALSE
        )
    args = merge_list(args, args_default)
    ylab = args$ylab
    args$ylab = ""
    args$x=x$discrim[, c(dim1, dim2)]
    do.call(plot, args)
    box(bty="l")
    axis(1, las=1, col=NA, col.ticks="black")
    axis(2, las=1, col=NA, col.ticks="black")
    title(ylab=ylab, line=4)
    nvar = dim(x$dframe)[2]
    for(i in 1:nvar){
        lines( rbind(x$discrim[i,c(dim1,dim2)],c(0,0)) )
        }
    text(x$discrim[,c(dim1,dim2)], rownames(x$discrim), pos = 3, xpd=TRUE)
    }
