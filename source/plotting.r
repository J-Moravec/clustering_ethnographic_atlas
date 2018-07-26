library("ape")

major_residence_mapping = c(
    "Avunculocal" = "Matrilocal",
    "Ambilocal" = "Ambilocal",
    "Optionally uxorilocal or avunculocal" = "Matrilocal",
    "Optionally patrilocal" = "Patrilocal",
    "Matrilocal" = "Matrilocal",
    "Neolocal" = "Neolocal",
    "No common residence" = "Neolocal",
    "Patrilocal" = "Patrilocal",
    "Uxorilocal" = "Matrilocal",
    "Virilocal" = "Patrilocal"
    )


# red, blue, green, purple
major_residence_colors = c(
    "Matrilocal" = "#E21E26",
    "Patrilocal" = "#1F9DD9",
    "Ambilocal" = "#0A9748",
    "Neolocal" = "#942581"
    )

major_residence_to_color = function(x){
    y = major_residence_mapping[x]
    y = major_residence_colors[y]
    y[is.na(y)] = "black"
    return(y)
    }

plot_legend = function(legend_pos = "topleft", legend, colors){
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
        cex = 3
        )
    }


plot = function(x, residence){
    par(mar = c(0,0,0,0))
    x = as.phylo(x)
    tip_names = x$tip.label
    legend = names(major_residence_colors)
    legend_colors = major_residence_colors
    tip_colors = major_residence_to_color(residence)
    branch_colors = "grey"
    font_size = 0.8

    .plot_tree = function(do_plot=FALSE, offset=0, show=FALSE, xlim=NULL){
        plot.phylo(
            x,
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


plot_png = function(x, residence, filename="cluster.png", width=1024, height=10240, ...){
    png(filename, width=width, height=height, ...)
    plot(x, residence)
    invisible(dev.off())
    }



