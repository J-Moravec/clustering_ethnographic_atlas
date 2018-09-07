library("ape")

source("module.r")
clustering = module("clustering.r")


### custom tanglegram from https://stackoverflow.com/a/12476186 modified
scale_coords = function(coords, limit){
    scaling_factor = (limit - 1)/(max(coords) - 1)
    coords = (coords - 1)*scaling_factor + 1
    coords
    }


tanglegram = function(tree1, tree2, cluster1, cluster2, text1="", text2=""){
    ntips1 = length(tree1$tip.label)
    ntips2 = length(tree2$tip.label)

    # a common scale
    ymax = max(ntips1, ntips2)

    # define layout of image
    layout(matrix(1:5,nrow=1),width=c(5,1,3,1,5))

    # various parameters:
    thickness_factor = 10 # arrow thickness
    text_cex = 5
    left_names_offset = 2
    similarity_offset = 4
    tree_lwd = 8
    tree_color = "darkgrey"
    spacing = 0.06
    arrow_length = 0.2
    min_thickness = 1
    # Tree 1 plot -----------------------------------------------------------------#
    par(mar=c(1,3,6,0))
    plot.phylo(
        tree1,
        show.tip.label=FALSE,
        edge.width=tree_lwd,
        edge.color=tree_color
        )
    # get tip coordinates
    plot_env = get("last_plot.phylo", envir = .PlotPhyloEnv)
    ytips1 = plot_env$yy[1:ntips1]
    # convert y coordinates to a common factor:
    ytips1 = scale_coords(ytips1, ymax)
    xlim = plot_env$x.lim
    ylim = plot_env$y.lim
    text(mean(xlim), max(ylim), label=text1, pos=3, cex=text_cex, xpd=TRUE, offset=similarity_offset)

    # Tree 1 labels ---------------------------------------------------------------#
    par(mar=c(1,0,6,0))
    plot(NA, bty="n", axes=FALSE, xlim=c(0,1), ylim=c(1,ymax), ylab="", xlab="")
    text(0, ytips1, labels=tree1$tip.label, pos=4, cex=text_cex, offset=left_names_offset)

    # Tree 2 plot -----------------------------------------------------------------#
    par(mfg=c(1,5))
    par(mar=c(1,0,6,3))
    plot.phylo(
        tree2,
        direction="leftwards",
        show.tip.label=FALSE,
        edge.width=tree_lwd,
        edge.color=tree_color
        )
    # get tip coordinates
    plot_env = get("last_plot.phylo", envir = .PlotPhyloEnv)
    ytips2 = plot_env$yy[1:ntips2]
    xlim = plot_env$x.lim
    ylim = plot_env$y.lim
    # convert y-coordinates to a common factor:
    ytips2 = scale_coords(ytips2, ymax)
    text(mean(xlim), max(ylim), label=text2, pos=3, cex=text_cex, xpd=TRUE, offset=similarity_offset)


    # Tree 2 labels ---------------------------------------------------------------#
    par(mfg=c(1,4))
    par(mar=c(1,0,6,0))
    plot(NA, bty="n", axes=FALSE, xlim=c(0,1), ylim=c(1, ymax), ylab="", xlab="")
    text(0, ytips2, labels=tree2$tip.label, pos=4, cex=text_cex)


    # Arrows ----------------------------------------------------------------------#
    par(mfg=c(1,3))
    par(mar=c(1,0,6,0))
    plot(NA, bty="n",axes=FALSE,xlim=c(0,1), ylim=c(1,ymax),ylab="",xlab="")
    # from right to left:
    thickness1 = clustering$clusters_comparison(cluster1, cluster2)
    for(i in 1:ntips1){
        for(j in 1:ntips2){
            if(thickness1[[i]][j] != 0){
                arrows(0, ytips1[i]-spacing, 1, ytips2[j]-spacing, length=arrow_length, lwd=thickness1[[i]][j]*thickness_factor+min_thickness, col=adjustcolor("red", alpha.f=0.7))
                }
            }
        }
    # from left to right
    thickness2 = clustering$clusters_comparison(cluster2, cluster1)
    for(i in 1:ntips2){
        for(j in 1:ntips1){
            if(thickness2[[i]][j] != 0){
                arrows(1, ytips2[i]+spacing, 0, ytips1[j]+spacing, length=arrow_length, lwd=thickness2[[i]][j]*thickness_factor+min_thickness, col=adjustcolor("blue", alpha.f=0.7))
                }
            }
        }
    similarity = clustering$clusters_similarity(cluster1, cluster2)
    similarity = formatC(similarity, digits=2, format="f")
    text(0.5, ymax, labels=paste0("sim=",similarity), pos=3, cex=text_cex, xpd=TRUE, offset=similarity_offset)
    }
