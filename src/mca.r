library("homals")
library("magrittr")
library("argparser")


# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/ea_variables.r", "ea_ordinal_variables")
import::from("lib/plot_utils.r", "plot_png")
import::from("lib/plot_utils.r", "plot_barplot")
import::from("lib/utils.r", "mkdir")
setwd(workdir)

main = function(input, figures, output, extract, explore){
    EA = readRDS(input)
    variable_type = get_variable_type(EA)
    mca_result = get_mca_result(EA, extract, variable_type)

    if(!is.na(figures)){
        plot_figures(mca_result, figures, explore)
        }

    if(!is.na(output)){
        save_mca_result(mca_result, output)
        }
    }


args_parser = function(){
    parser = arg_parser(
        "Performs a Multiple Correspondence Analysis (MCA) of Ethnographic Atlas"
        )
    parser = add_argument(
        parser, "input", type="character",
        help="Path to rds file containing Ethnographic Atlas",
        )
    parser = add_argument(
        parser, "--figures", type="character", default=NULL,
        help="Output folder for figures."
        )
    parser = add_argument(
        parser, "--output", type="character", default=NULL,
        help="Output path for filtered Ethnographic Atlas"
        )
    parser = add_argument(
        parser, "--extract", type="numeric", default=98,
        help="Number of extracted dimensions"
        )
    parser = add_argument(
        parser, "--explore", type="numeric", default=5,
        help="Number of dimensions that are explored with plots."
        )

    args = parse_args(parser)
    args
    }


#######################################################################
# Implementation:
#######################################################################
get_variable_type = function(EA){
    type = rep("nominal", ncol(EA))
    names(type) = colnames(EA)
    type[ea_ordinal_variables] = "ordinal"
    type
    }


get_mca_result = function(EA, ndim, variable_type){
    homals(EA, ndim=ndim, level=variable_type)
    }


save_mca_result = function(mca_result, file){
    saveRDS(mca_result, file)
    }



plot_figures = function(mca, folderpath, maxdim){
    # plot variability
    plot_variability(mca, folderpath)

    # define folders
    loaddir = file.path(folderpath, "loads")
    objectdir = file.path(folderpath, "objects")
    discrimdir = file.path(folderpath, "discrim")

    # create folders
    mkdir(loaddir)
    mkdir(objectdir)
    mkdir(discrimdir)

    # plot limits
    xylim_load = sapply(mca$loadings, function(x) x[1, 1:maxdim])
    xylim_load = c(min(xylim_load, 0), max(xylim_load, 0))*1.1
    xylim_object = range(mca$objscores[, 1:maxdim])*1.1
    xylim_discrim = range(mca$discrim[, 1:maxdim])*1.1

    # plot
    for(i in 1:maxdim){
        for(j in i:maxdim){
            if(i != j){
                plot_loadplot(mca, i, j, loaddir, lim=xylim_load)
                plot_objectplot(mca, i, j, objectdir, lim=xylim_object)
                plot_discrimplot(mca, i, j, discrimdir, lim=xylim_discrim)
                }
            }
        }
    }


plot_variability = function(mca, folderpath){
    variability = cumsum(mca$eigenvalues/sum(mca$eigenvalues))
    names(variability) = seq_along(variability)

    plot_png(
        file.path(folderpath, "eigen_var.png"),
        height = 480,
        width = 1024,
        plot_fun = plot_barplot,
        x = variability,
        names = FALSE,
        axisnames = TRUE,
        tr = NA,
        ylab = "percentage of explained variability",
        xlab = "eigenvalue",
        space = 0,
        sort = FALSE
        )
    }


plot_loadplot = function(mca, i, j, dir, lim){
    filename = paste0("loadplot_dim_", i, "_", j, ".png")
    path = file.path(dir, filename)
    plot_png(
        path, height = 480, width = 480,
        plot_fun = loadplot,
        x = mca, xlim = lim, ylim = lim,
        dim1=i, dim2=j
        )
    }


plot_objectplot = function(mca, i, j, dir, lim){
    filename = paste0("objectplot_dim_", i, "_", j, ".png")
    path = file.path(dir, filename)
    plot_png(
        path, height = 480, width = 480,
        plot_fun = objectplot,
        x = mca, xlim = lim, ylim = lim,
        dim1=i, dim2=j
        )
    }


plot_discrimplot = function(mca, i, j, dir, lim){
    filename = paste0("objectplot_dim_", i, "_", j, ".png")
    path = file.path(dir, filename)
    plot_png(
        path, height = 480, width = 480,
        plot_fun = discrimplot,
        x = mca, xlim = lim, ylim = lim,
        dim1=i, dim2=j
        )
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


######################################################################
# Execution (needs to be at the end)
######################################################################
if(!interactive()){
    args = args_parser()
    main(
        input = args$input,
        output = args$output,
        figures = args$figures,
        extract = args$extract,
        explore = args$explore
        )
    }
