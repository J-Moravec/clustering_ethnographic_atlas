# plot_utils.r
# Helper functions for plotting

plot_png = function(filename, width, height, plot_fun, ...){
    png(filename=filename, width=width, height=height)
    plot_fun(...)
    invisible(dev.off())
    }


plot_pdf = function(filename, width, height, plot_fun, ...){
    pdf(file=filename, width=width, height=height, onefile=FALSE)
    plot_fun(...)
    invisible(dev.off())
    }


plot_barplot = function(
    x, tr=0.1, cex=2, names=TRUE, axisnames=FALSE, sort=TRUE, ...
    ){
    par(mar = c(5, 5.5, 2, 0) + 0.1, mgp=c(3.5,1,0))
    if(sort) x = sort(x)
    o = barplot(
        x,
        col = "grey",
        border = NA,
        ylim = c(0,1),
        axes = FALSE,
        axisnames = axisnames,
        cex.names = cex,
        cex.lab = cex,
        ...
        )
    segments(0, tr, max(o)+o[1], tr, lwd=5, col=c("black"), lty=2)
    axis(2, las=1, cex.axis=cex)
    if(!is.na(tr)) axis(2, at=tr, las=1, cex.axis=cex, font=2)
    if(names){
        pic1 = seq(1, length(o), 2)
        pic2 = seq(2, length(o), 2)
        mtext(names(x)[pic1], side=1, at=o[pic1], cex=1.5, line=1)
        mtext(names(x)[pic2], side=1, at=o[pic2], cex=1.5, line=3)
        }
    }
