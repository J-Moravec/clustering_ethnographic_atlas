library("magrittr")
library("argparser")
library("ape")
library("phytools", quietly=TRUE)

scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/clusters.r", get_clusters)
import::from("lib/ea_variables.r", ea_classes_vnamed)
import::from("lib/utils.r", write_table)
import::from("lib/utils.r", mkdir)
import::from("lib/plot_utils.r", plot_pdf)
setwd(workdir)


args_parser = function(){
    parser = arg_parser(
        "Compare variables between clusters"
        )
    parser = add_argument(
        parser, "clustered", type="character",
        help = "path to rds file containing output from clustering"
        )

    parser = add_argument(
        parser, "--method", type="character",
        help = "clustering method to be extracted"
        )
    parser = add_argument(
        parser, "--k", type="character",
        help = "number of clusters to be extracted"
        )
    parser = add_argument(
        parser, "--ea", type="character",
        help = "ethnographic atlas with ethnographical variables"
        )
    parser = add_argument(
        parser, "--vars", type="character", nargs="Inf",
        help = "variables too be explored and tabularized"
        )
    parser = add_argument(
        parser, "--figures", type="character",
        help = "a folder for figures of variables"
        )
    parser = add_argument(
        parser, "--table", type="character",
        help = "path for comparison table"
        )
    args = parse_args(parser)
    if(is.na(args$method)) stop("Method parameter required")
    if(is.na(args$k)) stop("Parameter k required")
    if(is.na(args$ea)) stop("Parameter ea required")
    if(is.na(args$vars)) stop("Parameter vars required")
    args
    }


main = function(clustered, method, k, ea, vars, figures, table){
    vars = strsplit(vars, split=",", fixed=TRUE) %>% unlist
    clustered = readRDS(clustered)
    clustered = clustered[[method]]
    data = readRDS(ea)

    if(!is.na(figures)){
        mkdir(figures)
        }

    clusters = get_clusters(clustered, k)
    maximal_values = list()

    for(var in vars){
        clusters_freq = clusters_var_freq(clusters, var, data)
        if(!is.na(figures)){
            filename = paste0(method, "_k", k, "_", var, ".pdf")
            filename = file.path(figures, filename)
            plot_clusters_pie_chart(clusters_freq, filename)
            }
        maximal_values[[var]] = get_maximal_values(clusters_freq)
        }

    if(!is.na(table)){
        maximal_values_table(maximal_values, vars, table)
        }
    }


clusters_var_freq = function(clusters, var, data){
    lapply(clusters, cluster_var_freq, var=var, data=data, na.rm=FALSE)
    }


cluster_var_freq = function(cluster, var, data, na.rm=TRUE){
    vardata = data[[var]]
    names(vardata) = rownames(data)

    if(!na.rm){
        vardata[is.na(vardata)] = "NA"
        }

    vardata = factor(vardata)
    cluster_data = vardata[cluster]
    tab = table(cluster_data)
    tab = tab/sum(tab)
    tab
    }


colors = function(){
    colors = c(
        "#A6CEE3", "#1F78B4", "#B2DF8A",
        "#33A02C", "#FB9A99", "#E31A1C",
        "#FDBF6F", "#FF7F00", "#CAB2D6", "#000000"
        )
    names(colors) = as.character(1:10)
    names(colors)[10] = "NA"
    colors
    }


plot_clusters_pie_chart = function(clusters_freq, filename){
    plot_pdf(
        filename = filename,
        width = 12,
        height = 14,
        plot_fun = clusters_pie_chart,
        clusters_freq = clusters_freq
        )
    }


clusters_pie_chart = function(clusters_freq){
    legend_space = 30

    n = length(clusters_freq)
    mfrow_dim = square(n)
    colors = colors()
    colors = colors[names(clusters_freq[[1]])]

    par(mar=c(0,0,0,0), oma=c(0,legend_space, 0, 0), mfrow=mfrow_dim) 
    for(i in seq_len(n)){
        cluster_freq = clusters_freq[[i]]
        pie(cluster_freq, labels=NA, col=colors, border=NA, edges=1000)
        plot.window(c(0,100), c(0,100))
        text(50, 50, adj=0.5, label=as.character(i), cex=5, font=2, col="black")
        }

    par(mfrow=c(1,1))
    plot_legend("topleft", legend=names(cluster_freq), colors=colors, cex=3)
    }


plot_legend = function(legend_pos = "topleft", legend, colors, cex=3){
    par(mar=c(0,0,0,0), oma=c(0,0,0,0))
    plot.window(c(0,100), c(0,100))
    legend(
        legend_pos,
        pch = 16,
        col = colors,
        legend = legend,
        bty = "n",
        border = 1,
        bg = "transparent",
        xpd = NA,
        cex = cex
        )
    }


square = function(n){
    a = ceiling(sqrt(n))
    b = ceiling(n/a)
    c(a,b)
    }


get_maximal_values = function(clusters_freq){
    maximal_values = lapply(clusters_freq, get_maximal_value)
    do.call(rbind, maximal_values)
    }


get_maximal_value = function(cluster_freq){
    # remove NA
    cluster_freq = cluster_freq[names(cluster_freq) != "NA"]
    position = which.max(cluster_freq)
    name = names(cluster_freq)[position]
    value = cluster_freq[position] %>% "*"(., 100) %>% round
    paste0(name, " (", value, "%)")
    }


maximal_values_table = function(maximal_values, vars, filename){
    maximal_values = do.call(cbind, maximal_values)
    rownames = nrow(maximal_values) %>% seq_len %>% as.character
    colnames = c("", vars)
    maximal_values = cbind(rownames, maximal_values) %>% rbind(colnames, .)
    write_table(maximal_values, filename)
    }

if(!interactive()){
    args = args_parser()
    main(
        clustered = args$clustered,
        method = args$method,
        k = args$k,
        ea = args$ea,
        vars = args$vars,
        figures = args$figures,
        table = args$table
        )
    }
