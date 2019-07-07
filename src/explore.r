# explore.r
# (Graphical) eploration of chosen variables
# Output is text summary and number of figures.
library("argparser", quiet=TRUE)
library("magrittr")

# Import functions:
# This code change workdir for import
# so that a local path can be used instead:
scriptPath = function(){
    commandArgs(FALSE)[4] %>% sub("--file=", "", .) %>% dirname
    }
workdir = getwd()
setwd(scriptPath())
import::from("lib/plot_utils.r", "plot_png")
import::from("lib/plot_utils.r", "plot_barplot")
setwd(workdir)

main = function(input, summary, figures, output){
    EA = readRDS(input)
    # for whole dataset:
    summary_dataset = summary_dataset(EA)
    # for each variable:
    summary_variables = summary_variables(EA)
    bad_variables = bad_variables(summary_variables)
    filtered_dataset = filter_bad(EA, bad_variables)
    summary_filtered_dataset = summary_dataset(filtered_dataset)
    if(!is.na(summary)){
        write_summary(
            dataset = summary_dataset,
            filtered_dataset = summary_filtered_dataset,
            variables = summary_variables,
            bad = bad_variables,
            file = summary
            )
        }

    if(!is.na(figures)){
        plot_summary_figures(summary_variables, folder=figures)
        }


    if(!is.na(output)){
        saveRDS(filtered_dataset, output)
        }
    }


args_parser = function(){
    parser = arg_parser(
        "Explore distribution of variables in Ethnographic Atlas"
        )
    parser = add_argument(
        parser, "input", type="character",
        help="Path to rds file containing Ethnographic Atlas",
        )
    parser = add_argument(
        parser, "--summary", type="character", default=NULL,
        help="Output path for summary file."
        )
    parser = add_argument(
        parser, "--figures", type="character", default=NULL,
        help="Output folder for figures."
        )
    parser = add_argument(
        parser, "--output", type="character", default=NULL,
        help="Output path for filtered Ethnographic Atlas, saved as rds object"
        )

    args = parse_args(parser)
    args
    }

#######################################################################
# Implementation:
#######################################################################
summary_dataset = function(EA){
    dimension = dim(EA)
    unknown_data = sum(is.na(EA)) / (nrow(EA) * ncol(EA))
    categories = sapply(EA, table) %>% unlist %>% length
    summary = list(
        "dimension" = dimension,
        "unknown" = unknown_data,
        "categories" = categories
        )
    summary
    }


summary_variables = function(EA){
    freqs = sapply(EA, table)
    names = names(freqs)
    normalized_diversity = sapply(freqs, gini_simpson_normalized)
    max_freq_category = sapply(freqs, function(x) max(x/sum(x)))
    min_freq_category = sapply(freqs, function(x) min(x/sum(x)))
    unknown_data = sapply(EA, function(x) sum(is.na(x))/length(x) )
    relative_frequency = sapply(freqs, function(x) x/sum(x)) %>% unlist %>% sort
    names(relative_frequency) = seq_along(relative_frequency)
    summary = list(
        "names" = names,
        "diversity" = normalized_diversity,
        "maximum" = max_freq_category,
        "minimum" = min_freq_category,
        "unknown" = unknown_data,
        "relative_frequency" = relative_frequency
        )
    summary
    }


write_summary = function(dataset, filtered_dataset, variables, bad, file){
    text = paste0(
        "Summary of Ethnographic Atlas:\n",
        " -- societies: ", dataset$dimension[1], "\n",
        " -- variables: ", dataset$dimension[2], "\n",
        " -- categories total: ", dataset$categories, "\n",
        " -- uknown data: ", dataset$unknown, "%\n",
        "\n",
        "Underperforming Variables:\n",
        " -- small diversity (< 0.1):\n",
        collapse(bad$diversity), "\n",
        " -- overrepresented category (> 0.9):\n",
        collapse(bad$maximum), "\n",
        " -- too many unknown data (> 0.9):\n",
        collapse(bad$unknown), "\n",
        "Underperforming Variables:\n",
        " -- total: ", length(bad$all), "\n",
        " -- list:\n",
        collapse(bad$all), "\n",
        "\n",
        "After filtering, there will be:\n",
        " -- societies: ", filtered_dataset$dimension[1], "\n",
        " -- variables: ", filtered_dataset$dimension[2], "\n",
        " -- categories total: ", filtered_dataset$categories, "\n",
        " -- uknown data: ", filtered_dataset$unknown, "%\n"
        )
    cat(text, file=file)
    }


bad_variables = function(variables){
    names = variables$names
    diversity = names[variables$normalized_diversity < 0.1]
    maximum = names[variables$maximum > 0.9]
    unknown = names[variables$unknown > 0.9]
    all = c(diversity, maximum, unknown) %>% unique %>% sort
    bad = list(
        "names" = names,
        "diversity" = diversity,
        "maximum" = maximum,
        "unknown" = unknown,
        "all" = all
        )
    bad
    }


collapse = function(x, collapse=", "){
    paste0(x, collapse=collapse)
    }


gini_simpson = function(freq){
    rel_freq = freq / sum(freq)
    1 - sum(rel_freq^2)
    }


gini_simpson_normalized = function(freq){
    gini_simpson(freq) / (1 - 1/length(freq))
    }


filter_bad = function(EA, bad){
    EA = EA[!colnames(EA) %in% bad$all]
    EA
    }


plot_summary_figures = function(variables, folder){
    plot_png(
        file.path(folder, "gini_simpson_norm.png"),
        height = 1024,
        width = 2048,
        # plotting function:
        plot_fun = plot_barplot,
        x = variables$diversity,
        ylab="normalized Gini-Simpson diversity"
        )

    plot_png(
        file.path(folder, "smallest_category_perc.png"),
        height = 1024,
        width = 2048,
        plot_fun = plot_barplot,
        x = variables$minimum,
        tr = 0.01,
        ylab = "relative frequency"
        )


    plot_png(
        file.path(folder, "largest_category_perc.png"),
        height = 1024,
        width = 2048,
        plot_fun = plot_barplot,
        x = variables$maximum,
        tr = 0.9,
        ylab = "relative frequency"
        )

    plot_png(
        file.path(folder, "unknown_values_perc.png"),
        height = 1024,
        width = 2048,
        plot_fun = plot_barplot,
        x = variables$unknown,
        tr = 0.9,
        ylab = "percentage"
        )

    plot_png(
        file.path(folder, "low_frequency_categories.png"),
        height = 1024,
        width = 2048,
        plot_fun = plot_barplot,
        x = variables$relative_frequency,
        names = FALSE,
        axisnames = TRUE,
        tr = NA,
        ylab = "relative frequency",
        xlab = "order",
        space = -0.01
        )
    }


######################################################################
# Execution (needs to be at the end)
######################################################################
if(!interactive()){
    args = args_parser()
    main(args$input, args$summary, args$figures, args$output)
    }
