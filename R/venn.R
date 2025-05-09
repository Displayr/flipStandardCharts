#' \code{Venn}
#'
#' @description Venn diagram (also known as Euler diagram). Minimizes sum of squared residuals to find the best overlap. Where no diagram appears,
#' it is likely that there is a problem with the inputs. This is a wrapper for http://www.buildingwidgets.com/blog/2015/6/5/week-22-d3vennr,
#' which in turn is a wrapper for http://www.benfrederickson.com/venn-diagrams-with-d3.js/.
#'
#' @param x A \link{data.frame} \code{logical} (converted to logical using >= 1 if not) or a JSON-like list.
#' @param weights An optional vector of weights, or, the name or, the name of a variable in \code{x}. It may not be an expression.
#' @param colors A character vector specifying the color of each circle.
#' @param opacity Numeric; Opacity of circles as an alpha value (0 to 1).
#' @param data.label.font.autocolor Logical; Whether font color should be automatically set to the same as the circle color. When this is true, \code{data.label.font.color} is ignored.
#' @param global.font.family Font family of all text elements. These can be overriden for individual text elements.
#' @param data.label.font.color Font color of data labels (ignored if \code{data.label.font.autocolor}).
#' @param data.label.font.family Font family of data labels.
#' @param data.label.font.size The font size of the labels. Defaults to 20.
#' @param hovertext.font.family Font family of hovertext (i.e tooltips).
#' @param hovertext.font.size Font size of hovertext in pixels.
#' @param values.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers. This option only applies when \code{x} is a data.frame.
#' @examples
#' Venn(list(
#'    list("sets"= list(0), "label"= "Like", "size"= 100),
#'    list("sets"= list(1), "label"= "Love", "size"= 50),
#'    list("sets"= list(2), "label"= "Dislike", "size"= 100),
#'    list("sets"= list(3), "label"= "Hate", "size"= 50),
#'    list("sets"= list(0, 1), "size"= 50),
#'    list("sets"= list(0, 2), "size"= 0),
#'    list("sets"= list(2, 3), "size"= 50)))
#' @importFrom flipData CleanSubset CleanWeights
#' @importFrom flipFormat Labels
#' @importFrom flipChartBasics StripAlphaChannel
#' @importFrom d3vennR d3vennR
#' @importFrom verbs Sum
#' @export
#' @importFrom flipU StopForUserError
Venn <- function(x = NULL,
                 weights = NULL,
                 colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                            "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
                 opacity = NULL,
                 global.font.family = "Arial",
                 data.label.font.autocolor = TRUE,
                 data.label.font.color = "#FFFFFF",
                 data.label.font.family = global.font.family,
                 data.label.font.size = 14,
                 hovertext.font.family = global.font.family,
                 hovertext.font.size = 14,
                 values.hovertext.format = "")
{
    if (is.numeric(x))
    {
        ErrorIfNotEnoughData(x)
        x <- as.data.frame(x)
    }

    suffix <- ""
    if (is.data.frame(x))
    {
        if (any(sapply(x, is.factor)))
            StopForUserError("Data must consist of numeric (0 or 1) or logical (TRUE or FALSE) values ",
                             "indicating which cases (rows) are included in which sets (columns).")

        nms <- colnames(x)
        if (is.null(weights))
            weights <- rep(1, nrow(x))

        stat <- attr(x, "statistic")
        as.percentages <- percentFromD3(values.hovertext.format) || (values.hovertext.format == "" && !is.null(stat) && grepl("%", stat, fixed = TRUE))
        data.label.decimals <- decimalsFromD3(values.hovertext.format, 0)

        if (as.percentages) {
            weights <- weights / Sum(weights, remove.missing = FALSE) * 100
            suffix <- "%"
        }
        if (!is.logical(x[,1]))
            x <- as.data.frame(x >= 1)
        x <- convertDataFrameIntoJSON(x, nms, weights, data.label.decimals)

    } else if (percentFromD3(values.hovertext.format)) {
        suffix <- "%"
        for (i in seq(length(x)))
            x[[i]]$size <- x[[i]]$size * 100
    }

    # Tidying up parameters
    n.sets <- length(unique(unlist(sapply(x, function(s) return(unlist(s$sets))))))
    if (n.sets > 3)
        warning("In Venn diagrams with more than 3 variables, areas are only approximately proportional to set sizes.")
    if (is.null(colors))
    {
         # d3.schemeCategory10
        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
        data.label.font.autocolor <- TRUE # for compatibility with old wiki forms
    }
    colors <- rep(colors, length = n.sets)
    if (is.null(opacity))
        opacity <- 0.25
    if (data.label.font.autocolor)
        data.label.font.color <- NULL
    else
        data.label.font.color <- rep(vectorize(data.label.font.color), length = n.sets)

    # Creating the Venn diagram
    vv <- d3vennR(data = x, fontSize = data.label.font.size)
    vv$x$tasks <- list(
        venn_circles(colors, opacity, data.label.font.family, data.label.font.color),
        venn_tooltip(suffix = suffix, font.family = hovertext.font.family,
                     font.size = hovertext.font.size, opacity = opacity))
    vv$sizingPolicy$browser$fill <- TRUE
    result <- list(htmlwidget = vv)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Bar Clustered"
    result
}

venn_circles <- function(colors, opacity, font.family, font.colors)
{
    JS(paste0('
function(){
    var colors = ', toJSON(colors), ';
    var fontcolors = ', if (is.null(font.colors)) 'colors' else toJSON(font.colors), ';
    d3.select(this).selectAll(".venn-circle path")
          .style("fill", function(d,i) { return colors[i]; })
          .style("fill-opacity", ', opacity, ')
    d3.select(this).selectAll(".venn-circle text")
        .style("font-family", "', font.family, '")
        .style("fill", function(d,i) { return fontcolors[i]; })
}'
))}

venn_tooltip <- function(suffix = "", opacity, font.family, font.size)
{
    JS(paste0('
function(){
    var div = d3.select(this);

    // add a tooltip
    var tooltip = d3.select("body").append("div")
    .attr("class", "venntooltip")
    .style("position", "absolute")
    .style("text-align", "center")
    .style("background", "#333")
    .style("color","#ddd")
    .style("padding","4px")
    .style("border","0px")
    .style("border-radius","2px")
    .style("opacity",0);

    div.selectAll("path")
    .style("stroke-opacity", 0)
    .style("stroke", "#fff")
    .style("stroke-width", 0)

    // add listeners to all the groups to display tooltip on mousover
    div.selectAll("g")
    .on("mouseover", function(d, i) {

        // sort all the areas relative to the current item
        venn.sortAreas(div, d);

        // Display a tooltip with the current size
        tooltip.transition().duration(400).style("opacity", .9);
        tooltip.text(d.size + "', suffix, '")
            .style("font", "', font.size, 'px ', font.family, '")
            .style("font-weight", "bold");

        // highlight the current path
        var selection = d3.select(this).transition("tooltip").duration(400);
        selection.select("path")
        .style("stroke-width", 3)
        .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
        .style("stroke-opacity", 1);
    })

    .on("mousemove", function() {
        tooltip.style("left", (d3.event.pageX) + "px")
        .style("top", (d3.event.pageY - 28) + "px");
    })

    .on("mouseout", function(d, i) {
        tooltip.transition().duration(400).style("opacity", 0);
        var selection = d3.select(this).transition("tooltip").duration(400);
        selection.select("path")
        .style("stroke-width", 0)
        .style("fill-opacity", d.sets.length == 1 ? ', opacity, ': .0)
        .style("stroke-opacity", 0);
    });
}'
))}


#' convertDataFrameIntoJSON
#'
#' Creating JSON-like list from data frame
#' @param x The data.frame.
#' @param nms The names of the labels
#' @param weights Vector of weights
#' @param data.label.decimals number of decimal places.
#' @importFrom janitor round_half_up
#' @importFrom verbs SumEmptyHandling
#' @importFrom flipU StopForUserError
convertDataFrameIntoJSON <- function(x, nms, weights, data.label.decimals)
{
    # The filtered data might have length zero, so use SumEmptyHandling to ensure result isn't NA
    .sum <- function(cols)  round_half_up(SumEmptyHandling(weights[apply(x[, cols + 1, drop = FALSE], 1, all)], remove.missing = FALSE), data.label.decimals)
    nms <- as.character(nms)
    k <- ncol(x)

    if (is.null(dim(drop(x))) || length(dim(drop(x))) != 2L || NCOL(x) == 1L)
        StopForUserError("The supplied data needs to be a matrix or data.frame with at least two columns.")

    all.subsets <- allSubsets(k)
    sizes <- vapply(all.subsets, .sum, 0)
    x <- makeSetList(all.subsets, sizes, nms)
    return(x)
}

#' Creates all possible subsets of the integers 0, 1, 2,..., k-1
#' for a supplied integer, k.
#' @param size integer specifying largest set size, k
#' @return a list containing all the subsets excluding the empty set
#' and including the set {1, 2, ..., k}.
#' @references Code modified from
#' \url{http://rsnippets.blogspot.com/2012/04/generating-all-subsets-of-set.html}.
#' @noRd
allSubsets <- function(size) {
    set <- 0:(size - 1)
    idxs <- expand.grid(replicate(size, c(F, T), simplify = FALSE))
    out <- apply(idxs, 1, function(idx) set[idx])
    out <- out[-1]  # drop empty set
    lens <- vapply(out, length, 0L)
    out[order(lens, decreasing = FALSE)]
}

makeSetList <- function(sets, sizes, nms)
    mapply(makeOneSet, sets, sizes, MoreArgs = list(nms = nms),
           SIMPLIFY = FALSE, USE.NAMES = FALSE)

makeOneSet <- function(set, size, nms)
{
    out <- list(sets = as.list(set), size = size)
    if (length(set) == 1L)
        out$label  <- nms[set+1]
    return(out)
}
