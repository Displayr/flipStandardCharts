#' \code{Venn}
#'
#' @description Venn diagram (also known as Euler diagram). Minimizes sum of squared residuals to find the best overlap. Where no diagram appears,
#' it is likely that there is a problem with the inputs. This is a wrapper for http://www.buildingwidgets.com/blog/2015/6/5/week-22-d3vennr,
#' which in turn is a wrapper for http://www.benfrederickson.com/venn-diagrams-with-d3.js/.
#'
#' @param x A \link{data.frame} \code{logical} (converted to logical using >= 1 if not) or a JSON-like list.
#' @param weights An optional vector of weights, or, the name or, the name of a variable in \code{x}. It may not be an expression.
#' @param data.label.font.size The font size of the labels. Defaults to 20.
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
#' @importFrom d3vennR d3vennR
#' @export
Venn <- function(x = NULL,
                        weights = NULL,
                        data.label.font.size = 20,
                        values.hovertext.format = "")
{
    if (is.numeric(x))
    {
        ErrorIfNotEnoughData(x)
        x <- as.data.frame(x)
    }
    if (is.data.frame(x))
    {
        nms = Labels(x)
        if (is.null(weights))
            weights <- rep(1, nrow(x))

        stat <- attr(x, "statistic")
        as.percentages <- percentFromD3(values.hovertext.format) || (values.hovertext.format == "" && !is.null(stat) && grepl("%", stat, fixed = TRUE))
        data.label.decimals <- decimalsFromD3(values.hovertext.format, 0)

        if (as.percentages)
            weights <- weights / sum(weights) * 100
        if (!is.logical(x[,1]))
            x <- as.data.frame(x >= 1)
        x <- convertDataFrameIntoJSON(x, nms, weights, data.label.decimals)
    }
    # Creating the Venn diagram
    venn_tooltip(d3vennR(data = x, fontSize = data.label.font.size))
}

#' venn_tooltip
#'
#' Creating the tooltips
#' @param venn The JSON-like list.
#' @importFrom htmlwidgets JS
venn_tooltip <- function( venn ){
        venn$x$tasks[length(venn$x$tasks)+1] <- list(
            JS('
                            function(){
                            var div = d3.select(this);

                            // add a tooltip
                            var tooltip = d3.select("body").append("div")
                            .attr("class", "venntooltip")
                            .style("position", "absolute")
                            .style("text-align", "center")
                            .style("width", 128)
                            .style("height", 16)
                            .style("background", "#333")
                            .style("color","#ddd")
                            .style("padding","2px")
                            .style("border","0px")
                            .style("border-radius","8px")
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
                            tooltip.text(d.size);

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
                            .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                            .style("stroke-opacity", 0);
                            });
                            }
                            ')
            )
        venn
    }

#' convertDataFrameIntoJSON
#'
#' Creating JSON-like list from data frane
#' @param x The data.frame.
#' @param nms The names of the labels
#' @param weights Vector of weights
#' @param data.label.decimals number of decimal places.
convertDataFrameIntoJSON <- function(x, nms, weights, data.label.decimals)
{
    .sum <- function(cols)  {round(sum(weights[apply(x[,cols + 1, drop = FALSE], 1, all)]), data.label.decimals)}
    .nm <- function(col) {nms[col + 1]}
    nms <- as.character(nms)
    k <- ncol(x)
    error.msg <- "Venn diagrams can only be created with between 2 and 5 columns of data. You will need to create the input manually."
    if (is.null(dim(x)))
        stop(error.msg)
    x <- if (k == 2)
        list(
            list("sets"= list(0), "label"= .nm(0), "size"= .sum(0)),
            list("sets"= list(1), "label"= .nm(1), "size"= .sum(1)),
            list("sets"= list(0, 1), "size"= .sum(0:1)))
    else if (k == 3)
        list(
            list("sets"= list(0), "label"= .nm(0), "size"= .sum(0)),
            list("sets"= list(1), "label"= .nm(1), "size"= .sum(1)),
            list("sets"= list(2), "label"= .nm(2), "size"= .sum(2)),
            list("sets"= list(0, 1), "size"= .sum(c(0, 1))),
            list("sets"= list(0, 2), "size"= .sum(c(0, 2))),
            list("sets"= list(1, 2), "size"= .sum(c(1, 2))),
            list("sets"= list(0, 1, 2), "size"= .sum(c(0, 1, 2))))
    else if (k == 4)
        list(
            list("sets"= list(0), "label"= .nm(0), "size"= .sum(0)),
            list("sets"= list(1), "label"= .nm(1), "size"= .sum(1)),
            list("sets"= list(2), "label"= .nm(2), "size"= .sum(2)),
            list("sets"= list(3), "label"= .nm(3), "size"= .sum(3)),
            list("sets"= list(0, 1), "size"= .sum(c(0, 1))),
            list("sets"= list(0, 2), "size"= .sum(c(0, 2))),
            list("sets"= list(0, 3), "size"= .sum(c(0, 3))),
            list("sets"= list(1, 2), "size"= .sum(c(1, 2))),
            list("sets"= list(1, 3), "size"= .sum(c(1, 3))),
            list("sets"= list(2, 3), "size"= .sum(c(2, 3))),
            list("sets"= list(0, 1, 2), "size"= .sum(c(0, 1, 2))),
            list("sets"= list(0, 1, 3), "size"= .sum(c(0, 1, 2))),
            list("sets"= list(0, 2, 3), "size"= .sum(c(0, 2, 3))),
            list("sets"= list(1, 2, 3), "size"= .sum(c(1, 2, 3))),
            list("sets"= list(0, 1, 2, 3), "size"= .sum(c(0, 1, 2, 3))))
    else if (k == 5)
        list(
            list("sets"= list(0), "label"= .nm(0), "size"= .sum(0)),
            list("sets"= list(1), "label"= .nm(1), "size"= .sum(1)),
            list("sets"= list(2), "label"= .nm(2), "size"= .sum(2)),
            list("sets"= list(3), "label"= .nm(3), "size"= .sum(3)),
            list("sets"= list(4), "label"= .nm(4), "size"= .sum(4)),
            list("sets"= list(0, 1), "size"= .sum(c(0, 1))),
            list("sets"= list(0, 2), "size"= .sum(c(0, 2))),
            list("sets"= list(0, 3), "size"= .sum(c(0, 3))),
            list("sets"= list(0, 4), "size"= .sum(c(0, 4))),
            list("sets"= list(1, 2), "size"= .sum(c(1, 2))),
            list("sets"= list(1, 3), "size"= .sum(c(1, 3))),
            list("sets"= list(1, 4), "size"= .sum(c(1, 4))),
            list("sets"= list(2, 3), "size"= .sum(c(2, 3))),
            list("sets"= list(2, 4), "size"= .sum(c(2, 4))),
            list("sets"= list(3, 4), "size"= .sum(c(3, 4))),
            list("sets"= list(0, 1, 2), "size"= .sum(c(0, 1, 2))),
            list("sets"= list(0, 1, 3), "size"= .sum(c(0, 1, 2))),
            list("sets"= list(0, 1, 4), "size"= .sum(c(0, 1, 4))),
            list("sets"= list(0, 2, 3), "size"= .sum(c(0, 2, 3))),
            list("sets"= list(0, 2, 4), "size"= .sum(c(0, 2, 4))),
            list("sets"= list(0, 3, 4), "size"= .sum(c(0, 3, 4))),
            list("sets"= list(1, 2, 3), "size"= .sum(c(1, 2, 3))),
            list("sets"= list(1, 2, 4), "size"= .sum(c(1, 2, 4))),
            list("sets"= list(1, 3, 4), "size"= .sum(c(1, 3, 4))),
            list("sets"= list(2, 3, 4), "size"= .sum(c(2, 3, 4))),
            list("sets"= list(0, 1, 2, 3), "size"= .sum(c(0, 1, 2, 3))),
            list("sets"= list(0, 1, 2, 4), "size"= .sum(c(0, 1, 2, 4))),
            list("sets"= list(0, 1, 3, 4), "size"= .sum(c(0, 1, 3, 4))),
            list("sets"= list(0, 2, 3, 4), "size"= .sum(c(0, 2, 3, 4))),
            list("sets"= list(1, 2, 3, 4), "size"= .sum(c(1, 2, 3, 4))),
            list("sets"= list(0, 1, 2, 3, 4), "size"= .sum(c(0, 1, 2, 3, 4))))
    else
        stop(error.msg)
    return(x)
}
