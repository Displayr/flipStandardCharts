% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barpictograph.R
\name{BarPictograph}
\alias{BarPictograph}
\title{Create bar chart composed of icons}
\usage{
BarPictograph(
  x,
  image = "Stickman",
  custom.image = NULL,
  base.image = "",
  hide.base.image = !any(nzchar(custom.image)),
  base.icon.color = "",
  scale = NA,
  total.icons = NA,
  icon.ncol = NA,
  fix.icon.nrow = TRUE,
  label.color.asIcon = FALSE,
  colors = ChartColors(length(x)),
  fill.direction = c("From left", "From right")[1],
  global.font.family = "Arial",
  global.font.color = rgb(44, 44, 44, maxColorValue = 255),
  data.label.show = FALSE,
  data.label.position = "No",
  data.label.format = "",
  data.label.prefix = "",
  data.label.suffix = "",
  data.label.bigmark = ",",
  data.label.font.family = global.font.family,
  data.label.font.color = global.font.color,
  data.label.font.size = 8,
  data.label.align.horizontal = "Default",
  categories.tick.show = TRUE,
  categories.tick.font.family = global.font.family,
  categories.tick.font.color = global.font.color,
  categories.tick.font.size = 12,
  categories.tick.pad = 5,
  categories.tick.align.horizontal = "Default",
  categories.tick.align.vertical = "Default",
  background.fill.color = "transparent",
  pad.row = 2,
  pad.col = 2,
  pad.icon.row = 0,
  pad.icon.col = 0,
  graphic.width.inch = NA,
  graphic.height.inch = NA,
  graphic.resolution = 72,
  maximum.number.icons = 10000,
  print.config = FALSE
)
}
\arguments{
\item{x}{A \code{\link{list}}, where each vector contains data to be plotted. If the data is not numeric
it is coerced to be numeric.
Input data may be a matrix or a vector, containing the height of the columns
to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
will be parsed automatically.}

\item{image}{Name of icon to use.}

\item{custom.image}{URL of icon to use. If this parameter is supplied \code{image} is ignored.}

\item{base.image}{URL of image to use as base image. Only used if \code{custom.image} is supplied.}

\item{hide.base.image}{Turns off background image. In general, the base image should only be shown if the input data is a proportion.}

\item{base.icon.color}{Color of base image, supplied as a hex code or string. This is only used if the built-in icons are used.}

\item{scale}{Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.}

\item{total.icons}{Maximum number of icons in each table cell. By default, it will be determine based on \code{ceiling(x)}.}

\item{icon.ncol}{Configuration of icons in each table cell. Can be a single value or a vector with length equal to the number of columns.}

\item{fix.icon.nrow}{When \code{mode="bar" and hide.base.image=T}, set to \code{FALSE} to allow the bars to contain varying number of rows.}

\item{label.color.asIcon}{When set to \code{TRUE}, row and data labels are shown in the same color as the icons.}

\item{colors}{Character; a vector containing one or more colors specified as hex codes.}

\item{fill.direction}{Direction in which icons are filled. One of \code{"From left", "From right", "From top", "From bottom"}.}

\item{global.font.family}{Character; font family for all occurrences of any
font attribute for the chart unless specified individually.}

\item{global.font.color}{Global font color as a named color in character format
(e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).}

\item{data.label.show}{Logical; whether to show data labels.}

\item{data.label.position}{When \code{show.label.data}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"Next to bar", "Above row label", "Below row label"} (bar mode only). Note that the last two options will overrride \code{sublabel.left} and \code{sublabel.right}.}

\item{data.label.format}{A string representing a d3 formatting code.}

\item{data.label.prefix}{Character; prefix for data values.}

\item{data.label.suffix}{Character; suffix for data values.}

\item{data.label.bigmark}{Option to prettify large numbers. By default a comma is placed after a thousand.}

\item{data.label.font.family}{Character; font family for data label.}

\item{data.label.font.color}{Font color as a named color
in character format (e.g. "black") or an a hex code. This can be a single
color, a vector of colors (1 for each series/column), or a comma separated list
of colors}

\item{data.label.font.size}{Integer; Font size for data label.px.}

\item{data.label.align.horizontal}{Horizontal alignment of data labels labels. One of "left", "right", "middle" or "Default".}

\item{categories.tick.show}{Logical; Whether to show tick labels for each bar (i.e. rownames).}

\item{categories.tick.font.family}{Character; x-axis tick label font family}

\item{categories.tick.font.color}{X-axis tick label font color as a named color in
character format (e.g. "black") or an rgb value (e.g.
rgb(0, 0, 0, maxColorValue = 255)).}

\item{categories.tick.font.size}{x-axis tick label font size}

\item{categories.tick.pad}{Horizontal space between the row labels and the icons.}

\item{categories.tick.align.horizontal}{Horizontal alignment of tick labels for each bar. One of "left", "right", "middle" or "Default".}

\item{categories.tick.align.vertical}{Vertical alignment of tick labels for each bar. One of "top", "center", "bottom" or "Default".}

\item{background.fill.color}{Background color in character format
(e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}

\item{pad.row}{Single numeric specifying vertical spacing between graphic cells in the table.}

\item{pad.col}{Vertical spacing between cells in table.}

\item{pad.icon.row}{Numeric specifying vertical spacing between icons inside each table cell. May be a single value or a numeric matrix of the same dimensions as \code{x}.}

\item{pad.icon.col}{Horizontal spacing between icons inside each table cell.}

\item{graphic.width.inch}{Horizontal dimension of the chart output in inches. If these dimensions are not specified, the width-to-height ratio of the chart output may not match the desired dimensions.}

\item{graphic.height.inch}{Verical dimension of the chart output in inches.}

\item{graphic.resolution}{Number of pixels per inch. Should not have an actual effect but rounding errors sometimes occur if this is chosen poorly.}

\item{maximum.number.icons}{Maximum allowed number of icons. Note that increasing this value may cause the browser to crash.}

\item{print.config}{Print configuration string based to \code{rhtmlPictographs::graphic}. This is useful for debugging.}
}
\value{
A \code{plotly} chart.
}
\description{
Distribution Chart.
}
\examples{
BarPictograph(1:5, image = "Sick person")
}
