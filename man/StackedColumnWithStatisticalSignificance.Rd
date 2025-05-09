% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stackedcolumnannot.R
\name{StackedColumnWithStatisticalSignificance}
\alias{StackedColumnWithStatisticalSignificance}
\title{Stacked Column with annotations}
\usage{
StackedColumnWithStatisticalSignificance(
  x,
  num.categories.below.axis = 0,
  transpose = FALSE,
  row.names.to.remove = "NET, SUM, Total",
  column.names.to.remove = "NET, SUM, Total",
  reverse.series.order = FALSE,
  colors = ChartColors(max(1, NCOL(x), na.rm = TRUE)),
  opacity = NULL,
  type = "Stacked",
  global.font.family = "Arial",
  global.font.color = rgb(44, 44, 44, maxColorValue = 255),
  font.unit = "px",
  annot.footer.show = TRUE,
  annot.arrow.size = 15,
  annot.arrow.colors = ChartColors(9, "Strong colors"),
  annot.arrow.offset = NULL,
  annot.arrow.symbols = c("&#8593;", "&#8595;", "&#8673;"),
  annot.sig.level = 0.05,
  annot.legend.sep = " &#183; ",
  append.annot.differences.to.datalabel = FALSE,
  annot.differences.show = TRUE,
  annot.differences.decimals = 0,
  annot.differences.prefix = "",
  annot.differences.suffix = "",
  annot.differences.sign.show = TRUE,
  annot.differences.font.family = global.font.family,
  annot.differences.font.color = global.font.color,
  annot.differences.font.size = 10,
  annot.differences.offset = NULL,
  annot.hide.small.bar = FALSE,
  column.totals.above.show = FALSE,
  column.totals.above.font.family = global.font.family,
  column.totals.above.font.color = global.font.color,
  column.totals.above.font.size = 10,
  column.totals.below.show = FALSE,
  column.totals.below.font.family = global.font.family,
  column.totals.below.font.color = global.font.color,
  column.totals.below.font.size = 10,
  title = "",
  title.font.family = global.font.family,
  title.font.color = global.font.color,
  title.font.size = 16,
  title.align = "center",
  subtitle = "",
  subtitle.font.family = global.font.family,
  subtitle.font.color = global.font.color,
  subtitle.font.size = 12,
  subtitle.align = "center",
  footer = "",
  footer.font.family = global.font.family,
  footer.font.color = global.font.color,
  footer.font.size = 8,
  footer.wrap = TRUE,
  footer.wrap.nchar = 150,
  footer.align = "center",
  background.fill.color = "transparent",
  background.fill.opacity = 1,
  charting.area.fill.color = background.fill.color,
  charting.area.fill.opacity = 0,
  legend.show = NA,
  legend.orientation = "Vertical",
  legend.wrap = TRUE,
  legend.wrap.nchar = 30,
  legend.position.x = NULL,
  legend.position.y = NULL,
  legend.fill.color = background.fill.color,
  legend.fill.opacity = 0,
  legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
  legend.border.line.width = 0,
  legend.font.color = global.font.color,
  legend.font.family = global.font.family,
  legend.font.size = 10,
  legend.ascending = NA,
  hovertext.font.family = global.font.family,
  hovertext.font.size = 11,
  margin.top = NULL,
  margin.bottom = NULL,
  margin.left = NULL,
  margin.right = NULL,
  margin.inner.pad = NULL,
  margin.autoexpand = TRUE,
  grid.show = TRUE,
  y.title = "",
  y.title.font.color = global.font.color,
  y.title.font.family = global.font.family,
  y.title.font.size = 12,
  y.line.width = 0,
  y.line.color = rgb(0, 0, 0, maxColorValue = 255),
  y.tick.mark.length = 0,
  y.tick.mark.color = "transparent",
  y.bounds.minimum = NULL,
  y.bounds.maximum = NULL,
  y.tick.distance = NULL,
  y.zero = TRUE,
  y.zero.line.width = 3,
  y.zero.line.color = rgb(0, 0, 0, maxColorValue = 255),
  y.zero.line.dash = "Solid",
  y.data.reversed = FALSE,
  y.grid.width = 1 * grid.show,
  y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
  y.tick.show = TRUE,
  y.tick.suffix = "",
  y.tick.prefix = "",
  y.tick.format = "",
  y.hovertext.format = y.tick.format,
  y.tick.angle = NULL,
  y.tick.font.color = global.font.color,
  y.tick.font.family = global.font.family,
  y.tick.font.size = 10,
  x.title = "",
  x.title.font.color = global.font.color,
  x.title.font.family = global.font.family,
  x.title.font.size = 12,
  x.line.width = 0,
  x.line.color = rgb(0, 0, 0, maxColorValue = 255),
  x.tick.marks = "",
  x.tick.mark.length = 3,
  x.tick.mark.color = "transparent",
  x.bounds.minimum = NULL,
  x.bounds.maximum = NULL,
  x.tick.distance = NULL,
  x.zero = FALSE,
  x.zero.line.width = 0,
  x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
  x.zero.line.dash = "Solid",
  x.data.reversed = FALSE,
  x.grid.width = 0 * grid.show,
  x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
  x.tick.show = TRUE,
  x.tick.suffix = "",
  x.tick.prefix = "",
  x.tick.format = "",
  x.hovertext.format = x.tick.format,
  x.tick.angle = NULL,
  x.tick.font.color = global.font.color,
  x.tick.font.family = global.font.family,
  x.tick.font.size = 10,
  x.tick.label.wrap = TRUE,
  x.tick.label.wrap.nchar = 21,
  marker.border.width = 1,
  marker.border.colors = colors,
  marker.border.opacity = NULL,
  tooltip.show = TRUE,
  modebar.show = FALSE,
  zoom.enable = TRUE,
  axis.drag.enable = FALSE,
  bar.gap = 0.5,
  bar.corner.radius = 0,
  data.label.show = FALSE,
  data.label.centered = TRUE,
  data.label.font.autocolor = FALSE,
  data.label.font.family = global.font.family,
  data.label.font.size = 10,
  data.label.font.color = global.font.color,
  data.label.format = "",
  data.label.prefix = "",
  data.label.suffix = "",
  data.label.threshold = NULL
)
}
\arguments{
\item{x}{Input data may be a matrix or a vector, containing the height of the columns
to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
will be parsed automatically.}

\item{num.categories.below.axis}{Number of categories shown below
x-axis pointing downwards.}

\item{transpose}{Swaps the rows and columns of \code{x}. The rows
and columns of the additional statistics used to create annotations
are also respected. Note that PrepareData is not used before the function.}

\item{row.names.to.remove}{Names of rows that are not shown in the chart.}

\item{column.names.to.remove}{Names of columns that are not shown in the chart.}

\item{reverse.series.order}{Reverse the order in which the data series are shown
(i.e. the order of the columns in \code{x}).}

\item{colors}{Character; a vector containing one or more colors specified
as hex codes.}

\item{opacity}{Opacity of bars as an alpha value (0 to 1).}

\item{type}{One of "Column", "Stacked Column" or "100\% Stacked Column"}

\item{global.font.family}{Character; font family for all occurrences of any
font attribute for the chart unless specified individually.}

\item{global.font.color}{Global font color as a named color in character format
(e.g. "black") or an a hex code.}

\item{font.unit}{One of "px" of "pt". By default all font sizes are
specified in terms of pixels ("px"). But changing this to "pt" will
mean that the font sizes will be in terms points ("pt"), which will
be consistent with font sizes in text boxes.}

\item{annot.footer.show}{Append a description explaining annotations in chart footer.}

\item{annot.arrow.size}{Size of the arrows in pixels.}

\item{annot.arrow.colors}{A vector of colors for the arrows.
For column comparisons, the number of colors should equal the number of columns.
For z-Statistic, the colors are assigned to 'down' and 'up' arrows.}

\item{annot.arrow.offset}{Horizontal offset (towards the right) of
the arrows from the bars. If not specified, then it will be
determined from \code{bar.gap}.}

\item{annot.arrow.symbols}{A vector of three characters (or html symbols)
used to show arrows. They are (in order):
1. Up arrow for z-Statistics and upper-case Column Comparisons
2. Down arrow for z-Statistics
3. Up arrow for lower-case Column Comparisons}

\item{annot.sig.level}{Significance level above which arrows are shown
next to the columns.}

\item{annot.legend.sep}{The string used between different entries describing
the annotation symbols in the legend. By default it is a space as it works
with line-wrap etc. But "<br>" can also be a good option for readibility.}

\item{append.annot.differences.to.datalabel}{Logical; Show difference statistics
appended to the data labels (inside the bars).}

\item{annot.differences.show}{Logical; whether to show the difference
statistics if they are included in the input table.}

\item{annot.differences.decimals}{Number of decimals shown in the
difference annotations.}

\item{annot.differences.prefix}{Option text to prepend to annot.differences.}

\item{annot.differences.suffix}{Option text to append to annot.differences.}

\item{annot.differences.sign.show}{Logical; whether to show the sign
of the difference.}

\item{annot.differences.font.family}{Font family of the
differences annotations}

\item{annot.differences.font.color}{Font color of the
differences annotations}

\item{annot.differences.font.size}{Font size of the
differences annotations}

\item{annot.differences.offset}{Horizontal offset (towards the right)
of the difference annotation from the bars. If not specified, then
it will be determined from \code{bar.gap}.}

\item{annot.hide.small.bar}{Hide annotations (arrow and differences)
when the bar is smaller than the proportion of total range
(the proportion as specified by \code{data.label.threshold}.}

\item{column.totals.above.show}{Show data labels containing the total of
the categories above the x-axis.}

\item{column.totals.above.font.family}{Font family of \code{column.totals.above}.}

\item{column.totals.above.font.color}{Font color of \code{column.totals.above}.}

\item{column.totals.above.font.size}{Font size of \code{column.totals.above}.}

\item{column.totals.below.show}{Show data labels containing the total of
the categories below the x-axis.}

\item{column.totals.below.font.family}{Font family of \code{column.totals.below}.}

\item{column.totals.below.font.color}{Font color of \code{column.totals.below}.}

\item{column.totals.below.font.size}{Font size of \code{column.totals.below}.}

\item{title}{Character; chart title.}

\item{title.font.family}{Character; title font family. Can be "Arial Black",
"Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
"Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
"Times New Roman", "Trebuchet MS", "Verdana", "Webdings"}

\item{title.font.color}{Title font color as a named color in character
format (e.g. "black") or a hex code.}

\item{title.font.size}{Integer; Title font size; default = 10.}

\item{title.align}{Horizontal alignment of title.}

\item{subtitle}{Character}

\item{subtitle.font.family}{Character; subtitle font family}

\item{subtitle.font.color}{subtitle font color as a named color in
character format (e.g. "black") or an a hex code.}

\item{subtitle.font.size}{Integer; subtitle font size}

\item{subtitle.align}{Horizontal alignment of subtitle.}

\item{footer}{Character}

\item{footer.font.family}{Character; footer font family}

\item{footer.font.color}{footer font color as a named color in
character format (e.g. "black") or an a hex code.}

\item{footer.font.size}{Integer; footer font size}

\item{footer.wrap}{Logical; whether the footer text should be wrapped.}

\item{footer.wrap.nchar}{Number of characters (approximately) in each
line of the footer when \code{footer.wrap} \code{TRUE}.}

\item{footer.align}{Horizontal alignment of footer.}

\item{background.fill.color}{Background color in character format
(e.g. "black") or a hex code.}

\item{background.fill.opacity}{Background opacity as an alpha value (0 to 1).}

\item{charting.area.fill.color}{Charting area background color as
a named color in character format (e.g. "black") or a hex code.}

\item{charting.area.fill.opacity}{Charting area background opacity
as an alpha value (0 to 1).}

\item{legend.show}{Controls whether legend is shown. This can be a logical (\code{TRUE} or \code{FALSE});
or a string ("Show" or "Hide"). If it is \code{TRUE} or \code{NA} (the default), a legend will be
shown only if there is more than one data series. To force a legend to be shown even with 1
data series, use "Show" instead.}

\item{legend.orientation}{Character; One of 'Vertical' or 'Horizontal'}

\item{legend.wrap}{Logical; whether the legend text should be wrapped.}

\item{legend.wrap.nchar}{Number of characters (approximately) in each
line of the legend when \code{legend.wrap} \code{TRUE}.}

\item{legend.position.x}{A numeric controlling the position of the legend.
Values range from -0.5 (left) to 1.5 (right).}

\item{legend.position.y}{A numeric controlling the position of the legend.
Values range from 0 (bottom) to 1 (top).}

\item{legend.fill.color}{Legend fill color as a named color in character format
(e.g. "black") or a hex code.}

\item{legend.fill.opacity}{Legend fill opacity as an alpha value (0 to 1).}

\item{legend.border.color}{Legend border color as a named color in character
format (e.g. "black") or a hex code.}

\item{legend.border.line.width}{Width in pixels of the border
around the legend.  0 = no border.}

\item{legend.font.color}{Legend font color as a named color in character
format (e.g. "black") or a hex code.}

\item{legend.font.family}{Character; legend font family.}

\item{legend.font.size}{Integer; Legend font size.}

\item{legend.ascending}{Logical; TRUE for ascending, FALSE for descending.
By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.}

\item{hovertext.font.family}{Font family of hover text.}

\item{hovertext.font.size}{Font size of hover text.}

\item{margin.top}{Margin between plot area and the top of the graphic in pixels}

\item{margin.bottom}{Margin between plot area and the bottom of the graphic in pixels}

\item{margin.left}{Margin between plot area and the left of the graphic in pixels}

\item{margin.right}{Margin between plot area and the right of the graphic in pixels}

\item{margin.inner.pad}{Padding in pixels between plot proper
and axis lines}

\item{margin.autoexpand}{Logical; Whether extra space can be added to the margins
to allow space for axis/legend/data labels or other chart elements.}

\item{grid.show}{Logical; whether to show grid lines.}

\item{y.title}{Character, y-axis title; defaults to chart input values;
to turn off set to "FALSE".}

\item{y.title.font.color}{y-axis title font color as a named color in
character format (e.g. "black") or a hex code.}

\item{y.title.font.family}{Character; y-axis title font family}

\item{y.title.font.size}{Integer; y-axis title font size}

\item{y.line.width}{y-axis line width in pixels (0 = no line).}

\item{y.line.color}{y-axis line color as a named color in character format
(e.g. "black") or a hex code.}

\item{y.tick.mark.length}{Length of tick marks in pixels. Ticks are only shown when \code{y.line.width > 0}.}

\item{y.tick.mark.color}{Color of tick marks (default transparent).}

\item{y.bounds.minimum}{Minimum of range for plotting; For a date axis this should be supplied as a date string.
For a categorical axis, the index of the category (0-based) should be used.}

\item{y.bounds.maximum}{Maximum of range for plotting; NULL = no manual range set.}

\item{y.tick.distance}{Distance between tick marks. Requires that \code{y.bounds.minimum} and \code{y.bounds.maximum} have been set.}

\item{y.zero}{Whether the y-axis should include zero.}

\item{y.zero.line.width}{Width in pixels of zero line;}

\item{y.zero.line.color}{Color of horizontal zero line as a named
color in character format (e.g. "black") or an rgb value (e.g.
rgb(0, 0, 0, maxColorValue = 255)).}

\item{y.zero.line.dash}{Line type of y zero line. Can be one of 'Solid', 'Dot', 'Dash'.}

\item{y.data.reversed}{Logical; whether to reverse y-axis or not}

\item{y.grid.width}{Width of y-grid lines in pixels; 0 = no line}

\item{y.grid.color}{Color of y-grid lines as a named color in character
format (e.g. "black") or a hex code.}

\item{y.tick.show}{Whether to display the y-axis tick labels}

\item{y.tick.suffix}{y-axis tick label suffix}

\item{y.tick.prefix}{y-axis tick label prefix}

\item{y.tick.format}{A string representing a d3 formatting code.
See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format}

\item{y.hovertext.format}{A string representing a d3 formatting code
See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format}

\item{y.tick.angle}{y-axis tick label angle in degrees.
90 = vertical; 0 = horizontal}

\item{y.tick.font.color}{y-axis tick label font color as a named color
in character format (e.g. "black") or an a hex code.}

\item{y.tick.font.family}{Character; y-axis tick label font family}

\item{y.tick.font.size}{Integer; y-axis tick label font size}

\item{x.title}{Character, x-axis title; defaults to chart input values;
to turn off set to "FALSE".}

\item{x.title.font.color}{x-axis title font color as a named color in
character format (e.g. "black") or an a hex code.}

\item{x.title.font.family}{Character; x-axis title font family}

\item{x.title.font.size}{Integer; x-axis title font size}

\item{x.line.width}{x-axis line in pixels, 0 = no line}

\item{x.line.color}{x-axis line color as a named color in character format
(e.g. "black") or a hex code.}

\item{x.tick.marks}{Character; whether and where to show tick marks on the
x-axis.  Can be "outside", "inside", "none"}

\item{x.tick.mark.length}{Length of tick marks in pixels.}

\item{x.tick.mark.color}{Color of tick marks.}

\item{x.bounds.minimum}{Minimum of range for plotting; For a date axis this should be supplied as a date string.
For a categorical axis, the index of the category (0-based) should be used.}

\item{x.bounds.maximum}{Maximum of range for
plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum}

\item{x.tick.distance}{Tick mark distance in
x-axis units between minimum and maximum for plotting; NULL = no manual
range set.}

\item{x.zero}{Whether the x-axis should include zero.}

\item{x.zero.line.width}{Width in pixels of zero line.}

\item{x.zero.line.color}{Color of horizontal zero (origin) line as a named
color in character format (e.g. "black") or an rgb value (e.g.
rgb(0, 0, 0, maxColorValue = 255)).}

\item{x.zero.line.dash}{Line type of x zero line. Can be one of 'Solid', 'Dot', 'Dash'.}

\item{x.data.reversed}{Logical; whether to reverse x-axis or not}

\item{x.grid.width}{Width of y-grid lines in pixels; 0 = no line}

\item{x.grid.color}{Color of y-grid lines as a named color in character
format (e.g. "black") or a hex code.}

\item{x.tick.show}{Whether to display the x-axis tick labels}

\item{x.tick.suffix}{x-axis tick label suffix}

\item{x.tick.prefix}{x-axis tick label prefix}

\item{x.tick.format}{A string representing a d3 formatting code.
See https://github.com/mbostock/d3/wiki/Formatting#numbers}

\item{x.hovertext.format}{A string representing a d3 formatting code.
See https://github.com/mbostock/d3/wiki/Formatting#numbers}

\item{x.tick.angle}{x-axis tick label angle in degrees.
90 = vertical; 0 = horizontal}

\item{x.tick.font.color}{X-axis tick label font color as a named color in
character format (e.g. "black") or an a hex code.}

\item{x.tick.font.family}{Character; x-axis tick label font family}

\item{x.tick.font.size}{Integer; x-axis tick label font size}

\item{x.tick.label.wrap}{Logical; whether to wrap long labels on the x-axis.}

\item{x.tick.label.wrap.nchar}{Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.}

\item{marker.border.width}{Width in pixels of border/line
around series bars; 0 is no line}

\item{marker.border.colors}{Character; a vector containing one or more colors specified as hex codes.}

\item{marker.border.opacity}{Opacity of border around bars as an alpha value (0 to 1).}

\item{tooltip.show}{Logical; whether to show a tooltip on hover.}

\item{modebar.show}{Logical; whether to show the zoom menu buttons or not.}

\item{zoom.enable}{Logical; whether to enable zoom on the chart.
For Bar and Column charts with data labels it may be useful to turn off zoom
because data labels and annotations can be misplace on zoom.}

\item{axis.drag.enable}{Logical; whether to enable the user to drag along axes.
This interaction is available when the cursor shows up as a double-headed arrow
when hovering over an axis. It is turned off by default because it can sometimes
cause problems with data labels and annotations. Also, is only used when
\code{zoom.enable = TRUE}. Note that in similar functionality is already available
using zoom.}

\item{bar.gap}{Gap between adjacent bars with different coordinates in
bar or column charts (in plot fraction).}

\item{bar.corner.radius}{Rounding of bar corners as a percentage of bar width.}

\item{data.label.show}{Logical; whether to show data labels.}

\item{data.label.centered}{Logical; whether data labels in Stacked Column
charts should have the data labels vertically centered.}

\item{data.label.font.autocolor}{Logical; Whether font color should be
automatically determined. For Line and Radar charts, the data labels will
colored in the series color. For stacked bar and column charts the
data labels will be black or white depending on the color of the
bar (which background the data label). For non-stacked bar and column
charts, this option is ignored.}

\item{data.label.font.family}{Character; font family for data label.}

\item{data.label.font.size}{Integer; Font size for data label.px.}

\item{data.label.font.color}{Font color as a named color
in character format (e.g. "black") or an a hex code. This can be a single
color, a vector of colors (1 for each series/column), or a comma separated list
of colors}

\item{data.label.format}{A string representing a d3 formatting code.
See https://github.com/mbostock/d3/wiki/Formatting#numbers}

\item{data.label.prefix}{Character; prefix for data values.}

\item{data.label.suffix}{Character; suffix for data values.}

\item{data.label.threshold}{The proportion of the total range below which
data labels should not be displayed. Only applicable for pie, bar and column
charts.}
}
\description{
Stacked Column charts with annotations showing statistical
 significance of z-Statistics or Column Comparisons
}
\examples{
z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
      .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
Column(z, type="Stacked Column")
}
