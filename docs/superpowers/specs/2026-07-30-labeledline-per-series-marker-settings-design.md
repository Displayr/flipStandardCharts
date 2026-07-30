# labeledLine per-series marker settings

Date: 2026-07-30
Repos: `flipStandardCharts` (PR #132, branch `FS2-4532`), `rhtmlCombinedScatter`
Depends on: `flipStandardCharts` PR #135 (`RS-22973`, adds `labeledLine`)

## Problem

PR #132 exists so that `Line()` accepts a per-series value for `line.type`,
`line.thickness`, `marker.size` and `marker.symbols`, supplied either as a vector or as a
comma-separated string. When #132 was written, `labeledLine` did not exist; #135 has since
added it, so the same settings now have to work on the `rhtmlCombinedScatter` path as well
as the plotly path.

`marker.opacity` was originally a fifth setting in that list and has been dropped from it.
It is not an exposed parameter in Plugins, and per-series transparency is better expressed
by a colour with alpha, so it is specified as a scalar instead — see Scope decisions.

Rebasing #132 onto #135 already relocated the parsing into the shared
`prepareLineSeries` (`R/linechartseries.R`), so three of the five settings work on both
paths. Verified against the rebased branch:

| setting | plotly path | labeledLine |
| --- | --- | --- |
| `line.type` | per series | per series (`["solid","dot"]`) |
| `line.thickness` | per series | per series (`[2,8]`) |
| `marker.size` | per series | per point (`pointRadius 3,3,3,3,3,7,7,7,7,7`) |
| `marker.symbols` | per series | **not expressible — the widget has no symbol parameter** |
| `marker.opacity` | **errors** | **errors** |

The remaining work is `marker.symbols`, the errors in the opacity family, and a warning
that misfires once per-series input is legal.

### The opacity errors

`marker.opacity`, `opacity` and `marker.border.opacity` never went through
`readNumericSeries`, so a comma-separated string reaches arithmetic and colour code as
text. Both chart paths error:

| input | plotly path | labeledLine |
| --- | --- | --- |
| `marker.opacity = "0.5, 0.5"` | error | error |
| `marker.opacity = c(0.5, 0.9)` | error (`alpha must be of length 1 or the same length as x`) | ignored, warns |
| `opacity = "0.5, 0.9"` | error | error |
| `marker.border.opacity = "0.5, 0.9"` | error | error |

Two distinct causes:

- `R/linechartseries.R:177` — `opacity <- opacity * rep(1, ncol(chart.matrix))` throws
  `non-numeric argument to binary operator` on a string. Shared, so it breaks both paths.
- `R/linechart.R:256`, `:259`, `:287` — `toRGB(..., alpha = marker.opacity)` is not
  indexed by trace, so a per-series vector fails `toRGB`'s length check.

### The misfiring warning

`warnUnsupportedByAutoPlacement` decides whether a setting was touched by comparing the
raw argument with `identical()` against a scalar default
(`UNSUPPORTED.BY.AUTO.PLACEMENT`, `R/labeledlinechart.R:556`). Once per-series input is
legal that test is too crude: `marker.symbols = "circle, circle"` and
`c("circle", "circle")` both mean the default and both warn. `data.label.position` has the
same problem with `"Top, Top"`.

## Scope decisions

- **No per-series marker transparency, on either path.** A series that should be more
  transparent can be given an 8-digit hex or `rgba()` colour, so the widget does not need a
  `point.transparency` parameter and the plotly path does not need per-trace indexing.
  `marker.opacity` and `marker.border.opacity` are specified as scalars — neither is exposed
  in Plugins — and a comma-separated string or a vector collapses to its first value with a
  warning rather than erroring. Both paths then behave identically, so switching automatic
  placement on or off cannot change marker transparency.
- **`opacity` stays per series.** It already is on both paths — `opacity[i]` per plotly
  trace at `R/linechart.R:241`, folded into `line.colors` for the widget — and only its
  string form errors. This is deliberate and tested: `tests/testthat/test-labeledlinechart.R:448`
  asserts `opacity = c(0.3, 1)` reaches the widget as
  `["rgba(255,0,0,0.3)", "rgba(0,255,0,1)"]`, and the warning at `:430` exists precisely
  because the lines get an opacity each while the markers cannot. `marker.border.opacity`
  carries no such coverage — it is only ever tested as a scalar (`:386`, `:392`) — so the
  scalar contract costs nothing there.
- **The widget gains `pointSymbol` only.**
- **Lines and markers merge into one trace per series** (option L2 below) rather than
  adding a legend-proxy trace. Chosen for the trace count: the base case goes from two
  traces per series to one.

## Widget design (`rhtmlCombinedScatter`)

Points are drawn as plotly traces per group in `theSrc/scripts/PlotlyChartElements.js`.
plotly accepts arrays for `marker.symbol`, `marker.size` and `marker.opacity`, so a symbol
can be per point; `line.width` and `line.dash` are per-trace scalars, which is why the
existing line settings are recycled by group index and must stay that way.

1. **New parameter `point.symbol`**, scalar or per-point array, encoded in
   `theSrc/R/htmlwidget.R` the way `point.radius` and `point.border.color` are. `R/` is
   generated from `theSrc` by gulp — do not edit it directly.
2. **Merge the line and marker traces.** `createLineTrace` and
   `createScatterTraceForMarker` become one `mode: 'lines+markers'` trace per group,
   carrying the group's line style and a `marker` block with the per-point `symbol` and
   `size` slices (`_.at(..., data_index)`), the group colour and the opacity.
3. **The border trace takes the same symbol slice**, so a square marker gets a square
   border. `createScatterTraceForMarkerBorder` stays a separate trace — it exists to keep
   borders out of the legend symbols.
4. **Legend.** The merged trace owns the entry, so the swatch shows the line style and the
   symbol together. A per-point symbol array collapses to the series' first value for the
   swatch.
5. **Version bump**, because `flipStandardCharts` needs a floor above `1.1.0`.

Hover needs no dedicated trace: it belongs to whichever trace holds plotly's
`text`/`hoverinfo` pair — the tooltip payload, which plotly never draws on the plot. After
the merge that is the single trace per group, so the current handover (the line trace takes
`text` while the marker trace is `hoverinfo: 'skip'`, `PlotlyChartElements.js:154`, `:168`)
disappears.

The data labels are not plotly's and are not affected by the merge: they are drawn as SVG
`text` by the widget's own d3 layer in `RectPlot.drawLabs()` (`theSrc/scripts/RectPlot.js:342`)
and positioned by `LabelPlacement`, which is why `#135` had to emit `tspan` rather than
`span` markup around them. The one place plotly does render text is
`createScatterTraceForMarkerAnnotation` (`mode: 'markers+text'`), whose source is the
separate `marker.annotations` parameter, not `label`. The only coupling between the traces
and the labels is `addMarkerClickHandler` toggling a label by `.point` document position,
which invariant 3 below covers.

### Invariants the merge must preserve

`addMarkerClickHandler` (`theSrc/scripts/LabeledScatter.js:352`) selects every `.point` in
document order and maps position to data row through `markerIndexToDataIndex`. Rows with a
missing coordinate draw no marker and are skipped, so a shift in DOM order silently
toggles the wrong label. Trace order is set by the concatenation at
`PlotlyChartElements.js:136`: `[...plot_line_data, ...plot_data, ...plot_annotation_data]`.

These four tests go in **before** the merge, phrased so they survive it:

1. Exactly one trace per group carries plotly's `text`/`hoverinfo` pair (the tooltip
   payload, not the d3-drawn data labels), and it covers every point of the group — this is
   what makes hover work at radius 0, the default `marker.show = FALSE` line chart.
2. Exactly one trace per group has `showlegend: true`; after the merge its swatch carries
   both the line style and the symbol.
3. Marker-emitting traces appear in group order, with the border and annotation traces
   after all of them.
4. With `line.show` false the trace structure is unchanged — the Scatterplot and
   LabeledScatter regression guard, and the one place a snapshot is the right tool.

The two existing tests that pin the handover mechanism — "take over the legend entry and
tooltip from the markers" and "leave the legend and tooltip on the markers when no lines
are drawn" (`theSrc/scripts/PlotlyChartElementsLines.jest.test.js`) — describe behaviour
the merge removes, and get rewritten as part of it.

One check jest cannot make: hover over a hidden marker and over a gap, in a browser, since
plotly's hit-testing is not observable from the trace config.

## flipStandardCharts design

1. **`R/linechartseries.R`** — route `opacity` through `readNumericSeries`, replacing the
   `opacity * rep(1, n)` at `:177` that throws on a string. One change covers both paths,
   because both call `prepareLineSeries`.
2. **`R/linechartseries.R`** — collapse `marker.opacity` and `marker.border.opacity` to a
   single value in the same place, warning when the values supplied differ. Enforcing the
   scalar contract once means `toRGB` can never be handed a vector alpha, and
   `R/linechart.R:256`, `:259` and `:287` need no per-trace indexing. Note that parsing
   alone would not have been enough: `readNumericSeries("0.5, 0.9", n)` returns a vector,
   which still fails `toRGB`'s length check.
3. **Roxygen** — document `marker.opacity` and `marker.border.opacity` as single values in
   `Line()` and `labeledLine()`, and regenerate `man/Line.Rd`.
4. **`R/labeledlinechart.R`** — build `point.symbol` as a per-point vector the way
   `point.border.colors` is built at `:326`, but with `rep(x, each = n.row)` alone: unlike
   a border colour, an empty string is not a valid plotly symbol, so hidden markers keep
   their series' symbol and stay hidden by their zero radius.
5. Pass `color.transparency = marker.opacity` directly, dropping the
   `length(unique(marker.opacity)) == 1` guard at `:340`, which only existed because the
   value might have varied per series.
6. Remove `marker.symbols` from `UNSUPPORTED.BY.AUTO.PLACEMENT` (`:556`), and reduce the
   `marker.opacity` special case in `warnUnsupportedByAutoPlacement` to the fallback it is
   now the only case for: markers inherit `opacity` when `marker.opacity` is unset, and a
   per-series `opacity` cannot be given to the widget's markers alone. A `marker.opacity`
   the caller set is a scalar by contract and never warns.
7. **Narrow the unsupported check** in `warnUnsupportedByAutoPlacement` to compare
   vectorized effective values rather than raw arguments, so `"circle, circle"` and
   `"Top, Top"` no longer warn.
8. **`DESCRIPTION`** — raise the `rhtmlCombinedScatter` floor to the new widget version.

### Tests

- A per-series `marker.symbols` reaches the widget payload, as a vector and as a comma
  separated string.
- Neither a comma-separated string nor a vector of opacities errors on either path.
  Differing values collapse to the first with a warning, identically on both paths, while a
  per-series `opacity` still reaches the lines per series.
- One existing assertion changes: `test-labeledlinechart.R:432` expects
  `marker.opacity = c(0.3, 1)` to warn "does not support the setting 'marker.opacity'". Under
  the scalar contract that input is out of contract, so it collapses to `0.3` and warns that
  only the first value is used. `:430` and `:445`, which cover a per-series `opacity` with
  markers shown, keep the warning they have.
- An all-default per-series input (`"circle, circle"`, `"Top, Top"`) produces no warning;
  a genuinely unsupported setting still does.
- The existing `test-labeledlinechart.R` and `test-linechart.R` suites stay green.

## Sequencing

The widget ships first: `flipStandardCharts` cannot be verified until the new widget is
built and installed locally, which is the same trap that made `#135`'s `tooltipText`
assertions fail this morning. Building `rhtml*` on Node 22 needs the
`resolutions: graceful-fs ^4` workaround or an older Node via nvm.

1. Widget: tests, then the merge, then `pointSymbol`, then version bump and PR.
2. Install the widget locally; browser pass on hidden-marker hover and gaps.
3. `flipStandardCharts`: the six changes above, then the version floor.

## Risks

- The merge collapses hover ownership, legend ownership and trace layering at once. The
  hidden-marker hover case is where it is most likely to prove deeper than expected; if it
  does, fall back to a legend-proxy trace (option L1: real traces keep
  `showlegend: false`, one `x: [null], y: [null]` trace per series carries the swatch) and
  confirm the change of approach before continuing.
- `PlotlyChartElements.js` serves Scatterplot and LabeledScatter as well, so every change
  has to be inert when `line.show` is false.

## Out of scope

- Per-series `marker.opacity` or `marker.border.opacity` on either path — use a colour with
  alpha. Both are scalars by contract, so #132's aim covers four settings, not five.
- `marker.colors` independent of `colors`: the widget takes one set of series colours,
  shared by markers, lines and automatically coloured data labels.
- Symbols in the custom `Legend.js` legend, which serves LabeledScatter rather than the
  grouped line chart.
