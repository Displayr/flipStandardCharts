{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipStandardCharts";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
    Wrapper for other charting packages. The goal is to provide a
    relatively standard API across multiple packages, so that a user can create
    just about any 'standard' chart easily.
  '';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    jsonlite
    flipTransformations
    verbs
    httr
    rhtmlCombinedScatter
    rhtmlLabeledScatter
    flipTables
    hash
    rhtmlHeatmap
    flipChartBasics
    sp
    mgcv
    weights
    janitor
    abind
    plotly
    flipData
    parcoords
    htmltools
    Hmisc
    flipFormat
    pryr
    rhtmlPalmTrees
    htmlwidgets
    rhtmlDonut
    d3vennR
    flipU
    rhtmlPictographs
    xts
    flipTime
    stringr
    leaflet
    streamgraph
    dygraphs
    xml2
  ];
}
