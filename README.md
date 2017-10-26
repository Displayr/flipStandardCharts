[![](https://travis-ci.org/Displayr/flipStandardCharts.svg?branch=master)](https://travis-ci.org/Displayr/flipStandardCharts/)
[![Coverage Status](https://coveralls.io/repos/github/Displayr/flipStandardCharts/badge.svg?branch=master)](https://coveralls.io/github/Displayr/flipStandardCharts?branch=master)
# flipStandardCharts

Standard R interactive charts

## To install from GitHub
```
require(devtools)
install_github("Displayr/flipStandardCharts")
```

## Running tests
Unit tests for flipStandardCharts include generating pngs of some files and
comparing against previous pngs. The packages required for running
visual tests can be set up by:
```
install.packages("devtools")
install.packages("testthat")
install_github("MangoTheCat/visualTest")
install.packages("webshot")
webshot::install_phantomjs()
```
Once this is set-up, the visual tests will automatically run
along with the other unit tests when `devtools::test()` (or CMD-T in RStudio)
is run. 

The snapshots of the accepted tests are stored in `tests/testthat/accepted-snapshots`. New tests that are run for the first time will automatically generate new snapshots in `tests/testthat/accepted-snapshots`. Failed tests will generate snapshots with the corresponding name in `tests/testthat`. To accept the new snapshot, delete the old snapshot, and a new snapshot will automatically be created when `devtools::test()` is next run. . 

Note that `devtools::check()` (CMD-E in RStudio) will also run the tests and give appropriate error messages but no snapshots are output.

[![Displayr logo](https://mwmclean.github.io/img/logo-header.png)](https://www.displayr.com)
