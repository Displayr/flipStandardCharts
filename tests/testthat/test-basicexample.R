context("Basic example")
# This file demonstrates how to test combinations of parameters
# It can be then used to create snapshot tests
# See flipChartTests/tests/testthat/test-basicexample.R

# Set up various data types to test
set.seed(1234)
random <- rnorm(20)
positives <- abs(random)

# Create lists of all charting functions, and data types and options to use
# Functions excluded: Waterfall, Venn, Stream, HeatMap
charting.funcs <- c("Column", "Bar", "Area", "Line", "Pie", "Radar",
                    "Scatter", "LabeledScatter", "Box", "Bean", "Distribution",
                    "Density", "Violin")
dat.list <- c("random", "positives")

# Iterate through all combinations of charting functions and data types
for (func in charting.funcs)
{
    for (dat in dat.list)
    {
        # Create name which will appear in the error message if test fails
        # Filestem should be prefixed by test file name to avoid name conflicts
        filestem <- paste0("basicexample-", tolower(func), "-", dat)

        test_that(filestem, {

            # Create command that will create widget
            cmd <- paste0("pp <- ", func, "(", dat, ")")

            # Run command and check outputs
            # First, isolate specific cases which behave differently
            # For this example pie chart and radar charts
            # handle negative values differently from the other chart types
            if (filestem == "basicexample-pie-random")
                expect_warning(eval(parse(text=cmd)))


            # For snapshotting, 'expect_warning' can be unchanged,
            # But when widgets are expected to give error, they should
            # be skipped before entering 'test_that'
            # (i.e. line 44 moved to line 26, and use 'next' to skip snapshot)
            else if (filestem == "basicexample-radar-random")
                expect_error(eval(parse(text=cmd)))

            # General case for all other combinations
            else
                expect_error(eval(parse(text=cmd)), NA)

            # The following lines are useful for interactive viewing
            # But should be commented out after everything is set up
            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

            # Create snapshot and compare against reference (in flipChartTests)
            # If none exists, the snapshot will be accepted as the reference
            #expect_true(TestWidget(pp, filestem))



        })
    }
}
