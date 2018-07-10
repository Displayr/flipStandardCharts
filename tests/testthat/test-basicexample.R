context("Basic example")
# This file demonstrates how to test combinations of parameters
# It can be then used to create snapshot tests
# See flipChartTests/tests/testthat/test-basicexample.R

# Set up various data types to test
set.seed(1234)
random <- rnorm(20) # includes negative numbers
positives <- abs(random)

# Create lists of all charting functions, and data types and options to use
# Functions excluded: Waterfall, Venn, Stream, HeatMap
charting.funcs <- c("Column", "Bar", "Area", "Line", "Pie", "Donut", "Radar",
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
            # We need to separate cases which behave differently
            # For this example pie chart and radar charts handle
            # negative values differently from the other chart types
            # but only radar charts give an error
            if (filestem == "basicexample-radar-random")
            {
                expect_error(eval(parse(text=cmd)))
            }
            else
            {
                # These can be grouped together using suppressWarning
                # But this is more informative
                if (filestem == "basicexample-pie-random" || filestem == "basicexample-donut-random")
                    expect_warning(eval(parse(text=cmd)))
                else
                    expect_error(eval(parse(text=cmd)), NA)

                # The following lines are useful for interactive viewing
                # But should be commented out after everything is set up
                #print(pp)
                #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

                # Create snapshot and compare against reference (in flipChartTests)
                # If none exists, the snapshot will be accepted as the reference
                #expect_true(TestWidget(pp, filestem))
            }
        })
    }
}
