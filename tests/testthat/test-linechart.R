context("Line chart")

data("WorldPhones")

test_that("Line thickness",
{
    expect_error(Line(WorldPhones, line.thickness = ""), NA)
    expect_error(Line(WorldPhones, line.thickness = "1,4"), NA)
    expect_warning(Line(WorldPhones, line.thickness = "blah"),
                   "Non-numeric line thickness value 'blah' was ignored")
    expect_warning(Line(WorldPhones, line.thickness = "6,5,4..3,2,l"),
                   "Non-numeric line thickness values '4..3', 'l' were ignored")
})
