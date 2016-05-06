context("AsChartMatrix")
#  rm(list=ls())

data("good.examples")
data("bad.examples")
data("errorAsChartMatrix.examples")
data("errorIsChartMatrix.examples")
counter = 0
examples <- list(good = good.examples, bad = bad.examples, error = errorAsChartMatrix.examples, errorA = errorIsChartMatrix.examples)
for (example in examples)
{
    counter = counter + 1
    for (i in 1:length(example))
    {
        ex = example[[i]]
        type = names(examples)[counter]

        if (type != "error")
        {
            test_that(paste(type, names(example)[i], i), {
                my.chart.matrix <- AsChartMatrix(y = ex$Y, x = ex$X, transpose = ex$transpose)
                expect_that(IsChartMatrix(my.chart.matrix, ex$n.row, ex$n.columns),
                            switch(type,
                                   "good" = is_true(),
                                   "bad" = is_false(),
                                   "errorA" = throws_error()))
            })
        }
        else
        {
            test_that(paste(type, names(example)[i], i), {
                expect_that(AsChartMatrix(y = ex$Y, x = ex$X, transpose = ex$transpose), throws_error())
            })
        }
    }
}


# zx = error.examples[[9]]
# yx <- AsChartMatrix(y = zx$Y, x = zx$X, transpose = zx$transpose)
# IsChartMatrix(yx, 1, 5)

# data("good.examples")
# data("bad.examples")
# data("error.examples")
# counter = 0
# examples <- list(good = good.examples, bad = bad.examples, error = error.examples)
# for (example in examples)
# {
#     counter = counter + 1
#     for (i in 1:length(example))
#     {
#         ex = example[[i]]
#         type = names(examples)[counter]
#         test_that(paste(type, names(example)[i], i), {
#             my.chart.matrix <- AsChartMatrix(y = ex$Y, x = ex$X, transpose = ex$transpose)
#             expect_that(IsChartMatrix(my.chart.matrix, ex$n.row, ex$n.columns),
#                 switch(type,
#                        "good" = is_true(),
#                        "bad" = is_false(),
#                        "error" = throws_error()))
#         })
#     }
# }
#
# zx = error.examples[[1]]
# yx <- AsChartMatrix(y = zx$Y, x = zx$X, transpose = zx$transpose)
# IsChartMatrix(yx, 1, 5)

#
# at(x, is_true()) expect_true(x)
# expect_that(x, is_false()) expect_false(x)
# expect_that(x, is_a(y)) expect_is(x, y)
# expect_that(x, equals(y)) expect_equal(x, y)
# expect_that(x, is_equivalent_to(y)) expect_equivalent(x, y)
# expect_that(x, is_identical_to(y)) expect_identical(x, y)
# expect_that(x, matches(y)) expect_matches(x, y)
# expect_that(x, prints_text(y)) expect_output(x, y)
# expect_that(x, shows_message(y)) expect_message(x, y)
# expect_that(x, gives_warning(y)) expect_warning(x, y)
# expect_that(x, throws_error(y))
#
# data("good.examples")
# test.AsChartMatrix(good.examples, expect = "true")
# test.AsChartMatrix(bad.examples, expect = "false")
# test.AsChartMatrix(error.examples, expect = "error")
#
#
#
#
#
#
#
# test.AsChartMatrix <- function(test.list, expect = "true")
# {
#     item.length <- length(test.list)
#
#     for (i in 1:item.length)
#     {
#         example <- test.list[[i]]
#
#         if (expect == "true")
#         {
#             test_that(names(test.list)[i], {
#                 my.chart.matrix <- AsChartMatrix(y = example$Y, x = example$X, transpose = example$transpose)
#                 expect_true(IsChartMatrix(my.chart.matrix, nrow(my.chart.matrix), ncol(my.chart.matrix)))
#             })
#         }
#         else if (expect == "false")
#         {
#             test_that(names(test.list)[i], {
#                 my.chart.matrix <- AsChartMatrix(y = example$Y, x = example$X, transpose = example$transpose)
#                 expect_false(IsChartMatrix(my.chart.matrix, nrow(my.chart.matrix), ncol(my.chart.matrix)))
#             })
#         }
#         else if (expect == "error")
#         {
#             test_that(names(test.list)[i], {
#                 expect_error(AsChartMatrix(y = example$Y, x = example$X, transpose = example$transpose))
#             })
#         }
#     }
#
#     return("All passed")
# }
#

