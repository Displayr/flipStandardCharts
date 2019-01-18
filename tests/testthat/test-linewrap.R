context("Line wrap")

test_that("Line wrap",
{
    x1 <- c("A normal length", "B", "", "D")
    res1 <- flipStandardCharts:::autoFormatLongLabels(x1, wordwrap = TRUE, n = 100)
    expect_equal(x1, res1)

    x2 <- c("The quick brown dog", "The lazy dog", "The woman who swallowed a horse is dead of course")
    res2 <- flipStandardCharts:::autoFormatLongLabels(x2, wordwrap = TRUE, n = 10)
    expect_equal(res2, c("The quick<br>brown dog", "The lazy<br>dog",
                         "The woman<br>who<br>swallowed<br>a horse is<br>dead of<br>course"))

})
