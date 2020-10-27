context("Line wrap")

test_that("Line wrap",
{
    x1 <- c("A normal length", "B", "", "D")
    res1 <- autoFormatLongLabels(x1, wordwrap = TRUE, n = 100)
    expect_equal(res1, c("A normal length", "B", "", "D"))

    x2 <- c("The quick brown dog", "The lazy dog", "The woman who swallowed a horse is dead of course")
    res2 <- autoFormatLongLabels(x2, wordwrap = TRUE, n = 10)
    expect_equal(res2, c("The quick<br>brown dog", "The lazy<br>dog",
                         "The woman<br>who<br>swallowed<br>a horse is<br>dead of<br>course"))

    x.trailing <- c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ",
        "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care")
    res3 <- autoFormatLongLabels(x.trailing, wordwrap = TRUE)
    expect_equal(res3, c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi",
        "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care"))

    x.with.html <- "<b>Bugs & Spiders</b><br>(10 cases missing)"
    res4 <- autoFormatLongLabels(x.with.html, wordwrap = TRUE, n = 10)
    expect_equal(res4, "<b>Bugs &<br>Spiders</b><br>(10 cases<br>missing)")

    res5 <- autoFormatLongLabels(x.with.html, wordwrap = TRUE, n = 5)
    expect_equal(res5, "<b>Bugs<br>&<br>Spiders</b><br>(10<br>cases<br>missing)")

    x.short <- "Part-time worker"
    res6 <- autoFormatLongLabels(x.short, wordwrap = TRUE, n = 6)
    expect_equal(res6, "Part-time<br>worker")

    x.short.html <- "<b>Part-time</b> worker"
    res7 <- autoFormatLongLabels(x.short.html, wordwrap = TRUE, n = 6)
    expect_equal(res7, "<b>Part-time</b><br>worker")

    x.full.line <- c("A very very very long label for testing",
                     "Cost is a factor when deciding to SMS or phone")
    res8 <- autoFormatLongLabels(x.full.line, wordwrap = TRUE, n = 21)
    expect_equal(res8, c("A very very very long<br>label for testing",
                         "Cost is a factor when<br>deciding to SMS or<br>phone"))

})
