context("Margin padding")

x1 <- structure(c(1:2, NA, 4:10), .Names = c("a", "b", "c", "d", "e", "f", "g",
"h", "i", "j"))
x2 <- matrix(rpois(28, 5), 7, 4, dimnames=list(letters[1:7], LETTERS[1:4]))
x2[,2] <- NA
opt <- paste("margin.left = 0, margin.right = 0, margin.top = 0, margin.bottom = 0,",
             "margin.autoexpand = FALSE, data.label.show = TRUE,",
             "charting.area.fill.opacity = 0.4, charting.area.fill.color = \"red\",",
             "x.tick.show = FALSE, y.tick.show = FALSE", collapse = "")
charting.funcs <- c("Column", "Bar", "Pyramid", "BarMultiColor", "ColumnMultiColor",
                    "Area", "Line")

# Iterate through all combinations of charting functions and data types
for (func in charting.funcs)
{
    filestem <- paste0("marginpadding-", tolower(func))
    test_that(filestem,
    {
        # Create command that will create widget
        cmd <- paste0("pp <- ", func, "(x1, ", opt, ")")
        if (func %in% c("BarMultiColor", "ColumnMultiColor", "Pyramid"))
            expect_error(eval(parse(text=cmd)), NA)
        else
            expect_warning(eval(parse(text=cmd)), "Missing")
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    })

    filestem <- paste0("marginpadding-smallmult-", tolower(func))
    test_that(filestem,
    {
        # Create command that will create widget
        cmd <- paste0("pp <- SmallMultiples(x2, \"", func, "\", ",
                        opt, ", nrow = 1)")
        if (func %in% c("BarMultiColor", "ColumnMultiColor", "Pyramid"))
            expect_warning(eval(parse(text=cmd)), "Ignoring")
        else
            expect_warning(eval(parse(text=cmd)), "Missing")
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    })
}
