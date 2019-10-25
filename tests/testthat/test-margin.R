context("Margin text")

# A simple dataset - here we are just checking the positioning
# of the margin text, not data processing
dat <- structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21,
18, 20, 16, 14, 22, 24, 10), .Dim = c(10L, 2L), statistic = "n", .Dimnames = list(
    c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
    "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"
    ), c("Male", "Female")), name = "Q3. Age by Q2. Gender", questions = c("Q3. Age",
"Q2. Gender"))

# Long text used for footer, for checking line wrap
filler.text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."

# Pie charts do not yet have title/subtitle/footers
dist.funcs <- c("Box", "Bean", "Density", "Histogram", "Violin")
funcs <- c("Column", "Bar", "Area", "Line", "Scatter", "LabeledScatter", "Radar", "Pie", dist.funcs)
test.cases <- c('footer' = 'dat, footer = filler.text',
                'footer-size5' = 'dat, footer = filler.text, footer.font.size = 5, footer.font.color = "red"',
                'footer-xtitle' = 'dat, footer = filler.text, x.title = "Some label<br>which goes over two lines", x.title.font.size=20, footer.wrap.nchar = 150',
                'alltitles' = 'dat, footer = filler.text, title = "Chart", subtitle = "A brief description<br>that may be longer that one line", global.font.color = "red", title.font.color = "blue", subtitle.font.color = "grey", footer.font.color = "green"')

for (ff in funcs)
{
    for (i in 1:length(test.cases))
    {
        filestem <- paste0("margin-", tolower(ff), "-", names(test.cases)[i])
        if (grepl("footer-xtitle", filestem) && ff %in% c(dist.funcs, "Pie", "Radar"))
            next
        if (filestem == "margin-radar-footer-xtitle")
            next
        test_that(filestem, {

            cmd <- paste0("pp <-", ff, "(", test.cases[i], ")")
            if (ff == "Pie")
                expect_warning(eval(parse(text=cmd)), "Missing and non-positive values have been omitted. The color palette may not be shown in the way expected.")
            else
                expect_error(eval(parse(text=cmd)), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}
