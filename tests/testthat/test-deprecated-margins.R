context("(deprecated) Margins")

# These are already tested in the Standard R tests, but are repeated here
# so that developers can quickly re-run them. However, they have limited use
# as unit tests. The important thing is to check the output visually

table13 <- structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21,
18, 20, 16, 14, 22, 24, 10), .Dim = c(10L, 2L), statistic = "n", .Dimnames = list(
    c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
    "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"
    ), c("Male", "Female")), name = "Q3. Age by Q2. Gender", questions = c("Q3. Age",
"Q2. Gender"))

tab3 <- structure(c(2.98165137614679, 4.11009174311927, 3.07339449541284,
2.63302752293578, 3.34862385321101, 2.45565749235474, 3.40366972477064,
3.52905198776758, 4.02752293577982, 2.28440366972477), .Dim = 10L, statistic = "Average", .Dimnames = list(
    c("My friends would describe me as cultured, and refined",
    "I think it is important to be honest when giving complements",
    "I can be a little naïve at times", "I am the life of the party",
    "I am relaxed most of the time and not easily worried", "Living in a big city is important to me",
    "I think it is important to follow and maintain traditions",
    "I enjoy being attractive to the opposite sex", "I am young at heart",
    "I follow all the latest fashions")), name = "Q25. Respondent image (number multi)", questions = c("Q25. Respondent image (number multi)",
"SUMMARY"))

df <- structure(list(A = c(1, 2, 3, 4, 5, 6, 7, NaN, 9, 10),
B = c(2,3, NaN, 5, 6, 7, 8, NaN, 10, 11)), .Names = c("A", "B"), row.names = c("A",
"B", "C", "D", "E", "F", "G", "H", "I", "J"), class = "data.frame")

dat.longlabels <- structure(c(7.91102010395624, 6.96526937072327, 5.46318510403465,
5.38509189451583, 4.78058885624491, 4.33171962498696, 3.89343951521741,
3.55479479224441, 3.49925232784262, 3.2548930765863, 3.07402467818954,
3.0495443587413, 2.89422593526089, 2.79615757754444, 2.56564004442188,
1.61841320359909, 1.58338586529381, 1.55622840223137, 1.39226282992314,
1.15765067144292, 1.14529457127121, 1.11671971850934, 1.03469831507199,
0.239788448851907, -0.338584394730979, -0.652596783697478, -0.65957266310851,
-1.40220203380981, -1.69973936013571, -2.25076657961354, -2.69640999321625,
-4.76387713463588, -4.87316019423645, -6.39980157610997, -7), .Names = c("Reliable",
"Fun", "Upper-class", "Confident", "Sexy", "Intelligent", "Honest",
"Traditional", "Down-to-earth", "Health-conscious", "Humorous",
"Up-to-date", "Imaginative", "Beautiful", "Charming", "Carefree",
"Youthful", "Open to new experiences", "Masculine", "Wholesome",
"Urban", "Outdoorsy", "Hip", "Tough", "Innocent", "Rebellious",
"Older", "Weight-conscious", "Reckless", "Individualistic", "Feminine",
"Trying to be cool", "Sleepy", "Unconventional", "Very Very Very Very Long And Tedious And Way Too Long Label That Will Be Truncated"
))

test_that("Axis and data labels placement", {

    # wordwrap
    print(Chart(dat.longlabels[30:35], wordwrap.nchar=4))
    print(Chart(dat.longlabels[c(33:35,31:32)], wordwrap.nchar=4, x.title="X Axis", x.title.font.size=40))
    print(Chart(dat.longlabels[1:5], x.title="Kilometers per hour<br>from city centre", x.tick.angle=90, x.title.font.size=30))

    # bottom margin spacing
    print(Chart(dat.longlabels[c(1:5)], x.title="Kilometers per hour<br>from city centre"))
    print(Chart(dat.longlabels[c(1:5)], x.title="Kilometers per hour"))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour"))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour<br>from city center")) # too close, but cannot control x.title position
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour<br>from city center", x.title.font.size=20))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour<br>from city centre", x.tick.angle=90, x.title.font.size=20))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour<br>from city centre", x.tick.angle=90, x.title.font.size=12))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.title="Kilometers per hour<br>from city centre", x.tick.angle=90, x.tick.font.size=20))
    print(Chart(dat.longlabels[c(33:35,31:32)], x.tick.angle=90, x.tick.font.size=20))
    print(Chart(dat.longlabels[1:5], x.title="Kilometers per hour<br>from city centre", x.title.font.size=12, x.position="top"))
    print(Chart(dat.longlabels[1:5], x.title="Kilometers per hour<br>from city centre", x.title.font.size=42, x.position="top", x.tick.font.size=20))

    # footer
    print(Chart(dat.longlabels[1:5], type="Column", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))
    print(Chart(dat.longlabels[1:5], type="Column", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="<br>Kilometers per hour<br>from city center<br>", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="Kilometers per hour<br>from city center<br>", x.title.font.size=20, footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", x.title="Kilometers per hour", x.title.font.size=20, footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))


    x <- structure(c(0.833733170755138, -0.276047773214867, -0.355001838033406,
        0.0874874238042033, 2.25225573054322, 0.834460129133672, 1.31241550858861,
        2.50264540789068, 1.16823174338441, -0.426165577414356, -0.996129751611455,
        0.690224528114173, -0.134164642588417, 0.953253005573708, -0.10334193865127,
        -0.50703570126551, 0.102968160045374, -0.422491722827557, 0.614049435502584,
        -1.23656291494856, 1.13514560752989, -0.623717030867438), .Dim = c(11L,
        2L), .Dimnames = list(c("mNxRu", "ZH78N", "aJwAy", "4Wk8S", "l5pLl",
        "Kfmye", "9AiND", "cCMaz", "1zMGx", "1wxia", "P9VVC"), c("A",
        "B")))
    x.dates <- 1:10
    names(x.dates) <- sprintf("%02d/01/2017", x.dates)

    # subtitle
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", title="Chart", subtitle="The quick brown fox jumped over the lazy dog"))
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog"))
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", subtitle="The quick brown fox jumped over the lazy dog"))
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", x.title="Kilometers per hour", x.title.font.size=20, title.font.size=40, footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog"))
    print(Chart(dat.longlabels[c(33:35, 31:32)], type="Column", x.title="Kilometers per hour", x.title.font.size=20, title.font.size=40, footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog<br>And the dish ran away with the spoon"))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="Kilometers per hour", x.title.font.size=20, title.font.size=40, footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog<br>And the dish ran away with the spoon"))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="Kilometers per hour<br>", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog"))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="Kilometers per hour", footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", title="Chart", subtitle="The quick brown fox jumped over the lazy dog<br>And the cat ran away with the spoon"))
    print(Chart(dat.longlabels[1:5], type="Column", x.title="Kilometers per hour", title="Chart", subtitle="The quick brown fox jumped over the lazy dog<br>And the cat ran away with the spoon"))

    # with legend
     expect_warning(print(Chart(df, title="Chart", subtitle="Comparing different random numbers", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all.")))

     # scatterplot
     expect_warning(print(Chart(df, type= "Scatterplot", title="Chart", subtitle="Comparing different random numbers", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all.")))

     # radar chart
     print(Chart(table13, type="Radar", title="Chart"))
     print(Chart(table13, type="Radar", title="Chart", subtitle="Comparing different random numbers<br>", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all."))
     print(Chart(abs(x[1:10,1]), type="Radar", title="Chart", subtitle="Comparing different random numbers<br>", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all."))
     print(Chart(abs(x[1:10,1:2]), type="Radar", title="Chart", subtitle="Comparing different random numbers<br>", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all."))
     print(Chart(abs(x[1:10,1:2]), type="Radar", title="Chart", subtitle="Comparing different random numbers<br>", x.title="Random number", footer="This chart contains no real data and should not be considered meaningful at all.", title.font.size=40))
    print(Chart(df[-c(3,8),], footer="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", type="Radar", footer.wordwrap=100))

     # miscellaneous errors
     #expect_warning(print(Chart(x.dates, x.tick.angle=0)))
    })

