data.with.stats <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
18.4573002754821, 24.7933884297521, 15.9779614325069, 6.06060606060606,
8.26446280991736, 4.95867768595041, 100, 3.77906976744186, 15.9883720930233,
7.84883720930233, 18.0232558139535, 19.7674418604651, 13.0813953488372,
10.7558139534884, 4.06976744186047, 6.68604651162791, 100, 3.25318246110325,
10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
100, 0.442913092343004, 0.0000228306627828578, 0.0351514682974756,
0.881274082835059, 0.108843509396061, 0.275202305069102, 0.0240561692086175,
0.0210216801333983, 0.326003170694519, NA, 0.442913092343004,
0.0000228306627828578, 0.0351514682974756, 0.881274082835059,
0.108843509396061, 0.275202305069102, 0.0240561692086175, 0.0210216801333983,
0.326003170694519, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
), .Dim = c(10L, 3L, 2L), .Dimnames = list(c("Less than $15,000",
"$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
"$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
"$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
"Female", "NET"), c("Column %", "p")), name = "Income by Gender", questions = c("Income",
"Gender"))

Column(data.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p", type = "Arrow - up", threstype = "above threshold", threshold = 0.05, color = "#FF0000", size = 15)))

vec.with.stats <- structure(c(3.25318246110325, 10.8910891089109, 10.3253182461103,
18.2461103253182, 22.3479490806223, 14.5685997171146, 8.34512022630834,
6.22347949080622, 5.7991513437058, 100, 2.96352779053704e-11,
0.852323672450741, 0.506154634139539, 1.57276658363514e-09, 0,
0.00344142405418046, 0.0192720273455812, 0.0000354510822852101,
0.00000697993687950735, 0), .Dim = c(10L, 2L), .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET"), c("%", "p")), name = "Income", questions = c("Income",
"SUMMARY"))

Column(vec.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p", type = "Up arrow", threstype = "above threshold", threshold = 0.05, color = "#0000FF")))

Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Circle - filled", size = 30, color = "red", threstype = "above threshold", threshold = 0.05), list(data = "p", type = "Circle - thin outline", size = 30, color = "blue")))

Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Filled circle", size = 35, color = "blue"), list(data = "p", type = "Filled circle", size = 30, color = "red")))

Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Circle - filled", size = 40, color = "red"), list(data = "p", type = "Circle - filled", size = 30, color = "orange"), list(data = "p", type = "Circle - filled", size = 20, color = "yellow")))


aa <- list(list(type = "Circle - thin outline", data = "p", threstype = "above threshold",
    threshold = "-Inf", color = "red", size = 20, width = 1,
    offset = 0, font.family = "Arial", font.weight = "normal",
    font.style = "normal"), list(type = "Circle - thick outline", data = "p",
    threstype = "above threshold", threshold = "0.05", color = "blue",
    size = 20, width = 1, offset = 0, font.family = "Arial",
    font.weight = "normal", font.style = "normal"))

a2 <- list(list(type = "Circle - filled", data = "p", threstype = "below threshold",
    threshold = "0.10", color = "#C0C0C0", size = 25, width = NULL,
    offset = NULL, font.family = NULL, font.weight = NULL, font.style = NULL),
    list(type = "Circle - filled", data = "p", threstype = "below threshold",
        threshold = "0.05", color = "#80FFFF", size = 20, width = NULL,
        offset = NULL, font.family = NULL, font.weight = NULL,
        font.style = NULL), list(type = "Arrow - up", data = "p",
        threstype = "below threshold", threshold = "0.05", color = "#CD343C",
        size = 15, width = NULL, offset = NULL, font.family = NULL,
        font.weight = NULL, font.style = NULL))
a3 <- list(list(type = "Circle - filled", data = "p", threstype = "above threshold",
    threshold = "-Inf", color = "red", size = 20, width = 1,
    offset = 0, font.family = "Arial", font.weight = "normal",
    font.style = "normal"), list(type = "Circle - thin outline", data = "p",
    threstype = "above threshold", threshold = "0.05", color = "blue",
    size = 20, width = 1, offset = 0, font.family = "Arial",
    font.weight = "normal", font.style = "normal"))


