context("StackedColumnWithStatisticalSignificance")

tb1d.with.zstat <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
15.75, 7, 100, 1.1375, 0.575, -0.6625, 0.237500000000001, 0.462500000000001,
-2.9125, 0.6875, 4.175, -3.7, 80), .Dim = c(10L, 2L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    c("%", "z-Statistic")), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age", questions = c("Age", "SUMMARY"
))

tb.with.zstat <- structure(c(11.6455696202532, 11.6455696202532, 10.8860759493671,
9.36708860759494, 12.9113924050633, 8.10126582278481, 10.379746835443,
16.2025316455696, 8.86075949367089, 100, 13.0864197530864, 11.8518518518519,
9.87654320987654, 13.3333333333333, 10.3703703703704, 7.65432098765432,
13.3333333333333, 15.3086419753086, 5.18518518518519, 100, 12.375,
11.75, 10.375, 11.375, 11.625, 7.875, 11.875, 15.75, 7, 100,
-0.618747215492375, -0.090587150057158, 0.46815841782747, -1.76647195462751,
1.12105856951696, 0.234650215403031, -1.29111339181446, 0.347008210529188,
2.03711608927757, NA, 0.618747215492375, 0.0905871500571577,
-0.46815841782747, 1.76647195462751, -1.12105856951696, -0.234650215403031,
1.29111339181446, -0.347008210529188, -2.03711608927757, NA,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(10L, 3L, 2L
), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
"40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET"), c("Male", "Female", "NET"), c("Column %", "z-Statistic"
)), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"
), span = list(rows = structure(list(c("18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
10L)), columns = structure(list(c("Male", "Female", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
3L))), name = "table.Age.by.Gender.2", questions = c("Age", "Gender"
))

tb.with.colcmp <- structure(c("11.6455696202532", "11.6455696202532", "10.8860759493671",
"9.36708860759494", "12.9113924050633", "8.10126582278481", "10.379746835443",
"16.2025316455696", "8.86075949367089", "100", "13.0864197530864",
"11.8518518518519", "9.87654320987654", "13.3333333333333", "10.3703703703704",
"7.65432098765432", "13.3333333333333", "15.3086419753086", "5.18518518518519",
"100", "12.375", "11.75", "10.375", "11.375", "11.625", "7.875",
"11.875", "15.75", "7", "100", NA, NA, NA, NA, NA, NA, NA, NA,
"b", "-", NA, NA, NA, NA, NA, NA, NA, NA, NA, "-", "-", "-",
"-", "-", "-", "-", "-", "-", "-", "-"), .Dim = c(10L, 3L, 2L
), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
"40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET"), c("Male", "Female", "NET"), c("Column %", "Column Comparisons"
)), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"
), span = list(rows = structure(list(c("18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
10L)), columns = structure(list(c("Male", "Female", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
3L))), name = "table.Age.by.Gender", questions = c("Age", "Gender"
))

tb.pickany.transposed <- structure(c("6.125", "10.5", "64.625", "22.375", "25.5", "9.5",
    "91.25", NA, "A", "A B D E F", "A B F", "A B F", "a", "A B C D E F"),
    .Dim = c(7L, 1L, 2L), .Dimnames = list(c("Feminine", "Innocent",
    "Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional"),
    "Coke", c("%", "Column Comparisons")), basedescriptiontext = "sample size = 800",
    basedescription = list(Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
        Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = "PickAnyGrid", span = list(
        rows = structure(list("Coke"), class = "data.frame", .Names = "", row.names = 1L),
        columns = structure(list(c("Feminine", "Innocent", "Older",
        "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
        "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    8L))), name = "table.q5", questions = c("SUMMARY", "q5"), assigned.rownames = TRUE)

tb.num.multi.colcmp <- structure(c("1.8175", "3.60375", "5.42125", "0.7675", "2.34375",
    "3.11125", "1.9775", "3.73125", "5.70875", "0.415", "0.48625",
    "0.90125", "0.27125", "0.1425", "0.41375", "0.92375", "1.55625",
    "2.48", "6.1725", "11.86375", "18.03625", "B D E F", "B D E F",
    "B D E F", "D E", "D E F", "D E f", "B D E F", "B D E F", "B D E F",
    "e", "E", "E", NA, NA, NA, "D E", "D E", "D E", "-", "-", "-"
    ), .Dim = c(3L, 7L, 2L), .Dimnames = list(c("'out and about'",
    "'at home'", "SUM"), c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi", "Diet Pepsi", "Pepsi Max", "SUM"), c("Average", "Column Comparisons"
    )), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "NumberGrid", span = list(
    rows = structure(list(c("'out and about'", "'at home'", "SUM"
    )), class = "data.frame", .Names = "", row.names = c(NA,
    3L)), columns = structure(list(c("Coca-Cola", "Diet Coke",
    "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "SUM")),
    class = "data.frame", .Names = "", row.names = c(NA, 7L))),
    name = "table.q2.3", questions = c("q2", "SUMMARY"
))

tb.from.diff <- structure(c(0, 5.65217391304348, 8.69565217391304, 16.5217391304348,
27.3913043478261, 21.304347826087, 5.65217391304348, 10.4347826086957,
4.34782608695652, 0.884955752212389, 9.29203539823009, 6.63716814159292,
19.9115044247788, 23.8938053097345, 15.929203539823, 11.9469026548673,
3.53982300884956, 7.9646017699115, -2.75482093663912, -0.408432147562579,
-3.97652413462686, -1.9355611450473, 2.597915918074, 5.3263863935801,
-0.408432147562579, 2.17031979877834, -0.61085159899389, -2.89411401522947,
-6.69633669479321, -1.21166906770941, 1.8882486108253, 4.1263634492694,
2.8478081909858, 1.1910887013789, -0.52994443301091, 1.27855525828359,
0.000741964717945443, 0.418133073631607, 0.0600445895656511,
0.272184421444679, 0.242625061309944, 0.0547325308521204, 0.418133073631607,
0.191470045897168, 0.364765042528343, 0.00829099705476146, 0.00794061460769444,
0.291462982400215, 0.288087301305384, 0.123812599371673, 0.1750137223589,
0.331634498830038, 0.372587406232211, 0.285346836002909), .Dim = c(9L,
2L, 3L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
"$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
"$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
"$200,001 or more"), c("Male", "Female"), c("Column %", "Differences",
"p")))

tb.from.diff2 <- structure(c(7.63888888888889, 18.75, 10.4166666666667, 15.9722222222222,
    24.3055555555556, 9.02777777777778, 6.25, 3.47222222222222, 4.16666666666667,
    100, 0, 12.7659574468085, 4.25531914893617, 23.4042553191489,
    2.12765957446809, 31.9148936170213, 14.8936170212766, 4.25531914893617,
    6.38297872340426, 100, 2.89855072463768, 15.9420289855072, 4.34782608695652,
    20.2898550724638, 23.1884057971014, 17.3913043478261, 5.79710144927536,
    5.79710144927536, 4.34782608695652, 100, 0, 0, 0, 40.9090909090909,
    22.7272727272727, 0, 9.09090909090909, 13.6363636363636, 13.6363636363636,
    100, 0, 25, 0, 25, 25, 0, 25, 0, 0, 100, 0, 18.1818181818182,
    15.9090909090909, 4.54545454545455, 11.3636363636364, 11.3636363636364,
    20.4545454545455, 0, 18.1818181818182, 100, 0, 0, 0, 0, 100,
    0, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 100, 0, 0, 100, 3.77906976744186,
    15.9883720930233, 7.84883720930233, 18.0232558139535, 19.7674418604651,
    13.0813953488372, 10.7558139534884, 4.06976744186047, 6.68604651162791,
    100, 3.03362573099415, 8.22368421052632, -3.07017543859649, -0.804093567251464,
    1.27923976608187, -2.48538011695907, 0.328947368421053, -3.43567251461988,
    -3.07017543859649, 0, 0, -5.65509518477043, 1.62374020156775,
    3.66741321388578, -11.030235162374, 6.91489361702128, 5.68309070548712,
    -3.63941769316909, 2.43561030235162, 0, -4.18806344859066, 3.34360378865685,
    -1.95138651146867, 0.604815702385029, 4.29076800182586, -4.6559397466621,
    -0.502111149149834, 1.07269200045647, 1.98562136254707, 0, 0,
    -3.27868852459016, -13.1147540983607, 17.9582712369598, -0.22354694485842,
    -14.7540983606557, 5.81222056631893, -1.1177347242921, 8.71833084947839,
    0, 0, 6.25, -25, 6.25, 6.25, 0, 6.25, 0, 0, 0, 0, 9.17280917280917,
    6.9000819000819, -14.3734643734644, -13.8615888615889, 0.552825552825553,
    4.23832923832924, -1.8018018018018, 9.17280917280917, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -50, 0, 50, 0, 0, 0, 0.525887306338607,
    5.09728298411236, -2.476481036808, -0.222854511364758, -2.58050722015723,
    -1.48720436827736, 2.41069372718003, -2.15371204894576, 0.886895167922107,
    0, 0.11555623969873, 0.0138085981296738, 0.170639379120402, 0.414978049227306,
    0.383840239089087, 0.205386760460202, 0.446349431686231, 0.0524258524726636,
    0.0854745770571405, NaN, NaN, 0.198556072847318, 0.322131484625733,
    0.318628915444466, 0.00730119105499911, 0.208993437446842, 0.181824847033729,
    0.19993574207354, 0.283995595195018, NaN, 0.0863881400504023,
    0.265916591391671, 0.276746410304164, 0.460109411684052, 0.244830217925401,
    0.215495628312208, 0.444108207455423, 0.376671598150181, 0.241342917159737,
    NaN, NaN, 0.0795040830489257, 0.00191179973093683, 0.0724935617765572,
    0.491671557732891, 0.00102770098776143, 0.196026891225488, 0.449672251249587,
    0.142475134037769, NaN, NaN, 0.354160413817556, 0.0204844779779181,
    0.354160413817556, 0.354160413817556, NaN, 0.354160413817556,
    NaN, NaN, NaN, NaN, 0.0810796077359789, 0.135315880985766, 0.00196948062749968,
    0.0158724953521618, 0.461315665987327, 0.275765908152407, 0.0791182599021487,
    0.0810796077359789, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, 0.0377934092108062, NaN, 0.0377934092108062,
    NaN, NaN, NaN, 0.334201957695473, 0.0135337345255848, 0.090470829490374,
    0.46497080299871, 0.166267860110695, 0.254740307491537, 0.110796549903431,
    0.062408102690838, 0.290981514101983, NaN), .Dim = c(10L, 9L,
    3L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET"), c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care",
    "NET"), c("Column %", "Differences", "p")), questiontypes = c("PickOne",
    "PickOne"), span = list(rows = structure(list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET")), class = "data.frame",
    .Names = "", row.names = c(NA, 10L)),
    columns = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola", "Don't care",
    "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    9L))), questions = c("Income", "Preferred cola"))

tb.zstat.only <- structure(c(-8.03443400756784, -0.362599159486435, 0.209925829176357,
-2.42368911867248, 9.82834563871125, 0.782450817839146, -2.89209226937453,
1.82510677193538, -2.04973529771204, 3.17287792659536, 0.140392828610414,
-0.196549960054579, -1.03211191919445, 0.516055959597225, -1.80619585859029,
2.64478679293578, -1.61267487374133, 1.29013989899307, 0.763605271365248,
0.424225150758472, -0.254535090455084, 2.9695760553093, -2.96957605530931,
-0.933295331668639, 1.51380943091758, 0.216258490131084, 0.216258490131084,
0.865033960524334, -2.595101881573, -0.216258490131082, 6.24340711873983,
-2.49736284749593, 0.832454282498644, 0.01, -3.53793070061923,
-1.0405678531233, 5.80611561772338, -1.16122312354468, 1.08630550267083,
-2.06023457403088, -2.95924602451708, -0.711717398301574, 2.32058206788793,
2.64438421689554, -0.269835124173015, -3.18405446524157, -2.21264801821872,
0.701571322849838, 4.42421810285147, -1.17708555029993, 2.96300845420327,
-3.36890002327222, -2.88183014038949, 0.0405891569068948, NA,
NA, NA, NA, NA, NA), statistic = "z-Statistic", .Dim = c(6L,
10L), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
"Pepsi", "Diet Pepsi", "Pepsi Max"), c("Never", "Once or twice a year",
"Once every 3 months", "Once a month", "Once every 2 weeks",
"Once a week", "2 to 3 days a week", "4 to 5 days a week", "Every or nearly every day",
"NET")), basedescriptiontext = "sample size = 327", basedescription = list(
    Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
    Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOneMulti", span = list(
    rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
    "Pepsi", "Diet Pepsi", "Pepsi Max")), class = "data.frame", .Names = "", row.names = c(NA,
    6L)), columns = structure(list(c("Never", "Once or twice a year",
    "Once every 3 months", "Once a month", "Once every 2 weeks",
    "Once a week", "2 to 3 days a week", "4 to 5 days a week",
    "Every or nearly every day", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Cola.drinking.frequency", questions = c("Cola drinking frequency",
"SUMMARY"))


test_that("Charts with annotations",
{
    expect_error(StackedColumnWithStatisticalSignificance(tb1d.with.zstat), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb1d.with.zstat, transpose = TRUE,
        reverse.series.order = TRUE, column.totals.above.show = TRUE), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.with.zstat), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.with.zstat, transpose = TRUE,
        reverse.series.order = TRUE, column.totals.above.show = TRUE), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.with.colcmp), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.from.diff,
                annot.hide.small.bar = FALSE), NA)
    expect_warning(StackedColumnWithStatisticalSignificance(tb.from.diff,
                annot.hide.small.bar = TRUE), "Some significant values were not shown")
    expect_error(StackedColumnWithStatisticalSignificance(tb.from.diff2,
        column.totals.above.show = TRUE, column.totals.above.font.family = "Arial Black",
        annot.differences.prefix = "(", annot.differences.suffix = ")",
        data.label.show = TRUE, reverse.series.order = TRUE, num.categories.below.axis = 2), NA)

    xx <- structure(1:10, .Names = c("A", "B", "C", "D", "E", "F", "G",
        "H", "I", "J"))
    expect_warning(StackedColumnWithStatisticalSignificance(xx),
        "No annotations for statistical signficance are shown as input tables do not contain additional cell statistics such as 'Column Comparisons' or 'z-Statistic'", fixed = TRUE)

    expect_error(StackedColumnWithStatisticalSignificance(tb.zstat.only,
            reverse.series.order = TRUE, column.totals.above.show = TRUE), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.zstat.only,
            reverse.series.order = TRUE, column.totals.above.show = TRUE,
            num.categories.below.axis = 1), "All values in input data must be positive")
})


