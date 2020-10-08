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

test_that("Charts with annotations",
{
    expect_error(StackedColumnWithStatisticalSignificance(tb1d.with.zstat), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.with.zstat), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.with.colcmp), NA)
    expect_error(StackedColumnWithStatisticalSignificance(tb.from.diff), NA)

    xx <- structure(1:10, .Names = c("A", "B", "C", "D", "E", "F", "G",
        "H", "I", "J"))
    expect_warning(StackedColumnWithStatisticalSignificance(xx),
        "Input data does not contain cell statistics")
})


