context("Small Multiples")

dat <- structure(c(0.778425656, 0.402332362, 0.250728863, 0.361516035,
0.448979592, 0.455696203, 0.151898734, 0.379746835, 0.253164557,
0.46835443, 0.488888889, 0.631111111, 0.682222222, 0.595555556,
0.711111111, 0.402247191, 0.606741573, 0.703370787, 0.64494382,
0.687640449, 0.285714286, 0.314285714, 0.314285714, 0.542857143,
0.6, 0.285714286, 0.367346939, 0.469387755, 0.408163265, 0.510204082,
0.230496454, 0.407801418, 0.29787234, 0.35106383, 0.429078014
), .Dim = c(5L, 7L), .Dimnames = list(c("Price", "Access", "Range",
"Fresh", "Quality"), c("Aldi", "Costco", "Coles", "Woolworths",
"Harris Farm", "Foodland", "IGA")), assigned.rownames = TRUE)

test_that("Small Multiples",
{
    expect_error(SmallMultiples(dat, "Area", x.order="2,1,3,4,5,6"), NA) # only 6 out 7 columns shown
    expect_error(SmallMultiples(dat, "Area", x.order=""), NA)
})
