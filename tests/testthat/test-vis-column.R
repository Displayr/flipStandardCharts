context("Column Chart")

test_that("Default options", {
    xx <- 1:10
    names(xx) <- letters[1:10]
    expect_error(pp <- Column(xx), NA)
    filestem <- "column-default"
    acceptedfile <- paste0("accepted-snapshots/", filestem, ".png")
    difffile <- paste0(filestem, "-diff.png")


    # If accepted snapshot does not exist, create snapshot in accepted-snapshots
    if (!file.exists(acceptedfile))
        plotly::export(pp$plotly.plot, file=acceptedfile)
    else
    {
        # Create new snapshot
        plotly::export(pp$plotly.plot, file=difffile)
        res <- expect_equal(visualTest::isSimilar(file = difffile,
                              fingerprint = acceptedfile,
                              threshold = 0.001), TRUE)

        # If test fails, leave diff for visual inspection
        if (res)
            unlink(difffile)
    }
})
