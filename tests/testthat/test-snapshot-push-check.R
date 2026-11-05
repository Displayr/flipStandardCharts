# DPI-5056 test only (do not merge): the committed accepted snapshot is deliberately wrong, so CI has a mismatch to
# publish and push.
context("Snapshot push check")

test_that("Column chart matches its accepted snapshot", {
    pp <- Column(c(a = 1, b = 2, c = 3))
    expect_true(flipSnapshotTestUtils::TestWidget(pp, "snapshot-push-check-column"))
})
