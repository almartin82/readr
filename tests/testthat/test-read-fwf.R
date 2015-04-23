context("read_fwf")

test_that("trailing spaces ommitted", {
  spec <- fwf_empty("fwf-trailing.txt")
  expect_equal(spec$begin, c(0, 4))
  expect_equal(spec$end, c(3, 7))

  df <- read_fwf("fwf-trailing.txt", spec)
  expect_equal(df$X1, df$X2)
})


test_that("read_fwf correctly reads a text file from the web", {
  test_url <- "http://www.nj.gov/education/schools/achievement/14/njask5/state_summary.txt"

  ex <- read_fwf(
    file = test_url,
    col_positions = fwf_empty(test_url)
  )

  expect_equal(nrow(ex), 1860)
  expect_equal(ncol(ex), 184)

})
