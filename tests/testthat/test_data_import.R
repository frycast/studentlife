testthat::context("loading SL_tibbles")


testthat::test_that( "load tables from sensing", {
  loc <- "testdata/SL_testdata"
  for (i in 1:length(menu_data$sensing)) {
    studs <- load_SL_tibble(schema = "sensing", table = studentlife:::menu_data$sensing[i],
                            location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs) > 0)
  }
})

test_that( "load tables from EMA", {
  loc <- "testdata/SL_testdata"
  for (i in 1:length(menu_data$EMA)) {
    studs <- load_SL_tibble(schema = "EMA", table = studentlife:::menu_data$EMA[i],
                            location = loc, csv_nrows = 1, vars = c("resp_time"))
    testthat::expect_true(nrow(studs) > 0)
  }
})


test_that( "load tables from education", {
  loc <- "testdata/SL_testdata"
  for (i in 1:length(menu_data$education)) {
    studs <- load_SL_tibble(schema = "education", table = studentlife:::menu_data$education[i],
                            location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs) > 0)
  }
})


test_that( "load tables from survey", {
  loc <- "testdata/SL_testdata"
  for (i in 1:length(menu_data$survey)) {
    studs <- load_SL_tibble(schema = "survey", table = studentlife:::menu_data$survey[i],
                            location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs) > 0)
  }
})

test_that( "load tables from other", {
  loc <- "testdata/SL_testdata"
  for (i in 1:length(menu_data$other)) {
    studs <- load_SL_tibble(schema = "other", table = studentlife:::menu_data$other[i],
                            location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs) > 0)
  }
})


