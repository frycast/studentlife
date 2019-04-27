testthat::context("loading SL_tibbles")

#loc <- "tests/testthat/testdata/SL_testdata"
#studs <- load_SL_tibble(schema = "sensing", table = studentlife:::menu_data$sensing[10],
#                        location = loc, csv_nrows = 1, vars = "timestamp"); studs

testthat::test_that( "load tables from sensing with vars = timestamp", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$sensing)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "sensing", table = studentlife:::menu_data$sensing[i],
      location = loc, csv_nrows = 1, vars = "timestamp")
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true(
      "timestamp" %in% names(studs_list[[!!i]])
      || "start_timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
  }
})

testthat::test_that( "load tables from sensing", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$sensing)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "sensing", table = studentlife:::menu_data$sensing[i],
      location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
  }
})

testthat::test_that( "load tables from EMA with vars = timestamp", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$EMA)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "EMA", table = studentlife:::menu_data$EMA[i],
      location = loc, csv_nrows = 1, vars = "timestamp")
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true("timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
  }
})

testthat::test_that( "load tables from education", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$education)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "education", table = studentlife:::menu_data$education[i],
      location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
  }
})

testthat::test_that( "load tables from survey", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$survey)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "survey", table = studentlife:::menu_data$survey[i],
      location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
  }
})

testthat::test_that( "load tables from other", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$other)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "other", table = studentlife:::menu_data$other[i],
      location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true("timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
  }
})


