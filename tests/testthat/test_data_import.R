testthat::context("loading SL_tibbles")

#loc <- "tests/testthat/testdata/SL_testdata"
#studs <- load_SL_tibble(schema = "sensing", table = studentlife:::menu_data$sensing[1],
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
    testthat::expect_true(
      "timestamp_SL_tbl" %in% class(studs_list[[!!i]])
      || "interval_SL_tbl" %in% class(studs_list[[!!i]]))
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
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
    testthat::expect_true(
      "timestamp" %in% names(studs_list[[!!i]])
      || "start_timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
    testthat::expect_true(
      "timestamp_SL_tbl" %in% class(studs_list[[!!i]])
      || "interval_SL_tbl" %in% class(studs_list[[!!i]]))
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
  }
})

testthat::test_that( "load tables from sensing with missing csv_nrows", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$sensing)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "sensing", table = studentlife:::menu_data$sensing[i],
      location = loc)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true(
      "timestamp" %in% names(studs_list[[!!i]])
      || "start_timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
    testthat::expect_true(
      "timestamp_SL_tbl" %in% class(studs_list[[!!i]])
      || "interval_SL_tbl" %in% class(studs_list[[!!i]]))
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
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
    testthat::expect_s3_class(studs_list[[!!i]], "timestamp_SL_tbl")
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
  }
})

testthat::test_that( "load tables from EMA with vars = uid", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$EMA)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "EMA", table = studentlife:::menu_data$EMA[i],
      location = loc, csv_nrows = 1, vars = "timestamp")
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true("timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
    testthat::expect_s3_class(studs_list[[!!i]], "timestamp_SL_tbl")
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
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
    if ( studentlife:::menu_data$education[i] == "deadlines" )
      testthat::expect_s3_class(studs_list[[!!i]], "dateonly_SL_tbl")
  }
  testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
})

testthat::test_that( "load tables from survey", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$survey)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "survey", table = studentlife:::menu_data$survey[i],
      location = loc, csv_nrows = 1)
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
  }
})


testthat::test_that( "load tables from survey with vars = uid and csv_nrows missing", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$survey)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "survey", table = studentlife:::menu_data$survey[i],
      location = loc, vars = "uid")
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
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
    testthat::expect_s3_class(studs_list[[!!i]], "timestamp_SL_tbl")
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
  }
})


testthat::test_that( "load tables from other with vars = timestamp", {
  loc <- "testdata/SL_testdata"
  studs_list <- list()
  for (i in 1:length(menu_data$other)) {
    studs_list[[i]] <- load_SL_tibble(
      schema = "other", table = studentlife:::menu_data$other[i],
      location = loc, csv_nrows = 1, vars = "timestamp")
    testthat::expect_true(nrow(studs_list[[!!i]]) > 0)
    testthat::expect_true("timestamp" %in% names(studs_list[[!!i]]))
    testthat::expect_true("uid" %in% names(studs_list[[!!i]]))
    testthat::expect_s3_class(studs_list[[!!i]], "timestamp_SL_tbl")
    testthat::expect_s3_class(studs_list[[!!i]], "SL_tbl")
  }
})


