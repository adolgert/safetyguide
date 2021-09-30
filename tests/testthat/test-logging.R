for (sll_level in c("trace", "DEBUG", "Info", "warn", "error", "fatal")) {
  test_that("logging string_log_level happy path finds strings any case", {
    expect_true(is.integer(safetyguide:::string_log_level(sll_level)))
  })
}


test_that("logging string_log_level warns that name not found", {
  warned <- tryCatch(
    safetyguide:::string_log_level("nonexistent"),
    warning = function(w) {w[[1]]}
  )
  expect_equal(grep("nonexistent", warned), 1L)
})


test_that("logging.local_logging sets the default level to info", {
  local_logging()
  expect_equal(grep("kitsch", capture_output(safetyguide:::logerror("kitsch"))), 1)
  expect_equal(grep("unique", capture_output(safetyguide:::loginfo("unique"))), 1)
  expect_equal(
    grep("cowboy", capture_output(safetyguide:::logdebug("cowboy"))), integer(0))
})


test_that("logging.local_logging sets the level as chosen", {
  local_logging("debug")
  expect_equal(grep("flounder", capture_output(safetyguide:::logerror("flounder"))), 1)
  expect_equal(grep("unique", capture_output(safetyguide:::logdebug("unique"))), 1)
  expect_equal(
    grep("cowboy", capture_output(safetyguide:::logtrace("cowboy"))), integer(0))
})


test_that("logging.local_logging accepts only first string", {
  local_logging()
  expect_equal(
    grep("there",
         capture_output(safetyguide:::loginfo("hi", "there"))
    ),
    numeric(0)
  )
})
