test_provenance_spy_a <- function(filename, role) {
  prov.input.file(filename, role)
}


test_provenance_spy_b <- function(filename, role) {
  test_provenance_spy_a(filename, role)
}


test_provenance_spy_c <- function(filename, role) {
  test_provenance_spy_b(filename, role)
}


test_sys_calls_a <- function(a, b) {
  sys.calls()
}


test_sys_calls_b <- function(c, d) {
  test_sys_calls_a(c, d)
}


test_that("function embedding is picked up", {
  get.input.files(clear = TRUE)
  test_file <- "provenance_spy_a.txt"
  write.table(data.frame(), file = test_file, col.names = FALSE)
  test_provenance_spy_c(test_file, "embedding test input")
  file_list <- get.input.files()
  file.remove(test_file)
  get.input.files(clear = TRUE)

  stacks <- file_list[[1]]$stack
  stack_end <- length(stacks)
  expect_true(startsWith(stacks[[stack_end - 2]], "test_provenance_spy_c"))
  expect_true(startsWith(stacks[[stack_end - 1]], "test_provenance_spy_b"))
  expect_true(startsWith(stacks[[stack_end]], "test_provenance_spy_a"))
})



test_that("duplicates don't show up", {
  get.input.files(clear = TRUE)
  test_file <- "provenance_spy_a.txt"
  write.table(data.frame(), file = test_file, col.names = FALSE)
  test_provenance_spy_c(test_file, "embedding test input")
  test_provenance_spy_c(test_file, "embedding test input")
  file_list <- get.input.files()
  file.remove(test_file)
  get.input.files(clear = TRUE)
  expect_equal(length(file_list), 1)
})


test_that("written version is formatted", {
  get.input.files(clear = TRUE)
  test_file_a <- "provenance_spy_a.txt"
  write.table(data.frame(), file = test_file_a, col.names = FALSE)
  test_file_b <- "provenance_spy_b.txt"
  write.table(data.frame(), file = test_file_b, col.names = FALSE)
  test_provenance_spy_c(test_file_a, "first formatted")
  test_provenance_spy_b(test_file_b, "second formatted")
  file.remove(test_file_b)
  file.remove(test_file_a)
  toml <- "test_provenance_spy_formatted.toml"
  write.meta.data(toml)
  get.input.files(clear = TRUE)
  toml_conn <- file(toml, open = "rt")
  lines <- readLines(con = toml_conn)
  close(toml_conn)
  file.remove(toml)

  # Checking that the stack formatting includes arguments, by checking one of them.
  expect_gt(length(grep("test_provenance_spy_a\\(filename,role\\)", lines)), 0)

  # Dates shouldn't have qotes in TOML.
  dates <- grep("^creation_time", lines)
  dates_have_quotes <- grep("\"", lines[dates])
  expect_equal(length(dates_have_quotes), 0)
})
