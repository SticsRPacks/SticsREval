# ===========================================================================
# Tests: remove_null_values
# ===========================================================================
test_that("remove_null_values removes NULL elements from a list", {
  input    <- list(a = 1, b = NULL, c = "x")
  result   <- remove_null_values(input)
  expect_identical(result, list(a = 1, c = "x"))
})

test_that("remove_null_values returns empty list when all elements are NULL", {
  input  <- list(a = NULL, b = NULL)
  result <- remove_null_values(input)
  expect_identical(result, list())
})

test_that("remove_null_values returns unchanged list when no NULLs present", {
  input  <- list(a = 1, b = 2, c = 3)
  result <- remove_null_values(input)
  expect_identical(result, input)
})

test_that("remove_null_values handles an empty list", {
  expect_identical(remove_null_values(list()), list())
})

test_that("remove_null_values preserves non-NULL values of various types", {
  input  <- list(num = 42L, lgl = FALSE, chr = "hello", dbl = 3.14, null = NULL)
  result <- remove_null_values(input)
  expect_identical(
    result, list(num = 42L, lgl = FALSE, chr = "hello", dbl = 3.14)
  )
})

# ===========================================================================
# Tests: format_duration
# ===========================================================================

# Helper: build a fake start time that is `secs` seconds in the past
fake_start <- function(secs) Sys.time() - secs

test_that("format_duration returns HH:MM:SS.xx format by default", {
  start <- fake_start(3661)           # 1h 1m 1s
  end <- start + 3661
  result <- format_duration(start, end_time = end)
  expect_match(result, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{2}$")
})

test_that("format_duration formats zero elapsed time correctly", {
  t <- Sys.time()
  result <- format_duration(t, end_time = t)
  expect_identical(result, "00:00:00.00")
})

test_that("format_duration formats exactly one hour", {
  start <- Sys.time()
  end <- start + 3600
  result <- format_duration(start, end_time = end)
  expect_identical(result, "01:00:00.00")
})

test_that("format_duration formats 1h 30m 45.5s correctly", {
  start <- Sys.time()
  end <- start + (3600 + 1845 + 0.5)   # 1h 30m 45.5s
  result <- format_duration(start, end_time = end)
  expect_identical(result, "01:30:45.50")
})

test_that("format_duration with digits = 0 rounds seconds and omits decimal", {
  start  <- Sys.time()
  end    <- start + 65.6                   # 1m 05.6s → rounds to 06
  result <- format_duration(start, end_time = end, digits = 0)
  expect_match(result, "^\\d{2}:\\d{2}:\\d{2}$")
  expect_identical(result, "00:01:06")
})

test_that("format_duration with digits = 3 produces three decimal places", {
  start  <- Sys.time()
  end    <- start + 5.123
  result <- format_duration(start, end_time = end, digits = 3)
  expect_match(result, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{3}$")
})

# ===========================================================================
# Tests: format_species
# ===========================================================================
test_that("format_species returns 'None' for an empty vector", {
  expect_identical(format_species(character(0)), "None")
  expect_identical(format_species(NULL), "None")
})

test_that(
  "format_species returns the single species name when given one element",
  {
    expect_identical(format_species("Panthera leo"), "Panthera leo")
  }
)

test_that("format_species joins multiple species with ', '", {
  input  <- c("Panthera leo", "Ursus arctos", "Canis lupus")
  result <- format_species(input)
  expect_identical(result, "Panthera leo, Ursus arctos, Canis lupus")
})

test_that("format_species preserves order of species", {
  input  <- c("Z species", "A species", "M species")
  result <- format_species(input)
  expect_identical(result, "Z species, A species, M species")
})

test_that(
  "format_species handles numeric input (coerced to character by paste)",
  {
    expect_identical(format_species(c(1, 2, 3)), "1, 2, 3")
  }
)

# ===========================================================================
# Tests: safe_write_csv
# ===========================================================================
test_that("safe_write_csv writes a data frame to disk", {
  tmp  <- tempfile(fileext = ".csv")
  data <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  safe_write_csv(data, tmp)
  expect_true(file.exists(tmp))
  written <- readLines(tmp)
  expect_true(any(grepl("x,y", written, fixed = TRUE)))
  unlink(tmp)
})

test_that("safe_write_csv encodes NA values as 'NA' string", {
  tmp  <- tempfile(fileext = ".csv")
  data <- data.frame(x = c(1, NA, 3))
  safe_write_csv(data, tmp)
  content <- readLines(tmp)
  expect_true(any(grepl("NA", content, fixed = TRUE)))
  unlink(tmp)
})

test_that("safe_write_csv stops with a clear error when path is invalid", {
  data <- data.frame(x = 1)
  expect_error(
    safe_write_csv(data, file.path("nonexistent_dir", "file.csv")),
    regexp = "Error: unable to create"
  )
})

# ===========================================================================
# Tests: read_csv
# ===========================================================================
test_that("read_csv reads a simple CSV file correctly", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,2,3", "4,5,6"), tmp)

  stub(read_csv, "is_debug", function() FALSE)

  result <- read_csv(tmp)
  expect_s3_class(result, "data.frame")
  expect_identical(ncol(result), 3)
  expect_identical(nrow(result), 2)
  unlink(tmp)
})

test_that("read_csv trims whitespace from column names", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(" col1 , col2 ", "1,2"), tmp)

  stub(read_csv, "is_debug", function() FALSE)

  result <- read_csv(tmp)
  expect_named(result, c("col1", "col2"))
  unlink(tmp)
})

test_that("read_csv treats 'NA', 'NaN', and '' as NA", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("x,y,z", "NA,NaN,"), tmp)

  stub(read_csv, "is_debug", function() FALSE)

  result <- read_csv(tmp)
  expect_true(is.na(result$x[1]))
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$z[1]))
  unlink(tmp)
})

test_that("read_csv respects a custom delimiter", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;2"), tmp)

  stub(read_csv, "is_debug", function() FALSE)

  result <- read_csv(tmp, delimiter = ";")
  expect_identical(ncol(result), 2)
  unlink(tmp)
})

test_that("read_csv parses dates with format %Y-%m-%d", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("id,date", "1,2024-06-15"), tmp)

  stub(read_csv, "is_debug", function() FALSE)

  result <- read_csv(tmp)
  expect_s3_class(result$date, "Date")
  expect_identical(result$date[1], as.Date("2024-06-15"))
  unlink(tmp)
})
