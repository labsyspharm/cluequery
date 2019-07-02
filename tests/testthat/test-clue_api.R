test_that("Can retrieve API key", {
  skip_on_cran()
  api_key <- Sys.getenv("CLUE_API_KEY")
  expect_equal(nchar(api_key), 32)
})

test_that("Can submit example query", {
  skip_on_cran()
  api_key <- Sys.getenv("CLUE_API_KEY")
  session <- clue_start_session(api_key)
  up_gmt <- file.path("..", "data", "example_up.gmt")
  down_gmt <- file.path("..", "data", "example_down.gmt")
  # up_gmt <- file.path("tests", "data", "example_up.gmt")
  # down_gmt <- file.path("tests", "data", "example_down.gmt")
  response <- clue_submit_query(
    session,
    paste(sample(letters, 10, replace = TRUE), collapse = ""),
    up_gmt, down_gmt
  )
  expect_equal(response$status, "pending")
})
