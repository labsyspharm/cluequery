test_that("Can submit example query", {
  skip_on_cran()
  api_key <- Sys.getenv("CLUE_API_KEY")
  up_gmt <- file.path("..", "data", "example_up.gmt")
  down_gmt <- file.path("..", "data", "example_down.gmt")
  # up_gmt <- file.path("tests", "data", "example_up.gmt")
  # down_gmt <- file.path("tests", "data", "example_down.gmt")
  response <- clue_query_submit(
    up_gmt, down_gmt,
    name = paste(sample(letters, 10, replace = TRUE), collapse = "")
  )
  expect_equal(response$status, "pending")
})
