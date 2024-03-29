test_that("Can submit example query", {
  skip_on_cran()
  up_gmt <- file.path("data", "example_up.gmt")
  down_gmt <- file.path("data", "example_down.gmt")
  expect_true(all(file.exists(c(up_gmt, down_gmt))))
  response <- clue_query_submit(
    up_gmt, down_gmt,
    name = paste(sample(letters, 10, replace = TRUE), collapse = "")
  )
  expect_equal(response$status, "pending")
})

test_that("Can submit example queries", {
  skip_on_cran()
  up_gmt <- file.path("data", "example_up.gmt")
  down_gmt <- file.path("data", "example_down.gmt")
  expect_true(all(file.exists(c(up_gmt, down_gmt))))
  queries <- list(abc = list(up = up_gmt, down = down_gmt))
  response <- clue_queries_submit(queries)
  expect_equal(response$status, "pending")
})
