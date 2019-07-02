test_that("preparing and converting DESeq2 data to gmt works", {
  # deseq_res <- file.path("..", "data", "example_deseq2_result.csv.xz") %>%
  deseq_res <- file.path("tests", "data", "example_deseq2_result.csv.xz") %>%
    readr::read_csv()
  prepared <- clue_prepare_deseq2(deseq_res, "test_gene_set")
  expect_equal(
    as.list(table(prepared$direction)),
    list(down = 172, up = 305)
  )
  gmts <- clue_gmt_from_df(prepared)
  expect_named(gmts, c("up", "down"), ignore.order = TRUE)
})
