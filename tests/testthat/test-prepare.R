test_that("preparing and converting DESeq2 data to gmt works", {
  deseq_res_path <- system.file(
    "extdata", "example_deseq2_result.csv.xz", package = "clueR", mustWork = TRUE
  )
  deseq_res <- readr::read_csv(deseq_res_path)
  prepared <- clue_prepare_deseq2(deseq_res, "test_gene_set")
  expect_equal(
    as.list(table(prepared$direction)),
    list(down = 172, up = 305)
  )
  gmts <- clue_gmt_from_df(prepared)
  expect_named(gmts, c("up", "down"), ignore.order = TRUE)
})
