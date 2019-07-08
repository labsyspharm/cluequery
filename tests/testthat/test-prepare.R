test_that("preparing and converting DESeq2 data to gmt works", {
  deseq_res_path <- system.file(
    "extdata", "example_deseq2_result.csv.xz", package = "clueR", mustWork = TRUE
  )
  deseq_res <- readr::read_csv(deseq_res_path)

  prepared <- expect_warning(
    clue_gmt_from_deseq2(deseq_res, "test_gene_set")
  )
  gmts <- purrr::map(prepared, cmapR::parse.gmt)
  expect_equal(
    length(gmts$up$test_gene_set$entry),
    150
  )
  expect_equal(
    length(gmts$down$test_gene_set$entry),
    143
  )
})
