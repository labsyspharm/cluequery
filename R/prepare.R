#' Check if genes are in BING space
#'
#' @export
clue_check_bing <- function(genes) {
  is_bing <- genes %in% gene_space[gene_space$bing, ]$entrez_id
  if (sum(is_bing) < length(genes)) {
    warning("Some genes not in BING space")
  }
  genes[is_bing]
}

#' Prepare a DESeq2 result for Clue
#'
#' @export
clue_prepare_deseq2 <- function(result_df, gene_set_name, alpha = 0.05) {
  bing_genes <- clue_check_bing(result_df$gene_id)
  gene_set_df <- result_df %>%
    dplyr::filter(gene_id %in% bing_genes, padj <= alpha) %>%
    dplyr::arrange(log2FoldChange) %>%
    dplyr::mutate(
      direction = ifelse(log2FoldChange <= 0, "down", "up"),
      gene_set = gene_set_name
    )
  gene_set_df
}

#' Prepare gmt files from data frames
#'
#' @export
clue_gmt_from_df <- function(gene_set_df) {
  bing_genes <- clue_check_bing(gene_set_df$gene_id)
  gene_set_gmt <- gene_set_df %>%
    dplyr::filter(gene_id %in% bing_genes) %>%
    dplyr::group_by(gene_set, direction) %>%
    dplyr::filter(dplyr::n() >= MIN_N_QUERY) %>%
    dplyr::slice(1:min(MAX_N_QUERY, n())) %>%
    dplyr::summarize(
      gmt = list(list(head = gene_set[1], desc = gene_set[1], len = dplyr::n(), entry = gene_id))
    ) %>%
    ungroup() %>%
    mutate(
      tmp_file = vapply(1:n(), function(x) tempfile(fileext = "gmt"))
    )

  purrr::walk2(
    gene_set_gmt$gmt,
    gene_set_gmt$tmp_file,
    function(gmt, f) {
      cmapR::write_gmt(gmt, f)
    }
  )
  purrr::set_names(gene_set_gmt$tmp_file, gene_set_gmt$gene_set)
}
