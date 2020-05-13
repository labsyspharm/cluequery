#' Parse result folder
#'
#' Clue calculates different scores.
#' \href{https://clue.io/connectopedia/connectivity_scores}{Connectopedia} gives
#' an overview of their interpretation.
#'
#' @param path Path to Clue result gz file or extracted folder.
#' @param score_level Return result on cell level (cell) or summarized (summary)
#' @param score_type Return either normalized scores (ns) or CMap connectivity
#' scores (tau). See details.
#' @param result_type Return either perturbagen level information (pert) or
#' aggregated perturbagen class level information (pcl).
#' @return A tibble in tidy format, with the score for a single
#' pertubation/gene set combination per row.
#' @export
clue_parse_result <- function(
  path, score_level = c("cell", "summary"), score_type = c("ns", "tau"), result_type = c("pert", "pcl")
) {
  if (tools::file_ext(path) == "gz") {
    tmp_dir <- tempfile("clueR-")
    dir.create(tmp_dir)
    untar(path, exdir = tmp_dir)
    path <- list.dirs(tmp_dir, recursive = FALSE)[1]
  }
  score_type <- match.arg(score_type)
  score_type_name <- switch(
    score_type,
    tau = "ps",
    ns = "ns"
  )
  result_type <- match.arg(result_type)
  result_type_name <- switch(
    result_type,
    pcl = "pcl",
    pert = "pert"
  )
  score_level <- match.arg(score_level)

  signatures_file <- file.path(path, "matrices", "gutc", paste0(score_type_name, "_", result_type_name, "_", score_level, ".gctx"))
  if (!file.exists(signatures_file))
    stop("Signatures file not found at ", signatures_file)
  signatures <- cmapR::parse_gctx(
    signatures_file
  )
  gene_sets <- signatures@cdesc[["id"]] %>%
    {if_else(str_starts(., "\\.?[0-9_]"), paste0("X", .), .)}
  signatures_df <- signatures@mat %>%
    data.frame() %>%
    tibble::as_tibble(rownames = "id") %>%
    dplyr::left_join(
      signatures@rdesc,
      by = "id"
    ) %>%
    tidyr::gather("gene_set", !!rlang::sym(score_type), dplyr::one_of(gene_sets))
  signatures_df
}
