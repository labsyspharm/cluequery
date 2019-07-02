#' Parse result folder
#'
#' @export
clue_parse_result <- function(path, score_type = c("ns", "tau"), result_type = c("pert", "pcl")) {
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

  signatures_file <- file.path(path, "matrices", "gutc", paste0(score_type_name, "_", result_type_name, "_", "summary.gctx"))
  if (!file.exists(signatures_file))
    stop("Signatures file not found at ", signatures_file)
  signatures <- cmapR::parse.gctx(
    signatures_file
  )
  signatures_df <- signatures@mat %>%
    data.frame() %>%
    tibble::as_tibble(rownames = "id")
  if (result_type == "pert") {
    signatures_df <- signatures_df %>%
      dplyr::left_join(
        signatures@rdesc %>% dplyr::select(id, pert_id, pert_iname, pert_type),
        by = "id"
      ) %>%
      dplyr::select(pert_id, pert_type, pert_iname, dplyr::everything(), -id)
  } else {
    signatures_df <- signatures_df %>%
      dplyr::left_join(
        signatures@rdesc %>% dplyr::select(id, pcl_size),
        by = "id"
      ) %>%
      dplyr::select(id, pcl_size, dplyr::everything())
  }
  signatures_df
}
