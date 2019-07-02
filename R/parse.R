#' Parse result folder
#'
#' @export
clue_parse_result <- function(path) {
  if (tools::file_ext(path) == "gz") {
    tmp_dir <- tempfile("clueR-")
    dir.create(tmp_dir)
    untar(path, exdir = tmp_dir)
    path <- list.dirs(tmp_dir, recursive = FALSE)[1]
  }
  signatures_file <- file.path(path, "matrices", "gutc", "ns_pert_summary.gctx")
  if (!file.exists(signatures_file))
    stop("Signatures file not found at ", signatures_file)
  signatures <- cmapR::parse.gctx(
    signatures_file
  )
  signatures@mat %>%
    data.frame() %>%
    tibble::as_tibble(rownames = "id") %>%
    dplyr::left_join(
      signatures@rdesc %>% dplyr::select(id, pert_id, pert_iname, pert_type),
      by = "id"
    ) %>%
    dplyr::select(pert_id, pert_type, pert_iname, dplyr::everything(), -id)
}
