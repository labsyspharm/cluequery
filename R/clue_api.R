# curl -i -X POST \
# -H "user_key: XXXXXXXX" \
# -H "Content-Type: multipart/form-data" \
# -F 'tool_id=sig_gutc_tool' \
# -F 'uptag-cmapfile=@/Users/foo/Downloads/uptag.gmt' \
# -F "name=BAR" \
# -F 'dntag-cmapfile=@/Users/foo/Downloads/dntag.gmt' \
# -F 'ignoreWarnings': true \
# -F "data_type=L1000" \
# -F "dataset=Touchstone" api_url

MIN_N_QUERY = 10
MAX_N_QUERY = 150
API_URL = "https://api.clue.io"

#' Start Clue session
#'
#' @export
clue_start_session <- function(api_key) {
  structure(list(api_key = api_key), class = "clue_session")
}

#' Submit Clue query
#'
#' @export
clue_submit_query <- function(
  session,
  name,
  up_gmt, down_gmt,
  use_fast_tool = TRUE
) {
  tool <- if (use_fast_tool) "sig_fastgutc_tool" else "sig_gutc_tool"
  request_url <- httr::modify_url(
    API_URL,
    path = "/api/jobs"
  )
  request_body <- list(
    name = name,
    tool_id = tool,
    `uptag-cmapfile` = httr::upload_file(up_gmt),
    `dntag-cmapfile` = httr::upload_file(down_gmt),
    ignoreWarnings = "false",
    data_type = "L1000",
    dataset = "Touchstone"
  )

  response <- httr::POST(
    request_url,
    httr::add_headers(
      user_key = session$api_key
    ),
    body = request_body,
    encode = "multipart",
    httr::verbose()
  )

  rj <- jsonlite::fromJSON(
    httr::content(response, "text"),
  )

  if (httr::http_error(response)) {
    stop("Job submission failed:", httr::content(response, "text"))
  }
  rj
}

#' Submit Clue query
#'
#' @export
clue_poll_job <- function(session, clue_job_response) {
  request_url <- httr::modify_url(
    API_URL,
    path = paste0("/api/jobs/findByJobId/job_id", clue_job_response$result$job_id)
  )

  response <- httr::GET(
    request_url,
    httr::add_headers(
      user_key = session$api_key,
      Accept = "application/json"
    ),
    httr::verbose()
  )
  response
}

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
