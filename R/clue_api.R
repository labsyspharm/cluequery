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
