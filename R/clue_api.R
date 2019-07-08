API_URL = "https://api.clue.io"

#' Submit Clue query
#'
#' @export
clue_query_submit <- function(
  up_gmt, down_gmt, name = NULL,
  api_key = NULL, use_fast_tool = TRUE
) {
  tool <- if (use_fast_tool) "sig_fastgutc_tool" else "sig_gutc_tool"
  api_key <- api_key %||% clue_retrieve_api_key()

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
      user_key = api_key
    ),
    body = request_body,
    encode = "multipart"
  )

  if (httr::http_error(response)) {
    stop("Job submission failed:", httr::content(response, "text"))
  }

  response_json(response)
}

#' Poll query job status
#'
#' @export
clue_query_poll <- function(clue_query, api_key = NULL) {
  job_id <- parse_job_id(clue_query)
  api_key <- api_key %||% clue_retrieve_api_key()

  request_url <- httr::modify_url(
    API_URL,
    path = paste0("/api/jobs/findByJobId/", job_id)
  )

  response <- httr::GET(
    request_url,
    httr::add_headers(
      user_key = api_key,
      Accept = "application/json"
    )
  )

  if (httr::http_error(response)) {
    stop("Error while polling job:", httr::content(response, "text"))
  }

  rj <- response_json(response)

  if (rj$errorMessage != "") {
    stop("Job failed:", jsonlite::toJSON(rj, pretty = TRUE))
  }

  rj
}

#' Wait for query completion
#'
#' @export
clue_query_wait <- function(
  clue_query, interval = 60, timeout = 3600, quiet = FALSE, api_key = NULL
) {
  if (interval < 60)
    stop("`interval` must be smaller than 60 in order to reduce burden on the server.")
  start_time <- as.integer(Sys.time())
  while(TRUE) {
    job_status <- clue_query_poll(clue_query, api_key = api_key)
    if (job_status$status == "completed") {
      if (!quiet)
        message("Job completed: ", job_status$job_id)
      return(job_status)
    }
    if (as.integer(Sys.time()) - start_time > timeout) {
      if (!quiet)
        warning("Job not completed during timeout period: ", job_status$job_id)
      return(NULL)
    }
    if (!quiet)
      message("Job not completed yet, waiting for: ", job_status$job_id)
    Sys.sleep(interval)
  }
}

#' Download query result
#'
#' @export
clue_query_download <- function(clue_query, destination = NULL, api_key = NULL) {
  job_id <- parse_job_id(clue_query)
  api_key <- api_key %||% clue_retrieve_api_key()

  job_status <- clue_query_poll(clue_query, api_key = api_key)

  if (job_status$status != "completed") {
    stop("Job not completed, can't download results:", jsonlite::toJSON(job_status, pretty = TRUE))
  }

  if (job_status$download_status != "completed") {
    stop("Job completed, but download not ready yet.", jsonlite::toJSON(job_status, pretty = TRUE))
  }

  destination <- destination %||% file.path(
    tempdir(), paste(job_status$params$name, job_id, "results.tar.gz", sep = "_")
  )
  download.file(paste0("https:", job_status$download_url), destination)

  destination
}

#' Retrieve API key
clue_retrieve_api_key <- function() {
  api_key <- Sys.getenv("CLUE_API_KEY")
  if (api_key == "") {
    stop("API key not found. Set the `CLUE_API_KEY` variable in your ~/.Renviron file.")
  }
  api_key
}

response_json <- function(response) {
  jsonlite::fromJSON(
    httr::content(response, "text")
  )
}

parse_job_id <- function(clue_query) {
  if(is.list(clue_query))
    # Assume it's the return value from `clue_query_submit`
    job_id <- clue_query$result$job_id
  else
    # Assume it's the job id directly
    job_id <- clue_query
  job_id
}
