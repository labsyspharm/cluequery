API_URL = "https://api.clue.io"

#' Submit Clue query
#'
#' GMT files can be generated using the \code{\link{clue_prepare_funs}}
#' functions or \code{\link[cmapR]{write.gmt}}.
#'
#' The experimental faster query tools is described at
#' \url{https://clue.io/contest}.
#'
#' @param up_gmt,down_gmt Path to GMT files containing the lists of up-
#' and down-regulated genes.
#' @param name Name for job.
#' @param api_key Clue API key. Leave empty if it is saved in
#' \code{~/.Renviron}.
#' @param use_fast_tool If TRUE (default), use experimental fast query tool.
#' @return Nested list of job parameters.
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

  invisible(response_json(response))
}

#' Poll query job status
#'
#' \code{clue_query_poll} fetches the current status of the job.
#' \code{clue_query_wait} blocks until the job either finishes or fails.
#' \code{clue_query_download} should be called to download the results after
#' the job is finished.
#'
#' @param clue_query Job ID or job parameters returned by
#' \code{\link{clue_query_submit}}
#' @param api_key Clue API key. Leave empty if it is saved in \code{~/.Renviron}
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

  if (!is.null(rj$errorMessage) && rj$errorMessage != "") {
    stop("Job failed:", jsonlite::toJSON(rj, pretty = TRUE))
  }

  invisible(rj)
}

#' @describeIn clue_query_poll Wait for query completion
#' @param interval Check every x seconds.
#' @param timeout Abort waiting after x seconds.
#' @param quiet Don't output periodic status updates.
#' @export
clue_query_wait <- function(
  clue_query, interval = 60, timeout = 3600, quiet = FALSE, api_key = NULL
) {
  if (interval < 60)
    stop("`interval` must be smaller than 60 in order to reduce burden on the server.")
  start_time <- as.integer(Sys.time())
  while(TRUE) {
    job_status <- clue_query_poll(clue_query, api_key = api_key)
    if (
      job_status$status == "completed" &&
      job_status$download_status %||% "false" == "completed"
    ) {
      if (!quiet)
        message("Job completed: ", job_status$job_id)
      return(invisible(job_status))
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

#' @describeIn clue_query_poll Download query result
#' @param destination Path to download destination.
#' @export
clue_query_download <- function(clue_query, destination = NULL, api_key = NULL) {
  job_id <- parse_job_id(clue_query)
  api_key <- api_key %||% clue_retrieve_api_key()

  job_status <- clue_query_poll(clue_query, api_key = api_key)

  if (job_status$status %||% "false" != "completed") {
    stop("Job not completed, can't download results:", jsonlite::toJSON(job_status, pretty = TRUE))
  }

  if (job_status$download_status %||% "false" != "completed") {
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
