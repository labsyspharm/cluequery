API_URL <- "https://api.clue.io"
MAX_JOBS <- 10

#' Submit Clue queries
#'
#' Submit query gene sets in GMT format as jobs to Clue.
#'
#' GMT files can be generated using the \code{\link{clue_prepare_funs}}
#' functions or \code{\link[cmapR]{write_gmt}}.
#'
#' @param up_gmt,down_gmt Path to GMT files containing the lists of up-
#' and down-regulated genes.
#' @param queries Named list of lists, each with an `up` and `down`
#'   slot containing up- and down-regulated GMT files. Job names
#'   are derived from the list names.
#' @param name Name for job.
#' @param api_key Clue API key. Leave empty if it is saved in
#'   \code{~/.Renviron}.
#' @return Nested list of job parameters returned by Clue.
#' @export
clue_query_submit <- function(
  up_gmt, down_gmt, name = NULL,
  api_key = NULL
) {
  api_key <- api_key %||% clue_retrieve_api_key()

  request_url <- httr::modify_url(
    API_URL,
    path = "/api/jobs"
  )
  request_body <- list(
    "name" = name,
    "uptag-cmapfile" = httr::upload_file(up_gmt),
    "dntag-cmapfile" = httr::upload_file(down_gmt),
    "data_type" = "L1000",
    "dataset" = "Touchstone",
    "ignoreWarnings" = "false",
    "tool_id" = "sig_gutc_tool"
    # "tool_id" = "sig_queryl1k_tool",
    # "ts_version" = "1.0"
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

#' @describeIn clue_query_submit Submit multiple queries to Clue
#' @param interval Check every x seconds.
#' @export
clue_queries_submit <- function(
  queries, api_key = NULL, interval = 60
) {
  if (interval < 60)
    stop("`interval` must be smaller than 60 in order to reduce burden on the server.")
  api_key <- api_key %||% clue_retrieve_api_key()

  jobs_running <- c()
  jobs_remaining <- names(queries)
  jobs <- list()

  while(length(jobs_remaining) > 0) {
    n_jobs_running <- length(jobs_running)
    if (n_jobs_running < MAX_JOBS) {
      to_be_submitted <- head(jobs_remaining, n = MAX_JOBS - n_jobs_running)
      new_jobs <- purrr::map(
        purrr::set_names(to_be_submitted),
        ~{
          q <- queries[[.x]]
          j <- clue_query_submit(
            q[["up"]], q[["down"]], .x, api_key = api_key
          )
          jobs_running <<- c(jobs_running, to_be_submitted)
          jobs_remaining <<- setdiff(jobs_remaining, to_be_submitted)
          j
        }
      )
      jobs <- c(jobs, new_jobs)
      message(paste("Jobs submitted:", paste(to_be_submitted, collapse = ",")))
    }
    job_status <- purrr::map(
      jobs[jobs_running],
      clue_query_poll, api_key = api_key
    )
    purrr::iwalk(
      job_status,
      ~{
        if (
          clue_query_status(.x, from_poll = TRUE) %in% c("completed", "failed")
        ) {
          jobs_running <<- setdiff(jobs_running, .y)
          message(paste("Job finished:", .y))
        }
      }
    )
    if (length(jobs_remaining) > 0)
      Sys.sleep(interval)
  }
  invisible(jobs)
}

#' Poll query job status
#'
#' Find out status of a job or wait for its completion.
#'
#' @param clue_query Job ID or job parameters returned by
#'   \code{\link{clue_query_submit}}
#' @param api_key Clue API key. Leave empty if it is saved in \code{~/.Renviron}
#' @return List of status codes from Clue.
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
  response_json(response)
}

#' @describeIn clue_query_poll Get job status
#' @param from_poll If TRUE, `clue_query` is assumed to be output from
#'   `clue_query_poll()` output. Otherwise it is assumed to be job ID.
#' @export
clue_query_status <- function(clue_query, api_key = NULL, from_poll = FALSE) {
  rj <- clue_query
  if (!from_poll)
    rj <- clue_query_poll(
      clue_query, api_key = api_key
    )
  if (!is.null(rj[["errorMessage"]]) && rj[["errorMessage"]] != "") {
    return("failed")
  }
  if (rj$status == "pending")
    return("pending")
  if (
    rj$status == "completed" &&
    rj$download_status %||% "false" == "completed"
  ) {
    return("completed")
  }
  return("running")
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
    rj <- clue_query_status(clue_query, api_key = api_key)
    job_status <- clue_query_status(rj, from_poll = TRUE)
    if (job_status == "completed") {
      if (!quiet)
        message("Job completed: ", rj$job_id)
      return(invisible(rj))
    }
    if (as.integer(Sys.time()) - start_time > timeout) {
      if (!quiet)
        warning("Job not completed during timeout period: ", rj$job_id)
      return(NULL)
    }
    if (!quiet)
      message("Job not completed yet, waiting for: ", rj$job_id)
    Sys.sleep(interval)
  }
  invisible(rj)
}

#' Download Clue job results
#'
#' Given a job ID this function will download the results of a Clue job
#' as compressed tarball to the given location or to a temprary folder
#'
#' @param clue_query Job ID or job parameters returned by
#'   \code{\link{clue_query_submit}}
#' @param destination Path to download destination.
#' @param api_key Clue API key. Leave empty if it is saved in \code{~/.Renviron}
#' @return Path to result tarball.
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

#' List Clue jobs
#'
#' List all non-deleted jobs submitted by this user.
#'
#' @param api_key Clue API key. Leave empty if it is saved in \code{~/.Renviron}
#' @return Data frame with job info from Clue.
#' @export
clue_list_jobs <- function(api_key = NULL) {
  api_key <- api_key %||% clue_retrieve_api_key()
  request_url <- httr::modify_url(
    API_URL,
    path = "/api/jobs",
    query = list(
      filter = r"-{{"where":{"status":{"neq":"deleted"}}}}-"
    )
  )
  response <- httr::GET(
    request_url,
    httr::add_headers(
      user_key = api_key,
      Accept = "application/json"
    )
  )
  if (httr::http_error(response)) {
    stop("Error while listing jobs:", httr::content(response, "text"))
  }
  response_json(response)
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
  else if (is.null(clue_query))
    stop("Invalid job id '", clue_query, "'.")
  else
    # Assume it's the job id directly
    job_id <- clue_query
  job_id
}
