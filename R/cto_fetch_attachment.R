

#' Download Media Attachments from SurveyCTO
#'
#' Scans a specified form for media fields (images, audio, video, etc.) and
#' downloads the files to a local directory.
#'
#' @description
#' This function identifies all media-type questions in a SurveyCTO form,
#' retrieves the associated URLs, and saves the files locally. It is designed
#' to be efficient: it only downloads files that do not already exist in the
#' destination folder.
#'
#' @param req A `httr2_request` object initialized via \code{\link{cto_request}()}.
#' @param form_id String. The unique ID of the SurveyCTO form.
#' @param private_key An optional path to a `.pem` private key file. Required
#'   if the form is encrypted.
#' @param start_date A `POSIXct` object. Only submissions received after
#'   this date/time will be downloaded. Defaults to "2000-01-01".
#' @param status A character vector of submission statuses to include.
#'   Options are "approved", "rejected", and "pending". Defaults to all three.
#' @param path String The local folder path where media should be saved.
#'   Defaults to a "media" folder in your current working directory.
#'
#' @return Returns \code{NULL} invisibly. Files are written directly to disk.
#'
#' @details
#' The function automatically handles:
#' \itemize{
#'   \item \strong{Deduplication:} Checks if the file already exists locally before downloading.
#'   \item \strong{Resilience:} If a single download fails, the function issues a warning and continues.
#'   \item \strong{Folder Creation:} Automatically creates the destination directory if it doesn't exist.
#' }
#'
#' @export
#'
#' @seealso [cto_request()], [cto_fetch_data()], [cto_form_dataset()]
#'
#'
#' @examples
#' \dontrun{
#' # 1. Setup the base request
#' req <- cto_request("my_server", "user", "pass")
#'
#' # 2. Download all approved media from a specific form
#' cto_fetch_attachment(
#'   req = req,
#'   form_id = "household_survey_v1",
#'   path = "downloads/media"
#' )
#'
#' # 3. Download encrypted media using a private key
#' cto_fetch_attachment(
#'   req = req,
#'   form_id = "sensitive_health_data",
#'   private_key = "keys/my_private_key.pem",
#'   start_date = as.POSIXct("2024-01-01")
#' )
#' }
cto_fetch_attachment <- function(
    req,
    form_id,
    private_key = NULL,
    start_date = as.POSIXct("2000-01-01"),
    status = c("approved", "rejected", "pending"),
    path = file.path(getwd(), "media")
    ) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  checkmate::assert_path_for_output(path, TRUE)

  if (!dir.exists(path)) dir.create(path)

  df <- cto_fetch_data(
    req = req,
    form_id = form_id,
    private_key = private_key,
    start_date = start_date,
    tidy = FALSE)

  form <- cto_form_definition(req, form_id)

  media_vars <- form$survey |>
    mutate(
      type = str_squish(.data$type),
      name = str_squish(.data$name),
      repeat_level = purrr::accumulate(
        .data$type,
        .init = 0,
        .f = function(i, x) {
          if (grepl("begin repeat", x, TRUE)) {
            i + 1
          } else if (grepl("end repeat", x, TRUE)) {
            i - 1
          } else {
            i
          }
        }
      )[-1],
      is_repeat      = .data$repeat_level > 0,
      is_media       = grepl("^image$|^audio$|^video$|^file|^text audit|^audio audit", .data$type, TRUE),
      regex_varname  = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, FALSE),
        \(n, r, m) gen_regex_varname(n, r, m)
      )
    ) |>
    dplyr::filter(.data$is_media) |>
    dplyr::pull("regex_varname")

  urls <- df |>
    select(matches(media_vars)) |>
    as.list() |>
    purrr::list_c()

  if (is.null(urls)) return(invisible(NULL))

  urls <- purrr::keep(urls, ~grepl("^http", .x))

  if (!is.null(private_key)) {
    req <- httr2::req_body_multipart(req, private_key = curl::form_file(private_key))
  }
  paths <- file.path(path, basename(urls))

  n_total <- length(urls)

  if (verbose) {
    pb <- list(
      type = "download",
      format = "Downloading {cli::pb_current}/{cli::pb_total} ({cli::ansi_trimws(cli::pb_percent)}) | {cli::ansi_trimws(cli::pb_rate_bytes)} ETA: {cli::pb_eta}"
    )
  } else pb <- FALSE

  purrr::walk2(
    urls,
    paths,
    .f = \(u, p) {

      if (!file.exists(p)) {
        tryCatch(
          {
            req_perform(req_url(req, u), path = p)
          },
          error = function(e) {
            cli::cli_alert_warning("Failed to download: {basename(u)}")
          }
        )
      }
    },
    .progress = pb
  )


  if (verbose) cli_progress_step("Downloading compelete!")
}
