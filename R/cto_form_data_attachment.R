
#' Download Attachments from SurveyCTO Form Data
#'
#' @description
#' Extracts attachment URLs (images, audio, video, signatures) from SurveyCTO form data
#' and downloads the files to a local directory. This function handles encrypted forms
#' if a private key is provided.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param form_id A string specifying the SurveyCTO form ID to inspect.
#' @param fields A `tidy-select` expression (e.g., `everything()`, `starts_with("img_")`)
#'   specifying which columns should be scanned for attachment URLs. Defaults to `everything()`.
#' @param private_key Optional. A character string specifying the path to a local
#'   RSA private key file. Required if the form is encrypted.
#' @param dir A character string specifying the local directory where files should be saved.
#'   Defaults to `"media"`. The directory must exist.
#' @param overwrite Logical. If `TRUE`, existing files with the same name in `dir`
#'   will be overwritten. If `FALSE` (the default), existing files are skipped.
#'
#' @details
#' This function performs the following steps:
#' 1. Fetches the form data using `cto_form_data`.
#' 2. Scans the selected `fields` for values matching the standard SurveyCTO API
#'    attachment URL pattern.
#' 3. Downloads the identified files sequentially to the specified `dir`.
#'
#' @return Returns `invisible(NULL)`. The function is called for its side effect
#'   of downloading files to the local disk.
#'
#' @seealso
#' [cto_form_data()] for the underlying data retrieval.
#'
#' [httr2::req_perform_sequential()] for the download mechanism.
#'
#' @examples
#' \dontrun{
#' library(httr2)
#'
#' # 1. Create an authenticated request
#' req <- cto_request("my_server", "username")
#'
#' # 2. Download all attachments from a form
#' cto_form_data_attachment(
#'   req = req,
#'   form_id = "household_survey_v1",
#'   dir = "downloads/photos"
#' )
#'
#' # 3. Download only specific image fields from an encrypted form
#' cto_form_data_attachment(
#'   req = req,
#'   form_id = "encrypted_health_survey",
#'   fields = starts_with("image_"),
#'   private_key = "keys/my_priv_key.pem",
#'   overwrite = TRUE
#' )
#' }
cto_form_data_attachment <- function(req, form_id, fields = everything(),
                                     private_key = NULL, dir = "media",
                                     overwrite = FALSE) {

  verbose <- isTRUE(getOption("scto.verbose", TRUE))
  checkmate::assert_directory(dir)
  checkmate::assert_logical(overwrite, len = 1, any.missing = FALSE)

  rgx <- "^https://.*\\.surveycto\\.com/api/v2/forms/.*/submissions/uuid:.*/attachments/.*\\.*$"

  df <- cto_form_data(req, form_id, private_key = private_key, tidy = FALSE)

  urls <- select(df, {{ fields }}) |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(grepl(rgx, .data$value)) |>
    dplyr::pull("value")

  file_paths <- file.path(dir, basename(urls))
  to_download <- if (overwrite) rep(TRUE, length(urls)) else !file.exists(file_paths)

  urls_to_fetch  <- urls[to_download]
  paths_to_fetch <- file_paths[to_download]

  skipped <- length(urls) - sum(to_download)
  if (skipped > 0) cli_inform("Skipping {.val {skipped}} existing file{?s}")

  if (length(paths_to_fetch) > 0) {
    if (verbose) cli_progress_step("Downloading {.val {length(paths_to_fetch)}} attachment{?s}")
    if (!is.null(private_key)) {
      req <- httr2::req_body_multipart(req, private_key = curl::form_file(private_key))
    }
    reqs <- purrr::map(urls_to_fetch, ~req_url(req, .x))
    httr2::req_perform_sequential(reqs, paths_to_fetch, "continue")
  }
  return(invisible())
}
