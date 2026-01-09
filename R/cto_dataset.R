
#' Download SurveyCTO Server Datasets
#'
#' @description
#' Downloads one or more server datasets from SurveyCTO as CSV files.
#'
#' By default, files are saved to a temporary directory and read into memory.
#' You can choose to persist files by specifying a `dir` or skip reading them
#' by setting `load = FALSE`.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param ids A character vector of dataset IDs to download.
#'   If `NULL` (the default), all available server datasets are downloaded.
#' @param load Logical; if `TRUE` (the default), the downloaded CSV files are
#'   read into R and returned as a list of data frames. If `FALSE`, the function
#'   returns a character vector of the file paths.
#' @param dir A string specifying the directory where CSV files will be saved.
#'   Defaults to [tempdir()].
#' @param overwrite Logical; if `TRUE`, existing files in `dir` will be
#'   overwritten. If `FALSE` (the default), existing files are skipped.
#'
#' @details
#' This function first retrieves the list of available server datasets.
#' If `ids` is provided, it validates them against this list.
#'
#' - **Progress Reporting**: When `options(scto.verbose = TRUE)` (default),
#'   a progress bar is displayed during download.
#' - **Skipping**: If `overwrite = FALSE`, files that already exist in `dir`
#'   are skipped to save bandwidth.
#' - **Failures**: If a specific dataset fails to download, a warning is issued,
#'   but the function continues downloading others.
#'
#' @return
#' * If `load = TRUE` (default): A data frame if one or named list of data frames (one per dataset).
#' * If `load = FALSE`: A character vector of file paths to the downloaded CSVs.
#' * Returns `NULL` if no datasets are found or successfully downloaded.
#'
#' @seealso
#' [cto_metadata()] to list available IDs manually.
#' [utils::read.csv()] for the underlying reader used when `load = TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' req <- cto_request(Sys.getenv("SCTO_SERVER"), Sys.getenv("SCTO_USER"))
#'
#' # 1. Download all datasets and read them into a list
#' all_data <- cto_dataset(req)
#'
#' # 2. Download specific datasets to a specific folder (do not read)
#' files <- cto_dataset(req,
#'   ids = c("household_survey", "roster"),
#'   dir = "my_data_folder",
#'   load = FALSE
#' )
#'
#' # 3. Force re-download of a dataset
#' cto_dataset(req, ids = "roster", overwrite = TRUE)
#' }
cto_dataset <- function(req, ids = NULL, load = TRUE, dir = tempdir(), overwrite = FALSE) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  assert_arg(req, c("httr2_request", "scto_request"), "req")
  assert_arg(ids, "character", "ids", NULL, TRUE)
  assert_arg(dir, "character", "dir")
  assert_arg(overwrite, "logical", "overwrite", 1)
  assert_arg(load, "logical", "load", 1)
  if (!dir.exists(dir)) dir.create(dir)

  if (is.null(ids)) {
    if (verbose) cli_progress_step("Checking available server datasets on {.field {req$server}}")
    metadata <- fetch_api_response(req, "console/forms-groups-datasets/get")
    ids <- pluck(metadata, "datasets", "id")
    if (length(ids) == 0) {
      cli_warn("No server datasets found on {.field {req$server}}")
      return(invisible())
      }
  }

  req <- httr2::req_url_query(req, as_attachment = "true")

  file_names <- paste0(ids, ".csv")
  paths_all  <- file.path(dir, file_names)

  to_download <- if (overwrite) rep(TRUE, length(ids)) else !file.exists(paths_all)

  urls_to_fetch  <- paste0("/api/v2/datasets/data/csv/", ids[to_download])
  paths_to_fetch <- paths_all[to_download]

  skipped <- length(ids) - sum(to_download)
  if (verbose && skipped > 0) cli_inform("Skipping {.val {skipped}} already existing file{?s}")

  if (length(paths_to_fetch) > 0) {
    pb <- if (verbose) download_pb else FALSE
    purrr::walk2(
      urls_to_fetch, paths_to_fetch, \(url, path) {
        tryCatch(
          fetch_api_response(req, url, path),
          error = \(e) cli_warn("Downloading {.val {basename(path)}} failed: {conditionMessage(e)}")
        )
      },
      .progress = pb
    )
  }

  ok <- file.exists(paths_all)
  if (load && sum(ok) > 0) {
    map_maybe_list(
      .x = paths_all[ok], .names = ids[ok],
      .f = ~suppressWarnings(readr::read_csv(.x, show_col_types = FALSE))
      )
  } else paths_all[ok]
}
