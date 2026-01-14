
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
  verbose <- isTRUE(getOption("scto.verbose", TRUE))

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_character(ids, null.ok = TRUE)
  checkmate::assert_logical(load, len = 1, any.missing = FALSE)
  checkmate::assert_directory(dir)
  checkmate::assert_logical(overwrite, len = 1, any.missing = FALSE)
  if (!dir.exists(dir)) dir.create(dir)

  if (is.null(ids)) {

    if (verbose) cli_progress_step("Checking available server datasets on {.field {req$server}}")

    metadata <- cto_metadata(req, "datasets")
    ids <- metadata$id

    if (length(ids) == 0) {
      cli_warn("No server datasets found on {.field {req$server}}")
      return(invisible())
      }
  }

  req <- httr2::req_url_query(req, asAttachment = TRUE)

  file_names <- paste0(ids, ".csv")
  paths_all  <- file.path(dir, file_names)

  to_download <- if (overwrite) rep(TRUE, length(ids)) else !file.exists(paths_all)

  urls_to_fetch  <- paste0("/api/v2/datasets/data/csv/", ids[to_download])
  paths_to_fetch <- paths_all[to_download]

  skipped <- length(ids) - sum(to_download)
  if (skipped > 0) cli_inform("Skipping {.val {skipped}} existing file{?s}")

  if (length(paths_to_fetch) > 0) {
    if (verbose) cli_progress_step("Downloading {.val {length(paths_to_fetch)}} dataset{?s}")
    reqs <- purrr::map(urls_to_fetch, ~req_url_path(req, .x))
    httr2::req_perform_sequential(reqs, paths_to_fetch, "continue")
  }

  if (load) {
    ok <- file.exists(paths_all)
    files_to_load <- paths_all[ok]
    ids_loaded <- ids[ok]

    if (length(files_to_load) == 0) return(invisible(NULL))

    out <- purrr::map(files_to_load, function(fp) {
      tryCatch(
        readr::read_csv(fp, show_col_types = FALSE),
        error = function(e) {
          cli_warn("Failed to read {.file {basename(fp)}}: {e$message}")
          return(NULL)
        }
      )
    }) |>
      purrr::set_names(ids_loaded)

    ok <- sum(!sapply(out, is.null))
    if (verbose) cli_progress_step("Loading {.val {ok}} dataset{?s} into memory")
    out <- out[!sapply(out, is.null)]

    return(out)

  } else {
    return(paths_all)
  }
}
