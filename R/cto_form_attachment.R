

#' Download Attachments from SurveyCTO Forms
#'
#' @description
#' Downloads files attached to a deployed SurveyCTO form (e.g., pre-loaded
#' CSV data, media files, or other server-side attachments).
#'
#' By default, files are downloaded to a temporary directory and any CSV files
#' found are read into memory.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param form_id A string specifying the SurveyCTO form ID to inspect.
#' @param filename Optional character vector of specific filenames to download
#'   (e.g., `"prices.csv"`). If `NULL` (default), all available attachments
#'   associated with the form are downloaded.
#' @param load Logical; if `TRUE` (the default), any downloaded **CSV files** #'   are read into R and returned as a list of data frames. Non-CSV files
#'   (images, audio) are downloaded but not read. If `FALSE`, the function
#'   returns a character vector of paths to all downloaded files.
#' @param dir A string specifying the directory where files will be saved.
#'   Defaults to [tempdir()].
#' @param overwrite Logical; if `TRUE`, existing files in `dir` will be
#'   overwritten. If `FALSE` (the default), existing files are skipped.
#'
#' @details
#' This function first uses [cto_form_metadata()] to retrieve the list of
#' "media files" (attachments) associated with the deployed form.
#'
#' - **File Types**: You can download any attached file type (images, audio, CSVs).
#'   However, `load = TRUE` will only parse `.csv` files.
#' - **Progress**: When `options(scto.verbose = TRUE)` (default), a progress
#'   bar tracks the download status.
#' - **Caching**: Files are not re-downloaded if they already exist in `dir`
#'   unless `overwrite = TRUE`.
#'
#' @return
#' * If `load = TRUE`: A named list of data frames (only for downloaded `.csv` files).
#'   Other file types are saved to disk but not returned in the list.
#' * If `load = FALSE`: A character vector of file paths to all downloaded files (invisibly).
#' * Returns `invisible(NULL)` if no attachments are found.
#'
#' @seealso
#' [cto_form_metadata()] to inspect available attachments without downloading.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' req <- cto_request("my_server", "username")
#'
#' # 1. Download all attachments; read CSVs into a list
#' attachments <- cto_form_attachment(req, "household_survey")
#'
#' # 2. Download specific files to a local "data" folder
#' cto_form_attachment(
#'   req,
#'   form_id  = "household_survey",
#'   filename = c("item_list.csv", "logo.png"),
#'   dir      = "data/raw",
#'   load     = FALSE
#' )
#'
#' # 3. Force re-download of a specific file
#' cto_form_attachment(
#'   req,
#'   form_id = "household_survey",
#'   filename = "prices.csv",
#'   overwrite = TRUE
#' )
#' }
cto_form_attachment <- function(req, form_id, filename = NULL, load = TRUE,
                                dir = tempdir(), overwrite = FALSE) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  assert_arg(filename, "character", "filename", NULL, TRUE)
  assert_arg(dir, "character", "dir", 1)
  assert_arg(overwrite, "logical", "overwrite", 1)
  assert_arg(load, "logical", "load", 1)
  if (!dir.exists(dir)) dir.create(dir)

  metadata <- cto_form_metadata(req, form_id)
  media_files <- pluck(metadata, "deployedGroupFiles", "mediaFiles")
  if (length(media_files) == 0) {
    cli_warn("No attachments found for {.val {form_id}}")
    return(invisible())
  }

  files <- names(media_files)

  if (!is.null(filename)) files <- files[files %in% filename]
  if (length(files) == 0) cli_abort(c(
    "x" = "All requested attachment not found",
    "i" = "Use {.fn cto_form_metadata} to see all available attachments"
    ))

  paths_all <- file.path(dir, files)
  to_download <- if (overwrite) rep(TRUE, length(files)) else !file.exists(paths_all)
  urls <- purrr::map_chr(files, ~pluck(media_files, .x, "downloadLink"))
  urls_to_fetch <- urls[to_download]
  paths_to_fetch <- paths_all[to_download]

  if (any(i <- grepl("^/forms", urls_to_fetch))) {
    urls_to_fetch[i] <- paste0(httr2::req_get_url(req), urls_to_fetch[i])
  }

  if (sum(to_download) > 0) {
    pb <- if (verbose) download_pb else FALSE
    purrr::walk2(
      urls_to_fetch, paths_to_fetch, \(url, path) {
        tryCatch(
          fetch_api_response(req_url(req, url), NULL, path),
          error = \(e) cli_warn("Downloading {.val {basename(path)}} failed: {conditionMessage(e)}")
        )
      },
      .progress = pb
    )
  }

  csv <- paths_all[grepl("\\.csv", paths_all)]
  ok <- file.exists(csv)
  if (load && sum(ok) > 0) {
    csv <- csv[ok]
    nms <- stringr::str_remove(basename(csv), "\\.csv")

    map_maybe_list(
      .x = csv, .names = nms,
      .f = ~suppressWarnings(readr::read_csv(.x, show_col_types = FALSE))
      )
    } else paths_all[file.exists(paths_all)]
  }
