

#' Download Data Attached to SurveyCTO Forms
#'
#' @description
#' `cto_get_form_datasets()` downloads data files attached to
#' a specific SurveyCTO form. It downloads these data to temporary local storage,
#' reads them into R, and cleans up the temporary files.
#'
#' @param req A `httr2_request` object initialized via
#' \code{\link{cto_request}()}.
#' @param form_id Character string. The unique ID of the SurveyCTO form.
#' @param file_name Optional character vector. The specific name(s) of the
#'   attached files to download (including the extension, often `.csv`). If `NULL`
#'   (the default), all files attached to the form will be downloaded.
#'
#' @details
#' This function scans your form for any uploaded/attached data files during
#' deployment, downloads these files in the background,
#' converts them into data frames, and cleans up after so no
#' cluttered files are left on your computer.
#'
#'
#' @return A `list` of data sets. Each item in the list is named after
#'   the original file. For example, if you download "prices.csv", you can
#'   access it in R using `output$prices`. Returns
#'   `NULL` invisibly if attached data files are found.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Create a request object
#' req <- cto_request("my_server", "username", "password")
#'
#' # Download all CSVs attached to a specific form
#' csv_data <- cto_get_form_datasets(req, "my_form_id")
#'
#' # Access a specific attached file called "villages.csv"
#' village_df <- csv_data$villages
#' }
cto_get_form_datasets <- function(
    req,
    form_id,
    file_name = NULL) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  if (verbose) cli::cli_progress_step("Preparing to download form datasets...", spinner = TRUE)

  if (!is.null(file_name)) checkmate::assert_character(file_name, min.len = 1)
  file_list <- cto_get_form_metadata(req, form_id)

  if (verbose) cli::cli_progress_step("Checking form dataset names...", spinner = TRUE)
  file_names <- names(file_list[["deployedGroupFiles"]][["mediaFiles"]])
  file_names <- file_names[grepl("\\.csv$", file_names, TRUE)]

  if (length(file_names) == 0) {
    cli::cli_warn("No data files attached to {.field {form_id}}.")
    return(invisible())
  }

  if (!is.null(file_name)) {
    if (any(!(file_name %in% file_names))) {
      cli::cli_abort(
        c(
          "x" = "At least one of the specified file was not found on the server.",
          "i" = "Available files for this form: {.field {paste(file_names, collapse = ', ')}}"
        ),
        call = NULL
      )
    }
    file_names <- file_name
  }

  if (verbose) cli::cli_progress_step("Downloading {.val {length(file_names)}} attached dataset{?s}...", spinner = TRUE)
  files <- purrr::map(file_names,
                  .f = \(fname) {
                    download_url <- file_list[["deployedGroupFiles"]][["mediaFiles"]][[fname]][["downloadLink"]]

                    if (grepl("^/forms", download_url)) {   # Server data
                      download_url <- stringr::str_glue("https://{secrets$servername}.surveycto.com{download_url}")
                    }

                    file_ext <- stringr::str_extract(fname, "\\.\\w+$")
                    temp_file <- tempfile(fileext = file_ext)

                    req |>
                      httr2::req_url(download_url) |>
                      cto_perform() |>
                      httr2::resp_body_raw() |>
                      writeBin(temp_file)

                    temp_file
                  })


  df_list <- purrr::map(
    files,
    .f = \(csv) {
      df <- suppressWarnings(readr::read_csv(csv, show_col_types = FALSE))
      unlink(csv)
      df
    }
    )

  names(df_list) <- stringr::str_remove(file_names, "\\.\\w+$")
  if (verbose) cli::cli_progress_done("Form datasets download complete!")
  return(invisible(df_list))
  }
