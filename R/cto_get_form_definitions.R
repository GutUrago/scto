

#' Download SurveyCTO Form Definitions
#'
#' @description
#' `cto_get_form_definitions()` downloads the XLSForm definition for a specified SurveyCTO
#' form. The function can retrieve either the currently
#' deployed version of the form or all available historical versions.
#'
#' @param req A `httr2_request` object initialized via
#' \code{\link{cto_request}()}.
#' @param form_id Character string. The unique identifier of the SurveyCTO form.
#' @param deployed_only Logical. If `TRUE` (default), only the currently deployed
#'   form version is retrieved. If `FALSE`, all available historical versions are
#'   downloaded and parsed.
#'
#' @details
#' The XLSForm file is downloaded from the SurveyCTO server and parsed using
#' \pkg{openxlsx2}. The returned object contains the three core XLSForm sheets
#' required to understand form structure and logic:
#' \code{survey}, \code{choices}, and \code{settings}.
#'
#' When `deployed_only = FALSE`, each form version is returned as a separate
#' list element, named using the corresponding form version identifier.
#'
#'
#' @return a `list`. If `deployed_only = TRUE`, the list contains three data frames.
#'
#' @export
#' @author Gutama Girja Urago
#'
#' @examples
#' \dontrun{
#' # Initialize a SurveyCTO request using environment variables
#' req <- cto_request(read_lines = Sys.getenv("SCTO_AUTH_FILE"))
#'
#' # Download the currently deployed version of a form
#' form_def <- cto_get_form_definitions(req, form_id = "household_survey")
#'
#' # Inspect the choices sheet
#' print(form_def$choices)
#'
#' # Download all historical versions of the form
#' all_defs <- cto_get_form_definitions(
#'   req,
#'   form_id = "household_survey",
#'   deployed_only = FALSE
#' )
#' }
cto_get_form_definitions <- function(
    req,
    form_id,
    deployed_only = TRUE
    ) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  if (verbose) cli::cli_progress_step("Preparing to download the form...", spinner = TRUE)

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id)
  checkmate::assert_flag(deployed_only)

  unix_ms <- as.numeric(Sys.time()) * 1000
  url_path <- glue("forms/{form_id}/files")

  if (verbose) cli::cli_progress_step("Checking the form version...", spinner = TRUE)

  file_list <- req |>
    cto_url_path_append(url_path) |>
    cto_url_query(t = unix_ms) |>
    cto_perform() |>
    cto_body_json(simplifyVector = TRUE)

  if (verbose) cli::cli_progress_step("Starting form download...", spinner = TRUE)
  if (deployed_only) {
    download_url <- file_list[["deployedGroupFiles"]][["definitionFile"]][["downloadLink"]]
    temp_file <- tempfile(fileext = ".xlsx")

    req |>
      httr2::req_url(download_url) |>
      cto_perform() |>
      httr2::resp_body_raw() |>
      writeBin(temp_file)

    sheets <- c("survey", "choices", "settings")
    form_out <- purrr::map(sheets,
                           ~openxlsx2::wb_to_df(
                             file = temp_file,
                             sheet = .x,
                             convert = FALSE,
                             skip_empty_rows = TRUE,
                             skip_empty_cols = TRUE))
    unlink(temp_file)
    names(form_out) <- sheets
  } else {
    all_versions <- dplyr::bind_rows(
      file_list[["deployedGroupFiles"]][["definitionFile"]],
      file_list[["previousDefinitionFiles"]])

    form_versions <- all_versions[["formVersion"]]
    download_urls <- all_versions[["downloadLink"]]

    form_out <- purrr::map2(form_versions, download_urls,
                .f = \(ver, url) {
                  temp_file <- tempfile(fileext = ".xlsx")

                  req |>
                    httr2::req_url(url) |>
                    cto_perform() |>
                    httr2::resp_body_raw() |>
                    writeBin(temp_file)

                  sheets <- c("survey", "choices", "settings")
                  out <- purrr::map(sheets,
                                    ~openxlsx2::wb_to_df(
                                      file = temp_file,
                                      sheet = .x,
                                      convert = FALSE,
                                      skip_empty_rows = TRUE,
                                      skip_empty_cols = TRUE))
                  unlink(temp_file)
                  names(out) <- sheets
                  out
                })
    names(form_out) <- paste0("version_", form_versions)
  }

  if (verbose) cli::cli_progress_done("Form download complete!")
  return(invisible(form_out))
}
