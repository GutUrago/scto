

#' Download SurveyCTO Form Metadata and Definitions
#'
#' @description
#' Functions for interacting with SurveyCTO form definitions.
#'
#' * `cto_form_metadata()` retrieves raw metadata for a form, including
#'   available definition files, version identifiers, and download URLs.
#' * `cto_form_definition()` downloads the specific XLSForm definition (Excel file)
#'   and optionally parses it into R data frames.
#'
#' @param req A `httr2_request` object created with [cto_request()].
#' @param form_id A string giving the unique SurveyCTO form ID.
#' @param version Optional string specifying a particular form version
#'   to download. If `NULL` (default), the currently deployed version is used.
#' @param load Logical; if `TRUE` (the default), the downloaded XLSForm
#'   is read into R as a list of tibbles (survey, choices, settings).
#'   If `FALSE`, the function returns the file path to the downloaded Excel file.
#' @param dir A directory where the XLSForm definition file should be saved.
#'   Defaults to [tempdir()].
#' @param overwrite Logical; if `TRUE`, an existing file in `dir` will be
#'   overwritten. If `FALSE` (the default), the existing file is used.
#'
#' @return
#' **`cto_form_metadata()`** returns a list containing the raw JSON metadata,
#' including keys for `deployedGroupFiles` and `previousDefinitionFiles`.
#'
#' **`cto_form_definition()`**:
#' * If `load = TRUE`: Returns a named list of three tibbles (`survey`, `choices`, `settings`).
#' * If `load = FALSE`: Returns a character string containing the file path.
#'
#' @details
#' **Version Handling**:
#' When `version` is supplied, the function validates it against the available
#' form versions returned by `cto_form_metadata()`. An informative error is raised
#' if the requested version does not exist.
#'
#' **Caching**:
#' If the specific version of the form definition already exists in `dir`,
#' it will not be re-downloaded unless `overwrite = TRUE`.
#'
#' @export
#'
#' @seealso
#' [cto_metadata()] for general server-level metadata.
#'
#' @examples
#' \dontrun{
#' req <- cto_request("my-org", "user@org.com")
#'
#' # 1. Get raw metadata
#' meta <- cto_form_metadata(req, "household_survey")
#'
#' # 2. Download and read the current form definition
#' form_def <- cto_form_definition(req, "household_survey")
#' head(form_def$survey)
#'
#' # 3. Download a specific historical version (don't read)
#' file_path <- cto_form_definition(
#'    req,
#'    "household_survey",
#'    version = "20231001",
#'    load = FALSE
#' )
#' }
cto_form_metadata <- function(req, form_id) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  assert_arg(req, c("httr2_request", "scto_request"), "req")
  assert_arg(form_id, "character", "form_id", 1)
  unix_ms <- as.numeric(Sys.time()) * 1000
  url_path <- str_glue("forms/{form_id}/files")
  req <- req_url_query(req, t = unix_ms)
  if (verbose) cli_progress_step("Reading {.val {form_id}} metadata")
  fetch_api_response(req, url_path)
}


#' @export
#' @rdname cto_form_metadata
cto_form_definition <- function(req, form_id, version = NULL, load = TRUE,
                                dir = tempdir(), overwrite = FALSE) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  assert_arg(version, "character", "version", 1, TRUE)
  assert_arg(dir, "character", "dir", 1)
  assert_arg(overwrite, "logical", "overwrite", 1)
  assert_arg(load, "logical", "load", 1)
  if (!dir.exists(dir)) dir.create(dir)

  metadata <- cto_form_metadata(req, form_id)
  df_versions <- dplyr::bind_rows(
    pluck(metadata, "deployedGroupFiles", "definitionFile"),
    pluck(metadata, "previousDefinitionFiles")
    )

  if (!is.null(version)) {
    df <- dplyr::filter(df_versions, .data$formVersion == version)
    if (nrow(df) == 0 && nrow(df_versions) > 0) {
      cli_abort(c(
        "x" = "{.val {form_id}} doesn't have the specified form version: {.val {version}}",
        "i" = "Use {.fn cto_form_metadata} to see available form versions."
        ))
    }
  } else {
    df <- df_versions[1, ]
    version <- df$formVersion[1]
    }

  filename <- file.path(dir, df$filename[1])
  if (!file.exists(filename) || overwrite) {
    url <- df$downloadLink[1]
    if (verbose) cli_progress_step("Downloading form definition version {.val {version}}")
    fetch_api_response(req_url(req, url), file_path = filename)
  }

  if (load && file.exists(filename)) {
    sheets <- c("survey", "choices", "settings")
    form_out <- purrr::map(sheets, ~readxl::read_excel(filename, .x, .name_repair = "minimal"))
    names(form_out) <- sheets
    return(form_out)
  } else filename
}
