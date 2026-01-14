
#' Download Form Files and Templates
#'
#' @description
#' These functions facilitate the retrieval of form-specific assets and templates from the server.
#'
#' * [cto_form_languages()]: Retrieves the list of languages defined in the form.
#' * [cto_form_stata_template()]: Downloads a Stata `.do` file template for importing form data.
#' * [cto_form_printable()]: Downloads a printable (HTML) version of the form definition.
#' * [cto_form_mail_template()]: Downloads a mail merge template for the form.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param form_id Character string. The unique ID of the SurveyCTO form.
#' @param dir String. The directory where downloaded files will be saved.
#' Defaults to the current working directory.
#' @param lang String. The language identifier to be used (e.g., `"English"`).
#' If `NULL`, the form's default language is used.
#' @param csv_dir String. The directory where the CSV data file is saved.
#' This path is embedded into the downloaded Stata do-file to automate data loading.
#' @param dta_dir String. The directory where the Stata `.dta` dataset should be saved.
#' @param relevancies Logical. If `TRUE`, includes relevance logic (skip patterns) in
#' the printable view. Defaults to `FALSE`.
#' @param constraints Logical. If `TRUE`, includes constraint logic in the printable view.
#' Defaults to `FALSE`.
#' @param type Integer (0-2). The format type of the mail merge template.
#' Defaults to `2`.
#' * `0`: Field names
#' * `1`: Field labels
#' * `2`: Both field names and labels
#' @param group_names Logical. If `TRUE`, includes group names in the variable headers.
#' Defaults to `FALSE`.
#'
#' @return
#' * `cto_form_languages()`: A list containing available languages and the default language index.
#' * All other functions: The local file path of the downloaded file, returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Authenticate
#' req <- cto_auth("my_server", "user", "password")
#' form <- "household_survey"
#'
#' # 1. Check available languages
#' langs <- cto_form_languages(req, form)
#' print(langs)
#'
#' # 2. Download a Stata template
#' # We specify where the CSV will eventually be located so the .do file is ready to run
#' cto_form_stata_template(
#'   req,
#'   form_id = form,
#'   dir = "downloads/",
#'   csv_dir = "C:/Data",
#'   dta_dir = "C:/Data"
#' )
#'
#' # 3. Download a printable version with logic shown
#' cto_form_printable(
#'   req,
#'   form_id = form,
#'   dir = "documentation/",
#'   relevancies = TRUE,
#'   constraints = TRUE
#' )
#'
#' # 4. Download a mail merge template
#' cto_form_mail_template(
#'   req,
#'   form_id = form,
#'   dir = "templates/",
#'   type = 2
#' )
#' }
#'
cto_form_languages <- function(req, form_id) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id)

  url_path <- str_glue("forms/{form_id}/languages")
  req <- req_url_query(req, t = as.double(Sys.time()) * 1000)

  if (verbose) cli_progress_step("Reading {.val {form_id}} languages")

  resp <- fetch_api_response(req, url_path)
  if (!is.null(resp$defaultIndex)) {
    resp$defaultIndex <- resp$defaultIndex + 1
  }
  resp
}

#' @export
#' @rdname cto_form_languages
cto_form_stata_template <- function(req, form_id, dir = getwd(), lang = NULL,
                               csv_dir = NULL, dta_dir = NULL) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id)
  checkmate::assert_directory(dir)
  checkmate::assert_string(lang, null.ok = TRUE)
  checkmate::assert_string(csv_dir, null.ok = TRUE)
  checkmate::assert_string(dta_dir, null.ok = TRUE)

  query <- list(
    dateTimeFormat = "MDY",
    repeat_option = 1,
    lang = lang,
    csvPath = csv_dir,
    stataPath = dta_dir,
    t = as.double(Sys.time()) * 1000
  )

  query <- Filter(Negate(is.null), query)
  url_path <- str_glue("forms/{form_id}/stata-template")

  if (verbose) cli_progress_step("Requesting {.val {form_id}} Stata template")
  resp <- fetch_api_response(req_url_query(req, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))

  if (verbose) cli_progress_step("Downloading {.file {path}}")
  fetch_api_response(req, url, path)
  invisible(path)
}


#' @export
#' @rdname cto_form_languages
cto_form_printable <- function(req, form_id, dir = getwd(), lang = NULL,
                               relevancies = FALSE, constraints = FALSE) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id)
  checkmate::assert_directory(dir)
  checkmate::assert_string(lang, null.ok = TRUE)
  checkmate::assert_flag(relevancies)
  checkmate::assert_flag(constraints)

  query <- list(
    lang = lang,
    relevancies = if (relevancies) "on" else NULL,
    constraints = if (constraints) "on" else NULL,
    download = 1,
    t = as.double(Sys.time()) * 1000
  )

  query <- Filter(Negate(is.null), query)
  url_path <- str_glue("forms/{form_id}/printable")

  if (verbose) cli_progress_step("Requesting {.val {form_id}} printable")
  resp <- fetch_api_response(req_url_query(req, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))

  if (verbose) cli_progress_step("Downloading {.file {path}}")
  fetch_api_response(req, url, path)
  invisible(path)
}


#' @export
#' @rdname cto_form_languages
cto_form_mail_template <- function(req, form_id, dir = getwd(),
                                   type = 2, group_names = FALSE) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id)
  checkmate::assert_directory(dir)
  checkmate::assert_flag(group_names)
  checkmate::assert_number(type, lower = 0, upper = 2)
  type <- floor(type)

  query <- list(
    type = type,
    groupnames = group_names,
    t = as.double(Sys.time()) * 1000
  )

  query <- Filter(Negate(is.null), query)
  url_path <- str_glue("forms/{form_id}/mail-merge-template")

  if (verbose) cli_progress_step("Requesting {.val {form_id}} mail merge template")
  resp <- fetch_api_response(req_url_query(req, !!!query), url_path)

  url <- resp[["url"]]
  path <- file.path(dir, basename(url))

  if (verbose) cli_progress_step("Downloading {.file {path}}")
  fetch_api_response(req, url, path)
  invisible(path)
}


