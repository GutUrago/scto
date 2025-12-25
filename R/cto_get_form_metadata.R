

#' Download SurveyCTO Form Metadata
#'
#' @description
#' `cto_get_form_metadata()` retrieves metadata associated with a SurveyCTO form
#' from the server. The metadata includes information about deployed and
#' historical form definition files, such as version identifiers, file names,
#' and download links.
#'
#'
#' @param req A `httr2_request` object initialized via
#' \code{\link{cto_request}()}.
#' @param form_id Character string. The unique ID of the SurveyCTO form.
#'
#' @details
#' The metadata is retrieved via the SurveyCTO API and returned as a nested list
#' mirroring the server response. It typically includes details for the
#' currently deployed form definition as well as any previously deployed
#' versions.
#'
#'
#' @return
#' An invisible named list containing form metadata as returned by the
#' SurveyCTO server.

#' @export
#' @author Gutama Girja Urago
#'
#' @examples
#' \dontrun{
#' # Initialize a SurveyCTO request
#' req <- cto_request(read_lines = Sys.getenv("SCTO_AUTH_FILE"))
#'
#' # Retrieve metadata for a form
#' meta <- cto_get_form_metadata(req, form_id = "household_survey")
#'
#' # Inspect available form versions
#' str(meta)
#' }

cto_get_form_metadata <- function(
    req,
    form_id) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  if (verbose) cli::cli_progress_step("Preparing to download form metadata...", spinner = TRUE)

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  checkmate::assert_string(form_id, min.chars = 1)

  unix_ms <- as.numeric(Sys.time()) * 1000
  url_path <- glue("forms/{form_id}/files")

  if (verbose) cli::cli_progress_step("Downloading form metadata...", spinner = TRUE)
  form_metadata <- req |>
    cto_url_path_append(url_path) |>
    cto_url_query(t = unix_ms) |>
    cto_perform() |>
    cto_body_json(simplifyVector = TRUE)

  if (verbose) cli::cli_progress_done("Form metadata download complete!")
  return(invisible(form_metadata))
}

