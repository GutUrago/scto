
#' Retrieve Metadata from a SurveyCTO Server
#'
#' This function retrieves structural metadata regarding forms, groups, and
#' datasets available on the server.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param which A character string specifying which subset of metadata to return.
#'   One of:
#'   \itemize{
#'     \item `"all"` (default): Returns a list containing groups, datasets, and forms.
#'     \item `"groups"`: Returns a data frame of form groups.
#'     \item `"datasets"`: Returns a data frame of server datasets.
#'     \item `"forms"`: Returns a data frame of deployed forms.
#'   }
#'
#' @return
#'
#'`cto_form_ids` returns **a vector** of available form IDs, and `cto_metadata` returns an
#'object containing the requested metadata.
#'   \itemize{
#'     \item If `which = "all"`, returns a named list.
#'     \item Otherwise, returns the specific `data.frame` requested.
#'   }
#'
#'
#' @export
#'
#' @seealso [cto_request()], [cto_form_data()], [cto_form_metadata()], [cto_form_definition()]
#'
#'
#' @examples
#' \dontrun{
#' # Authenticate first
#' req <- cto_request("myserver", "myuser")
#'
#' # Available form IDs
#' ids <- cto_form_ids(req)
#'
#' # Get all metadata
#' meta <- cto_metadata(req)
#' str(meta)
#'
#' # Get just the forms data frame
#' forms_df <- cto_metadata(req, which = "forms")
#' head(forms_df)
#' }
cto_form_ids <- function(req) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  assert_arg(req, c("httr2_request", "scto_request"), "req")
  if (verbose) cli_progress_step("Checking available forms IDs on {.field {req$server}}")
  url_path <- "api/v2/forms/ids"
  fetch_api_response(req, url_path)
}

#' @export
#' @rdname cto_form_ids
cto_metadata <- function(req, which = c("all", "datasets", "forms", "groups")) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  which <- match.arg(which)
  assert_arg(req, c("httr2_request", "scto_request"), "req")
  if (verbose) cli_progress_step("Reading metadata from {.field {req$server}}")
  url_path <- "console/forms-groups-datasets/get"
  metadata <- fetch_api_response(req, url_path)
  if (which == "all") metadata else metadata[[which]]
}

