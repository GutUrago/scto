

#' Download and Process SurveyCTO Data
#'
#' Downloads data from a SurveyCTO server in wide JSON format. It supports
#' encrypted forms via a private key and offers an automated "tidy" mode that
#' cleans column types based on the XLSForm definition.
#'
#' @param req A `httr2_request` object initialized via
#' \code{\link{cto_request}()}.
#' @param form_id A character string specifying the unique ID of the form.
#' @param private_key An optional path to a `.pem` private key file. Required
#'   if the form is encrypted.
#' @param start_date A `POSIXct` object. Only submissions received after
#'   this date/time will be downloaded. Defaults to "2000-01-01".
#' @param status A character vector of submission statuses to include.
#'   Options are "approved", "rejected", and "pending". Defaults to all three.
#' @param tidy Logical. If `TRUE`, the function fetches the form's XLSForm
#'   definition and automatically converts columns to their correct types
#'   (numeric, date, datetime), order variables and removes structural fields (groups/notes).
#'
#' @returns A `data.frame`. If `tidy = FALSE`, returns the raw data obtained from
#' returned JSON object. If `tidy = TRUE`, returns a cleaned dataset with optimized types
#' and column ordering.
#'
#' @details
#' When `tidy = TRUE`, this function simplifies the transition from data
#' collection to data analysis by automatically handling common SurveyCTO
#' formatting tasks:
#' \itemize{
#'   \item **Data Type Correction:** Converts text-based numbers, dates,
#'     and timestamps into actual R numeric and date objects.
#'   \item **Structural Cleanup:** Removes "administrative" rows and columns
#'     used only for form layout, such as notes, group headers, and repeat markers.
#'   \item **Column Organization:** Reorders columns so that key metadata
#'     (like submission dates) appears first, followed by survey questions
#'     in their original order.
#'   \item **Media Handling:** Cleans links for images, audio, or video files
#'     to show just the filename, making it easier to reference attachments.
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Basic request
#' req <- cto_request("my_server", "user", "pass")
#'
#' # Download raw data
#' raw <- cto_get_data(req, "my_form_id")
#'
#' # Download and tidy encrypted data
#' clean <- cto_get_data(
#'   req,
#'   "my_form_id",
#'   private_key = "keys/my_key.pem",
#'   tidy = TRUE
#' )
#' }
cto_get_data <- function(
    req,
    form_id,
    private_key = NULL,
    start_date = as.POSIXct("2000-01-01"),
    status = c("approved", "rejected", "pending"),
    tidy = TRUE
    ) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  if (verbose) cli::cli_progress_step("Preparing to download...", spinner = TRUE)

  checkmate::assert_class(req, c("httr2_request", "scto_request"))
  if (!is.null(start_date)) checkmate::assert_class(start_date, "POSIXct")
  checkmate::assert_character(status, min.len = 1, max.len = 3)
  if (!is.null(private_key)) checkmate::assert_file(private_key, access = "r")
  checkmate::assert_flag(tidy)

  status <- match.arg(status, several.ok = TRUE)
  start_date <- as.numeric(start_date)

  url_path <- glue("api/v2/forms/data/wide/json/{form_id}?date={start_date}")

  req_data <- req |>
    cto_url_path_append(url_path) |>
    cto_url_query(r = status, .multi = "pipe")

  if (!is.null(private_key)) {
    req_data <- httr2::req_body_multipart(req_data, private_key = curl::form_file(private_key))
  }

  if (verbose) cli::cli_progress_step("Downloading the data...", spinner = TRUE)

  raw_data <- cto_perform(req_data) |>
    cto_body_json(simplifyVector = TRUE, flatten = TRUE)

  if (verbose) cli::cli_progress_done("Downloading complete!")

  if (!tidy) return(raw_data)

  if (verbose) cli::cli_progress_step("Tidying the data...", spinner = TRUE)

  form <- cto_get_form_definitions(req, form_id)

  survey <- form$survey |>
    dplyr::mutate(
      type = stringr::str_squish(.data$type),
      name = stringr::str_squish(.data$name),
      repeat_level = purrr::accumulate(
        .data$type,
        .init = 0,
        .f = function(i, x) {
          if (grepl("begin repeat", x, TRUE)) {
            i + 1
          } else if (grepl("end repeat", x, TRUE)) {
            i - 1
          } else {
            i
          }
        }
      )[-1],
      is_repeat      = .data$repeat_level > 0,
      is_numeric     = grepl("^select_one|^integer|^decimal|^sensor_", .data$type, TRUE),
      is_slt_multi   = grepl("^select_multiple", .data$type, TRUE),
      is_date        = grepl("^date|^today", .data$type, TRUE),
      is_datetime    = grepl("^datetime|^start|^end$", .data$type, TRUE),
      is_null_fields = grepl("^note|^begin group|^end group|^begin repeat|^end repeat", .data$type, TRUE),
      is_media       = grepl("^image$|^audio$|^video$|^file|^text audit|^audio audit", .data$type, TRUE),
      regex_varname  = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, .data$is_slt_multi),
        \(n, r, m) gen_regex_varname(n, r, m)
      ),
      regex_varname  = ifelse(grepl("^begin repeat", .data$type, TRUE),
                              paste0("^", .data$regex_varname, "_count", "$"),
                              .data$regex_varname)
    )

  cs_dates <- c("CompletionDate", "SubmissionDate")
  all_fields <- survey$regex_varname[!survey$is_null_fields]
  null_fields <- survey$regex_varname[survey$is_null_fields]
  numeric_fields <- survey$regex_varname[survey$is_numeric]
  multi_field <- survey$regex_varname[survey$is_slt_multi]
  date_fields <- survey$regex_varname[survey$is_date]
  datetime_fields <- c(cs_dates, survey$regex_varname[survey$is_datetime])
  media_fields <- survey$regex_varname[survey$is_media]

  tidy_data <- raw_data |>
    dplyr::select(
      !dplyr::matches(null_fields)
    ) |>
    dplyr::select(
      dplyr::any_of(cs_dates),
      dplyr::matches(all_fields),
      dplyr::everything()
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::matches(numeric_fields), as.numeric),
      dplyr::across(dplyr::matches(date_fields), ~as.Date(.x, format = "%B %d, %Y")),
      dplyr::across(dplyr::matches(datetime_fields), ~as.POSIXct(.x, format = "%B %d, %Y %I:%M:%S %p")),
      dplyr::across(dplyr::matches(media_fields), ~ifelse(grepl("^https", .x, TRUE), basename(.x), .x))
    )
  # Handle geopoints here
  if (verbose) cli::cli_progress_done("Tidying complete!")
  tidy_data
}



