

#' Download and Process SurveyCTO Data
#'
#' Downloads data from a SurveyCTO server in wide JSON format. It supports
#' encrypted forms via a private key and offers an automated "tidy" mode that
#' cleans column types based on the XLSForm definition.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param form_id String. The unique ID of the SurveyCTO form.
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
#'
#' @returns A `data.frame`. If `tidy = FALSE`, returns the raw data obtained from
#' returned JSON object. If `tidy = TRUE`, returns a cleaned data with optimized types
#' and column ordering.
#'
#' @details
#' When `tidy = TRUE`, this function simplifies the transition from data
#' collection to data analysis by automatically handling common SurveyCTO
#' formatting tasks:
#' \itemize{
#'   \item **Data Type Correction:** Converts text-based numbers, dates,
#'     and timestamps into actual R numeric and date objects. It leverages
#'      \code{\link[readr]{parse_guess}()}, which uses a number of heuristics to
#'      determine which type of vector is "best".
#'   \item **Structural Cleanup:** Removes "administrative" rows and columns
#'     used only for form layout, such as notes, group headers, and repeat markers.
#'   \item **Column Organization:** Reorders columns so that key metadata
#'     (like submission dates) appears first, followed by survey questions
#'     in their original order.
#'   \item **Media Handling:** Cleans links for images, audio, or video files
#'     to show just the filename, making it easier to reference attachments.
#'     \item **Geopoint Splitting:** Splits geopoint fields into four variables,
#'     with _latitude, _longitude, _altitude and _accuracy suffixes.
#' }
#'
#' @export
#'
#' @seealso [cto_request()], [cto_form_attachment()]
#'
#'
#' @examples
#' \dontrun{
#' # Basic request
#' req <- cto_request("my_server", "user", "pass")
#'
#' # Download raw data
#' raw <- cto_form_data(req, "my_form_id")
#'
#' # Download and tidy encrypted data
#' clean <- cto_form_data(req, "my_form_id", "keys/my_key.pem")
#' }
cto_form_data <- function(
    req,
    form_id,
    private_key = NULL,
    start_date = as.POSIXct("2000-01-01"),
    status = c("approved", "rejected", "pending"),
    tidy = TRUE
    ) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  assert_arg(req, c("httr2_request", "scto_request"), "req")
  assert_arg(form_id, "character", "form_id", 1)
  assert_arg(private_key, "character", "private_key", 1, TRUE)
  if (!is.null(private_key) & !file.exists(private_key)) {
    cli_abort("No file found at {.file {private_key}}")
  }
  assert_arg(start_date, "POSIXct", "start_date", 1)
  assert_arg(status, "character", "status", 1)
  assert_arg(tidy, "logical", "tidy", 1)

  status <- match.arg(status, several.ok = TRUE)
  start_date <- as.numeric(start_date)

  url_path <- str_glue("api/v2/forms/data/wide/json/{form_id}")
  req_data <- req_url_query(req, date = start_date, r = status, .multi = "pipe")

  if (!is.null(private_key)) {
    req_data <- httr2::req_body_multipart(req_data, private_key = curl::form_file(private_key))
  }

  if (verbose) cli_progress_step("Fetching {.val {form_id}} data")

  raw_data <- fetch_api_response(req_data, url_path)

  if (length(raw_data) == 0) return(data.frame())

  if (!tidy) return(raw_data)

  if (verbose) cli_progress_step("Tidying {.val {form_id}} data")

  form <- cto_form_definition(req, form_id)

  survey <- form$survey |>
    mutate(
      type = str_squish(.data$type),
      name = str_squish(.data$name),
      repeat_level = purrr::accumulate(
        .data$type,
        .init = 0,
        .f = function(i, x) {
          if (grepl("begin repeat", x, TRUE)) i + 1
          else if (grepl("end repeat", x, TRUE)) i - 1
          else i
        }
      )[-1],
      is_repeat      = .data$repeat_level > 0,
      is_gps         = grepl("^geopoint", .data$type, TRUE),
      is_numeric     = grepl("^select_one|^integer|^decimal|^sensor_", .data$type, TRUE),
      is_slt_multi   = grepl("^select_multiple", .data$type, TRUE),
      is_date        = grepl("^date|^today", .data$type, TRUE),
      is_datetime    = grepl("^datetime|^start|^end$", .data$type, TRUE),
      is_null_fields = grepl("^note|^begin group|^end group|^end repeat", .data$type, TRUE),
      is_media       = grepl("^image$|^audio$|^video$|^file|^text audit|^audio audit", .data$type, TRUE),
      regex_varname  = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, .data$is_slt_multi),
        \(n, r, m) gen_regex_varname(n, r, m)
      ),
      regex_varname = ifelse(
        grepl("^begin repeat", .data$type, TRUE),
        stringr::str_replace(.data$regex_varname, r"(\[0-9\]\+\$)", "count"),
        .data$regex_varname
        )

    )

  cs_dates        <- c("CompletionDate", "SubmissionDate")
  all_fields      <- survey$regex_varname[!survey$is_null_fields]
  null_fields     <- survey$regex_varname[survey$is_null_fields]
  numeric_fields  <- survey$regex_varname[survey$is_numeric]
  multi_field     <- survey$regex_varname[survey$is_slt_multi]
  date_fields     <- survey$regex_varname[survey$is_date]
  datetime_fields <- c(cs_dates, survey$regex_varname[survey$is_datetime])
  media_fields    <- survey$regex_varname[survey$is_media]
  gps_fields      <- survey$regex_varname[survey$is_gps]

  tidy_data <- select(raw_data, any_of(cs_dates), matches(all_fields), everything())

  if (length(null_fields) > 0) tidy_data <- select(tidy_data, !matches(null_fields))

  tidy_data <- mutate(tidy_data, across(
    matches(datetime_fields), ~ as.POSIXct(.x, format = "%B %d, %Y %I:%M:%S %p")
  ))

  if (length(numeric_fields) > 0) {
    tidy_data <- mutate(
      tidy_data, across(matches(numeric_fields), as.numeric)
    )
  }

  if (length(date_fields) > 0) {
    tidy_data <- mutate(
      tidy_data, across(matches(date_fields), ~ as.Date(.x, format = "%B %d, %Y"))
    )
  }

  if (length(media_fields) > 0) {
    tidy_data <- mutate(
      tidy_data, across(matches(media_fields), ~ ifelse(grepl("^https", .x, TRUE), basename(.x), .x))
    )
  }

  if (length(gps_fields) > 0) {
    nms <- names(tidy_data)
    suffix <- c("latitude", "longitude", "altitude", "accuracy")
    keep_idx <- sapply(gps_fields, function(pattern) {
      actual_col <- grep(pattern, nms, value = TRUE)
      if (length(actual_col) == 0) return(FALSE)
      check_fld <- paste0(actual_col[1], "_", suffix[1])
      !(check_fld %in% nms)
    })
    gps_fields <- gps_fields[keep_idx]
  }

  if (length(gps_fields) > 0) {
    tidy_data <- tidyr::separate_wider_delim(
      data = tidy_data,
      cols = matches(gps_fields),
      delim = " ",
      names = c("latitude", "longitude", "altitude", "accuracy"),
      names_sep = "_",
      too_few = "align_start",
      cols_remove = FALSE
    )
  }

  tidy_data <- mutate(
    tidy_data, across(is.character, readr::parse_guess)
  )

  return(tidy_data)
}



