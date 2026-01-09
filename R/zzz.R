
#' @importFrom cli cli_abort cli_warn cli_inform cli_progress_step
#' @importFrom stringr str_c str_glue str_extract str_squish str_replace_all str_remove_all
#' @importFrom httr2 req_url req_url_path req_url_query req_perform resp_body_json resp_body_raw
#' @importFrom rlang `:=` .data
#' @importFrom dplyr mutate select across
#' @importFrom tidyr matches all_of any_of everything
#' @importFrom purrr map pluck
NULL

# Assertion ----
assert_arg <- function(x, cls, nm, len = NULL, null_ok = FALSE, na_ok = FALSE) {
  if (is.null(x)) {
    if (null_ok) return(invisible(TRUE))
    cli::cli_abort("{.arg {nm}} must not be {.val NULL}.")
  }

  if (!na_ok && anyNA(x)) {
    cli::cli_abort("{.arg {nm}} must not contain {.val NA}.")
  }

  if (!inherits(x, cls)) {
    cli::cli_abort(
      "{.arg {nm}} must be an object of class {.cls {cls}}, not {.cls {class(x)}}."
    )
  }

  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(
      "{.arg {nm}} must have length {.val {len}}, not {.val {length(x)}}."
    )
  }

  invisible(TRUE)
}

# Fetch API response ----
fetch_api_response <- function(req, url_path = NULL, file_path = NULL) {
  if (!is.null(url_path)) {
    req <- req_url_path(req, url_path)
  }
  resp <- req_perform(req)
  httr2::resp_check_status(resp)
  if (!is.null(file_path)) {
    bin <- httr2::resp_body_raw(resp)
    if (length(bin) == 0) {
      cli::cli_abort("Empty response body when downloading {.val {basename(file_path)}}")
    }
    writeBin(bin, file_path)
    return(invisible(file_path))
  } else {
    resp_body_json(
      resp,
      simplifyVector = TRUE,
      flatten = TRUE
    )
  }
}


# Download progress bar ----
download_pb <- list(
  type  = "download",
  clear = FALSE,
  format = paste0(
    "Downloading {cli::pb_current}/{cli::pb_total} ",
    "({cli::ansi_trimws(cli::pb_percent)}) | ETA: {cli::pb_eta}"
  ),
  format_done = paste0(
    "{cli::col_green(cli::symbol$tick)} ",
    "Downloaded {.val {cli::pb_total}} file{?s} ",
    "{cli::col_grey(paste0('[', cli::pb_elapsed, ']'))}"
  )
)


# Map and return the simplest object ----
map_maybe_list <- function(.x, .f, ..., .names = NULL) {
  res <- purrr::map(.x, .f, ...)
  out <- if (length(res) == 1) res[[1]] else res
  if (!is.null(.names) && length(res) > 1) names(out) <- .names
  out
}

# Center text -----
center_text <- function(text, fill = " ", width = 78) {
  assert_arg(fill, "character", "fill", 1)
  if (nchar(text) < width) {
    padding_total <- width - nchar(text)
    left_pad  <- floor(padding_total / 2)
    right_pad <- ceiling(padding_total / 2)
    paste0(strrep(fill, left_pad), text, strrep(fill, right_pad))
  } else text
}

# Helper ----
gen_regex_varname <- function(name, rpt_lvl, multi, mp = "_*[0-9]+") {
  if (rpt_lvl == 0) {
    if (multi) {
      return(paste0("^", name, mp, "$"))
    } else {
      return(paste0("^", name, "$"))
    }
  } else {
    rpt <- strrep("_[0-9]+", rpt_lvl)
    if (multi) {
      return(paste0("^", name, mp, rpt, "$"))
    } else {
      return(paste0("^", name, rpt, "$"))
    }
  }
}
