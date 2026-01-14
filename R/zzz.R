
#' @importFrom cli cli_abort cli_warn cli_inform cli_progress_step
#' @importFrom stringr str_c str_glue str_extract str_squish str_replace_all str_remove_all
#' @importFrom httr2 req_url req_url_path req_url_query req_perform resp_body_json resp_body_raw
#' @importFrom rlang `:=` .data
#' @importFrom dplyr mutate select across
#' @importFrom tidyr matches all_of any_of everything
NULL



# Fetch API response ----
fetch_api_response <- function(req, url_path = NULL, file_path = NULL) {
  if (!is.null(url_path)) {
    req <- req_url_path(req, url_path)
  }
  if (is.null(file_path)) {
    req_perform(req) |>
      resp_body_json(
        simplifyVector = TRUE,
        flatten = TRUE
      )
  } else {
    req_perform(req, file_path)
  }
}


# Map and return the simplest object ----
map_maybe_list <- function(.x, .f, ..., .names = NULL) {
  res <- purrr::map(.x, .f, ...)
  out <- if (length(res) == 1) res[[1]] else res
  if (!is.null(.names) && length(res) > 1) names(out) <- .names
  out
}

# Center text -----
center_text <- function(text, fill = " ", width = 78) {
  checkmate::assert_string(fill)
  if (nchar(text) < width) {
    padding_total <- width - nchar(text)
    left_pad  <- floor(padding_total / 2)
    right_pad <- ceiling(padding_total / 2)
    paste0(strrep(fill, left_pad), text, strrep(fill, right_pad))
  } else text
}

# Generate regex variable name ----
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
