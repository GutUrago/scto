#' @importFrom checkmate assert_string assert_class assert_flag assert_character
#' @importFrom cli cli_abort cli_warn cli_inform cli_progress_step
#' @importFrom stringr str_c str_glue str_extract str_squish str_replace_all str_remove_all
#' @importFrom httr2 req_url req_url_path req_url_query req_perform resp_body_json resp_body_raw
#' @importFrom rlang `:=` .data
#' @importFrom dplyr mutate select across
#' @importFrom tidyr matches all_of any_of everything
NULL


# Confirm Form ID ----
cto_form_ids <- function(req, team_id = NULL) {
  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  assert_class(req, c("httr2_request", "scto_request"))
  if (!is.null(team_id)) assert_character(team_id)
  endpoint <- req |>
    req_url_path("/api/v2/forms/ids")
  if (!is.null(team_id)) endpoint <- req_url_query(endpoint, teamId = team_id)
  req_perform(endpoint) |>
    resp_body_json(simplifyVector = TRUE)
}



# Center text -----
center_text <- function(text, fill = " ", width = 78) {
  assert_character(fill, n.chars = 1)
  if (nchar(text) < width) {
    padding_total <- width - nchar(text)
    left_pad  <- floor(padding_total / 2)
    right_pad <- ceiling(padding_total / 2)
    paste0(strrep(fill, left_pad), text, strrep(fill, right_pad))
  } else text
}

# Helper ----
gen_regex_varname <- function(name, rpt_lvl, multi, mp = "_*[0-9]+") { # Add here multi place holder
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
