
`:=` <- rlang::`:=`


# Request perform ----

cto_perform <- function(req, ...) {
  httr2::req_perform(req = req, ...)
}

cto_url_path_append <- function(req, ...) {
  httr2::req_url_path_append(req, ...)
}

cto_url_query <- function(.req, ...) {
  httr2::req_url_query(.req, ...)
}

cto_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}


# Helper ----
gen_regex_varname <- function(name, rpt_lvl, multi) {
  if (rpt_lvl == 0) {
    if (multi) {
      return(paste0("^", name, "_*[0-9]+", "$"))
    } else {
      return(paste0("^", name, "$"))
    }
  } else {
    rpt <- strrep("_[0-9]+", rpt_lvl)
    if (multi) {
      return(paste0("^", name, "_*[0-9]+", rpt, "$"))
    } else {
      return(paste0("^", name, rpt, "$"))
    }
  }
}
