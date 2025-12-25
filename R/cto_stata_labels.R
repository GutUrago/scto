


#' Generate Stata Variable and Value Labels from SurveyCTO Form Definitions
#'
#' @description
#' This function automates the creation of a Stata `.do` file that applies
#' variable labels, value labels, and notes to a dataset. It parses the
#' XLSForm structure from SurveyCTO, handles multi-language forms, and
#' accounts for complex structures like repeat groups and select-multiple fields.
#'
#' @param req A `httr2_request` object initialized via
#' \code{\link{cto_request}()}.
#' @param form_id Character string. The unique ID of the SurveyCTO form.
#' @param export Character (optional). The file path where the resulting `.do`
#' file should be saved. Must end in `.do`.
#' @param overwrite Logical. Whether to overwrite the file if it already
#' exists at the `export` path. Defaults to `TRUE`.
#'
#' @details
#' The function performs several cleaning and transformation steps:
#' \itemize{
#'   \item \strong{Language Detection:} Automatically selects the default
#'   language labels or falls back to English if no default is specified.
#'   \item \strong{Value Labels:} Creates Stata `label define` commands for
#'   all `select_one` and `select_multiple` choice lists.
#'   \item \strong{Regex Mapping:} For variables inside repeat groups, the
#'   function generates Stata-compatible regex patterns to ensure labels
#'   are applied to all indexed versions (e.g., `var_1`, `var_2`).
#'   \item \strong{Label Cleaning:} Removes HTML tags, escapes special Stata
#'   characters, and handles dynamic SurveyCTO string interpolation (e.g., `${var}`).
#' }
#'
#' @return A character vector containing the lines of the generated Stata `.do`
#' file, returned invisibly.
#'
#' @export
#'
#' @author Gutama Girja Urago
#'
#' @examples
#' \dontrun{
#' # Authenticate and generate labels
#' req <- cto_request("my_server", "username", "password")
#' cto_stata_labels(req, "household_survey", export = "labels.do")
#' }
cto_stata_labels <- function(
    req,
    form_id,
    export = NULL,
    overwrite = TRUE
    ) {

  if (!is.null(export)) checkmate::assert_path_for_output(export, overwrite, "do")
  form <- cto_get_form_definitions(req, form_id)

  center_line <- function(text, width = 78) {
    if (nchar(text) < width) {
      padding_total <- width - nchar(text)
      left_pad  <- floor(padding_total / 2)
      right_pad <- ceiling(padding_total / 2)
      paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
    } else text
  }

  ts <- format(Sys.time(), format = '%b %d, %Y at %H:%M %Z')
  t1 <- center_line(glue("{toupper(form_id)} VARIABLE AND VALUE LABELS"))
  t2 <- center_line(glue("Generated on {ts} by 'scto' Package in R"))
  do_file_content <- glue(
    strrep("*", 80),
    paste0("*", t1, "*"),
    paste0("*", t2, "*"),
    strrep("*", 80),
    "", "",
    .sep = "\n"
  )

  var_label_col <- names(form$survey)[grepl("^label", names(form$survey), TRUE)]
  if (length(var_label_col) == 0) cli::cli_abort("No column with label name found in the form.")
  if (length(var_label_col > 1)) {
    default_lang <- form$settings$default_language
    dl <- ifelse(is.null(default_lang) || is.na(default_lang) || default_lang == "", "english", default_lang)
    var_label_col <- var_label_col[grepl(dl, var_label_col, TRUE)]
    if (length(var_label_col) > 1) var_label_col <- var_label_col[1]
  }

  if (var_label_col %in% names(form$choices)) {
    val_label_col <- var_label_col
  } else {
    val_label_col <- names(form$survey)[grepl("^label", names(form$choices), TRUE)]
    if (length(val_label_col) == 0) cli::cli_abort("No column with label name found in the form.")
    if (length(val_label_col > 1)) {
      default_lang <- form$settings$default_language
      dl <- ifelse(is.null(default_lang) || is.na(default_lang) || default_lang == "", "english", default_lang)
      val_label_col <- val_label_col[grepl(dl, val_label_col, TRUE)]
      if (length(val_label_col) > 1) val_label_col <- val_label_col[1]
    }
  }

  survey <- form$survey
  if (any(grepl("^disabled$", names(survey)))) {
    survey <- dplyr::filter(survey, !grepl("yes", .data$disabled, TRUE))
  }

  valid_choices <- survey |>
    dplyr::filter(grepl("select_one", .data$type, TRUE)) |>
    dplyr::mutate(list_name = stringr::str_extract(.data$type, "(?<= )\\S+")) |>
    dplyr::pull("list_name") |>
    unique()

  choice_sets <- form$choices |>
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)),
                  list_name = stringr::str_squish(.data$list_name)) |>
    dplyr::filter(!is.na(.data$value), .data$list_name %in% valid_choices) |>
    dplyr::summarise(
      stata_cmd = paste0('label define ', dplyr::first(.data$list_name), ' ',
                         paste0(.data$value, ' "', .data[[val_label_col]], '"', collapse = " "),
                         ', modify'),
      .by = "list_name"
    )

  do_file_content <- c(
    do_file_content,
    "* --- Value Labels ---",
    "label define slt_multi_binary 1 \"Yes\" 0 \"No\"",
    choice_sets[["stata_cmd"]],
    "", ""
  )

  var_labels <- survey |>
    dplyr::filter(!is.na(.data[[var_label_col]])) |>
    dplyr::mutate(
      type = stringr::str_replace_all(.data$type, "\\\n", " "),
      type = stringr::str_squish(.data$type),
      name = stringr::str_replace_all(.data$name, "\\\n", " "),
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
      list_name      = ifelse(grepl("^select_", .data$type, TRUE),
                              stringr::str_extract(.data$type, "(?<= )\\S+"),
                              NA_character_),
      is_slt_multi   = grepl("^select_multiple", .data$type, TRUE),
      list_name      = ifelse(.data$is_slt_multi, "slt_multi_binary", .data$list_name),
      is_null_fields = grepl("^note|^begin group|^end group|^begin repeat|^end repeat", .data$type, TRUE),
      regex_varname  = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, .data$is_slt_multi),
        \(n, r, m) gen_regex_varname(n, r, m)
      )
    ) |>
    dplyr::filter(!.data$is_null_fields) |>
    dplyr::mutate(
      !!var_label_col := stringr::str_remove_all(.data[[var_label_col]], "<[^<>]*>"),
      !!var_label_col := stringr::str_replace_all(.data[[var_label_col]], stringr::fixed("${"), "\\${"),
      !!var_label_col := stringr::str_squish(.data[[var_label_col]]),
      !!var_label_col := stringr::str_replace_all(.data[[var_label_col]], '"', "'"),
      !!var_label_col := stringr::str_replace_all(.data[[var_label_col]], "\\\n", " ")
    ) |>
    dplyr::mutate(
      var_label = stringr::str_remove(.data[[var_label_col]], paste0(.data$name, "(\\W+)?")),
      var_label = stringr::str_trunc(.data$var_label, 80),
      var_note = .data[[var_label_col]]
    ) |>
    dplyr::filter(.data$var_label != "" & !is.na(.data$var_label))


  labels_set <- var_labels |>
    dplyr::mutate(
      stata_cmd = dplyr::case_when(
        # Condition 1: Choice variables in the repeat group
        .data$repeat_level > 0 & !is.na(.data$list_name) ~
          paste(
            "cap {",
            glue("\tunab vars : {.data$name}*"),
            glue("\tforeach rgvar of local vars {{"),
            glue('\t\tif regexm(\"`rgvar\'\", "{.data$regex_varname}") {{'),
            glue('\t\t\tcap label variable `rgvar\' "{.data$var_label}"'),
            glue('\t\t\tcap note `rgvar\': "{.data$var_note}"'),
            glue("\t\t\tcap label values `rgvar\' {.data$list_name}"),
            "\t\t}",
            "\t}",
            "}",
            sep = "\n"
          ),

        # Condition 2: Non-choice variables in the repeat group
        .data$repeat_level > 0 ~
          paste(
            "cap {",
            glue("\tunab vars : {.data$name}*"),
            glue("\tforeach rgvar of local vars {{"),
            glue('\t\tif regexm(\"`rgvar\'\", "{.data$regex_varname}") {{'),
            glue('\t\t\tcap label variable `rgvar\' "{.data$var_label}"'),
            glue('\t\t\tcap note `rgvar\': "{.data$var_note}"'),
            "\t\t}",
            "\t}",
            "}",
            sep = "\n"
          ),

        # Condition 3: Non-repeat select multiple variables
        .data$repeat_level == 0 & .data$is_slt_multi ~
          paste(
            "cap {",
            glue("\tunab vars : {.data$name}*"),
            glue("\tforeach rgvar of local vars {{"),
            glue('\t\tif regexm(\"`rgvar\'\", "{.data$regex_varname}") {{'),
            glue('\t\t\tcap label variable `rgvar\' "{.data$var_label}"'),
            glue('\t\t\tcap note `rgvar\': "{.data$var_note}"'),
            glue("\t\t\tcap label values `rgvar\' {.data$list_name}"),
            "\t\t}",
            "\t}",
            "}",
            sep = "\n"
          ),

        # Condition 4: Non-repeat select one variables
        .data$repeat_level == 0 & !is.na(.data$list_name) ~
          paste(
            glue("cap label variable {.data$name} \"{.data$var_label}\""),
            glue("cap note variable {.data$name} \"{.data$var_note}\""),
            glue("cap label values {.data$name} \"{.data$list_name}\""),
            sep = "\n"
          ),

        # Condition 5: Default (Label + Note only)
        TRUE ~
          paste(
            glue("cap label variable {.data$name} \"{.data$var_label}\""),
            glue("cap note variable {.data$name} \"{.data$var_note}\""),
            sep = "\n"
          )
      )
    )


  do_file_content <- c(
    do_file_content,
    "* --- Variable Labels ---",
    labels_set[["stata_cmd"]],
    "", ""
  )

  if (!is.null(export)) {
    writeLines(do_file_content, export)
    cli::cli_alert_success("Stata labels written to {.file {export}}")
  }

  return(invisible(do_file_content))
}

