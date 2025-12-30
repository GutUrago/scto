


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
#' @param path Character (optional). The file path where the resulting `.do`
#' file should be saved. Must end in `.do`.
#' @param overwrite Logical. Whether to overwrite the file if it already
#' exists at the `path` path. Defaults to `TRUE`.
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
#' cto_stata_labels(req, "household_survey", path = "labels.do")
#' }
cto_stata_labels <- function(req, form_id, path = NULL, overwrite = TRUE) {

  if (!is.null(path)) checkmate::assert_path_for_output(path, overwrite, "do")
  form <- cto_form_definition(req, form_id)

  ts <- format(Sys.time(), format = '%b %d, %Y at %H:%M %Z')
  t1 <- center_text(str_glue("{toupper(form_id)} VARIABLE AND VALUE LABELS"))
  t2 <- center_text(str_glue("Generated on {ts} by 'scto' Package in R"))

  do_file_content <- str_glue(
    strrep("*", 80),
    paste0("*", t1, "*"),
    paste0("*", t2, "*"),
    strrep("*", 80),
    "", "",
    .sep = "\n"
  )

  var_label_col <- names(form$survey)[grepl("^label", names(form$survey), TRUE)]
  if (length(var_label_col) == 0) cli_abort("No column with label name found in the form.")
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
    if (length(val_label_col) == 0) cli_abort("No column with label name found in the form.")
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


  valid_choices_s1 <- survey |>
    dplyr::filter(grepl("select_one", .data$type, TRUE)) |>
    mutate(list_name = str_extract(.data$type, "(?<= )\\S+")) |>
    dplyr::pull("list_name") |>
    unique()

  valid_choices_sm <- survey |>
    dplyr::filter(grepl("select_multiple", .data$type, TRUE)) |>
    mutate(list_name = str_extract(.data$type, "(?<= )\\S+")) |>
    dplyr::pull("list_name") |>
    unique()

  choices_all <- form$choices |>
    mutate(
      value = suppressWarnings(as.numeric(.data$value)),
      list_name = str_squish(.data$list_name),
      label_clean = str_remove_all(.data[[val_label_col]], "<[^<>]*>"),
      label_clean = str_replace_all(.data$label_clean, '"', "'"),
      label_clean = str_squish(.data$label_clean)
    ) |>
    dplyr::filter(!is.na(.data$value))


  choice_sets_s1 <- choices_all |>
    dplyr::filter(.data$list_name %in% valid_choices_s1) |>
    dplyr::summarise(
      stata_cmd = paste0('label define ', dplyr::first(.data$list_name), ' ',
                         paste0(.data$value, ' "', .data$label_clean, '"', collapse = " "),
                         ', modify'),
      .by = "list_name"
    )

  multi_lookup <- choices_all |>
    select("list_name", "value", "label_clean") |>
    mutate(
      value = ifelse(
        .data$value < 0, paste0("_", abs(.data$value)), as.character(.data$value)
        )
    ) |>
    dplyr::filter(.data$list_name %in% valid_choices_sm) |>
    dplyr::group_split(.data$list_name)

  names(multi_lookup) <- sort(valid_choices_sm)


  do_file_content <- c(
    do_file_content,
    paste0("*", center_text(" VALUE LABELS ", "-"), "*"),
    "",
    "label define slt_multi_binary 1 \"Yes\" 0 \"No\", modify",
    choice_sets_s1[["stata_cmd"]],
    "", ""
  )

  var_labels <- survey |>
    mutate(
      type = str_replace_all(.data$type, "\\\n", " "),
      type = str_squish(.data$type),
      name = str_replace_all(.data$name, "\\\n", " "),
      name = str_squish(.data$name),
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
                              str_extract(.data$type, "(?<= )\\S+"),
                              NA_character_),
      is_slt_multi   = grepl("^select_multiple", .data$type, TRUE),
      list_multi     = ifelse(.data$is_slt_multi,
                              str_extract(.data$type, "(?<= )\\S+"),
                              NA_character_),
      list_name      = ifelse(.data$is_slt_multi, "slt_multi_binary", .data$list_name),
      is_null_fields = grepl("^note|^begin group|^end group|^begin repeat|^end repeat", .data$type, TRUE),
      regex_varname  = purrr::pmap_chr(
        list(.data$name, .data$repeat_level, .data$is_slt_multi),
        \(n, r, m) gen_regex_varname(n, r, m)
      )
    ) |>
    dplyr::filter(!.data$is_null_fields, !is.na(.data[[var_label_col]])) |>
    mutate(
      !!var_label_col := str_remove_all(.data[[var_label_col]], "<[^<>]*>"),
      !!var_label_col := str_replace_all(.data[[var_label_col]], stringr::fixed("${"), "\\${"),
      !!var_label_col := str_squish(.data[[var_label_col]]),
      !!var_label_col := str_replace_all(.data[[var_label_col]], '"', "'"),
      !!var_label_col := str_replace_all(.data[[var_label_col]], "\\\n", " ")
    ) |>
    mutate(
      var_label = stringr::str_remove(.data[[var_label_col]], paste0(.data$name, "(\\W+)?")),
      var_label = stringr::str_trunc(.data$var_label, 80),
      var_note = .data[[var_label_col]]
    ) |>
    dplyr::filter(.data$var_label != "" & !is.na(.data$var_label))


  labels_set <- var_labels |>
    mutate(
      is_repeat = .data$repeat_level > 0,
      has_list  = !is.na(.data$list_name),

      stata_cmd = dplyr::case_when(
        .data$is_slt_multi ~ purrr::pmap_chr(
          list(.data$regex_varname, .data$list_multi, .data$var_label,
               .data$var_note, .data$repeat_level, .data$name),
          function(n, l, v, vn, r, ln) {

            cmd <- str_c(
              "cap {\n",
              "\tunab vars : ", ln, "*\n",
              "\tforeach var of local vars {\n",
              "\t\tif regexm(\"`var'\", \"", n, "\") {\n",
              "\t\tif regexm(\"`var'\", \"", v, "\") {\n",
              "\t\t\tcap note `var': \"", vn, "\"\n",
              "\t\t\tcap label values `var' slt_multi_binary\n"
            )

            if (!is.na(l) && !is.null(multi_lookup[[l]])) {
              choices <- multi_lookup[[l]]

              choice_cmds <- purrr::pmap_chr(
                list(choices$value, choices$label_clean),
                function(val, lbl) {

                  regex_pattern <- stringr::str_replace(n, stringr::fixed("*[0-9]+"), val)

                  full_label <- paste0(lbl, " - ", v)
                  if(nchar(full_label) > 80) full_label <- stringr::str_trunc(full_label, 80)

                  str_c(
                    "\t\tif regexm(\"`var'\", \"", regex_pattern, "\") {\n",
                    "\t\t\tcap label variable `var' \"", full_label, "\"\n",
                    "\t\t\t}\n"
                  )

                })
              cmd <- paste0(cmd, paste(choice_cmds, collapse = ""))

            } else {
              cmd <- paste0(cmd, "\t\tcap label variable `var' \"", v, "\"\n")
            }
            str_c(cmd, "\t\t}\n", "\t}\n", "}")
          }
        ),
        .data$is_repeat ~
          str_c(
            "cap {\n",
            "\tunab vars : ", .data$name, "*\n",
            "\tforeach var of local vars {\n",
            "\t\tif regexm(\"`var'\", \"", .data$regex_varname, "\") {\n",
            "\t\t\tcap label variable `var' \"", .data$var_label, "\"\n",
            "\t\t\tcap note `var': \"", .data$var_note, "\"\n",
            dplyr::if_else(
              .data$has_list,
              str_c("\t\t\tcap label values `var' ", .data$list_name, "\n"),
              ""
            ),
            "\t\t}\n",
            "\t}\n",
            "}"
          ),
        .data$has_list ~
          str_c(
            "cap label variable ", .data$name, " \"", .data$var_label, "\"\n",
            "cap note variable ", .data$name, " \"", .data$var_note, "\"\n",
            "cap label values ", .data$name, " ", .data$list_name
          ),

        # ---- Default ----
        TRUE ~
          str_c(
            "cap label variable ", .data$name, " \"", .data$var_label, "\"\n",
            "cap note variable ", .data$name, " \"", .data$var_note, "\""
          )
      )
    )


  do_file_content <- c(
    do_file_content,
    paste0("*", center_text(" VARIABLE LABELS ", "-"), "*"),
    "",
    labels_set[["stata_cmd"]],
    "", ""
  )

  defaults <- c(
    'cap replace KEY = instanceID if KEY==""',
    'cap drop instanceID',
    'cap label variable KEY "Unique submission ID"',
    'cap label variable SubmissionDate "Date/time submitted"',
    'cap label variable CompletionDate "Date/time review completed',
    'cap label variable formdef_version "Form version used on device"',
    'cap label variable review_status "Review status"',
    'cap label variable review_comments "Comments made during review"',
    'cap label variable review_corrections "Corrections made during review"'
  )

  do_file_content <- c(
    do_file_content,
    paste0("*", center_text(" DEFAULTS FIELDS ", "-"), "*"),
    "",
    defaults, "", "",
    paste0("*", center_text(" THE END! ", "-"), "*")
  )

  if (!is.null(path)) {
    writeLines(do_file_content, path)
    cli::cli_alert_success("Stata labels written to {.file {path}}")
  }

  return(invisible(do_file_content))
}
