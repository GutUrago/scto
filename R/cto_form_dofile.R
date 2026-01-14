


#' Generate Stata Variable and Value Labels from SurveyCTO Form Definitions
#'
#' @description
#' This function automates the creation of a Stata `.do` file that applies
#' variable labels, value labels, and notes to a dataset. It parses the
#' XLSForm structure from SurveyCTO, handles multi-language forms, and
#' accounts for complex structures like repeat groups and select-multiple fields.
#'
#' @param req A `httr2_request` object initialized via [cto_request()].
#' @param form_id Character string. The unique ID of the SurveyCTO form.
#' @param path Character (optional). The file path where the resulting `.do`
#' file should be saved. Must end in `.do`.
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
#'
#' @examples
#' \dontrun{
#' # Authenticate and generate labels
#' req <- cto_request("my_server", "username", "password")
#' cto_form_dofile(req, "household_survey", path = "labels.do")
#' }
cto_form_dofile <- function(req, form_id, path = NULL) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  if (!is.null(path)) checkmate::assert_path_for_output(path, TRUE, "do")
  form <- cto_form_definition(req, form_id)

  if (!is.null(path)) {
    cli_progress_step("Writing {.val {form_id}} Stata do-file to {.file {path}}")
  } else {
    cli_progress_step("Writing {.val {form_id}} Stata do-file")
  }

  # --- 1. Header Generation ---
  ts <- format(Sys.time(), format = '%b %d, %Y at %H:%M %Z')
  t1 <- center_text(str_glue("{toupper(form_id)} VARIABLE AND VALUE LABELS"))
  t2 <- center_text(str_glue("Generated on {ts} by 'scto' Package in R"))

  header_content <- str_glue(
    strrep("*", 80),
    paste0("*", t1, "*"),
    paste0("*", t2, "*"),
    strrep("*", 80),
    "", "",
    .sep = "\n"
  )

  # --- 2. Label Column Detection ---
  find_label_col <- function(cols, choices_cols = FALSE) {
    matches <- cols[grepl("^label", cols, TRUE)]
    if (length(matches) == 0) cli_abort("No column with label name found.")

    if (length(matches) > 1) {
      default_lang <- form$settings$default_language
      dl <- if (is.null(default_lang) || is.na(default_lang) || default_lang == "") "english" else default_lang
      matches_lang <- matches[grepl(dl, matches, TRUE)]
      if (length(matches_lang) > 0) return(matches_lang[1])
      return(matches[1])
    }
    return(matches)
  }

  var_label_col <- find_label_col(names(form$survey))

  val_label_col <- if (var_label_col %in% names(form$choices)) {
    var_label_col
  } else {
    find_label_col(names(form$choices))
  }

  # --- 3. Process Survey & Extract List Names ---
  survey <- form$survey
  if (any(grepl("^disabled$", names(survey)))) {
    survey <- dplyr::filter(survey, !grepl("yes", .data$disabled, TRUE))
  }

  # Extract list names and types efficiently
  select_types <- survey |>
    dplyr::filter(grepl("select_", .data$type, TRUE)) |>
    mutate(
      type_clean = str_squish(.data$type),
      list_name = str_extract(.data$type_clean, "(?<= )\\S+"),
      is_multi = grepl("select_multiple", .data$type_clean, TRUE),
      .keep = "none"
    )

  valid_choices_s1 <- unique(select_types$list_name[!select_types$is_multi])
  valid_choices_sm <- unique(select_types$list_name[select_types$is_multi])

  # --- 4. Process Choices ---
  choices_all <- form$choices |>
    mutate(
      value = suppressWarnings(as.numeric(.data$value)),
      list_name = str_squish(.data$list_name),
      label_clean = .data[[val_label_col]] |>
        str_remove_all("<[^<>]*>") |>
        str_replace_all('"', "'") |>
        str_squish()
    ) |>
    dplyr::filter(!is.na(.data$value))

  # Generate 'label define' commands for select_one
  choice_sets_s1 <- choices_all |>
    dplyr::filter(.data$list_name %in% valid_choices_s1) |>
    dplyr::summarise(
      stata_cmd = paste0('label define ', dplyr::first(.data$list_name), ' ',
                         paste0(.data$value, ' "', .data$label_clean, '"', collapse = " "),
                         ', modify'),
      .by = "list_name"
    )

  # Prepare lookup for select_multiple (using a list for fast access)
  multi_lookup <- choices_all |>
    dplyr::filter(.data$list_name %in% valid_choices_sm) |>
    select("list_name", "value", "label_clean") |>
    mutate(
      value = ifelse(.data$value < 0, paste0("_", abs(.data$value)), as.character(.data$value))
    ) |>
    dplyr::group_split(.data$list_name)

    names(multi_lookup) <- sort(unique(choices_all$list_name[choices_all$list_name %in% valid_choices_sm]))


    # --- 5. Process Variables ---

  # Pre-clean survey data
  var_labels <- survey |>
    mutate(
      orig_order = dplyr::row_number(),
      type = str_squish(str_replace_all(.data$type, "\\\n", " ")),
      name = str_squish(str_replace_all(.data$name, "\\\n", " ")),

      # Calculate repeat levels
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
      is_slt_multi   = grepl("^select_multiple", .data$type, TRUE),
      list_name_raw  = str_extract(.data$type, "(?<= )\\S+"),
      list_name      = ifelse(.data$is_slt_multi, "slt_multi_binary", .data$list_name_raw),
      list_multi     = ifelse(.data$is_slt_multi, .data$list_name_raw, NA_character_),
      is_null_fields = grepl("^note|^begin group|^end group|^begin repeat|^end repeat", .data$type, TRUE)
    ) |>
    dplyr::filter(!.data$is_null_fields, !is.na(.data[[var_label_col]])) |>
    # Generate regex only for relevant rows later if possible, but structure implies we need it here
    mutate(
      regex_varname = purrr::pmap_chr(list(.data$name, .data$repeat_level, .data$is_slt_multi), gen_regex_varname),

      # Efficient cleaning of the label column
      cleaned_label = .data[[var_label_col]] |>
        str_remove_all("<[^<>]*>") |>
        str_replace_all(stringr::fixed("${"), "\\${") |>
        str_replace_all('"', "'") |>
        str_replace_all("\\\n", " ") |>
        str_squish(),

      var_label = stringr::str_trunc(
        stringr::str_remove(.data$cleaned_label, paste0(.data$name, "(\\W+)?")),
        80
      ),
      var_note = .data$cleaned_label,
      has_list = !is.na(.data$list_name) & !.data$is_slt_multi
    ) |>
    dplyr::filter(.data$var_label != "" & !is.na(.data$var_label))

  # --- SPLIT-APPLY-COMBINE STRATEGY ---

  # 1. Complex Select Multiple Logic
  vars_multi <- var_labels |>
    dplyr::filter(.data$is_slt_multi) |>
    mutate(stata_cmd = purrr::pmap_chr(
      list(.data$regex_varname, .data$list_multi, .data$var_label, .data$var_note, .data$name),
      function(n, l, v, vn, ln) {

        base_cmd <- str_c(
          "cap {\n",
          "\tunab vars : ", ln, "*\n",
          "\tforeach var of local vars {\n",
          "\t\tif regexm(\"`var'\", \"", n, "\") {\n",
          "\t\t\tcap label variable `var' \"", v, "\"\n",
          "\t\t\tcap note `var': \"", vn, "\"\n",
          "\t\t\tcap label values `var' slt_multi_binary\n"
        )

        choice_cmds <- ""
        if (!is.na(l) && l %in% names(multi_lookup)) {
          choices <- multi_lookup[[l]]
          patterns <- stringr::str_replace(n, stringr::fixed("*[0-9]+"), choices$value)
          full_labels <- paste0(choices$label_clean, " - ", v)
          full_labels <- ifelse(nchar(full_labels) > 80, stringr::str_trunc(full_labels, 80), full_labels)

          choice_cmds <- paste0(
            "\t\tif regexm(\"`var'\", \"", patterns, "\") {\n",
            "\t\t\tcap label variable `var' \"", full_labels, "\"\n",
            "\t\t\t}\n",
            collapse = ""
          )
        } else {
          choice_cmds <- paste0("\t\tcap label variable `var' \"", v, "\"\n")
        }

        str_c(base_cmd, choice_cmds, "\t\t}\n", "\t}\n", "}")
      }
    ))

  # 2. Logic for Repeats (Non-Multi)
  vars_repeat <- var_labels |>
    dplyr::filter(!.data$is_slt_multi & .data$is_repeat) |>
    mutate(stata_cmd = str_c(
      "cap {\n",
      "\tunab vars : ", .data$name, "*\n",
      "\tforeach var of local vars {\n",
      "\t\tif regexm(\"`var'\", \"", .data$regex_varname, "\") {\n",
      "\t\t\tcap label variable `var' \"", .data$var_label, "\"\n",
      "\t\t\tcap note `var': \"", .data$var_note, "\"\n",
      ifelse(.data$has_list, paste0("\t\t\tcap label values `var' ", .data$list_name, "\n"), ""),
      "\t\t}\n",
      "\t}\n",
      "}"
    ))

  # 3. Simple Variables (Non-Multi, Non-Repeat)
  vars_simple <- var_labels |>
    dplyr::filter(!.data$is_slt_multi & !.data$is_repeat) |>
    mutate(stata_cmd = str_c(
      "cap label variable ", .data$name, " \"", .data$var_label, "\"\n",
      "cap note variable ", .data$name, " \"", .data$var_note, "\"",
      ifelse(.data$has_list, paste0("\ncap label values ", .data$name, " ", .data$list_name), "")
    ))

  # Combine and Restore Order
  labels_set <- dplyr::bind_rows(vars_multi, vars_repeat, vars_simple) |>
    dplyr::arrange(.data$orig_order)

  # --- 6. Final File Assembly ---

  do_file_content <- c(
    header_content,
    paste0("*", center_text(" VALUE LABELS ", "-"), "*"),
    "",
    "label define slt_multi_binary 1 \"Yes\" 0 \"No\", modify",
    choice_sets_s1[["stata_cmd"]],
    "", "",
    paste0("*", center_text(" VARIABLE LABELS ", "-"), "*"),
    "",
    labels_set[["stata_cmd"]],
    "", "",
    paste0("*", center_text(" DEFAULT FIELDS ", "-"), "*"),
    "",
    c(
      'cap replace KEY = instanceID if KEY==""',
      'cap drop instanceID',
      'cap label variable KEY "Unique submission ID"',
      'cap label variable SubmissionDate "Date/time submitted"',
      'cap label variable CompletionDate "Date/time review completed"',
      'cap label variable formdef_version "Form version used on device"',
      'cap label variable review_status "Review status"',
      'cap label variable review_comments "Comments made during review"',
      'cap label variable review_corrections "Corrections made during review"'
    ),
    "", "",
    paste0("*", center_text(" THE END! ", "-"), "*")
  )

  if (!is.null(path)) writeLines(do_file_content, path)
  return(invisible(do_file_content))
}
