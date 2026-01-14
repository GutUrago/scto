

#' Create an Authenticated SurveyCTO Request Object
#'
#' @description
#' Establish a connection to a SurveyCTO server. This function initializes an
#' authenticated `httr2` request object with the necessary server URL and
#' credentials.
#'
#' @details
#' ## Security Best Practices
#' It is highly recommended to avoid hard-coding passwords in your scripts.
#'
#' * **Interactive Session:** Leave `password = NULL`. You will be prompted to
#'     enter it securely in the console.
#' * **Automation/Scripts:** Store your credentials in your `.Renviron` file
#'     (e.g., `SCTO_PASSWORD`) and retrieve them with `Sys.getenv()`.
#'
#' @param server String. The subdomain of your SurveyCTO server.
#'   For example, if the full URL is `https://my-org.surveycto.com`,
#'   set this to `"my-org"`.
#' @param username String. The username or email address associated with the account.
#' @param password String. The user password. Defaults to `NULL`.
#'   If `NULL`, the authentication flow will prompt you for the password
#'   interactively.
#'
#' @return A `httr2_request` object with the additional class `scto_request`.
#'
#' @seealso [httr2::req_auth_basic()], [usethis::edit_r_environ()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Interactive authentication (Recommended for interactive sessions)
#' # You will be prompted to enter a password securely.
#' req <- cto_request("my-org", "user@org.com")
#'
#' # 2. Environment variables (Recommended for scripts/automation)
#' # Assumes you have set SCTO_PASSWORD in your .Renviron
#' req <- cto_request("my-org", "user@org.com", Sys.getenv("SCTO_PASSWORD"))
#'
#' # 3. Direct authentication (Discouraged for security reasons)
#' req <- cto_request("my-org", "user@org.com", "my_secret_password")
#' }
cto_request <- function(server, username, password = NULL) {

  verbose <- isTRUE(getOption("scto.verbose", TRUE))

  checkmate::assert_string(server)
  checkmate::assert_string(username)
  checkmate::assert_string(password, null.ok = TRUE)

  if (verbose) cli_progress_step("Requesting access to {.field {server}}")

  cookie_jar <- tempfile()
  base_url <- str_glue("https://{server}.surveycto.com")
  req <- httr2::request(base_url) |>
    httr2::req_user_agent("scto package in R") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_cookie_preserve(cookie_jar) |>
    httr2::req_error(body = cto_error_body)

  if (verbose) cli_progress_step("Verifying credentials")

  resp <- httr2::req_perform(req)

  csrf_exist <- httr2::resp_header_exists(resp, "x-csrf-token")
  if (csrf_exist) {
    csrf <- httr2::resp_header(resp, "x-csrf-token")
    req <- httr2::req_headers(req, `x-csrf-token` = csrf)
  }

  req[["server"]] <- server
  class(req) <- c(class(req), "scto_request")
  if (verbose) cli_progress_step("Access granted!")
  return(req)
}


#' Helper to parse SurveyCTO errors
#' @noRd
cto_error_body <- function(resp) {

  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) NULL
  )

  if (!is.null(body) && !is.null(body$error$message)) {
    return(body$error$message)
  }

  return(NULL)
}
