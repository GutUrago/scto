

#' Create an Authenticated SurveyCTO Request Object
#'
#' @description
#' `cto_request()` initializes a base `httr2` request object with the necessary
#' server URL and Basic Authentication credentials. This object can then be
#' extended with specific API endpoints (e.g., for forms or datasets).
#'
#' You can provide credentials either directly via arguments or by pointing
#' to a local text file containing the credentials.
#'
#' @param servername String. The name of your SurveyCTO server (the subdomain).
#' For example, if your URL is \code{https://example.surveycto.com},
#' use \code{"example"}.
#' @param username String. The username/email for a user account with API access.
#' @param password String. The password for the specified user.
#' @param read_lines Path to a local text file and checked only if any of
#' the `servername`, `username`, and `password` arguments are missing.
#' It uses \code{\link[base]{readLines}()} to read the lines. The file must
#' contain exactly three lines:
#' \enumerate{
#'   \item The server name (e.g., "my-organization")
#'   \item The username (email address)
#'   \item The password
#' }
#'
#' @details
#' The function utilizes the \code{scto.verbose} option to control console
#' feedback. By default, it informs the user when authentication starts
#' and succeeds. You can silence this by setting \code{options(scto.verbose = FALSE)}.
#'
#' @return A \code{httr2_request} object prepared with the base URL and
#' authentication headers.
#'
#' @section Security & Credential Management:
#' While providing credentials via an \code{read_lines} is more convenient than
#' hard-coding them in scripts, ensure the file is excluded from version
#' control (e.g., listed in your \code{.gitignore}) to protect sensitive data.
#'
#' For maximum security and workflow flexibility, it is highly recommended to
#' store your credentials as environment variables in your \code{.Renviron} file.
#' This allows you to call the function by passing the environment variables
#' directly to the arguments—see examples below.
#'
#' The object returned by this function is a standard \code{httr2} request.
#' \code{httr2} is designed to comply with high-level secret management
#' standards; specifically, it utilizes obfuscated storage for credentials in
#' memory and ensures that secrets like your password are not leaked into R
#' console logs or tracebacks.
#'
#' @export
#'
#' @author Gutama Girja Urago
#'
#' @seealso \code{\link[usethis]{edit_r_environ}()}, \code{\link{Sys.setenv}()}
#'
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Direct authentication
#' req <- cto_request("my-org", "user@org.com", "pw")
#'
#' # Authentication via file
#' # Create a file "creds.txt" with 3 lines: my-org, user@org.com, pw
#' req <- cto_request(read_lines = "creds.txt")
#'
#' # Using environment variables for enhanced security (Recommended)
#' # In the  .Renviron file, set:
#' # SCTO_AUTH_FILE="path/to/creds.txt"
#' req <- cto_request(read_lines = sys.getenv("SCTO_AUTH_FILE"))
#'
#' # Or set individual variables:
#' # SCTO_SERVER="my-org"
#' # SCTO_USER="user@org.com"
#' # SCTO_PASS="pw"
#' req <- cto_request(
#' servername = Sys.getenv("SCTO_SERVER"),
#' username = Sys.getenv("SCTO_USER"),
#' password = Sys.getenv("SCTO_PASS"))
#' }
cto_request <- function(
    servername = NULL,
    username = NULL,
    password = NULL,
    read_lines = NULL
    ) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))
  if (verbose) cli::cli_progress_step("Preparing to authenticate...", spinner = TRUE)

  if (is.null(servername) || is.null(username) || is.null(password)) {
    checkmate::assert_file_exists(read_lines, access = "r")
    creds <- readLines(read_lines, warn = FALSE)

    if (length(creds) != 3) {
      cli::cli_abort(c(
        "Invalid format in {.file {auth_file}}.",
        "x" = "The file must contain exactly 3 lines:",
        "*" = "Line 1: Server name",
        "*" = "Line 2: Username",
        "*" = "Line 3: Password"
      ))
    }
    servername <- trimws(creds[1])
    username   <- trimws(creds[2])
    password   <- trimws(creds[3])
  }

  checkmate::assert_string(servername, min.chars = 1)
  checkmate::assert_string(username, min.chars = 1)
  checkmate::assert_string(password, min.chars = 1)

  if (verbose) cli::cli_progress_step("Authenticating with {.field {servername}}...", spinner = TRUE)

  req <- glue("https://{servername}.surveycto.com") |>
    httr2::request() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_retry(max_tries = 3)

  resp <- cto_perform(req) |>
  cto_check_status(servername)
  class(req) <- c(class(req), "scto_request")
  if (verbose) cli::cli_progress_done("Authenticated successfully with {.field {servername}}")
  req
}


# Customized errors ----

cto_check_status <- function(resp, servername) {
  status <- httr2::resp_status(resp)
  if (status == 200) return(invisible(TRUE))
  cli::cli_abort(
    c(
      "x" = "SurveyCTO API request to {.field {servername}} failed [Status {.val {status}}].",

      # 400: URL Error
      if (status == 400) {
        c("!" = "The request to {.val {servername}} was malformed.",
          "*" = "Check that your URL parameters (like {.field date}) are correctly formatted.")
      }
      # 401: Authentication
      else if (status == 401) {
        c("!" = "Authentication to {.val {servername}} failed.",
          "*" = "Confirm your username is registered on the server.",
          "*" = "Verify that your password is correct for this specific server.")
      }
      # 403: Access Denied
      else if (status == 403) {
        c("!" = "Permission denied on {.val {servername}}.",
          "*" = "Your user role must have {.strong 'Can download data'} permissions.",
          "*" = "On multi-team servers, ensure your role has access to the form's group.")
      }
      # 404: Not Found
      else if (status == 404) {
        c("!" = "Resource not found on {.val {servername}}.",
          "i" = "Check for typos in the {.field form_id} and ensure the server address is correct.")
      }
      # 409: Parallel requests
      else if (status == 409) {
        c("!" = "Parallel request conflict on {.val {servername}}.",
          "i" = "SurveyCTO API does not support concurrent requests.",
          "v" = "Please wait for your other API calls to finish and try again.")
      }
      # 412: API access disabled
      else if (status == 412) {
        c("!" = "API access is disabled for this user on {.val {servername}}.",
          "*" = "Go to the server console and enable 'Allow API access' for this user role.")
      }
      # 417: Rate Limit
      else if (status == 417) {
        c("!" = "Rate limit reached for {.val {servername}}.",
          "i" = "Full data pulls (date=0) are restricted to once every 300 seconds.",
          "v" = "Wait a few minutes before requesting all submissions again.")
      }
    )
  )
}



