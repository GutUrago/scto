

#' Create an Authenticated SurveyCTO Request Object
#'
#' @description
#' Initializes a `httr2` request object with the necessary server URL and
#' Basic Authentication credentials.
#'
#'
#' @param server String. The name of your SurveyCTO server (the subdomain).
#' For example, if your URL is `https://example.surveycto.com`, use `"example"`.
#' @param username String. The username/email for a user account.
#' @param password String. The password for the specified user. You should avoid
#' entering the password directly when calling this function, as it will be
#' captured by `.Rhistory`. Instead, leave it unset (`NULL`) and the default behavior
#' will prompt you for it interactively. If needed for automation purposes,
#' it's recommended to always use the environment variables.
#'
#' @details
#' All functions in this package use `scto.verbose` option to control console
#' feedback. By default, it informs the user which process has started
#' and completed. You can silence this by setting `options(scto.verbose = FALSE)`.
#'
#' @return A modified and authenticated `httr2_request` object.
#'
#' @export
#'
#'
#' @seealso [httr2::req_auth_basic()], [usethis::edit_r_environ()], [Sys.setenv()]
#'
#'
#' @examples
#' \dontrun{
#' # Direct authentication (Not Recommended)
#' req <- cto_request("my-org", "user@org.com", "pw")
#'
#' # Input password when prompted (Interactively)
#' req <- cto_request("my-org", "user@org.com")
#'
#' # Using environment variables for enhanced security (Recommended)
#' req <- cto_request("my-org", Sys.getenv("USER"), Sys.getenv("PASS"))
#' }
cto_request <- function(server, username, password = NULL) {

  verbose <- isTRUE(getOption("scto.verbose", default = TRUE))

  assert_arg(server, "character", "server", 1)
  assert_arg(username, "character", "username", 1)
  assert_arg(password, "character", "password", 1, TRUE)

  if (verbose) cli_progress_step("Requesting access to {.field {server}}")

  cookie_jar <- tempfile()
  base_url <- str_glue("https://{server}.surveycto.com")
  req <- httr2::request(base_url) |>
    httr2::req_user_agent("scto package in R") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_cookie_preserve(cookie_jar) |>
    httr2::req_error(
      body = \(resp) {
        error <- tryCatch(
        resp_body_json(resp)$error,
        error = function(e) NULL
      )
      if (!is.null(error) && !is.null(error$message)) {
        return(error$message)
      } else return(NULL)
    })

  if (verbose) cli_progress_step("Verifying credentials")
  resp <- req_perform(req)

  if (httr2::resp_header_exists(resp, "x-csrf-token")) {
    req <- httr2::req_headers(
      req,
      `x-csrf-token` = httr2::resp_header(resp, "x-csrf-token")
    )
  }
  req[["server"]] <- server
  class(req) <- c(class(req), "scto_request")
  if (verbose) cli_progress_step("Access granted!")
  return(req)
}

