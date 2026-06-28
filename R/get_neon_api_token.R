#' Check to see if a NEON api token is installed
#' @description This function checks if a NEON API token exists in your \code{.Renviron} file so it can be called securely without being stored in your code.
#' @param token The API token provided to you from the NEON formatted in quotes. A key can be acquired at \url{https://www.neonscience.org/resources/learning-hub/tutorials/api-token-setup}
#' @seealso [neon_api_token()]


#' @export


# Check to see if a NEON API token is installed
get_neon_api_token <- function(token) {


  # changelog and author contributions / copyrights
  #   John Zobitz (2026-05-31)
  #     original creation


  # If a token is supplied, return it
  if (!is.null(token) && length(token) == 1 && nzchar(token)) {
    return(token)

  } else if (Sys.getenv("NEON_TOKEN") == "") {
    rlang::abort(c(
      "A NEON API token is required for accessing NEON data.",
      "i" = stringr::str_wrap("Get a NEON API token at https://www.neonscience.org/resources/learning-hub/tutorials/api-token-setup, then pass it to a neonSoilFlux functions with the `token` argument or store it for future sessions with `neon_api_token(\"YOUR TOKEN\", install = TRUE)`.")
    ))

  } else {

    return(Sys.getenv('NEON_TOKEN'))

  }
}
