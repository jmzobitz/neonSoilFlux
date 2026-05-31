#' Install a NEON API token in Your \code{.Renviron} File for repeated Use
#' @description This function will add your NEON API token to your \code{.Renviron} file so it can be called securely without being stored in your code. After you have installed your key, it can be called any time by typing \code{Sys.getenv("NEON_TOKEN")} and can be used in package functions by simply typing NEON_TOKEN If you do not have an \code{.Renviron} file, the function will create on for you.
#' If you already have an \code{.Renviron} file, the function will append the key to your existing file, while making a backup of your original file for disaster recovery purposes.
#' This function was adapted from \code{census_api_key()} in the \code{tidycensus} library. \url{https://cran.r-project.org/web/packages/tidycensus/index.html}.
#' @param token The API token provided to you from the NEON formatted in quotes. A token can be acquired at \url{https://www.neonscience.org/resources/learning-hub/tutorials/api-token-setup}.
#' @param install if TRUE, will install the token in your \code{.Renviron} file for use in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite an existing NEON_TOKEN that you already have in your \code{.Renviron} file.
#' @seealso [get_neon_api_token()]
#' @importFrom utils write.table read.table
#' @examples
#'
#' \dontrun{
#' neon_api_token("111111abc", install = TRUE)
#' # First time, reload your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("NEON_API_TOKEN")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' neon_api_token("111111abc", overwrite = TRUE, install = TRUE)
#' # First time, relead your environment so you can use the token without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("NEON_TOKEN")
#' }
#' @export

neon_api_token <- function(token, overwrite = FALSE, install = FALSE){

  # changelog and author contributions / copyrights
  #   John Zobitz (2026-05-31)
  #     original creation

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("NEON_API_TOKEN", oldenv),]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("NEON_TOKEN",tv))){
          stop("A NEON_TOKEN already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    tokenconcat <- paste0("NEON_TOKEN='", token, "'")
    # Append API key to .Renviron file
    write(tokenconcat, renv, sep = "\n", append = TRUE)
    message('Your API token has been stored in your .Renviron and can be accessed by Sys.getenv("NEON_TOKEN"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(token)
  } else {
    message("To install your API token for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(NEON_TOKEN = token)
  }

}
