#' Locate the MarConsNet targets data store
#'
#' Determines the appropriate file path to the
#' \code{MarConsNetTargets/app_targets} directory based on the
#' execution environment. The function checks a series of
#' predefined locations corresponding to common deployment
#' scenarios (e.g. Shiny Server, Linux samba share, Windows samba
#' share, or OneDrive-synchronized directories) and returns the
#' first valid path found.
#'
#' If no expected directory is found, a warning is issued and the
#' current working directory is returned as a fallback.
#'
#' A message is printed indicating which data store path is being
#' used.
#'
#' @details
#' The function checks for the data store in the following order:
#' \enumerate{
#'   \item \code{MarConsNetTargets/app_targets} relative to the
#'         current project directory (e.g. Shiny Server deployment)
#'   \item Linux samba share path
#'         (\code{/srv/sambashare/MarConsNet/...})
#'   \item Windows samba share UNC path
#'         (\code{//wpnsbio9039519.mar.dfo-mpo.ca/...})
#'   \item A OneDrive-synchronized directory defined by the
#'         \code{OneDriveCommercial} environment variable
#' }
#'
#' This approach allows the same application code to run across
#' development, server, and analyst environments without manual
#' path reconfiguration.
#'
#' @return
#' A character string giving the file path to the selected
#' MarConsNet targets data store.
#' @examples
#' \dontrun{
#' # Determine which data store will be used
#' library(targets)
#' store_path <- path_to_store()
#' tar_load(APPTABS, store = store_path)
#' }
#'
#' @export
path_to_store <- function(){
  if(dir.exists("MarConsNetTargets/app_targets")){
    # in the project folder (e.g. on the shiny server)
    store = "MarConsNetTargets/app_targets"
  } else if(dir.exists("/srv/sambashare/MarConsNet/MarConsNetTargets/app_targets")){
    # on a linux machine on the sambashare
    store = "/srv/sambashare/MarConsNet/MarConsNetTargets/app_targets"
  } else if (dir.exists("//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/MarConsNet/MarConsNetTargets/app_targets")) {
    # on a windows machine on the sambashare
    store <- "//wpnsbio9039519.mar.dfo-mpo.ca/sambashare/MarConsNet/MarConsNetTargets/app_targets"
  } else if(dir.exists(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets"))){
    # on a windows machine with OneDriveCommercial set up
    store = file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","app_targets")
  } else {
    warning("MarConsNet data store not found. Please check the directory paths.")
    store = getwd()
  }
  message(paste("Using data store at:",store))
  store
}
