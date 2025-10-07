#' path_to_store
#'
#' Convenience function that returns our commonly used paths to the targets data store
#'
#' @returns character
#' @export
#'
#' @examples
#' STORE <- path_to_store()
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
