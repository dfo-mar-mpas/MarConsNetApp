#' Save indicator plots to disk
#'
#' Iterates over a data frame containing ggplot objects and saves
#' each valid plot to disk using a standardized file naming
#' convention based on area and indicator identifiers.
#'
#' Plots associated with non-conservation areas or missing plot
#' objects are skipped.
#'
#' @param df
#' A data frame containing at minimum the columns:
#' \describe{
#'   \item{\code{plot}}{A list column of ggplot objects.}
#'   \item{\code{areaID}}{Character identifier for the conservation area.}
#'   \item{\code{indicator}}{Character identifier for the indicator.}
#' }
#'
#' @param STORE
#' Character string giving the base directory in which plots will
#' be saved. Defaults to the parent directory of the MarConsNet
#' targets store as determined by \code{\link{path_to_store}}.
#'
#' @details
#' For each row in \code{df}, this function:
#' \enumerate{
#'   \item Skips rows where the \code{plot} object is \code{NULL}.
#'   \item Skips rows associated with \code{"Non_Conservation_Area"}.
#'   \item Saves the plot as a PNG file under
#'         \code{STORE/data/plot/}.
#'   \item Constructs filenames of the form
#'         \code{plot_<areaID>_<indicator>.png}.
#' }
#'
#' Filenames are sanitized using \code{make.names()} to ensure
#' compatibility across operating systems.
#'
#' @return
#' A character vector of file paths corresponding to plots that
#' were successfully saved.
#'
#' @seealso
#' \code{\link{path_to_store}} for determining the base storage
#' location.
#'
#' @importFrom ggplot2 ggsave
#'
#' @examples
#' #' \dontrun{
#' # Save plots generated for indicator scoring
#' plot_files <- save_plots(plot_df)
#'
#' # Inspect saved file paths
#' print(plot_files)
#' }
#'
#' @export
save_plots <- function(df, STORE = dirname(path_to_store())){
  if(!all(c("plot","areaID","indicator") %in% names(df))){
    stop("Data frame must contain columns: plot, areaID, indicator")
  }
  allplotnames <- NULL

  for(i in 1:nrow(df)){
    message(i)
    if(!is.null(df$plot[[i]])&df$areaID[[i]]!="Non_Conservation_Area"){
      filename <-  file.path(STORE,
                             "data", "plot",
                             make.names(paste0("plot_",
                                               df$areaID[i],
                                               "_",
                                               df$indicator[i],
                                               ".png")))

      allplotnames <- c(allplotnames,filename)
      ggsave(filename,df$plot[[i]])
    }
  }
  allplotnames
}
