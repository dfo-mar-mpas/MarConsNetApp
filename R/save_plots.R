#' Save plots
#'
#' @param df one of the objective dataframes. Must contain columns: data, plot, areaID, indicator
#' @param STORE path to store (default as the dirname(path_to_store()))
#'
#' @returns character vector of filenames where plots were saved
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data frame
#' df <- data.frame(
#'  data = I(list(mtcars, mtcars)),
#'  plot = I(list(ggplot2::qplot(mpg, wt, data = mtcars), ggplot2::qplot(mpg, wt, data = mtcars))),
#'  areaID = c("Area1", "Area2"),
#'  indicator = c("Indicator1", "Indicator2")
#'  )
#'  save_plots(df)
#'  }
save_plots <- function(df, STORE = dirname(path_to_store())){
  if(!all(c("data","plot","areaID","indicator") %in% names(df))){
    stop("Data frame must contain columns: data, plot, areaID, indicator")
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
