#'  Change names from NAME_E to PPT Tags
#'
#'  This function extracts the names of the protected and conserved areas
#'  from `data_CPCAD_areas` and converts them to tags used in the
#'  Project Planning Tool
#'
#'  @param df a data frame with names "flower", "place", "tab", "link"
#'
#'  @export

NAME_to_tag <- function(df=NULL) {
  if (!(unique(names(df) %in% c("flower", "place", "tab", "link") == TRUE))) {
    stop("Names must be: flower, place, tab, link")
  } else {

    # 1. Dealing with MPAs first
    mpa <- which(grepl("Marine Protected Area", df$place))
    df$place[mpa] <- gsub("Marine Protected Area", "MPA", df$place[mpa])

    mpa2 <- which(grepl("Estuary", df$place))

    mpaKeep <- intersect(mpa, mpa2)
    df$place[mpaKeep] <- gsub("Estuary ", "", df$place[mpaKeep])

    mpa3 <- which(grepl("Western", df$place))
    df$place[mpa3] <- tolower("WEBCA")

    df$place[mpa] <- gsub("\\.", "", df$place[mpa])
  }

  df$place[mpa] <- tolower(gsub(" ", "_", df$place[mpa]))

  return(df)

}
