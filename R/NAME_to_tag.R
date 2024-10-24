#'  Change names from NAME_E to PPT Tags
#'
#'  This function extracts the names of the protected and conserved areas
#'  from `data_CPCAD_areas` and converts them to tags used in the
#'  Project Planning Tool
#'
#'  @param df a data frame with names "flower", "place", "tab", "link"
#'  @param names a chacter string to clean up
#'
#'  @export

NAME_to_tag <- function(df=NULL, names=NULL) {

  if (!(is.null(df))) {
    NAMES <- df$place
  } else {
    NAMES <- names
    NAMES <- names
  }

  # 1. Dealing with MPAs first
  #browser()

  if (any(grepl("Marine Protected Area", NAMES))) {
    mpa <- which(grepl("Marine Protected Area", NAMES))
    if (!(is.null(df))) {
    df$place[mpa] <- gsub("\\.","", gsub("Marine Protected Area", "MPA", NAMES[mpa]))
    } else {
      NAMES[mpa] <- gsub("\\.","", gsub("Marine Protected Area", "MPA", NAMES[mpa]))

    }
  }

  if (any(which(grepl("Estuary", NAMES)))) {
  mpa2 <- which(grepl("Estuary", NAMES))
  mpaKeep <- intersect(mpa, mpa2)
  if (!(is.null(df))) {
  df$place[mpaKeep] <- gsub("Marine Protected Area", "MPA", gsub("Estuary ", "", NAMES[mpaKeep]))
  } else {
    NAMES[mpaKeep] <- gsub("Estuary ", "", NAMES[mpaKeep])
  }
  }

if (any(grepl("Western", NAMES))) {
  mpa3 <- which(grepl("Western", NAMES))
  if (!(is.null(df))) {
  df$place[mpa3] <- tolower("WEBCA")
  } else {
    NAMES[mpa3] <- tolower("WEBCA")
  }
  }

  if (exists("mpa")) {
    if (!(is.null(df))) {
  df$place[mpa] <- gsub("\\.", "", df$place[mpa])
  df$place[mpa] <- tolower(gsub(" ", "_", df$place[mpa]))
    } else {
      NAMES[mpa] <- gsub("\\.", "", NAMES[mpa])
      NAMES[mpa] <- tolower(gsub(" ", "_", NAMES[mpa]))
    }
  }

  if (!(is.null(df))) {
  return(df)
  } else {
    return(unlist(unname(NAMES)))
  }

}
