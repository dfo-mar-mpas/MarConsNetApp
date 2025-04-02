## APPTABS
ftabs <- data.frame(flower=unique(c(Ecological$grouping, Ecological$labels)), place="Scotian_Shelf")
mytabs <- NULL
for (i in seq_along(MPAs$NAME_E)) {
  df <- ftabs
  df$place <- MPAs$NAME_E[i]
  mytabs[[i]] <- df
}

MYTABS <- do.call(rbind, mytabs)
apptabs <- rbind(ftabs, MYTABS)
apptabs$tab <- paste0("tab_", seq_along(1:length(apptabs$flower)))
apptabs$link <- paste0("link_", seq_along(1:length(apptabs$flower)))
home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
apptabs <- rbind(home, apptabs)
APPTABS <- apptabs

## areas
areas <- MPAs$NAME_E


## objectives/ data_Objectives

data_objectives <- function(type=NULL, area="St. Anns Bank Marine Protected Area") {
  if (is.null(type)) {
    stop("Must provide a type argument of either network or site")
  }

  if (!(type %in% c("network", "site"))) {
    stop("Must provide a type argument of either network or site")
  }

  if (type == "network") {
    urls <- "https://www.dfo-mpo.gc.ca/oceans/networks-reseaux/scotian-shelf-plateau-neo-ecossais-bay-baie-fundy/development-developpement-eng.html"
  } else if (type == "site") {
    if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
      urls <- 'https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/westernemerald-emeraudewestern-eng.html'
    } else {
      if (area == "St. Anns Bank Marine Protected Area") {
        u <- "stanns-sainteanne"
      } else if (area == "Musquash Estuary, Private Land Component" ) {
        u <- "musquash"
      } else if (area == "Laurentian Channel Marine Protected Area") {
        u <- "laurentian-laurentien"
      } else if (area == "Gully Marine Protected Area") {
        u <- "gully"
      } else {
        return(NULL)
      }
      # else if (area == "bancsDesAmericains_MPA") {
      #   u <- "american-americains"
      # }

      urls <- paste0("https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/",u,"/index-eng.html")
    }

  } # End Site

  pages <- lapply(urls,read_html)
  response <- lapply(urls, GET)
  lines <- strsplit(content(response[[1]], as="text"), "\n")

  if (type == "site") {
    minLine <- which(grepl("Conservation Objectives", lines[[1]]))+1
    maxLine <- which(grepl("Prohibitions", lines[[1]]))-1

    # Unique for bansDesAmericains
    # if (area == "bancsDesAmericains_MPA") {
    #   minLine <- which(lines[[1]] == "    <p>The conservation objectives for the Banc-des-Am\u00e9ricains Marine Protected are to:</p>\r")+2
    #   maxLine <- which(grepl("These objectives promote", lines[[1]]))-2
    # } else

      if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)" ) {
      minLine <- which(grepl("support", lines[[1]], ignore.case=TRUE))[1]
      maxLine <- which(grepl("support", lines[[1]], ignore.case=TRUE))[2]
    }

    final <- lines[[1]][minLine:maxLine]
    if (any(final == "        </ul>\r" )) {
      final <- final[-which(final == "        </ul>\r")]
    }
    if (any(final == "        <ul>\r")) {
      final <- final[-which(final == "        <ul>\r")]
    }

    if (any(final == "          <ul>\r")) {
      final <- final[-which(final == "          <ul>\r")]
    }

    if (any(final == "          </ul>\r")) {
      final <- final[-which(final == "          </ul>\r")]
    }

    final <- sub("^(.*)<[^<]*$", "\\1", final) # Remove everything after the last <
    final <- sub("^[^>]*>", "", final) # remove everything before first >

    if (any(final == "          ")) {
      final <- final[-which(final == "          ")]
    }

    if (any(final == "            ")) {
      final <- final[-which(final == "            ")]
    }

    if (any(final == "    ")) {
      final <- final[-which(final == "    ")]
    }
  } else if (type == "network") {
    minLine <- which(grepl("The objectives for the conservation", lines[[1]],ignore.case = TRUE))+2
    maxLine <- which(grepl("Selecting conservation priorities", lines[[1]], ignore.case = TRUE))-2
    final <- lines[[1]][minLine:maxLine]

    final <- sub("^(.*)<[^<]*$", "\\1", final) # Remove everything after the last <
    final <- sub("^[^>]*>", "", final) # remove everything before first >
  }


  if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
    if (length(final) == 2) {
      source(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data", "site_objectives", "webca.R"))
      final <- c(final, CO3)
    }
  }

  final <- paste0("-", final, "\n")


  return(final)
}

objectives <- lapply(areas, function(x) data_objectives(type="site", area=x))


## Objectives_processed
OBJECTIVES <- vector(mode="list", length(objectives))
for (i in seq_along(OBJECTIVES)) {
  O <- objectives[[i]]
  for (j in seq_along(O)) {
    OBJECTIVES[[i]][[j]] <- newLine(O[j])
  }
}
OBJECTIVES <- lapply(OBJECTIVES, unlist)
names(OBJECTIVES) <- areas
Objectives_processed <- OBJECTIVES

Objectives_processed <- Objectives_processed[which(unname(unlist(lapply(Objectives_processed, function(x) !(is.null(x))))))]

## Context and data_context

data_context <- function(type=NULL, area="St. Anns Bank Marine Protected Area") {
  if (is.null(type)) {
    stop("Must provide a type argument of either network or site")
  }

  if (!(type %in% c("network", "site"))) {
    stop("Must provide a type argument of either network or site")
  }

  if (type == "network") {
    urls <- "https://www.dfo-mpo.gc.ca/oceans/networks-reseaux/scotian-shelf-plateau-neo-ecossais-bay-baie-fundy/development-developpement-eng.html"
  } else if (type == "site") {

    if (area == "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)") {
      urls <- 'https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/westernemerald-emeraudewestern-eng.html'

    } else {
      if (area == "St. Anns Bank Marine Protected Area") {
        u <- "stanns-sainteanne"
      } else if (area == "Musquash Estuary Marine Protected Area") {
        u <- "musquash"
      } else if (area == "Laurentian Channel Marine Protected Area") {
        u <- "laurentian-laurentien"
      } else if (area == "Gully Marine Protected Area") {
        u <- "gully"
      }  else {
        return(NULL)
      }

      # else if (area == "bancsDesAmericains_MPA") {
      #   u <- "american-americains"
      # }

      urls <- paste0("https://www.dfo-mpo.gc.ca/oceans/mpa-zpm/",u,"/index-eng.html")
    }

  } # End Site

  pages <- lapply(urls,read_html)
  response <- lapply(urls, GET)
  lines <- strsplit(content(response[[1]], as="text"), "\n")

  if (type == "site") {
    minLine <- which(grepl("Location", lines[[1]], ignore.case=TRUE))
    if (length(minLine) > 0) {
      minLine <- minLine[1]  # Use only the first occurrence
    }

    maxLine <- which(grepl("Conservation Objective", lines[[1]], ignore.case=TRUE))
    if (length(maxLine) > 0) {
      maxLine <- maxLine[1] - 1  # Use only the first occurrence and subtract 1
    }

    if (area == "WEBCA") {
      maxLine <- which(grepl("0.18%", lines[[1]]))
    }

    final <- lines[[1]][minLine:maxLine]


    final <- sub("^(.*)<[^<]*$", "\\1", final) # Remove everything after the last <
    final <- sub("^[^>]*>", "", final) # remove everything before first >


  } else if (type == "network") {
    minLine <- which(grepl("Creating the network plan", lines[[1]],ignore.case = TRUE))+1
    maxLine <- which(grepl("Setting conservation objectives", lines[[1]], ignore.case = TRUE))-1

    final <- lines[[1]][minLine:maxLine]

    final <- sub("^(.*)<[^<]*$", "\\1", final) # Remove everything after the last <
    final <- sub("^[^>]*>", "", final) # remove everything before first >
  }

  if (any(final == "        ")) {
    final <- final[-which(final == "        ")]
  }

  #final <- sapply(final, function(x) paste0("- ", x, "\n"))

  return(final)
}


Context <- lapply(areas, function(x) data_context(type="site", area=x))
Context <- Context[which(unname(unlist(lapply(Context, function(x) !(is.null(x))))))]



## fp
fp <- read_excel(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data","metaframework.xlsx"))
fp$label_Framework[which(fp$label_Framework == "Gully_MPA_CO")] <- "Gully Marine Protected Area"
fp$label_Framework[which(fp$label_Framework == "Musquash_MPA_CO")] <- "Musquash Estuary, Private Land Component"
fp$label_Framework[which(fp$label_Framework == "Corsair and Georges Canyons CO")] <- "Corsair and Georges Canyons Conservation Area (Restricted Bottom Fisheries Zone)"
fp$label_Framework[which(fp$label_Framework == "Emerald Basin and Sambro Bank Sponge CO")] <- "Emerald Basin Sponge Conservation Area"
fp$label_Framework[which(fp$label_Framework == "Jordan Basin CO")] <- "Jordan Basin Conservation Area"
fp$label_Framework[which(fp$label_Framework == "Northeast Channel Coral CO")] <- "Northeast Channel Coral Conservation Area (Restricted Bottom Fisheries Zone)"
fp$label_Framework[which(fp$label_Framework == "WEBCA_CO")] <- "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)"
fp$label_Framework[which(fp$label_Framework == "st_Anns_Bank_MPA_CO")] <- "St. Anns Bank Marine Protected Area"
fp$label_Framework[which(fp$label_Framework == "Scotian_Shelf_CO")] <- "Scotian_Shelf"
fp$label_Framework[which(fp$label_Framework == "EBM")] <- "EBM"
fp$label_Framework[which(fp$label_Framework == "Eastern Canyons CO")] <- "Eastern Canyons Conservation Area"

## odf

O <- data.frame(
  objectives = c(0, unlist(Objectives_processed, use.names = FALSE), N_Objectives)
)
O$flower_plot <- 0
O$area <- 0
get_first_four_words <- function(texts) { # 7 words
  lapply(texts, function(text) {
    words <- strsplit(text, " ")[[1]] # Split each string into words
    first_four_words <- paste(words[1:min(7, length(words))], collapse = " ") # Concatenate the first four words (or fewer if there are not enough words)
    return(first_four_words)
  })
}

for (i in seq_along(O$objectives)) {
  message(i)
  ob <- gsub("[-\n]", "", O$objectives[i])
  if (!(O$objectives[i] == "0")) {
    keep <- which(tolower(get_first_four_words(fp$label_Objective)) == tolower(get_first_four_words(ob)[[1]]))
    if (length(keep) > 1) {
      browser()
    }
    if (!(length(keep) == 0)) {
      O$flower_plot[i] <- fp$Flowerplot_connection[keep]
      O$area[i] <- fp$label_Framework[keep]
    } else {
      message("i is also wrong ", i)
    }
  } else {
    O$flower_plot[i] <- "flower_0"
    O$area[i] <= "area_0"
  }
}

O$tab <- 0
O$link <- 0
for (i in seq_along(O$objectives)) {
  message("i = ", i)
  if (!(i == 1)) {
    if (!(grepl("Indicator", O$flower_plot[i]))) {
      k1 <- which(APPTABS$place %in% O$area[i]) # SAME AREA AND FLOWER
      k2 <- which(APPTABS$flower == O$flower_plot[i])
      if (length(k2) == 0) {
        if (grepl("Environmental", O$flower_plot[i], ignore.case=TRUE)) {
          k2 <- which(APPTABS$flower == "Environmental Representativity")
        }
      }
      keep <- intersect(k1,k2)
      O$tab[i] <- APPTABS$tab[keep]
      O$link[i] <- APPTABS$link[keep]
    } else {
      k <- which(binned_indicators$indicators == trimws(gsub("-", "", gsub("\n", "", O$objectives[i]))), "right")
      O$tab[i] <- binned_indicators$tab[k]
      O$link[i] <- binned_indicators$link[k]

    }
  } else {
    O$tab[i] <- "tab_0"
    O$link[i] <- "link_0"
  }

}
odf <- O


## pillar_ecol_df (TABS)

target_bin_weight <- 1

pedf <- aggregate_groups("pillar",
                         "Ecological",
                         weights_ratio=NA,
                         weights_sum = NA,
                         ecol_obj_biodiversity_df,
                         ecol_obj_habitat_df,
                         ecol_obj_productivity_df)|>
  mutate(PPTID = as.character(PPTID))

x <-  pedf |>
  group_by(objective, bin, areaID) |>
  reframe(indicator = unique(areaID),
          areaID = "Scotian Shelf",
          score = weighted.mean(score,weight,na.rm = TRUE),
          score = if_else(is.nan(score),NA,score),
          PPTID = paste(PPTID, collapse = "; ")) |>
  group_by(bin) |>
  mutate(weight=target_bin_weight/n()) |>
  ungroup() |>
  bind_rows(pedf) |>
  # mutate(tab=make.names(paste0(areaID,
  #                              "_",
  #                              indicator)))
  mutate(tab=paste0("tab_", seq(length(APPTABS$flower) + 1, length(APPTABS$flower) + length(objective))))

pillar_ecol_df <- x
