tar_load(data_otn_proportion_tags_detected_in_multiple_mpas)
tar_load(ind_otn)

if (any(names(ind_otn) == "NAME_E.x")) {
  names(ind_otn)[which(names(ind_otn) == "NAME_E.x")] <- "areaID"
}



x <- data.frame("areaID"=MPAs$NAME_E)
x$indicator <- "Proportion of Tags Detected in More than One MPA"
x$type <- "Ocean Tracking Network"
x$units <- "%"
x$scoring <- "connectivity-proportion"
x$PPTID <- NA
x$project_short_title <- NA
x$climate <- FALSE
x$design_target <- FALSE
x$data <- NULL
x$score <- NA
x$status_statement <- NA
x$trend_statement <- "There is no relevant trend statement available."
x$source <- "Ocean Tracking Network"
x$climate_expectation <- "FIXME"
x$indicator_rationale <- "The exchange of individuals between conservation sites can support ecosystem resilience, population recovery, genetic exchange, and the maintenance of biodiversity"
x$bin_rationale <- NA
x$plot <-NULL

# Doing the score and statement status

tracking <- split(ind_otn, ind_otn$tag_id)

otn_areas <- vector("list", length = length(tracking))
names(otn_areas) <- names(tracking)
for (i in seq_along(tracking)) {
  message(i)
  oa <- unique(tracking[[i]]$areaID)
  if (length(oa) == 1 && oa == "Non_Conservation_Area") {
    oa <- NULL
  }

  if (!(is.null(oa)) && any(oa == "Non_Conservation_Area")) {
    oa <- oa[-which(oa == "Non_Conservation_Area")]
  }
  otn_areas[[i]] <- oa
}
otn_areas_old <- otn_areas
otn_areas <- Filter(Negate(is.null), otn_areas)

proportion_in_different_mpa <- data.frame(areaID=unique(unlist(otn_areas)), number_in_mpa=NA, proportion_in_mpa_and_another=NA, connected_MPA=NA)

for (i in seq_along(proportion_in_different_mpa$areaID)) {
  message(i)
  keep <- unname(which(sapply(otn_areas, function(x) proportion_in_different_mpa$areaID[i] %in% x)))
  proportion_in_different_mpa$number_in_mpa[i] <- length(keep)

  if(!any(unname(unlist(lapply(otn_areas, length)))[keep] > 1)) { # Checking if in more than 1 MPA
    proportion_in_different_mpa$proportion_in_mpa_and_another[i] <- 0
  } else {
    multiple_mpas <- length(which(unname(unlist(lapply(otn_areas, length)))[keep] > 1))
    proportion_in_different_mpa$proportion_in_mpa_and_another[i] <- round(multiple_mpas/length(keep)*100,2)
    proportion_in_different_mpa$connected_MPA[i] <- paste0(unique(unlist(unname(otn_areas[keep])))[-which(unique(unlist(unname(otn_areas[keep]))) == proportion_in_different_mpa$areaID[i])], collapse=",")
  }
}

for (i in seq_along(unique(x$areaID))) {
  if (any(proportion_in_different_mpa$areaID == unique(x$areaID)[i])) {
    x$score[which(x$areaID == unique(x$areaID)[i])] <- proportion_in_different_mpa$proportion_in_mpa_and_another[which(proportion_in_different_mpa$areaID == unique(x$areaID)[i])]
    keep <- which(proportion_in_different_mpa$areaID == unique(x$areaID)[i])
    connected_mpa <- ifelse(proportion_in_different_mpa$proportion_in_mpa_and_another[keep] == 0, ".",paste0("(", proportion_in_different_mpa$connected_MPA[keep], ")."))

    ss <- paste0("This protected area has had ",proportion_in_different_mpa$number_in_mpa[keep], " OTN tags detected in the area. Of those, ", proportion_in_different_mpa$proportion_in_mpa_and_another[keep], " % were detected in another MPA ",connected_mpa )
    x$status_statement[which(x$areaID == unique(x$areaID)[i])] <- ss
  } else {
    x$score[which(x$areaID == unique(x$areaID)[i])] <- NA
    x$status_statement[which(x$areaID == unique(x$areaID)[i])] <- NA

  }

}


# Plotting
x$plot <- vector("list", nrow(x))
x$data <- vector("list", nrow(x))


for (i in seq_along(unique(x$areaID))) {
  message(i)
  name_of_interest <- unique(x$areaID)[i]
  k1 <- which(!(unname(sapply(otn_areas_old, is.null))))
  k2 <- which(unname(sapply(otn_areas_old, function(x) name_of_interest %in% x)))
  keep <- intersect(k1,k2)
  if (!(length(keep)== 0)) {
  df <- do.call(rbind, tracking[keep])[-which(do.call(rbind, tracking[keep])$areaID == "Non_Conservation_Area"),]

  map <- leaflet() %>%
    addTiles()

  for (m in seq_along(unique(df$areaID))) {
    map <- map %>%
      addPolygons(
        data = MPAs$geoms[which(MPAs$NAME_E == unique(df$areaID)[m])],
        color = "gray",    # border color
        weight = 1,             # border thickness (smaller)
        fill = TRUE,            # keep the fill if needed
        fillOpacity = 0.3       # adjust fill transparency
      )  }


  subset_track <- split(df, df$tag_id)
  all_tags <- unique(unlist(lapply(subset_track, function(tt) tt$tag)))

  # Create a color palette for the tags
  pal <- colorFactor(palette = viridis(length(all_tags)), domain = all_tags)
  #pal <- colorFactor(palette = "Set3", domain = all_tags)


  for (t in seq_along(subset_track)) {
    tt <- subset_track[[t]]
    tt <- tt[order(tt$eventDate),] # order by time

    coords <- st_coordinates(tt)

    map <- map %>%
      addPolylines(
        lng = coords[,1],
        lat = coords[,2],
        color = pal(tt$tag[1]),   # same color for all points in this track
        weight = 2
      ) %>%
      addCircleMarkers(
        lng = coords[,1],
        lat = coords[,2],
        radius = 3,
        color = pal(tt$tag),      # points colored by tag
        fillOpacity = 0.8,
        label = tt$tag            # show tag on hover
      )
  }

  # Add a legend for the tags
  map <- map %>%
    addLegend("bottomright",
              pal = pal,
              values = all_tags,
              title = "Tag",
              opacity = 1)



  # tmp_html <- tempfile(fileext = ".html")
  # saveWidget(map, tmp_html, selfcontained = TRUE)
  #
  # # 3. Take snapshot as PNG
  # tmp_png <- tempfile(fileext = ".png")
  # webshot(tmp_html, file = tmp_png, vwidth = 800, vheight = 600)
  #
  # # 4. Read PNG into magick image object
  # img <- image_read(tmp_png)
  #
  # # 5. Store the image in the list-column
  # x$plot[[i]] <- img

  x$plot[[i]] <- map

  x$data[[i]] <- ind_otn[which(ind_otn$areaID == name_of_interest),][,c("tag_id", "geometry")]

  } else {
    map <- NULL
  }

  # Now doing data:


}

