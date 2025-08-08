library(sf)
library(dplyr)

# Step 1: Get areas
# tar_load(MPAs)
# areas <- MPAs
#
#
# #   get_spatial_layer(
# #   "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0",
# #   where =/ "BIOME='M' AND MGMT_E='Fisheries And Oceans Canada'"
# # ) |>
# #   group_by(NAME_E, NAME_F) |>
# #   summarise(geoms = st_make_valid(st_union(geoms)), .groups = "drop")
#
# bbox_coords <- matrix(c(
#   -171, 24,  # xmin, ymin (Hawaii/Alaska/Florida range)
#   -50,  24,  # xmax, ymin
#   -50,  84,  # xmax, ymax
#   -171, 84,  # xmin, ymax
#   -171, 24   # close the polygon
# ), ncol = 2, byrow = TRUE)
#
# bbox <- st_polygon(list(bbox_coords)) |>
#   st_sfc(crs = st_crs(areas)) |>
#   st_make_valid()
#
# # Step 4: Get union of all MPAs
# all_mpa_union <- st_union(areas$geoms)
#
# # Step 5: Compute difference (the "outside" area)
# outside_geom <- st_difference(bbox, all_mpa_union) |> st_make_valid()
#
# # Step 6: Create the Outside row
# outside_row <- tibble(
#   NAME_E = "Non_Conservation_Area",
#   NAME_F = "Extérieur",
#   geoms = outside_geom,
#   region = NA
# )
#
# # Step 7: Bind with existing areas
# areas_full <- bind_rows(areas, outside_row)
#
# MPAs <- areas_full

# Make fake bin level (only aggregion)
tar_load(ecol_obj_habitat_df)
new_row <- ecol_obj_habitat_df[which(ecol_obj_habitat_df$indicator == "Bloom Amplitude" & grepl("Emerald Bank", ecol_obj_habitat_df$areaID)),]
new_row$areaID <- "Non_Conservation_Area"
ecol_obj_habitat_df <- rbind(ecol_obj_habitat_df, new_row)
tar_load(ecol_obj_productivity_df)


# PILLAR_ECOL_DF

tar_load(APPTABS)
tar_load(regions)
target_bin_weight <- 1

pedf <- aggregate_groups("pillar",
                         "Ecological",
                         weights_ratio=NA,
                         weights_sum = NA,
                         #ecol_obj_biodiversity_df,
                         ecol_obj_habitat_df,
                         ecol_obj_productivity_df) |>
  mutate(PPTID = as.character(PPTID)) |>
  left_join(dplyr::select(as.data.frame(MPAs), NAME_E, region), by = c("areaID"="NAME_E"))

# 🔴 Make a filtered version for summarizing (excludes Non_Conservation_Area)
pedf_filtered <- pedf |> filter(areaID != "Non_Conservation_Area")

x <- pedf_filtered |>
  group_by(objective, bin, areaID, region) |>
  reframe(
    indicator = unique(areaID),
    areaID = unique(region),
    score = weighted.mean(score, weight, na.rm = TRUE),
    score = if_else(is.nan(score), NA, score),
    PPTID = paste(PPTID, collapse = "; ")
  ) |>
  group_by(bin) |>
  mutate(weight = target_bin_weight / n()) |>
  ungroup() |>

  # 🔴 Bind in full data, including Non_Conservation_Area
  bind_rows(pedf) |>

  mutate(tab = paste0("tab_", seq(length(APPTABS$flower) + 1, length(APPTABS$flower) + length(objective))))

areas <- unique(x$areaID)

pillar_list <- split(x, x$areaID)

# 🔴 Keep Non_Conservation_Area in pillar_list
mpa_list <- pillar_list[!names(pillar_list) %in% regions$NAME_E]

mpa_list <- lapply(mpa_list, function(ddff) {
  ddff[order(ddff$score, na.last = TRUE), ]
})

region_list <- pillar_list[names(pillar_list) %in% regions$NAME_E]
region_list <- lapply(region_list, function(ddff) {
  ddff[order(ddff$score, na.last = TRUE), ]
})

for (i in seq_along(region_list)) {
  reg <- region_list[[i]]
  for (j in 1:nrow(reg)) {
    reg2 <- reg[j,]
    region_bin <- reg2$indicator
    keep <- which(names(mpa_list) == region_bin)
    if (length(keep) > 0) {
      mpa_list[[keep]] <- rbind(reg2, mpa_list[[keep]])
    }
  }
}

data_pillar_ecol_df <- do.call(rbind, mpa_list)








## PILLAR_ECOL_DF


pillar_ecol_df <- dplyr::select(data_pillar_ecol_df, -plot, -data)







## MPA_REPORT_CARD

d_pedf <- data_pillar_ecol_df[-which(data_pillar_ecol_df$areaID == "Non_Conservation_Area"),]

MPA_report_card <- left_join(MPAs,d_pedf |>
            dplyr::select(-data,-plot) |>
            filter(indicator %in% MPAs$NAME_E) |>
            calc_group_score(grouping_var = "indicator") |>
            mutate(grade = if_else(is.nan(score),
                                   NA,
                                   calc_letter_grade(score))),
          by=c("NAME_E"="indicator"))


## Now dealing with O&M
# NEED UNIQUE LAT/LON/TIME

tar_target(cost_of_mpas,

           # This looks at the number of samples that were collected in our source dataset.
           # A 'sample' is considered a unique lat/lon and year. If multiple samples were
           # taken at the exact same location in the same year, this would only show up once.
           # Additionally, if there is no 'time' column, a 'sample' is simply considered each
           # unique lat and lon. This approach also assumes that we have pulled all data.
           # This should be considered when considering data sets such as AZMP.
           # It takes the number of samples overall and compares it to the number of samples in the MPA

           # Note that we still don't have any pricing information for 'polygon' type sampling.
           # This is because all of our indicators like this use external open data.

           command={
             om
             MPAs
             dped <- bin_habitat_ThreatstoHabitat_df

             #dped <- data_pillar_ecol_df[-which(data_pillar_ecol_df$indicator %in% MPAs$NAME_E),]
             MPA <- MPAs[-which(MPAs$NAME_E %in% "Non_Conservation_Area"),]

             OM <- om |>
               dplyr::select(project_id, fiscal_year, amount, funding_source_display, category_type)

             ppt <- dped %>%
               group_by(PPTID)

             # Ignoring external data for now
             if (any(is.na(ppt$PPTID))) {
             ppt <- ppt[-which(is.na(ppt$PPTID)),]
             }

             ppt <- split(ppt, ppt$PPTID)

             OM$number_of_project_stations <- NA

             for (i in seq_along(ppt)) {
               PPT <- ppt[[i]]
               d <- PPT$data
               dd <- bind_rows(d)
               if ("year" %in% names(dd)) {
               combinations <- dd |> distinct(geometry, year) |> nrow()
               } else {
                 # This assumes each unique location was sampled once. This is a caveat and should be documented.
                 # Representation (JAIM)
                 combinations <- dd |> distinct(geoms) |> nrow()

               }
               OM$number_of_project_stations[which(OM$project_id == as.numeric(names(ppt)[i]))] <- combinations # FIXME: Note, need to get the same to see if they happened at different times
             }

             # NOW I HAVE THE NUMBER OF STATIONS

             OM <- OM[which(OM$project_id %in% names(ppt)),]
             OM <- split(OM, OM$project_id)

             price_per_station <- list()
             for (i in seq_along(OM)) {
               price_per_station[[i]] <- sum(OM[[i]]$amount) / unique(OM[[i]]$number_of_project_stations)
             }
             names(price_per_station) <- names(OM)

             # Now that we have price per station we can determine how many unique stations (and time) are within the MPA

             # I HAVE NUMBER OF STATIONS AND COST PER STATION

             percent_sites_in_mpa <- vector("list", length(MPA$NAME_E))
             names(percent_sites_in_mpa) <- MPA$NAME_E

             for (i in seq_along(MPA$NAME_E)) {
               message(paste0("i = ", i))

               # TEST
               if (!(length(ppt) == 0)) {
               percent_sites_in_mpa[[i]] <- data.frame(project_id=names(price_per_station), percent_sites_in_mpa=rep(NA,length(names(price_per_station))), price_per_station=rep(NA, length(names(price_per_station))), area=MPA$NAME_E[i])

               } else {
                 percent_sites_in_mpa[[i]] <- data.frame(project_id=NA, percent_sites_in_mpa=NA, price_per_station=NA, area=MPA$NAME_E[i])
                 next
               }

               mpa_name <- MPA$NAME_E[i]
               for (j in seq_along(names(price_per_station))) {
                 message(paste0("j = ", j))
                 pps <- names(price_per_station)[j]


                 ddff_list <- dped$data[
                   dped$PPTID == pps &
                     dped$areaID == mpa_name
                 ]

                 if (any(!vapply(ddff_list, is.null, logical(1)))) {
                   if ("year" %in% names(do.call(rbind, ddff_list))) {
                   result <- bind_rows(ddff_list) |>
                     dplyr::select(year, geometry)
                   } else {
                     result <- bind_rows(ddff_list) |>
                       dplyr::select(geoms)
                   }
                 } else {
                   result <- NULL
                 }
                 if (!(is.null(result))) {
                   if ("year" %in% names(result)) {
                   sites_in_mpa <- result |> distinct(geometry, year) |> nrow() # This is the number of unique stations in the MPA
                   } else {
                     sites_in_mpa <- result |> distinct(geoms) |> nrow()
                   }

                   # Find percentage of stations in the MPA
                   total_sites <- unique(OM[[which(names(OM) == pps)]]$number_of_project_stations)

                   percent_sites_in_mpa[[i]]$percent_sites_in_mpa[j] <- sites_in_mpa/total_sites*100
                   percent_sites_in_mpa[[i]]$price_per_station[j] <- price_per_station[[which(names(price_per_station) == pps)]]

                 } else {
                   percent_sites_in_mpa[[i]]$percent_sites_in_mpa[j] <- 0
                   percent_sites_in_mpa[[i]]$price_per_station[j] <- price_per_station[[which(names(price_per_station) == pps)]]


                   }
               }
             }

             x <- do.call(rbind, percent_sites_in_mpa)
             rownames(x) <- NULL
             if (any(is.na(x$project_id))) {
             x <- x[-which(is.na(x$project_id)),] # These are like placeholders, outside data, etc.
             }
             x
           }




)
















