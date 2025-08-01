library(sf)
library(dplyr)

# Step 1: Get areas
tar_load(MPAs)
areas <- MPAs


#   get_spatial_layer(
#   "https://maps-cartes.ec.gc.ca/arcgis/rest/services/CWS_SCF/CPCAD/MapServer/0",
#   where =/ "BIOME='M' AND MGMT_E='Fisheries And Oceans Canada'"
# ) |>
#   group_by(NAME_E, NAME_F) |>
#   summarise(geoms = st_make_valid(st_union(geoms)), .groups = "drop")

bbox_coords <- matrix(c(
  -171, 24,  # xmin, ymin (Hawaii/Alaska/Florida range)
  -50,  24,  # xmax, ymin
  -50,  84,  # xmax, ymax
  -171, 84,  # xmin, ymax
  -171, 24   # close the polygon
), ncol = 2, byrow = TRUE)

bbox <- st_polygon(list(bbox_coords)) |>
  st_sfc(crs = st_crs(areas)) |>
  st_make_valid()

# Step 4: Get union of all MPAs
all_mpa_union <- st_union(areas$geoms)

# Step 5: Compute difference (the "outside" area)
outside_geom <- st_difference(bbox, all_mpa_union) |> st_make_valid()

# Step 6: Create the Outside row
outside_row <- tibble(
  NAME_E = "Non_Conservation_Area",
  NAME_F = "Extérieur",
  geoms = outside_geom,
  region = NA
)

# Step 7: Bind with existing areas
areas_full <- bind_rows(areas, outside_row)

MPAs <- areas_full









# Checked indicator level
 # tar_load(data_azmp_Discrete_Occupations_Sections)
 # data <- data_azmp_Discrete_Occupations_Sections  |>
 #   dplyr::select(longitude, latitude, year, depth, temperature)
 #
 #
 # leaflet() %>%
 #   addTiles() %>%
 #   addPolygons(data = areas_full$geoms[length(areas_full$geoms),],
 #               fillColor = "pink",
 #               fillOpacity = 0.5,
 #               color = "black",
 #               weight = 1) %>%
 #   addCircleMarkers(lat=data$latitude, lng=data$longitude,
 #                    radius=1, color="blue", fillOpacity=0.5)


# ind <- process_indicator(data = data,
#                   indicator_var_name = "temperature",
#                   indicator = "Temperature",
#                   type = "Discrete Occupations Sections",
#                   units = "C",
#                   scoring = "desired state: decrease",
#                   PPTID = 579,
#                   source="AZMP",
#                   project_short_title = "AZMP",
#                   climate = TRUE,
#                   other_nest_variables="depth",
#                   areas = MPAs,
#                   climate_expectation="FIXME",
#                   indicator_rationale="Changes in temperature influence not only the distribution of species associated with particular water masses (e.g., Alvarez Perez and Santana 2022), but also affect growth and development rates, generation times and productivity of all species (e.g., Shoji et al. 2011; Szuwalski et al. 2021; Millington et al. 2022).",
#                   bin_rationale="FIXME",
#                   plot_type=c('time-series','map'),
#                   plot_lm=FALSE)


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
           command={
             om
             dped <- data_pillar_ecol_df[-which(data_pillar_ecol_df$indicator %in% MPAs$NAME_E),]


             OM <- om |>
               dplyr::select(project_id, fiscal_year, amount, funding_source_display, category_type)

             ppt <- dped %>%
               group_by(PPTID)

             # Ignoring external data for now
             ppt <- ppt[-which(is.na(ppt$PPTID)),]

             ppt <- split(ppt, ppt$PPTID)

             OM$number_of_project_stations <- NA

             for (i in seq_along(ppt)) {
               PPT <- ppt[[i]]
               d <- PPT$data
               dd <- bind_rows(d)
               combinations <- dd |> distinct(geometry, year) |> nrow()
               OM$number_of_project_stations[which(OM$project_id == as.numeric(names(ppt)[i]))] <- combinations # FIXME: Note, need to get the same to see if they happened at different times
             }

             OM <- OM[which(OM$project_id %in% names(ppt)),]
             OM <- split(OM, OM$project_id)

             price_per_station <- list()
             for (i in seq_along(OM)) {
               price_per_station[[i]] <- sum(OM[[i]]$amount) / unique(OM[[i]]$number_of_project_stations)
             }
             names(price_per_station) <- names(OM)

             # Now that we have price per station we can determine how many unique stations (and time) are within the MPA

             cost_per_site <- vector("list", length(MPAs$NAME_E))
             names(cost_per_site) <- MPAs$NAME_E

             cost_per_site <- vector("list", length(MPAs$NAME_E))
             names(cost_per_site) <- MPAs$NAME_E


             cost_per_site <- vector("list", length(MPAs$NAME_E))
             names(cost_per_site) <- MPAs$NAME_E

             for (i in seq_along(MPAs$NAME_E)) {
               message(i)
               #i <- 45 # FIXME
               mpa_name <- MPAs$NAME_E[i]

               for (j in seq_along(names(price_per_station))) {
                 message(j)
                 #j <- 2 # FIXME

                 pps <- names(price_per_station)[j]

                 ddff_list <- dped$data[
                   dped$PPTID == pps &
                     $areaID == mpa_name
                 ]

                 if (any(!vapply(ddff_list, is.null, logical(1)))) {
                   result <- bind_rows(ddff_list) |>
                     dplyr::select(year, geometry)
                 } else {
                   result <- NULL
                 }
                 if (!(is.null(result))) {
                   combinations <- result |> distinct(geometry, year) |> nrow() # This is the number of unique stations in the MPA

                   # Find percentage of stations in the MPA
                   cost_per_site[[i]][[j]] <- combinations*price_per_station[[j]]
                 } else {
                   cost_per_site[[i]][[j]] <- 0
                 }
               }
             }

             final_cost_per_site <- lapply(cost_per_site, function(x) sum(unlist(x)))
           }
)
















