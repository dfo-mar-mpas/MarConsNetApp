m <- tar_manifest(script='inst/_targets.R')
data_targets <- m$name[startsWith(m$name, "data_")]

publication_years <- data.frame(
  name = character(),
  publication_year = character()
)


### IF LIST, look for info in there
### if marea object look for it in there
### Should also maybe look at rawdata (rawdata_inaturalist_download/ data_inaturalist)
### data_vessel_traffic might cause problems
for (i in seq_along(data_targets)) {
  message(i)

  target_name <- data_targets[i]

  obj <- targets::tar_read_raw(target_name)

  publication_year <- if ("year_of_publication" %in% names(obj)) {
    unique(obj$year_of_publication)
  } else {
    NA
  }

  stagnant_source <- if ("stagnant_source" %in% names(obj)) {
    unique(obj$stagnant_source)
  } else {
    NA
  }

  publication_years <- rbind(
    publication_years,
    data.frame(
      name = target_name,
      publication_year = publication_year,
      stagnant_source = stagnant_source
    )
  )
}

publication_years


## Need to address these targets
data_targets <- data_targets[-which(data_targets %in% c("data_designtargets_gdb", "data_designtargets_lookup", 'data_obis', 'data_inaturalist', 'data_inseadistance_matrix','data_musquash_MMMP_birds','data_protconn_EL_by_region','data_WORMS_species_distributions','data_otn_tags','data_WORMS_species_distributions_polygons'))]
