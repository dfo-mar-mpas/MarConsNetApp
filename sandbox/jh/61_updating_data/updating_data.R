m <- tar_manifest(script='inst/_targets.R')
data_targets <- m$name[startsWith(m$name, "data_")]
data_targets <- data_targets[-which(data_targets %in% c("data_designtargets_gdb", "data_designtargets_lookup", 'data_inseadistance_matrix','data_protconn_EL_by_region'))] # This is ok
publication_years <- data.frame(
  name = character(),
  publication_year = character()
)


### IF LIST, look for info in there
### if marea object look for it in there
for (i in seq_along(data_targets)) {
  message(paste0(i, " of ", length(data_targets)))
  target_name <- data_targets[i]

  obj <- targets::tar_read_raw(target_name)

  # Check year_of_publication
  if ("year_of_publication" %in% names(obj)) {
    publication_year <- unique(as.vector(obj$year_of_publication))
  } else if (is.list(obj) && "year_of_publication" %in% names(obj[[1]])) {
    if (!('array' %in% class(unique(obj[[1]]$year_of_publication)))) {
      publication_year <- unique(obj[[1]]$year_of_publication)
    } else {
      publication_year <- as.vector(unique(obj[[1]]$year_of_publication))[1]
    }
  } else {
    publication_year <- NA
  }

  # Check stagnant_source
  if ("stagnant_source" %in% names(obj)) {
    stagnant_source <- unique(as.vector(obj$stagnant_source))
  } else if (is.list(obj) && "stagnant_source" %in% names(obj[[1]])) {
    if (!('array' %in% class(unique(obj[[1]]$year_of_publication)))) {
    stagnant_source <- unique(obj[[1]]$stagnant_source)
    } else {
      stagnant_source <- as.vector(unique(obj[[1]]$stagnant_source))[1]

    }
  } else {
    stagnant_source <- NA
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
data_targets <- data_targets[-which(data_targets %in% c('data_obis','data_WORMS_species_distributions','data_otn_tags','data_WORMS_species_distributions_polygons'))]
