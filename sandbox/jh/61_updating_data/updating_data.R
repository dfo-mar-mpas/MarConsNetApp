m <- tar_manifest(script='inst/_targets.R')
data_targets <- m$name[startsWith(m$name, "data_")]

publication_years <- data.frame(
  name = character(),
  publication_year = character()
)

for (i in seq_along(data_targets)) {
  message(i)

  target_name <- data_targets[i]

  obj <- targets::tar_read_raw(target_name)

  publication_year <- if ("year_of_publication" %in% names(obj)) {
    unique(obj$year_of_publication)
  } else {
    NA
  }

  publication_years <- rbind(
    publication_years,
    data.frame(
      name = target_name,
      publication_year = publication_year
    )
  )
}

publication_years
