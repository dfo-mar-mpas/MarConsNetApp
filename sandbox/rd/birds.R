# require(auk)
# install.packages("naturecounts",
#                  repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                            CRAN = 'https://cloud.r-project.org'))
require(naturecounts)
require(rgbif)
require(targets)
require(spocc)
require(sf)
require(ggplot2)
require(purrr)
require(tibble)
require(vegan)
require(tidyr)
require(dplyr)


tar_load(c("MPAs", "rv_data"))

inter <- st_read(
  "../../../Downloads/Musquash_IntertidalArea_AIA/MusquashIntertidalArea.shp"
) |>
  st_transform(st_crs(MPAs))

aia <- st_read(
  "../../../Downloads/Musquash_IntertidalArea_AIA/MusquashAIA_Feb2015.shp"
) |>
  st_transform(st_crs(MPAs))

MPAs <- MPAs |>
  mutate(
    geoms = if_else(
      NAME_E == "Musquash Estuary Marine Protected Area",
      st_union(c(geoms, inter$geometry, aia$geometry)) |>
        st_combine(),
      geoms
    )
  )


# test <- nc_data_dl(
#   region = list(bbox = st_bbox(MPAs[32, ])),
#   username = "daigleremi",
#   info = "exploring data availability for MPAs"
# ) |>
#   left_join(meta_species_taxonomy(), by = join_by(species_id))

# test2 <- test |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
#   st_join(MPAs) |>
#   filter(!is.na(NAME_E))

# test2$scientific_name |> unique() |> length()

# ggplot(test) + geom_bar(aes(y = english_name))

# nc_results <- map(which(MPAs$region == "Maritimes"), function(i) {
#   print(MPAs$NAME_E[i])
#   nc_data_dl(
#     region = list(bbox = st_bbox(st_buffer(MPAs[i, ], 1000))),
#     username = "daigleremi",
#     info = "exploring data availability for MPAs"
#   )
# }) |>
#   bind_rows() |>
#   left_join(meta_species_taxonomy(), by = join_by(species_id)) |>
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
#   st_join(MPAs, join = st_is_within_distance, 1000) #|>
# # filter(!is.na(NAME_E))

# spcounts <- nc_results |>
#   filter(
#     !is.na(region),
#     class == "Aves",
#     collection == "MBBA2PC",
#     survey_year <= 2017,
#     survey_year <= 2013
#   ) |>
#   group_by(region, NAME_E) |>
#   reframe(n = unique(scientific_name) |> length() |> as.integer(), n_obs = n())
# ggplot(spcounts) +
#   geom_col(aes(x = NAME_E, y = n, fill = region)) +
#   coord_flip() +
#   labs(x = "Marine Protected Area", y = "Number of Bird Species") +
#   theme_minimal()

# ####### Using spocc to get bird data from eBird

# # results <- map(1:nrow(MPAs), function(i) {
# results <- map(which(MPAs$region == "Maritimes"), function(i) {
#   print(MPAs$NAME_E[i])
#   birds <- occ_search(
#     classKey = 212, # 212 is the key for Aves (birds)
#     geometry = st_as_text(st_as_sfc(st_bbox(MPAs[i, ]))),
#     limit = 100000,
#     hasCoordinate = TRUE,
#     hasGeospatialIssue = FALSE
#   )

#   # nc_data_dl(
#   #   region = list(bbox = st_bbox(MPAs[i, ])),
#   #   username = "daigleremi",
#   #   info = "exploring data availability for MPAs"
#   # )
#   birds$data
# }) |>
#   bind_rows() |>
#   # left_join(meta_species_taxonomy(), by = join_by(species_id)) |>
#   st_as_sf(
#     coords = c("decimalLongitude", "decimalLatitude"),
#     crs = 4326,
#     remove = FALSE
#   ) |>
#   st_join(MPAs) |>
#   filter(!is.na(NAME_E))

# results <- map(which(MPAs$region == "Maritimes"), function(i) {
#   print(MPAs$NAME_E[i])
#   birds <- spocc::occ(
#     query = "Spinus tristis",
#     from = "ebird",
#     ebirdopts = list(method = 'ebirdgeo', lat = 42, lng = -76, dist = 50)
#   )

#   birds$data
# }) |>
#   bind_rows() |>
#   # left_join(meta_species_taxonomy(), by = join_by(species_id)) |>
#   st_as_sf(
#     coords = c("decimalLongitude", "decimalLatitude"),
#     crs = 4326,
#     remove = FALSE
#   ) |>
#   st_join(MPAs) |>
#   filter(!is.na(NAME_E))

# spcounts <- results |>
#   filter(!is.na(region)) |> #,
#   # class=="Aves") |>
#   group_by(region, NAME_E) |>
#   reframe(n = unique(scientificName) |> length() |> as.integer(), n_obs = n())
# ggplot(spcounts) +
#   geom_col(aes(x = NAME_E, y = n, fill = region)) +
#   coord_flip() +
#   labs(x = "Marine Protected Area", y = "Number of Bird Species") +
#   theme_minimal()

# ggplot(filter(spcounts, region == "Maritimes")) +
#   geom_col(aes(x = NAME_E, y = n, fill = region)) +
#   coord_flip() +
#   labs(y = "Number of Bird Species") +
#   theme_minimal()

##### MMMP ####
# manual download of MMMP data from NatureCounts
mmmp_naturecounts_data <- read.delim(
  "C:/Users/DaigleR/Downloads/naturecounts_request_257783_1752519752346/mmmp_naturecounts_data.txt",
  header = TRUE
) |>
  filter(!is.na(DecimalLatitude), !is.na(DecimalLongitude)) |>
  st_as_sf(
    coords = c("DecimalLongitude", "DecimalLatitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_join(MPAs) #|>
# st_join(MPAs, join = st_is_within_distance, 10000) |>
# filter(!is.na(NAME_E))
# filter(NAME_E == "Musquash Estuary Marine Protected Area")

musquashbirds <- mmmp_naturecounts_data |>
  st_filter(st_buffer(MPAs[32, ], 2000)) |>
  filter(
    !grepl("53|52|10|11", SurveyAreaIdentifier),
    as.numeric(DecimalLongitude) < (-66.31),
    YearCollected > 2012,
    YearCollected < 2018,
    ScientificName != "",
    !is.na(ScientificName)
  ) |>
  as.data.frame() |>
  mutate(
    DecimalLatitude = round(DecimalLatitude, 3),
    DecimalLongitude = round(DecimalLongitude, 3)
  ) |>
  group_by(
    DecimalLatitude,
    DecimalLongitude,
    YearCollected,
    MonthCollected,
    DayCollected,
    CollectorNumber,
    ScientificName
  ) |>
  reframe(n = n()) |>
  pivot_wider(names_from = ScientificName, values_from = n, values_fill = 0) |>
  select(
    -DecimalLatitude,
    -DecimalLongitude,
    -YearCollected,
    -MonthCollected,
    -DayCollected,
    -CollectorNumber
  )


spcounts <- mmmp_naturecounts_data |>
  filter(!is.na(region), YearCollected <= 2017, YearCollected <= 2013) |>
  group_by(region, NAME_E) |>
  reframe(n = unique(ScientificName) |> length() |> as.integer(), n_obs = n())


ggplot(spcounts) +
  geom_col(aes(x = NAME_E, y = n, fill = region)) +
  coord_flip() +
  labs(x = "Marine Protected Area", y = "Number of Bird Species") +
  theme_minimal()


############ musquash birds

musquashbirds <- mmmp_naturecounts_data |>
  st_filter(st_buffer(MPAs[32, ], 2000)) |>
  filter(
    !grepl("53|52|10|11", SurveyAreaIdentifier),
    as.numeric(DecimalLongitude) < (-66.31),
    YearCollected > 2012,
    YearCollected < 2018,
    # YearCollected==2015,
    ScientificName != "",
    !is.na(ScientificName)
  ) |>
  as.data.frame() |>
  mutate(
    DecimalLatitude = round(DecimalLatitude, 3),
    DecimalLongitude = round(DecimalLongitude, 3),
    # neweventid = paste(DecimalLatitude,DecimalLongitude, YearCollected,MonthCollected,Collector,sep = "_")
    neweventid = paste(DecimalLatitude, DecimalLongitude, YearCollected)
  ) |>
  group_by(neweventid, ScientificName) |>
  reframe(n = n()) |>
  pivot_wider(names_from = ScientificName, values_from = n, values_fill = 0) |>
  select(-neweventid)

specaccum(musquashbirds, method = "random") |>
  plot()


pool <- estaccumR(musquashbirds)

# pool$mmfit <- pool$chao
# pool$mmfit[] <- NA
# pool$mmVm <- pool$mmfit
# for (i in 2:nrow(pool$mmfit)) {
#   for (j in 1:ncol(pool$mmfit)) {
#     # for (j in 1:10) {

#     spacc <- specaccum(
#       slice_sample(musquashbirds, n = i),
#       permutations = 1,
#       method = "random"
#     )
#     fit <- try(fitspecaccum(spacc, model = "michaelis-menten"))
#     if (inherits(fit, "try-error")) {
#       next
#     } else {
#       # pool$mmVm[i,j] <- mean(coef(fit)[1,])
#       # pool$mmfit[i,j] <- tail(fit$richness,1)
#       pool$mmVm[i, j] <- mean(coef(fit)[1])
#       pool$mmfit[i, j] <- tail(fit$richness, 1)
#     }
#   }
# }
pool$SC <- pool$chao
pool$SC[] <- NA
for (i in 1:nrow(pool$SC)) {
  for (j in 1:ncol(pool$SC)) {
    print(i)
        data <- musquashbirds |>
          slice_sample(n=i)|> 
          apply(2,sum,na.rm = TRUE)
    
        a1 <- sum(data==1,na.rm = TRUE)
        a2 <- sum(data==2,na.rm = TRUE)
        N <- sum(data,na.rm = TRUE)
        s0 <- sum(data>0,na.rm = TRUE)
        pool$SC[i,j] <- 1-(a1/N)*(((N-1)*a1)/((N-1)*a1+2*a2))

  }
}
# View(pool$SC)



poolwide <- data.frame(
  sites = pool$mean[, 1],
  S.obs = pool$means[, 2],
  se.obs = apply(pool$S, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.chao = pool$means[, 3],
  se.chao = apply(pool$chao, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  # S.mmfit = apply(pool$mmfit, 1, mean, na.rm = TRUE),
  # se.mmfit = apply(pool$mmfit, 1, sd, na.rm = TRUE),
  # S.mmVm = apply(pool$mmVm, 1, mean, na.rm = TRUE),
  # se.mmVm = apply(pool$mmVm, 1, sd, na.rm = TRUE),
  S.SC = apply(pool$SC, 1, mean, na.rm = TRUE),
  se.SC = apply(pool$SC, 1, sd, na.rm = TRUE)
)

poollong <- poolwide |>
  pivot_longer(
    cols = c(
      S.obs,
      se.obs,
      S.chao,
      se.chao,
      # S.mmfit,
      # se.mmfit,
      # S.mmVm,
      # se.mmVm,
      S.SC,
      se.SC
    ),
    names_to = c(".value", "method"),
    names_sep = "\\."
  ) |>
  arrange(method, sites) |>
  mutate(
    `Species Richness` = case_when(
      method == "obs" ~ "Observed",
      method == "chao" ~ "Chao Estimator",
      method == "mmfit" ~ "Michaelis-Menten Fit",
      method == "mmVm" ~ "Michaelis-Menten Estimate",
      method == "SC" ~ "Sample Coverage"
    )
  )

# scores <- poolwide |>
#   mutate(CV = 100 - se.chao / S.chao * 100, discovered = S.obs / S.chao * 100)

# fit <- fitspecaccum(
#   specaccum(musquashbirds, method = "random"),
#   model = "michaelis-menten"
# )

ggplot(poollong |> filter(`Species Richness` == "Sample Coverage")) +
  geom_line(aes(x = sites, y = S, color = `Species Richness`), lwd = 1) +
  geom_ribbon(
    aes(
      x = sites,
      ymin = S - se,
      ymax = S + se,
      fill = `Species Richness`,
      group = `Species Richness`
    ),
    alpha = 0.2
  ) +
  
  labs(x = "Number of Sites", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.9)) +
  # coord_cartesian(ylim = c(0, 100)) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired")


############# birds

birds <- mmmp_naturecounts_data |>
  filter(
    NAME_E == "Musquash Estuary Marine Protected Area",
    # YearCollected==2014,
    YearCollected > 2012,
    YearCollected < 2018,
    ScientificName != "",
    !is.na(ScientificName)
  ) |>
  group_by(SurveyAreaIdentifier) |>
  mutate(effort = length(unique(SamplingEventIdentifier))) |>
  group_by(SurveyAreaIdentifier, ScientificName, effort) |>
  reframe(n = n()) |>
  pivot_wider(names_from = ScientificName, values_from = n, values_fill = 0) |>
  select(-SurveyAreaIdentifier)


# spacc <- specaccum(birds[,-1])
# plot(spacc)
# spaccdf <- data.frame(spacc[c("sites", "richness", "sd")])
# fit <- fitspecaccum(specaccum(birds[,-1]), model = "michaelis-menten")
# fitdf <- data.frame(fit[c("sites", "richness", "sd")])
# pool <- specpool(birds[,-1])

# ggplot(spaccdf)+
#   # geom_line(aes(x = sites, y = richness), color = "blue") +
#   # geom_ribbon(aes(x = sites, ymin = richness - sd, ymax = richness + sd), alpha = 0.2, fill = "blue")+
#   geom_line(data=fitdf,aes(x = sites, y = richness), color = "red") +
#   geom_ribbon(data=fitdf,aes(x = sites, ymin = richness - sd, ymax = richness + sd), alpha = 0.2, fill = "red") +
#   labs(x = "Number of Sites", y = "Species Richness",
#        title = "Species Accumulation Curve for Musquash Estuary MPA") +
#   theme_minimal()

# data.frame(sites=1:30) |>
#   mutate(richness = coef(fit)[1]/(coef(fit)[2]+ sites))

# poolaccum(birds[, -1]) |>
#   t() |>
#   as.data.frame() |>
#   mutate(sites = 1:n()) |>
#   ggplot() +
#   geom_line(aes(x = sites, y = S.obs), color = "blue") +
#   geom_ribbon(
#     aes(x = sites, ymin = S.obs - S.se, ymax = S.obs + S.se),
#     alpha = 0.2,
#     fill = "blue"
#   ) +
#   geom_line(aes(x = sites, y = S.chao1), color = "red") +
#   geom_ribbon(
#     aes(x = sites, ymin = S.chao1 - se.chao1, ymax = S.chao1 + se.chao1),
#     alpha = 0.2,
#     fill = "red"
#   ) +
#   labs(
#     x = "Number of Sites",
#     y = "Species Richness",
#     title = "Species Richness Estimates for Musquash Estuary MPA"
#   ) +
#   theme_minimal()

# totalsp <- estimateR(BCI[1:10,]) |>
#   t() |>
#   as.data.frame() |>
#   mutate(sites = 1:n())

# ggplot(totalsp)+
#   geom_line(aes(x = sites, y = S.obs), color = "blue") +
#   # geom_ribbon(aes(x = sites, ymin = S.obs - S.se, ymax = S.obs + S.se), alpha = 0.2, fill = "blue")+
#   geom_line(aes(x = sites, y = S.chao1), color = "red") +
#   geom_ribbon(aes(x = sites, ymin = S.chao1 - se.chao1, ymax = S.chao1 + se.chao1), alpha = 0.2, fill = "red") +
#   labs(x = "Number of Sites", y = "Species Richness",
#        title = "Species Richness Estimates for Musquash Estuary MPA") +
#   theme_minimal()

pool <- estaccumR(birds[, -1])

pool$mm <- pool$chao
pool$mm[] <- NA
for (i in 2:nrow(pool$mm)) {
  for (j in 1:ncol(pool$mm)) {
    spacc <- specaccum(
      slice_sample(birds[, -1], n = i),
      permutations = 20,
      method = "random"
    )
    fit <- try(fitspecaccum(spacc, model = "michaelis-menten"))
    if (inherits(fit, "try-error")) {
      next
    } else {
      pool$mm[i, j] <- coef(fit)[1]
    }
  }
}

fit <- fitspecaccum(
  specaccum(
    slice_sample(birds[, -1], n = 10),
    permutations = 1,
    method = "random"
  ),
  model = "michaelis-menten"
)
plot(fit)
View(pool$mm)


poolwide <- data.frame(
  sites = pool$mean[, 1],
  S.obs = pool$means[, 2],
  se.obs = apply(pool$S, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.chao = pool$means[, 3],
  se.chao = apply(pool$chao, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.mm = apply(pool$mm, 1, mean, na.rm = TRUE),
  se.mm = apply(pool$mm, 1, sd, na.rm = TRUE)
)

poollong <- poolwide |>
  pivot_longer(
    cols = c(S.obs, se.obs, S.chao, se.chao, S.mm, se.mm),
    names_to = c(".value", "method"),
    names_sep = "\\."
  ) |>
  arrange(method, sites) |>
  mutate(
    `Species Richness` = case_when(
      method == "obs" ~ "Observed",
      method == "chao" ~ "Chao Estimator",
      method == "mm" ~ "Michaelis-Menten Fit"
    )
  )

# scores <- poolwide |>
#   mutate(CV = 100 - se.chao / S.chao * 100, discovered = S.obs / S.chao * 100)

ggplot(poollong) +
  geom_line(aes(x = sites, y = S, color = `Species Richness`), lwd = 1) +
  geom_ribbon(
    aes(
      x = sites,
      ymin = S - se,
      ymax = S + se,
      fill = `Species Richness`,
      group = `Species Richness`
    ),
    alpha = 0.2
  ) +
  # geom_line(data = scores, aes(x = sites, y = CV), lwd = 1, color = "black") +
  # geom_line(
  #   data = scores,
  #   aes(x = sites, y = discovered),
  #   lwd = 1,
  #   color = "yellow"
  # ) +
  labs(x = "Number of Sites", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.9))


####### rv survey
rv_wide <- rv_data |>
  filter(
    NAME_E ==
      "Western/Emerald Banks Conservation Area (Restricted Fisheries Zone)",
    !is.na(SPEC.GSSPECIES),
    !is.na(LONGITUDE),
    !is.na(LATITUDE),
    YEAR > 2015
  ) |>
  group_by(MISSION, SETNO, SPEC.GSSPECIES) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(
    names_from = SPEC.GSSPECIES,
    values_from = n,
    values_fill = 0
  ) |>
  select(-MISSION, -SETNO)

pool <- estaccumR(rv_wide)

poolwide <- data.frame(
  sites = pool$mean[, 1],
  S.obs = pool$means[, 2],
  se.obs = apply(pool$S, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.chao = pool$means[, 3],
  se.chao = apply(pool$chao, 1, function(x) {
    sd(x, na.rm = TRUE)
  })
)


poollong <- poolwide |>
  pivot_longer(
    cols = c(S.obs, S.chao, se.obs, se.chao),
    names_to = c(".value", "method"),
    names_sep = "\\."
  ) |>
  arrange(method, sites) |>
  mutate(
    `Species Richness` = case_when(
      method == "obs" ~ "Observed",
      method == "chao" ~ "Chao Estimator",
    )
  )

scores <- poolwide |>
  mutate(CV = 100 - se.chao / S.chao * 100, discovered = S.obs / S.chao * 100)

ggplot(poollong) +
  geom_line(aes(x = sites, y = S, color = `Species Richness`), lwd = 1) +
  geom_ribbon(
    aes(
      x = sites,
      ymin = S - se,
      ymax = S + se,
      fill = `Species Richness`,
      group = `Species Richness`
    ),
    alpha = 0.2
  ) +
  # geom_line(data = scores, aes(x = sites, y = CV), lwd = 1, color = "black") +
  # geom_line(
  #   data = scores,
  #   aes(x = sites, y = discovered),
  #   lwd = 1,
  #   color = "yellow"
  # ) +
  labs(x = "Number of Sites", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.9))







test <- iNEXT(musquashbirds |> as.matrix() |> t(), q=0, datatype="abundance")
test$DataInfo
a1=test$DataInfo$f1[1]
a2=test$DataInfo$f2[1]
N=test$DataInfo$n[1]
s0=test$DataInfo$S.obs[1]
s0+(a2^2/(2*a2))*((N-1)/N)


###### Sample coverage

1-(a1/N)*(((N-1)*a1)/((N-1)*a1+2*a2))

pool <- estaccumR(musquashbirds)



pool$SC <- pool$chao
pool$SC[] <- NA
for (i in 1:nrow(pool$SC)) {
  for (j in 1:ncol(pool$SC)) {
        data <- musquashbirds |>
          slice_sample(n=1)|> 
          apply(2,sum,na.rm = TRUE)
    
        a1 <- sum(data==1,na.rm = TRUE)
        a2 <- sum(data==2,na.rm = TRUE)
        N <- sum(data,na.rm = TRUE)
        s0 <- sum(data>0,na.rm = TRUE)
        pool$SC[i,j] <- 1-(a1/N)*(((N-1)*a1)/((N-1)*a1+2*a2))

  }
}


###### Sample coverage

1-(a1/N)*(((N-1)*a1)/((N-1)*a1+2*a2))

      pool$mmVm[i, j] <- mean(coef(fit)[1])
      pool$mmfit[i, j] <- tail(fit$richness, 1)
  }
}


########### infauna

tar_load("data_musquash_benthic_infauna")
infauna <- data_musquash_benthic_infauna |>
  as.data.frame() |>
  filter(
    !is.na(scientificName_Nom_scientifique),
    grepl(" ", scientificName_Nom_scientifique)
  ) |>
  group_by(set_id, scientificNameID_Identifiant_du_nom_scientifique) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(
    names_from = scientificNameID_Identifiant_du_nom_scientifique,
    values_from = n,
    values_fill = 0
  ) |>
  select(-set_id)

pool <- estaccumR(infauna)

pool$mmfit <- pool$chao
pool$mmfit[] <- NA
pool$mmVm <- pool$mmfit
for (i in 2:nrow(pool$mmfit)) {
  for (j in 1:ncol(pool$mmfit)) {
    # for (j in 1:10) {

    fit <- try(R.utils::withTimeout(
      {
        spacc <- specaccum(
          slice_sample(infauna, n = i),
          permutations = 1,
          method = "random"
        )
        fitspecaccum(spacc, model = "michaelis-menten")
      },
      timeout = 15,
      onTimeout = "error"
    ))
    if (inherits(fit, "try-error")) {
      next
    } else {
      # pool$mmVm[i,j] <- mean(coef(fit)[1,])
      # pool$mmfit[i,j] <- tail(fit$richness,1)
      pool$mmVm[i, j] <- mean(coef(fit)[1])
      pool$mmfit[i, j] <- tail(fit$richness, 1)
    }
  }
}

# fit <- fitspecaccum(specaccum(slice_sample(birds[, -1],n=10),permutations = 1, method = "random"), model = "michaelis-menten");plot(fit)
# View(pool$mm)

poolwide <- data.frame(
  sites = pool$mean[, 1],
  S.obs = pool$means[, 2],
  se.obs = apply(pool$S, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.chao = pool$means[, 3],
  se.chao = apply(pool$chao, 1, function(x) {
    sd(x, na.rm = TRUE)
  }),
  S.mmfit = apply(pool$mmfit, 1, mean, na.rm = TRUE),
  se.mmfit = apply(pool$mmfit, 1, sd, na.rm = TRUE),
  S.mmVm = apply(pool$mmVm, 1, mean, na.rm = TRUE),
  se.mmVm = apply(pool$mmVm, 1, sd, na.rm = TRUE)
)

poollong <- poolwide |>
  pivot_longer(
    cols = c(
      S.obs,
      se.obs,
      S.chao,
      se.chao,
      S.mmfit,
      se.mmfit,
      S.mmVm,
      se.mmVm
    ),
    names_to = c(".value", "method"),
    names_sep = "\\."
  ) |>
  arrange(method, sites) |>
  mutate(
    `Species Richness` = case_when(
      method == "obs" ~ "Observed",
      method == "chao" ~ "Chao Estimator",
      method == "mmfit" ~ "Michaelis-Menten Fit",
      method == "mmVm" ~ "Michaelis-Menten Estimate"
    )
  )

scores <- poolwide |>
  mutate(CV = 100 - se.chao / S.chao * 100,
    discovered = S.obs / S.chao * 100,
  slope = S.obs-lag(S.obs,n=1)
  )

ggplot(poollong) +
  geom_line(aes(x = sites, y = S, color = `Species Richness`), lwd = 1) +
  geom_ribbon(
    aes(
      x = sites,
      ymin = S - se,
      ymax = S + se,
      fill = `Species Richness`,
      group = `Species Richness`
    ),
    alpha = 0.2
  ) +
  geom_line(data = scores, aes(x = sites, y = CV), lwd = 1, color = "black") +
  geom_line(
    data = scores,
    aes(x = sites, y = slope),
    lwd = 1,
    color = "yellow"
  ) +
  labs(x = "Number of Sites", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.9)) +
  coord_cartesian(ylim = c(0, 300)) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired")



coverage <- musquashbirds |> 
  mutate(across(everything(), ~cummin(as.numeric(. > 0)))) |> 
  rowMeans(na.rm = TRUE)
plot(coverage, type = "l", xlab = "Number of Sites", ylab = "Coverage",
     main = "Coverage of Species Richness in Musquash Estuary MPA")
# Load Paracou data (number of trees per species in two 1-ha plot of a tropical forest)
data(Paracou618)
# Ns is the vector of abundances of the metacommunity
Ns <- Paracou618.MC$N
# Calculate the sample coverage of the metacommunity
Coverage(Ns)    # Stored in Paracou618.SampleCoverage
test