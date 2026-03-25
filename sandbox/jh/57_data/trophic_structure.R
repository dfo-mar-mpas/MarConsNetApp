library(dplyr)
df <- rv_data
# 1️⃣ Define size threshold for small vs large (adjust as needed)
size_threshold <- 30  # e.g., 30 cm

# 2️⃣ Filter for demersal species only
demersal <- df %>%
  filter(XTYPE == "Demersal" | XTYPEDESC == "Demersal")

# 3️⃣ Classify fish as small or large
demersal <- demersal %>%
  mutate(size_class = ifelse(FLEN > size_threshold, "Large", "Small"))

# 4️⃣ Summarize counts or biomass per area / region
# Example using NAFO_ZONE; can also use LAT/LONG for spatial aggregation
proportion_summary <- demersal %>%
  group_by(NAFO_ZONE, size_class) %>%
  summarize(
    n_fish = sum(TOTNO, na.rm=TRUE),           # number of individuals
    biomass = sum(FWT, na.rm=TRUE)             # total biomass
  ) %>%
  ungroup() %>%
  # calculate proportion of large/small fish per NAFO_ZONE
  group_by(NAFO_ZONE) %>%
  mutate(
    prop_count = n_fish / sum(n_fish),
    prop_biomass = biomass / sum(biomass)
  ) %>%
  ungroup()

# 5️⃣ Inspect
proportion_summary
