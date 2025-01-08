library(dplyr)
library(tidyr)

indicatorbins <- data.frame(objective=rep(c("Biodiversity",
                                           "Habitat; Biodiversity",
                                           "Productivity"),
                                         times=c(3,5,3)),
                            bin=c("Genetic Diversity",
                                     "Species Diversity; Connectivity",
                                     "Functional Diversity",

                                     "Environmental Representativity",
                                     "Key Fish Habitat",
                                     "Connectivity",
                                     "Uniqueness",
                                     "Threats to Habitat",

                                     "Biomass Metrics",
                                     "Structure and Function",
                                     "Threats to Productivity"),
                            ind_status=runif(11,55,100),
                            weight=1,
                            area_name = "Random Example MPA")
plot_flowerplot(indicatorbins, title="Normal", grouping = "objective", labels = "bin", score = "ind_status",)

# EXPAND
expanded_df <- indicatorbins %>%
  separate_rows(objective, sep = ";\\s*") %>% # Split 'grouping' by ';'
  separate_rows(bin, sep = ";\\s*")      # Split 'labels' by ';'
plot_flowerplot(expanded_df, grouping = "objective",
                labels = "bin",
                score = "ind_status",title="Expanded") # Problem ( not unique bins)


# THE PROBLEM IT'S WEIGHTS. IT HAS TO INSTEAD DO WITH UNIQUE LABELS BEING PRINTED OUT.
# PILLAR_ECOL_DF HAS NON UNIQUE LABLES, BUT IT WORKS, LET'S DUPLICATE THAT.

plot_flowerplot(pillar_ecol_df,
                grouping = "objective",
                labels = "bin",
                score = "ind_status",
                title="pillar_ecol_df")

# COMPARE MY DF (after it's been expanded)!!
# The unique objectives are the same
# The bins are the same
# Weights are different, but if I change all df weights to 1 I still get a problematic plot
df$weight <- 1
plot_flowerplot(df,
                grouping = "objective",
                labels = "bin",
                score = "ind_status",
                title="pillar_ecol_df")

# Maybe it is that pillar_ecol_df doesn't have any indicators that fall under 2 objectives
ind <- unique(pillar_ecol_df$ind_name)
for (i in seq_along(ind)) {
  k <- pillar_ecol_df[which(pillar_ecol_df$ind_name == ind[i]),]
  message(paste0(unique(k$objective), " for ", i))
}

ind2 <- unique(df$ind_name)
for (i in seq_along(ind2)) {
  k <- df[which(df$ind_name == ind2[i]),]
  message(paste0(unique(k$objective), " for ", i))
}

# Go back and remove multiple objective from df (and then expand)
df <- data.frame(area_name=area_name, ind_name=ind_name, ind_status=ind_status, ind_trend=ind_trend, ind_projects=ind_projects,
                 ind_rawdata_type=ind_rawdata_type, ind_certainty=ind_certainty, bin=bin, weight=weight, objective=objective,
                 pillar=pillar)
df$bin <- trimws(toupper(df$bin), "both")
df$objective <- trimws(toupper(df$objective), "both")

df <- df[- which(grepl(";", df$objective)),] #TEST


df<- df %>%
  mutate(
    bin = strsplit(as.character(bin), ";"),
    objective = strsplit(as.character(objective), ";")
  ) %>%
  unnest(bin) %>%  # Expand `bin` into rows
  unnest(objective) %>%  # Expand `objective` into rows
  mutate(
    bin = trimws(bin),  # Remove leading/trailing whitespace
    objective = trimws(objective)
  )

ind2 <- unique(df$ind_name)
for (i in seq_along(ind2)) {
  k <- df[which(df$ind_name == ind2[i]),]
  message(paste0(unique(k$objective), " for ", i))
}

plot_flowerplot(df,
                grouping = "objective",
                labels = "bin",
                score = "ind_status",
                title="remove duplicate bin")





