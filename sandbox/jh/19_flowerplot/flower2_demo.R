library(dplyr)
library(tidyr)

# OLD pillar_ecol_df

indicatorbins <- pdf
plot_flowerplot(indicatorbins, title="OLD pillar_ecol_df (this workds in app)", grouping = "objective", labels = "bin", score = "ind_status",)

# COMPARE MY DF (after it's been expanded)!!
# The unique objectives are the same

length(unique(pdf$objective))

length(unique(pillar_ecol_df$objective))

# The bins are the same
length(unique(pdf$bin))
length(unique(pillar_ecol_df$bin)) # We are missing Threats to productivity


# Doing proper fix to pillar_ecol_df (without removing those that fit into two bins)
df <- data.frame(area_name=area_name, ind_name=ind_name, ind_status=ind_status, ind_trend=ind_trend, ind_projects=ind_projects,
                 ind_rawdata_type=ind_rawdata_type, ind_certainty=ind_certainty, bin=bin, weight=weight, objective=objective,
                 pillar=pillar)
df$bin <- trimws(toupper(df$bin), "both")
df$objective <- trimws(toupper(df$objective), "both")


df <- df %>%
  mutate(bin = strsplit(as.character(bin), "; ")) %>%
  unnest(bin)

for (i in seq_along(df$objective)) {
  message(i)
  df$objective[i] <- Ecological$grouping[which(tolower(Ecological$labels) == trimws(tolower(df$bin[i]), "both"))]
}






df$objective <- toTitleCase(tolower(df$objective))
df$bin <- toTitleCase(tolower(df$bin))

length(unique(pdf$bin))
length(unique(df$bin)) # We are missing Threats to productivity

# Now the bins and objectives match
plot_flowerplot(df, title="NEW pillar_ecol_df", grouping = "objective", labels = "bin", score = "ind_status",)

weight <- weight



target_weight <- sum(df$weight)/length(Ecological$labels)


W <- NULL
# after expansion
for (i in seq_along(Ecological$labels)) {
  keep <-which(tolower(df$bin) == tolower(Ecological$labels[i]))
  bl <- target_weight/sum(df$weight[keep])
  df$weight[keep] <- df$weight[keep]*bl

}
