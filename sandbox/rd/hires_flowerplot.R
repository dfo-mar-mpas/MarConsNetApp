require(targets)
require(ggplot2)
require(MarConsNetAnalysis)
require(dplyr)

tar_load(pillar_ecol_df)

webmrdata <- pillar_ecol_df |>
  filter(areaID=="Western and Emerald Banks Marine Refuge")
webmrdata$score[grepl(pattern = "Network design",webmrdata$indicator)] <- NA

webmrdata$score[webmrdata$indicator=="Nutrient Conditions (Nitrate)"] <- 100


p <- plot_flowerplot(webmrdata,
                grouping = "objective",
                labels = "bin",
                score = "score",
                max_score=100,
                min_score=0,
                title=" ")
p
ggsave("flowerplot_pillar_ecol_WEbanks.png",width=6,height=6,dpi=600)


#French
key <- data.frame(
  english = c(
    "Habitat",
    "Productivity",
    "Biodiversity",
    "Environmental Representativity",
    "Biomass Metrics",
    "Connectivity",
    "Species Diversity",
    "Key Fish Habitat",
    "Functional Diversity",
    "Uniqueness",
    "Threats to Habitat",
    "Structure and Function",
    "Genetic Diversity",
    "Threats to Productivity"
  ),
  french = c(
    "Habitat",
    "Productivité",
    "Biodiversité",
    "Représentativité environnementale",
    "Mesures de la biomasse",
    "Connectivité",
    "Diversité des espèces",
    "Habitat clé pour les poissons",
    "Diversité fonctionnelle",
    "Unicité",
    "Menaces aux habitats",
    "Structure et fonction",
    "Diversité génétique",
    "Menaces à la productivité"
  )
)

webmrdatafr <- webmrdata |>
  mutate(across(
    .cols = c(bin, objective),
    .fns = ~key$french[match(., key$english)],
    .names = "{.col}_fr"
  )) |>
  mutate(
    objective_fr = factor(objective_fr,
                          levels = c("Biodiversité", "Habitat", "Productivité")),
    bin_fr = factor(bin_fr,
                    levels = key$french[c(9, 13, 7, 6, 4, 8, 11, 10, 5, 12, 14)])
  )

p <- plot_flowerplot(webmrdatafr,
                     grouping = "objective_fr",
                     labels = "bin_fr",
                     score = "score",
                     max_score=100,
                     min_score=0,
                     title=" ")
p

ggsave("flowerplot_pillar_ecol_WEbanks_fr.png",width=6,height=6,dpi=600)
