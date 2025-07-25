# get workflow
workflow <- tar_glimpse(names = c("fish_weight_per_1.75kn_tow"), script = "inst/_targets.R")

# get relevant targets
relevant_targets <- workflow$x$nodes$name

# get manifest
manifest <- tar_manifest(script = "inst/_targets.R") |>
  filter(name %in% relevant_targets) |>
  # the rest of this pipe is just to make the code  in the table look nice
  rowwise() |>
  mutate(cleancommand = if_else(startsWith(command, "{"),
                           paste(strsplit(command, "\n")[[1]][-c(1, length(strsplit(command, "\n")[[1]]))], collapse="\n"),
                           command))
# print command for target
manifest$command[manifest$name=="fish_weight_per_1.75kn_tow"]

# or as a rmd code chunk
knitr::asis_output(paste("```r\n", unlist(manifest$command[manifest$name=="fish_weight_per_1.75kn_tow"]), "\n```", sep=""))
