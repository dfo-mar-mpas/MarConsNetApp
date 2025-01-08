area_name <- binned_indicators$mpa_name

ind_name <- indicator_to_plot$indicators

ind_status <- NULL
for (i in seq_along(indicator_to_plot$status_grade)) {
  if (indicator_to_plot$status_grade[i] == "A") {
    ind_status[[i]] <- 5
  } else if (indicator_to_plot$status_grade[i] == "B") {
    ind_status[[i]] <- 4
  } else if (indicator_to_plot$status_grade[i] == "C") {
    ind_status[[i]] <- 3
  } else if (indicator_to_plot$status_grade[i] == "D") {
    ind_status[[i]] <- 2
  } else if (indicator_to_plot$status_grade[i] == "F") {
    ind_status[[i]] <- 1
  } else {
    ind_status[[i]] <- 0
  }
}
ind_status <- unlist(ind_status)


extract_first_number <- function(sentence) {
  match <- regexpr("-?\\d+\\.?\\d*", sentence)
  if (match != -1) {
    as.numeric(regmatches(sentence, match))
  } else {
    NA
  }
}

ind_trend <- NULL
for (i in seq_along(indicator_to_plot$trend)) {
  if (grepl("BLANK", indicator_to_plot$trend[i])) {
    ind_trend[i] <- NA
  } else {
    ind_trend[i] <- extract_first_number(indicator_to_plot$trend[i])

  }
}
ind_trend <- unlist(lapply(indicator_to_plot$trend, function(x) extract_first_number(x)))

ind_projects <- indicator_to_plot$project

ind_rawdata_type <- "Expert opinion"

ind_certainty <- "certain"

bin <- binned_indicators$indicator_bin #FIXME

weight <- NULL

Ecological$labels[which(Ecological$labels == "Environmental Representativity")] <- "Environmental (Representativity)"

objectives <- list()  # Initialize as a list
for (i in seq_along(indicator_to_plot$indicator_bin)) {
  II <- indicator_to_plot$indicator_bin[i]
  sp <- trimws(strsplit(II, ";")[[1]], "both")
  weight[i] <- 1/length(sp)
  objectives[[i]] <- vector("list", length(sp))  # Initialize objectives[[i]] as a list with the correct length
  for (j in seq_along(sp)) {
    message("i =", i, " j = ", j)
    objectives[[i]][[j]] <- Ecological$grouping[which(tolower(Ecological$labels) == tolower(sp[j]))]
  }
}

objectives <- lapply(objectives, function(x) unlist(x))
objectives <- lapply(objectives, unique)
objective <- unlist(lapply(objectives, function(x) paste0(x, collapse=" ; ")))


pillar <- "Ecological"



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

plot_flowerplot(df,
                grouping = "objective",
                labels = "bin",
                score = "ind_status",
                max_score=5,
                min_score=0,
                title="Test problem")

# CONCLUSION: The problem is when a certain indicator not only falls into multiple bins,
# but then also goes into multiple pillars

