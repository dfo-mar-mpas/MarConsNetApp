tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate", "cost_of_mpas", "salary"))

# NEXT
pillar_ecol_df <- pillar_ecol_df[-which(is.na(pillar_ecol_df$objectives)),]
obj <- list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE)[which(grepl("objectives.xlsx",list.files(file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data"), full.names = TRUE) ))]
x <-read_excel(obj)
x$Objective <- sub("\\.$", "", x$Objective)
x$Objective <- sub("\\;$", "", x$Objective)

indicator_objectives <- trimws(unique(unlist(strsplit(pillar_ecol_df$objectives, ";;;"))), 'both')
indicator_objectives <- indicator_objectives[-which(indicator_objectives == "NA")]


ped_objectives <- vector("list", length=length(indicator_objectives))

for (i in seq_along(ped_objectives)) {
  message(i)
  keep <- which(grepl(indicator_objectives[i], pillar_ecol_df$objectives, fixed = TRUE))
  if (x$Framework[which(x$Objective == indicator_objectives[i])] %in% MPAs$NAME_E) {
    keep2 <- which(pillar_ecol_df$areaID == x$Framework[which(x$Objective == indicator_objectives[i])])
    keep <- intersect(keep, keep2)
  }

  ped_objectives[[i]] <- pillar_ecol_df[keep, ]

}
names(ped_objectives) <- indicator_objectives

# Adding objectives that aren't yet accounted for

new_objectives <- x$Objective[which(!(x$Objective %in% names(ped_objectives)))]

for (obj_name in new_objectives) {
  # create a blank NA df with the same columns
  na_df <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(pillar_ecol_df)))
  names(na_df) <- names(pillar_ecol_df)

  # add it to the list with the name
  ped_objectives[[obj_name]] <- na_df
}


objective_indicators <- ped_objectives

# NEXT
tar_load(pillar_ecol_df)
ot <- data.frame(objectives=names(objective_indicators), tab=NA)

max(sort(as.numeric(sub(".*_", "", pillar_ecol_df$tab))))
start <- max(sort(as.numeric(sub(".*_", "", pillar_ecol_df$tab))))+1
end <- start+(length(ot$objectives)-1)
tabs <- start:end
ot$tab <- paste0("tab_", tabs)

objective_tabs <- ot

# NEXT
tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate", "cost_of_mpas", "salary"))

# NEXT
tar_load(c("ind_nitrate", "ind_silicate", "ind_phosphate", "ind_chlorophyll", "ind_bloom_amplitude", "ind_bloom_timing"))
x <- rbind(
  ind_nitrate[ , setdiff(names(ind_nitrate), c("data", "plot"))],
  ind_silicate[ , setdiff(names(ind_silicate), c("data", "plot"))],
  ind_phosphate[ , setdiff(names(ind_phosphate), c("data", "plot"))],
  ind_chlorophyll[ , setdiff(names(ind_chlorophyll), c("data", "plot"))],
  ind_bloom_amplitude[ , setdiff(names(ind_bloom_amplitude), c("data", "plot"))],
  ind_bloom_timing[ , setdiff(names(ind_bloom_timing), c("data", "plot"))]
)
x$theme <- "Primary Production"
x$weight <- NA
for (i in seq_along(x$areaID)) {
  X <- x[i,]
  keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
  x$weight[i] <- pillar_ecol_df$weight[keep]
}
theme_primary_production <- x

# NEXT
tar_load(c("ind_zooplankton", "ind_zooplankton_community_composition"))

x <- rbind(
  ind_zooplankton[ , setdiff(names(ind_zooplankton), c("data", "plot"))],
  ind_zooplankton_community_composition[ , setdiff(names(ind_zooplankton_community_composition), c("data", "plot"))]
)
x$theme <- "Secondary Production"

x$weight <- NA
for (i in seq_along(x$areaID)) {
  message(i)
  X <- x[i,]
  keep <- which(pillar_ecol_df$areaID == X$areaID & pillar_ecol_df$indicator == X$indicator)
  x$weight[i] <- pillar_ecol_df$weight[keep]
}
theme_secondary_production <- x

# NEXT

theme_trophic_structure_and_function <- data.frame(
               areaID = NA,
               indicator = NA,
               type = NA,
               units = NA,
               scoring = NA,
               PPTID = NA,
               project_short_title = NA,
               climate = NA,
               design_target = NA,
               score = NA,
               status_statement = NA,
               trend_statement = NA,
               source = NA,
               climate_expectation = NA,
               indicator_rationale = NA,
               objectives = NA,
               bin_rationale = NA,
               theme="Trophic Structure and Function",
               weight=NA
             )

theme_table <- rbind(theme_primary_production, theme_secondary_production, theme_trophic_structure_and_function)
tar_load(c("APPTABS","pillar_ecol_df","all_project_geoms","MPA_report_card","MPAs","areas","regions","odf","flowerPalette","indicatorFlower","Objectives_processed","N_Objectives","om","Ecological", "Context", "collaborations", "deliverables", "csas", "climate", "cost_of_mpas", "salary"))

source("R/app.R"); app()

