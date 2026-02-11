


# TEST
source("inst/set_up.R"); source("inst/raw_data.R"); source("inst/indicators.R"); source("inst/frameworks.R")


list(
  raw_data_targets,
  indicator_targets,
  framework_targets,

  tar_target(name = Ecological,
             command = {
               data.frame(grouping=rep(c("Biodiversity",
                                         "Habitat",
                                         "Productivity"),
                                       times=c(3,5,3)),
                          labels=c("Genetic Diversity",
                                   "Species Diversity",
                                   "Functional Diversity",

                                   "Environmental Representativity",
                                   "Key Fish Habitat",
                                   "Connectivity",
                                   "Uniqueness",
                                   "Threats to Habitat",
                                   "Biomass Metrics",
                                   "Structure and Function",
                                   "Threats to Productivity")) |>
                 mutate(weight=runif(11,1,10),
                        angle=(cumsum(weight)-weight/2)/sum(weight)*360)
             }),

  tar_target(name = APPTABS,
             command = {
               apptabs <- expand.grid(flower=unique(c(Ecological$grouping, Ecological$labels)),
                                      place=unique(c(MPAs$region,MPAs$NAME_E))) |>
                 mutate(tab = paste0("tab_", 1:length(flower)),
                        link = paste0("link_", 1:length(flower)))
               home <- data.frame(flower="home", place="home", tab="tab_0", link="link_0")
               rbind(home, apptabs)
             }),

  tar_render_rep(
    name = mpa_report,
    path = file.path("data", "report.Rmd"),
    params = tibble(mpas = unique(objective_tabs$area[objective_tabs$area %in% MPAs$NAME_E])) |>
      mutate(output_file = paste0(make.names(mpas), ".html")),
    output_dir = file.path(dirname(path_to_store()), "data", "reports"),
    quiet = TRUE
  ),

  tar_render(
    name = network_report,
    path = file.path("data","network_report.Rmd"),
    output_dir = file.path(dirname(path_to_store()), "data", "reports"),
    output_file = "Maritimes.html",
    params = list(mpas = "Maritimes"),
    quiet = TRUE
  ),

  tar_target(name = flowerPalette,
             command = {
               grades <- c("A", "B", "C", "D", "F")
               palette <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(length(grades)))
               names(palette) <- grades
               palette
             }),

  tar_target(name=indicatorFlower,
             command = {
               palette <- flowerPalette
               names(palette) <- c(100,75,50,25,0)
               palette

             }
  ),

  tar_target(objective_tabs,
             # FOR TABS, THERE ARE APPTABS, WHICH ARE FOR FLOWER, PILLAR_ECOL_DF WHICH ARE FOR INDICATORS, AND OBJECTIVE_TABS
             # WHICH ARE FOR OBJECTIVES
             command={
               ped <- pillar_ecol_df
               ot <- data.frame(objectives=objectives_df$Objective, tab=NA, area=objectives_df$Framework)

               start <- max(sort(as.numeric(sub(".*_", "", ped$tab))))+1
               end <- start+(length(ot$objectives)-1)
               tabs <- start:end
               ot$tab <- paste0("tab_", tabs)
               ot$link <- paste0("link_", tabs)
               ot

             }
  ),

  # Process each objective separately
  tar_target(biodiversity_geoms, {
    ecol_obj_biodiversity_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source, scale,
                    climate_expectation, indicator_rationale, bin_rationale, indicator) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  tar_target(habitat_geoms, {
    ecol_obj_habitat_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source, scale,
                    climate_expectation, indicator_rationale, bin_rationale, indicator) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  tar_target(productivity_geoms, {
    ecol_obj_productivity_df |>
      filter(!map_lgl(data, is.null)) |>
      mutate(data = map(data, process_geom_data)) |>
      dplyr::select(data, type, project_short_title, PPTID, areaID, source, scale,
                    climate_expectation, indicator_rationale, bin_rationale, indicator) |>
      unnest(cols = data) |>
      st_as_sf()
  }),

  tar_target(all_indicator_project_geoms, {
    x <- rbind(biodiversity_geoms, habitat_geoms, productivity_geoms)|>
      transform(gtype = as.character(st_geometry_type(geometry))) |>
      (\(x) {
        rbind(
          x[x$gtype == "MULTIPOINT", ] |> st_cast("POINT"),
          x[x$gtype == "MULTILINESTRING", ] |> st_cast("LINESTRING"),
          x[x$gtype == "MULTIPOLYGON", ] |> st_cast("POLYGON"),
          x[!grepl("^MULTI", x$gtype), ]
        )
      })() |>
      subset(select = -gtype)
    x

  }),

  tar_target(all_project_geoms, {
    all_indicator_project_geoms |>
      group_by(project_short_title, areaID, PPTID,source,geometry)|>
      summarise(type = paste(type,";;"),
                scale = paste(scale, ";;")) |>
      st_make_valid() |>
      distinct()

  }),

  tar_target(labels,
             command={
               distinct_rows <- unique(all_project_geoms[c("project_short_title", "PPTID", "source")])
               unique(paste0(distinct_rows$project_short_title, " (", ifelse(is.na(distinct_rows$PPTID),distinct_rows$source,distinct_rows$PPTID), ")"))
             }),

  tar_target(map_palette,
             command={
               data.frame("Project"=labels, "Color"= palette <- viridis::viridis(length(labels)))
             }),


  tar_target(name = MPA_report_card,
             command = {

               mrc <- left_join(MPAs,pillar_ecol_df |>
                                  filter(indicator %in% MPAs$NAME_E,
                                         areaID != "Non_Conservation_Area") |>
                                  calc_group_score(grouping_var = "indicator") |>
                                  mutate(grade = if_else(is.nan(score),
                                                         NA,
                                                         calc_letter_grade(score))),
                                by=c("NAME_E"="indicator"))

               mrc

             }),


  tar_target(cost_of_mpas,

             # This looks at the number of samples that were collected in our source dataset.
             # A 'sample' is considered a unique lat/lon and year. If multiple samples were
             # taken at the exact same location in the same year, this would only show up once.
             # Additionally, if there is no 'time' column, a 'sample' is simply considered each
             # unique lat and lon. This approach also assumes that we have pulled all data.
             # This should be considered when considering data sets such as AZMP.
             # It takes the number of samples overall and compares it to the number of samples in the MPA

             # Note that we still don't have any pricing information for 'polygon' type sampling.
             # This is because all of our indicators like this use external open data.

             command={
               project_costs <- om |>
                 group_by(project_id) |>
                 reframe(totalamount = sum(amount))

               all_project_geoms_single_obs_per_row <- all_indicator_project_geoms |>
                 select(PPTID,areaID,geometry) |>
                 filter(!is.na(PPTID),
                        areaID!="Non_Conservation_Area") |>
                 distinct()



               project_samples_total <- all_project_geoms_single_obs_per_row |>
                 group_by(PPTID) |>
                 reframe(totalsites=n()) |>
                 left_join(project_costs,by = c("PPTID"="project_id")) |>
                 mutate(price_per_station = totalamount/totalsites)

               # cost_of_mpas
               all_project_geoms_single_obs_per_row |>
                 group_by(PPTID,areaID ) |>
                 reframe(sites=n()) |>
                 tidyr::complete(PPTID, areaID = MPAs$NAME_E[MPAs$NAME_E!="Non_Conservation_Area"],fill=list(sites = 0)) |>
                 left_join(project_samples_total,by="PPTID") |>
                 mutate(percent_sites_in_mpa = sites/totalsites) |>
                 rename(area=areaID,
                        project_id=PPTID) |>
                 dplyr::select(project_id, area, percent_sites_in_mpa, price_per_station)

             }
  ),



  tar_target(name = upload_all_data_to_shiny,
             command = {

               if(Sys.getenv("USERPROFILE")=="C:\\Users\\DaigleR"){
                 serveruser="rdaigle"
               } else if(Sys.getenv("USERPROFILE")=="C:\\Users\\HarbinJ"){
                 serveruser="jharbin"
               } else {
                 return(TRUE)
               }

               # Upload the targets folder, and only certain objects needed by the app
               upload_objects <- c("APPTABS", "pillar_ecol_df", "all_project_geoms", "MPA_report_card",
                                   "MPAs", "regions", "flowerPalette", "indicatorFlower",
                                   "N_Objectives", "om", "Ecological", "Context", "collaborations",
                                   "deliverables", "csas", "climate_change", "cost_of_mpas","salary",
                                   "theme_table", "objective_tabs", "objective_indicators",
                                   "map_palette","labels")


               # Add all targets folder subdirectories
               subdirs <- list.dirs(path_to_store(), full.names = TRUE, recursive = FALSE)


               system(paste0(
                 'scp -r -i ',
                 file.path(Sys.getenv("USERPROFILE"), '.ssh', 'id_rsa'),
                 ' -P 22 ',
                 paste(shQuote(subdirs[!grepl("objects",subdirs)]), collapse = ' '),
                 ' "', serveruser, '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/app_targets" '
               ))

               # Add specific files from objects subdirectory
               system(paste0('ssh -i ',
                             file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                             ' -p 22 ',
                             serveruser,
                             '@mar-spa.ent.dfo-mpo.ca "mkdir -p /home/rdaigle/MarConsNetTargets/app_targets/objects"'))


               object_files <- file.path(path_to_store(), "objects", upload_objects)

               system(paste0(
                 'scp -i ',
                 file.path(Sys.getenv("USERPROFILE"), '.ssh', 'id_rsa'),
                 ' -P 22 ',
                 paste(shQuote(object_files), collapse = ' '),
                 ' "', serveruser, '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/app_targets/objects/" '
               ))

               # Create and upload data folder
               system(paste0('ssh -i ',
                             file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                             ' -p 22 ',
                             serveruser,
                             '@mar-spa.ent.dfo-mpo.ca "mkdir -p /home/rdaigle/MarConsNetTargets/data"'))


               # copy data files
               datafiles <- list.files(file.path(dirname(path_to_store()),'data'),include.dirs = FALSE)
               datadirs <- list.dirs(file.path(dirname(path_to_store()),'data'),recursive = FALSE, full.names = FALSE)
               for(f in datafiles[!(datafiles %in% datadirs)]){
                 system(paste0('scp -i ',
                               file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                               ' -P 22 "',
                               file.path(dirname(path_to_store()),'data',f),
                               '" "',
                               serveruser,
                               '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
               }


               # copy data directories
               for(f in datadirs[datadirs %in% c("reports","plot")]){
                 system(paste0('scp -r -i ',
                               file.path(Sys.getenv("USERPROFILE"),'.ssh','id_rsa'),
                               ' -P 22 "',
                               file.path(dirname(path_to_store()),'data',f),
                               '" "',
                               serveruser,
                               '@mar-spa.ent.dfo-mpo.ca:/home/rdaigle/MarConsNetTargets/data/'))
               }





               TRUE
             },
             deployment = "main",
             priority = 0,
             cue = tar_cue(mode = "always")),


  tar_target(name = N_Objectives,
             command = {

               STORE <- path_to_store()
               obj <- paste0(dirname(STORE),"/data/objectives.xlsx")
               obj_excel <-read_excel(obj)

               NO <- obj_excel$Objective[obj_excel$Framework == "Maritimes"]

               #NO <- data_objectives(type="network")

               no <- vector(mode="list", length(NO))
               for (i in seq_along(NO)) {
                 O <- NO[[i]]

                 NO[[i]] <- newLine(O)
               }
               paste0("-", unlist(NO), "\n")
             }),

  tar_target(name = Context,
             command = {
               c <- lapply(MPAs$NAME_E, function(x) data_context(type="site", area=x))
               names(c) <- MPAs$NAME_E
               c <- c[which(unname(unlist(lapply(c, function(x) !(is.null(x))))))]
               c
             }),


  tar_target(windowsdependenttargets,
             command = {
               rv_data
               rv_data_det
               om
               csas
               deliverables
               collaborations
               salary
               return(TRUE)
             },
             deployment = "worker")


) |>
  unlist(recursive = FALSE)
