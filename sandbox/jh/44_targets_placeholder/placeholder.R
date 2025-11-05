list(
tar_target(control_polygons,
           command= {
             MPAs
             cp <- MPAs |>
               filter(NAME_E!="Non_Conservation_Area") |>
               rowwise() |>
               mutate(geoms = st_difference(st_buffer(geoms,20000),geoms))|>
               mutate(forty_km = st_difference(st_buffer(geoms,40000),geoms)) |>
               mutate(sixty_km = st_difference(st_buffer(geoms,60000),geoms)) |>
               mutate(eighty_km = st_difference(st_buffer(geoms,80000),geoms)) |>
               mutate(hundred_km = st_difference(st_buffer(geoms,100000),geoms))

           }),

tar_target(ind_temperature_inside_outside,
           command={
             control_polygons
             MPAs
             data <- data_azmp_Discrete_Occupations_Sections  |>
               dplyr::select(longitude, latitude, year, depth, temperature)

             x <- process_indicator(data = data,
                                    indicator_var_name = "temperature",
                                    indicator = "Temperature Inside Outside Comparison",
                                    type = "Discrete Occupations Sections",
                                    units = "C",
                                    scoring = "control site linear trend: less inside",
                                    PPTID = 579,
                                    source="AZMP",
                                    project_short_title = "AZMP",
                                    climate = TRUE,
                                    other_nest_variables="depth",
                                    areas = MPAs,
                                    climate_expectation="FIXME",
                                    indicator_rationale="Changes in temperature influence not only the distribution of species associated with particular water masses (e.g., Alvarez Perez and Santana 2022), but also affect growth and development rates, generation times and productivity of all species (e.g., Shoji et al. 2011; Szuwalski et al. 2021; Millington et al. 2022).",
                                    bin_rationale="FIXME",
                                    plot_type=c('outside-comparison','map'),
                                    control_polygon = control_polygons,
                                    plot_lm=FALSE,
                                    objectives = c(
                                      "Maintain/promote ecosystem structure and functioning",
                                      "Maintain Ecosystem Resistance",
                                      "Help maintain ecosystem structure, functioning and resilience (including resilience to climate change)"
                                    ))
             x
           })
)
