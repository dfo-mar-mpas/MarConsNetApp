tar_target(name = indicator_to_plot,
           command = {
             species <- c("COD(ATLANTIC)", "HADDOCK")

             ITP <- data.frame(indicator=unique(plotindy), plot=rep(0), type="plot", status=rep(0), trend=rep(0))
             ITP$plot[which(ITP$indicator == "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock")] <- "plot_rv_abundance(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]])"


             #JAIM HERE. FIX BELOW LINE AND indicatorStatus on app.
             ITP$plot[which(ITP$indicator == "Physical conditions (e.g. temperature, salinity, light levels, wind, sea-surface height) within the MR (surface and bottom) and both upstream and downstream, as measured on the AZMP’s line.")] <- "plot_azmp_physical()"


             for (i in seq_along(ITP$indicator)) {
               message('i = ', i, " or ", ITP$indicator[i])
               TREND <- "A linear regression has shown a XX  of YY UU over the last ZZ years. The linear trend for the last 5 years was a TID of LR UU."
               STATUS <- "The most recent year (RR) shows NN UU. The most recent 5 year mean was MM UU."
               itp <- ITP$indicator[i]
               if (itp == "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock") {
                 df <- RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]
                 t <- round(unname(coef(lm(df$abundance ~ df$year))[2]),2)
                 y <- length(df$year)
                 u <- "average # of haddock per tow"
                 r <- sort(df$year)[length(df$year)]
                 n <- df$abundance[which(df$year == r)]
                 m <- mean(df$abundance[which(df$year %in% tail(sort(df$year),5))], na.rm=TRUE)
                 lr <- round(unname(coef(lm(df$abundance[which(df$year %in% tail(sort(df$year),5))] ~ df$year[which(df$year %in% tail(sort(df$year),5))]))[2]),2)
                 tid <- ifelse(lr > 0, "increase", "decrease")
               } else if (itp %in% c(
                 "Physical conditions (e.g. surface temperature) within the MR",
                 "Physical conditions (e.g. bottom temperature) within the MR",
                 "Physical conditions (e.g. surface salinity) within the MR",
                 "Physical conditions (e.g. bottom salinity) within the MR",
                 "Physical conditions (e.g. surface light) within the MR",
                 "Physical conditions (e.g. bottom light) within the MR",
                 "Physical conditions (e.g. surface wind) within the MR",
                 "Physical conditions (e.g. sea surface height) within the MR",
                 "Biological conditions (e.g. surface chlorophyl) within the MR",
                 "Biological conditions (e.g. surface nitrate) within the MR",
                 "Biological conditions (e.g. surface phosphate) within the MR",
                 "Biological conditions (e.g. surface silicate) within the MR"
               )) {


                 if (itp == "Physical conditions (e.g. surface temperature) within the MR") {
                   df <- plot_azmp_physical(parameter="temperature", type="surface", dataframe=TRUE)
                 } else if (itp == "Physical conditions (e.g. bottom temperature) within the MR") { # HERE 117
                   df <- plot_azmp_physical(parameter="temperature", type="bottom", dataframe=TRUE)
                 } else if (itp == "Physical conditions (e.g. surface salinity) within the MR") {
                   df <- plot_azmp_physical(parameter="salinity", type="surface", dataframe=TRUE)
                 } else if (itp == "Physical conditions (e.g. bottom salinity) within the MR") {
                   df <- plot_azmp_physical(parameter="salinity", type="bottom", dataframe=TRUE)
                 } else if (itp == "Biological conditions (e.g. surface chlorophyl) within the MR") {
                   df <- plot_azmp_physical(parameter="chlorophyll", type="surface", dataframe=TRUE)
                 } else if (itp == "Biological conditions (e.g. surface nitrate) within the MR") {
                   df <- plot_azmp_physical(parameter="nitrate", type="surface", dataframe=TRUE)
                 } else if (itp == "Biological conditions (e.g. surface phosphate) within the MR") {
                   df <- plot_azmp_physical(parameter="phosphate", type="surface", dataframe=TRUE)
                 }
                 t <- round(unname(coef(lm(df$avg_parameter ~ df$year))[2]),2)
                 y <- length(df$year)
                 u <- "average surface parameter" # FIXME
                 r <- sort(df$year)[length(df$year)]
                 n <- df$avg_parameter[which(df$year == r)]
                 m <- mean(df$avg_parameter[which(df$year %in% tail(sort(df$year),5))], na.rm=TRUE)
                 lr <- round(unname(coef(lm(df$avg_parameter[which(df$year %in% tail(sort(df$year),5))] ~ df$year[which(df$year %in% tail(sort(df$year),5))]))[2]),2)
                 tid <- ifelse(lr > 0, "increase", "decrease")
               } else {
                 t <- "BLANK"
                 y <- "BLANK"
                 u <- "BLANK"
                 r <- "BLANK"
                 n <- "BLANK"
                 m <- "BLANK"
                 lr <- "BLANK"
                 tid <- "BLANK"
               }

               if (!(t == "BLANK")) {
                 if (t > 0) {
                   TREND <- gsub("XX", "increase", TREND)
                 } else {
                   TREND <- gsub("XX", "decrease", TREND)
                 }
               } else {
                 TREND <- gsub("XX", "BLANK", TREND)
               }

               TREND <- gsub("YY", t, TREND)
               TREND <- gsub("ZZ", y, TREND)
               TREND <- gsub("UU", u, TREND)
               TREND <- gsub("LR", lr, TREND)
               TREND <- gsub("TID", tid, TREND)



               ITP$trend[i] <- TREND

               # STATUS
               STATUS <- gsub("RR", r, STATUS)
               STATUS <- gsub("UU", u, STATUS)
               STATUS <- gsub("NN", n, STATUS)
               STATUS <- gsub("MM", m, STATUS)


               ITP$status[i] <- STATUS
             }

             ITP

           })
