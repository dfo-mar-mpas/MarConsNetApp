# THIS SCRIPT IS USED TO FILL IN THE STATUS AND TREND STATEMENTS

for (i in seq_along(indicator_to_plot$indicator)) {
  TREND <- "A linear regression has shown a XX  of YY UU over the last ZZ years"
  STATUS <- "The most recent year (RR) shows NN UU."
  itp <- indicator_to_plot$indicator[i]
  if (itp == "-Support productivity objectives for\n groundfish species of Aboriginal,\n commercial, and/or recreational\n importance, particularly NAFO Division\n 4VW haddock \n") {
    t <- round(unname(coef(lm(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$abundance ~ RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$year))[2]),2)
    y <- length(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$year)
    u <- "average # of haddock per tow"
    r <- sort(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$year)[length(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$year)]
    n <- RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$abundance[which(RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]$year == r)]
  } else {
    t <- "BLANK"
    y <- "BLANK"
    u <- "BLANK"
    r <- "BLANK"
    n <- "BLANK"
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

  indicator_to_plot$trend[i] <- TREND

  # STATUS
  STATUS <- gsub("RR", r, STATUS)
  STATUS <- gsub("UU", u, STATUS)
  STATUS <- gsub("NN", n, STATUS)

  indicator_to_plot$status[i] <- STATUS
}








