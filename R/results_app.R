# THIS SCRIPT IS USED TO FILL IN THE STATUS AND TREND STATEMENTS

for (i in seq_along(indicator_to_plot$indicator)) {
  TREND <- "A linear regression has shown a XX  of YY UU over the last ZZ years. The regression for the last 5 years was LR UU."
  STATUS <- "The most recent year (RR) shows NN UU. The most recent 5 year mean was MM UU."
  itp <- indicator_to_plot$indicator[i]
  if (itp == "Support productivity objectives for groundfish species of Aboriginal, commercial, and/or recreational importance, particularly NAFO Division 4VW haddock") {
    df <- RV_ABUNDANCE[[which(names(RV_ABUNDANCE) == 'WEBCA')]][[which(species == 'HADDOCK')]]

    t <- round(unname(coef(lm(df$abundance ~ df$year))[2]),2)
    y <- length(df$year)
    u <- "average # of haddock per tow"
    r <- sort(df$year)[length(df$year)]
    n <- df$abundance[which(df$year == r)]
    m <- mean(df$abundance[which(df$year %in% tail(sort(df$year),5))], na.rm=TRUE)
    lr <- round(unname(coef(lm(df$abundance[which(df$year %in% tail(sort(df$year),5))] ~ df$year[which(df$year %in% tail(sort(df$year),5))]))[2]),2)
  } else {
    t <- "BLANK"
    y <- "BLANK"
    u <- "BLANK"
    r <- "BLANK"
    n <- "BLANK"
    m <- "BLANK"
    lr <- "BLANK"
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


  indicator_to_plot$trend[i] <- TREND

  # STATUS
  STATUS <- gsub("RR", r, STATUS)
  STATUS <- gsub("UU", u, STATUS)
  STATUS <- gsub("NN", n, STATUS)
  STATUS <- gsub("MM", m, STATUS)


  indicator_to_plot$status[i] <- STATUS
}








