
library(tidyverse)
library(ggplot2)
library(lubridate)

records <- read_csv("C:/Users/MARTINH/Documents/Github/MarConsNetApp/sandbox/hm/02_data_ingestion/data_sources.csv")

records <- records %>%
  transmute(
    Title = Title,
    StartDate = as.Date(parse_date_time(`Start Date`, orders = c("ymd", "mdy", "dmy", "Ymd HMS"))),
    EndDate   = as.Date(parse_date_time(`End Date`, orders = c("ymd", "mdy", "dmy", "Ymd HMS")))
  )
 # ) %>%
#  filter(!is.na(StartDate), !is.na(EndDate))

complete <- records %>%
  filter(!is.na(StartDate), !is.na(EndDate))

ongoing <- records %>%
  filter(!is.na(StartDate), is.na(EndDate))

# --- Base plot ---
ggplot() +

  # Completed tasks (bars)
  geom_segment(
    data = complete,
    aes(
      x = StartDate,
      xend = EndDate,
      y = Title,
      yend = Title
    ),
    linewidth = 5,
    color = "steelblue"
  ) +

  # Ongoing tasks (milestones)
  geom_point(
    data = ongoing,
    aes(
      x = StartDate,
      y = Title
    ),
    size = 3,
    color = "orange"
  ) +

  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +


  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.title = element_text(face = "bold")
  ) +

  labs(
    title = "Data Ingestion Progress",
    x = "Timeline",
    y = NULL
  )


#library(Microsoft365R)
#library(dplyr)
#library(ggplot2)

# Connect to site
#site <- get_sharepoint_site(
#  "https://086gc.sharepoint.com/sites/MaritimesConservationNetworkApp"
#)

# Get list
#record_list <- site$get_list("Data Sources")

# Pull all items
#items <- record_list$list_items()

#records <- bind_rows(
#  lapply(items, function(x) x$properties)
#)

#records <- records %>%
#  transmute(
#    Title = Title,
#    StartDate = as.Date(`Start Date`),
#    EndDate = as.Date(`End Date`)
#  ) %>%
#  filter(
#    !is.na(StartDate),
#    !is.na(EndDate)
#  )

#ggplot(records) +
#  geom_segment(
#    aes(
#      x = StartDate,
#      xend = EndDate,
#      y = reorder(Title, StartDate),
#      yend = reorder(Title, StartDate)
#    ),
#    linewidth = 8
#  ) +
#  labs(
#    title = "Project Schedule",
#    x = "Date",
#    y = NULL
#  ) +
#  theme_minimal()












