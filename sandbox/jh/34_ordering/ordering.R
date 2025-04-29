areas <- unique(pillar_ecol_df$areaID)
regions <- c("Gulf", "Maritimes", "Quebec", "Newfoundland & Labrador") # FIXME: Add in Pacific and Arctic


pillar_list <- split(pillar_ecol_df, pillar_ecol_df$areaID)

mpa_list <- pillar_list[-which(names(pillar_list) %in% regions)]
# Order based on score
mpa_list <- lapply(mpa_list, function(ddff) {
  ddff[order(ddff$score, na.last = TRUE), ]
})

region_list <-  pillar_list[which(names(pillar_list) %in% regions)]
region_list <- lapply(region_list, function(ddff) {
  ddff[order(ddff$score, na.last = TRUE), ]
})
# Order based on score

for (i in seq_along(region_list)) {
reg <- region_list[[i]]
for (j in 1:nrow(reg)) {
  reg2 <- reg[j,]
  region_bin <- reg2$indicator
  keep <- which(names(mpa_list) == region_bin)
  mpa_list[[keep]] <- rbind(reg2, mpa_list[[keep]])
}

}


# In r, there is a row for every indicator bin, for every mpa that makes up that region. E.g. if there is 3 mpas in that region there are 3*11 rows.

dt <- do.call(rbind, mpa_list)

pillar_ecol_df <- dt





# TRYING TABLE
library(DT)
library(dplyr)
library(jsonlite)

# Sample data
ddff <- data.frame(
  Indicator = c("A", "B", "C", "D", "E", "F"),
  areaID = c("Gulf", "Site1", "Maritimes", "Site2", "Site3", "Quebec"),
  Status = c("Good", "Poor", "Fair", "Good", "Unknown", "Fair"),
  Trend = c("Up", "Down", "Stable", "Down", "Up", "Stable"),
  Projects = c(3, 2, 5, 4, 2, 3),
  Score = c(80, 70, 75, 65, 60, 85),
  Method = c("Survey", "Model", "Mixed", "Survey", "Model", "Mixed"),
  stringsAsFactors = FALSE
)

# Define parent areas
regions <- c("Gulf", "Maritimes", "Quebec", "Newfoundland & Labrador")

# Identify parent and child rows
ddff <- ddff %>%
  mutate(
    is_parent = areaID %in% regions,
    parent_id = ifelse(is_parent, areaID, NA)
  )

# Manually assign site rows to their region
ddff$parent_id[ddff$areaID == "Site1"] <- "Gulf"
ddff$parent_id[ddff$areaID == "Site2"] <- "Maritimes"
ddff$parent_id[ddff$areaID == "Site3"] <- "Maritimes"

# Separate into parent and child rows
parents <- ddff %>% filter(is_parent)
children <- ddff %>% filter(!is_parent)

# Build child HTML tables for each parent
child_map <- lapply(parents$areaID, function(region) {
  kids <- children %>% filter(parent_id == region)
  if (nrow(kids) == 0) return(NULL)

  # Create HTML table manually
  html_rows <- apply(kids, 1, function(row) {
    paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
  })

  paste0("<table class='table' style='margin-left:50px;'>",
         paste(html_rows, collapse = ""),
         "</table>")
})
names(child_map) <- parents$areaID

# Generate the table
datatable(
  parents,
  escape = FALSE,
  callback = JS(
    sprintf(
      "
      table.rows().every(function() {
        $(this.node()).css('cursor', 'pointer');
      });

      var childMap = %s;

      var format = function(html) {
        return '<div>' + html + '</div>';
      };

      table.on('click', 'tr', function() {
        var tr = $(this).closest('tr');
        var row = table.row(tr);
        var region = row.data()[1];  // areaID column (adjust if needed)

        if (childMap[region]) {
          if (row.child.isShown()) {
            row.child.hide();
            tr.removeClass('shown');
          } else {
            row.child(format(childMap[region])).show();
            tr.addClass('shown');
          }
        }
      });
      ",
      toJSON(child_map, auto_unbox = TRUE, null = "null")
    )
  ),
  options = list(dom = 't'),
  rownames = FALSE
)

