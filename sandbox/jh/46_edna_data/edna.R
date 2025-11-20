root <- "../eDNA-for-MPAs/data/"

# Get all CSVs that contain 'GOTeDNA' anywhere in the full path
eDNA_csv <- list.files(
  path = root,
  pattern = "GOTeDNA.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

eDNA_csv
