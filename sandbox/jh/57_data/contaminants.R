#install.packages(c("pdftools", "dplyr", "stringr", "readr"))
library(pdftools)
library(dplyr)
library(stringr)
library(readr)

pdf_url <- "https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/261398.pdf"

# download locally
tmp <- tempfile(fileext = ".pdf")
download.file(pdf_url, tmp, mode = "wb")

# extract text from all pages
pdf_text <- pdf_text(tmp)

tp <- pdf_text[which(grepl("Table", pdf_text))]

## GETTING A LIST OF TABLES

# pdf_text already contains all pages
# create a data frame with page numbers and text
toc_df <- data.frame(
  page = seq_along(pdf_text),
  text = pdf_text,
  stringsAsFactors = FALSE
)

# detect lines containing "Table" (case-insensitive)
lot_raw <- tp[which(grepl("LIST OF TABLES", tp))]

# combine into one string
lot_text <- paste(lot_raw, collapse = "\n")

# split into lines
lot_lines <- str_split(lot_text, "\n")[[1]]

# trim whitespace and remove empty lines
lot_lines <- str_trim(lot_lines)
lot_lines <- lot_lines[lot_lines != ""]

# remove trailing page numbers (assumes page numbers are 1-3 digits at line end)
lot_clean <- str_trim(str_remove(lot_lines, "\\s+\\d+$"))

# optional: remove lines that are just "LIST OF TABLES" or headers
lot_clean <- lot_clean[!str_detect(lot_clean, regex("LIST OF TABLES", ignore_case = TRUE))]

# create data frame
tables_df <- data.frame(
  table_name = lot_clean,
  stringsAsFactors = FALSE
)

df <- tables_df[35:170, , drop = FALSE]
list_of_tables <- df[-which(grepl("Table of Contents", df$table_name)),]

# According to AI, tables of interest:

tbls <- c("2.1", "2.2", "3.10", "3.11", "3.12", "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.9", "2.10", "2.11", "2.12", "3.5", "3.6", "3.7", "3.8", "3.31", "3.32", "3.33")

final <- data.frame(location=NA, type=NA, concentration=NA, unit=NA)

for (i in seq_along(tbls)) {
  txt <- paste0("Table ", tbls[i],'. ')
  k1 <- which(grepl(txt, tp, fixed=TRUE))
  k2 <- which(!(grepl("LIST OF TABLES", tp, fixed=TRUE)))
  keep <- intersect(k1,k2)

}
