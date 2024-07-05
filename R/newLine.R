# Function to insert newline at the 40th character or at the next space
newLine <- function(text, line_length = 40) {
  words <- strsplit(text, " ")[[1]]
  current_line <- ""
  lines <- character()

  for (word in words) {
    if (nchar(current_line) + nchar(word) > line_length) {
      lines <- c(lines, current_line)
      current_line <- paste(word, sep = " ")
    } else {
      if (current_line != "") {
        current_line <- paste(current_line, word, sep = " ")
      } else {
        current_line <- word
      }
    }
  }

  if (current_line != "") {
    lines <- c(lines, current_line)
  }

  return(paste(lines, collapse = "\n "))
}
