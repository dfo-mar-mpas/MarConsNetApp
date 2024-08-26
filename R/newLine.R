#' Add new line at nth character
#'
#' This function adds a new line at the specified character of a string.
#' If that specified character is in the middle of a word, it will find
#' the next space and add a new line there.
#'
#' @param text a character string
#' @param line_length a number indicating which character to add the new line
#' @export
#' @examples
#' \dontrun{
#' newLine("This is a test to show the user how new line works.", 40)
#' }
#' @author Jaimie Harbin
#' @return a character string with new lines added at the nth character
#'
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
