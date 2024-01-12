#' String to Title format
#'
#' Capitalise first letters of each word to match title format
#'
#' @references \url{https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string}
#'
#' @param str A string
#'
#' @return A capitalised string
#'
#' @examples
#' str <- "OSLO - BLINDERN"
#' str_to_title(str)
#'
#' @export
str_to_title <- function(str){

  s <- strsplit(tolower(str), " ")[[1]]

  paste(toupper(substring(s, 1, 1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}
