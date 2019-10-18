#' Create a random sequence of n alphanumeric chars
#' @param n length of sequence
#' @export
random_string = function(n) {
  paste0(sample(c(0:9, LETTERS, letters), n, replace=T), collapse = '')
}

#' Add prefix and suffix to a string only if string is not empty
#' @param x string to wrap
#' @param before string to add as a suffix if x is not empty
#' @param after string to add as a postfix if x is not empty
#' @export
wrap_string = function(x, before=NULL, after=NULL) {
  ifelse(is.null(x) | x == "", "", paste0(before, x, after))
}

#' Ensure path has an ending /
#' @param x string to end by a slash
#' @export
ending_slash = function(x) {
  paste0(x, ifelse(grepl("/$", x), "", "/"))
}
