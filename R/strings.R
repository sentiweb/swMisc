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

#' Create a time based stamp to be used as suffix in file names
#' @param time string with type of time string to use, "datetime" will output date and time (with only separator between date and date like 20220402-103402 for 2 May 2022, 10:34:02), default is "datetime"
#' @param random integer length of random string to add to the time, no random if 0 (default behavior)
#' @param sep string with separator to use between time and random part
#' @export
file_stamp = function(time="datetime", random=0, sep="-") {
  now = Sys.time()
  if(time == "timestamp") {
    stamp = as.character(as.integer(now))
  }
  fmt = switch(time,
    datetime="%Y%m%d-%H%M%S",
    date="%Y%m%d",
    time
  )
  stamp = format(now, format=fmt)
  if(random > 0) {
    stamp = paste(stamp, random_string(random), sep=sep)
  }
  stamp
}
