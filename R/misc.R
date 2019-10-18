#' Test if x is an error class from try
#' @param x object to test
#' @export
#' @importFrom methods is
is.error <- function(x) {
  is(x, "try-error")
}

#' get file name with good extension .r or .R
#' @param file file path to find without extension
#' @param should.exists logical, error if file doesnt exists with any extension
#' @return path
#' @export
get_r_file = function(file, should.exists=FALSE) {
  f = paste0(file,".R")
  if( file.exists(f)) {
    return(f)
  }
  f = paste0(file,".r")
  if(should.exists && !file.exists(f)) {
    stop(paste0(f, "not found"))
  }
  return(f)
}



