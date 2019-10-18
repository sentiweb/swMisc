
#' Compare installed version of a package
#' @param pkg package name
#' @param version version to test
#' @param compare comparator to use '>','<','==','>=','<='
#' @param not_found value to return if package is not installed
#' @return boolean
#' @importFrom utils compareVersion packageDescription
#' @export
has_package_version <- function(pkg, version, compare, not_found = NA) {
  res = packageDescription(pkg, lib.loc = NULL, fields = "Version")
  if( is.na(res) ) {
    return(not_found)
  }
  v = compareVersion(res, version)
  switch(compare,
         ">"= (v == 1),
         "<"= (v < 0),
         ">="= (v >= 1),
         "<="= (v <= 0),
         "=="= v == 0
  )
}
