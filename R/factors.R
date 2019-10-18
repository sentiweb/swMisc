#' Replace factor levels by new values
#' @param .x factor or vector of values (transformed into factor if not a factor)
#' @param .dots list with the form (new=old) of levels to
#' @param .warn if TRUE a warning will be thrown if unknown level is mentionned, else throw an error
#' @param ... renaming argument new=old, NSE evaluation
#' @export
replace_factor <- function(.x, ..., .dots=NULL, .warn=TRUE) {
  .x = as.factor(.x)
  if( !is.null(.dots) ) {
    nn = .dots
  } else {
    nn = list(...)
  }
  old.names = levels(.x)
  new.names = names(nn) # new names are key of args
  u = match(nn, old.names)
  if( any( is.na(u) ) ) {
    m = paste('unknown levels ', paste(nn[is.na(u)], collapse=','))
    if ( .warn ) {
      warning(m)
    } else {
      stop(m)
    }
  }
  f = match(old.names, nn)
  i = !is.na(f) # NA= unknown old names in new names, do not replace
  f = f[i]
  if(length(f) > 0) {
    old.names[i] = new.names[f]
    levels(.x) <- old.names
  }
  .x
}
