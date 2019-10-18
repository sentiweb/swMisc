
#' Replace names with given pairlist where value are old names and key are new names
#'
#' replace_names(data.frame(), new.name=old.name,...)
#' ex replace_names(d, toto="x") replace column "x" by "toto"
#'
#' @param x data.frame
#' @param ... named list of column to rename (names are new names to assign)
#' @export
replace_names <- function(x, ...) {
  nn = list(...)
  if(is.list(nn[[1]])) {
    nn = nn[[1]]
  }
  if( !is.list(nn) ) {
    stop("replacements should be a list")
  }
  old.names = names(x)
  new.names = names(nn) # new names are key of args
  u = match(nn, old.names)
  if( any(is.na(u)) ) {
    warning(paste('unknown columns ', paste(nn[is.na(u)],collapse=',')))
  }
  f = match(old.names, nn)
  i = !is.na(f) # NA= unknown old names in new names
  f = f[i]
  if(length(f) > 0) {
    old.names[i] = new.names[f]
    names(x) <- old.names
  } else {
    warning("nothing to replace")
  }
  return(x)
}

#' Merge two lists (x + y not in x)
#'
#' Missing entries in x will be taken from y. Dont handle nested lists (use \code{\link[utils]{modifyList}})
#' @param x primary list to update
#' @param y secondary list, only entries not in x will be used
#' @export
merge_list <- function(x, y) {
  if(length(x) == 0)
    return(y)

  if(length(y) == 0)
    return(x)

  ny = names(y)
  i = ny[ !ny %in% names(x) ]
  x[i] = y[i]
  x
}

#' Return available columns in data.frame
#' Ancestor of dplyr::select_vars()
#' @param data data.frame
#' @param columns list of column to extract
#' @param error if TRUE raise error if any non existent column, warning only if FALSE
#' @export
select_columns = function(data, columns, error=FALSE) {
  n = names(data)
  missing = columns[!columns %in% n]
  if(length(missing) > 0) {
    msg = paste("Missing columns:", paste(missing, collapse = ','))
    if(error) {
      stop(msg)
    }
    warning(msg)
  }
  columns = columns[columns %in% n]
  columns
}

#' Select columns from a data.frame
#' Ancestor of dplyr::select()
#' @param data data.frame
#' @param columns list of column to extract
#' @param error raise error if unknown column
#' @return data.frame
#' @export
select_df <- function(data, columns, error=FALSE) {
  columns = select_columns(data, columns, error=error)
  data[, columns, drop=FALSE]
}
