% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{select_columns}
\alias{select_columns}
\title{Return available columns in data.frame
Ancestor of dplyr::select_vars()}
\usage{
select_columns(data, columns, error = FALSE)
}
\arguments{
\item{data}{data.frame}

\item{columns}{list of column to extract}

\item{error}{if TRUE raise error if any non existent column, warning only if FALSE}
}
\description{
Return available columns in data.frame
Ancestor of dplyr::select_vars()
}
