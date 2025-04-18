% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odbc_utils.R
\name{fn_delete_odbc}
\alias{fn_delete_odbc}
\title{Post any entity to a RESTful web service}
\usage{
fn_delete_odbc(entity_type, pk, inputs, con, obj = FALSE, debug = FALSE)
}
\arguments{
\item{entity_type}{= dh_feature, dh_properties, ...}

\item{pk}{= primary key column name, e.g. hydroid, pid, ...}

\item{inputs}{contents of record to post in list(pid, propname, propvalue, ...)}

\item{con}{connection to ODBC server}

\item{obj}{optional class with extra query info}

\item{debug}{Print out debug info if true}
}
\description{
Post any entity to a RESTful web service
}
\examples{
NA
}
\seealso{
NA
}
