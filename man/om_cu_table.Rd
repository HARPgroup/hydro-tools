% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/om_cu_table.R
\name{om_cu_table}
\alias{om_cu_table}
\title{Generate a table of CIA model result summary statistsics for both Rseg and Facility models, comparing across scenarios}
\usage{
om_cu_table(
  fac_report_info,
  pr_data,
  cu_post_var,
  cu_pre_var,
  cu_threshold,
  cu_decimals
)
}
\arguments{
\item{fac_report_info}{facility json list}

\item{pr_data}{riverseg json list}

\item{cu_post_var}{list of runids of interest}

\item{cu_pre_var}{list of facility metrics of interest}

\item{cu_threshold}{list of riverseg metrics of interest}

\item{cu_decimals}{vhydro url}
}
\value{
dataframe of summary stats
}
\description{
Generate a table of CIA model result summary statistsics for both Rseg and Facility models, comparing across scenarios
}
\examples{
NA
}
\seealso{
NA
}
