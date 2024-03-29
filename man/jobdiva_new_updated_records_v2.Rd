% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobdiva_new_updated_records_v2.R
\name{jobdiva_new_updated_records_v2}
\alias{jobdiva_new_updated_records_v2}
\title{JobDiva New Updated Records v2}
\usage{
jobdiva_new_updated_records_v2(
  entity,
  from_date = as.character(format(Sys.Date() - 1, "\%m/\%d/\%Y \%H:\%M:\%S")),
  to_date = paste0(as.character(format(Sys.Date(), "\%m/\%d/\%Y")), " 23:59:59")
)
}
\arguments{
\item{entity}{(type: string) -- a string indicating which entity to find 
(Company, Candidate, Contact, etc.)}

\item{from_date}{(type: string) -- the earliest (starting) date for when the entity records where updated (by default it is 1 day before the system date)}

\item{to_date}{(type: string) -- the lastest (ending) date for when the entity records where updated (by default it is the system date)}
}
\value{
A dataframe of new updated records based on the given timeframe.
}
\description{
This function will return the dataframe that corresponds the recently updated records for the given entity and timeframe (from VERSION 2 of JobDiva's API)
}
