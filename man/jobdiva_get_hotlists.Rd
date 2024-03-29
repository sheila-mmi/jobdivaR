% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobdiva_get_hotlists.R
\name{jobdiva_get_hotlists}
\alias{jobdiva_get_hotlists}
\title{Query all contact or candidate hotlists in JD}
\usage{
jobdiva_get_hotlists(entity)
}
\arguments{
\item{entity}{(type: string) -- a string indicating which entity to get 
(Candidate or Contact)}
}
\value{
Content from the HTTP response of the search
}
\description{
This function will return the dataframe of contact or candidate hotlists based on the input
}
