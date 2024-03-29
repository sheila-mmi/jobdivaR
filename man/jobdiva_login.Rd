% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobdiva_login.R
\name{jobdiva_login}
\alias{jobdiva_login}
\title{JobDiva API Login}
\usage{
jobdiva_login(
  client_id = 2425,
  username = "API.jobdiva@itmmi.com",
  pass = "ItmmI550$"
)
}
\arguments{
\item{client_id}{(type: string) -- a string indicating with the client id given by JobDiva}

\item{username}{(type: string) -- email address of the designated API account}

\item{pass}{(type: string) -- password of the designated API account}
}
\value{
A string that is the key for authentication into JobDiva's API
}
\description{
This function will return the key for authentication into JobDiva's API
}
