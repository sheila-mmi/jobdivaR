% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobdiva_update_contact.R
\name{jobdiva_update_contact}
\alias{jobdiva_update_contact}
\title{JobDiva Update Contact}
\usage{
jobdiva_update_contact(jobdiva_contact_id, update_df)
}
\arguments{
\item{jobdiva_contact_id}{(type: string) -- a string with the JobDiva contact id of the contact
to be updated}

\item{update_df}{(type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'}
}
\value{
status of whether or not the contact was successfully updated.
}
\description{
This function will update contact information in JobDiva
}
