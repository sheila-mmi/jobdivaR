#' Search Candidate Profiles
#' 
#' This function will return the HTTP content from searching for a candidate profile
#' 
#' @param candidate_email (type: string) -- a string with the email address to query the candidate by 
#' @return Content from the HTTP content of the search                     
#' @export

jobdiva_search_candidate_by_email = function(candidate_email)
{
  
  email = as.character(str_replace(candidate_email,'@', '%40'))
  base_url = 'https://api.jobdiva.com/api/jobdiva/searchCandidateProfile?email='
  
  request = httr::POST(url = paste0(base_url, candidate_email)
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  return(results)
  
}
