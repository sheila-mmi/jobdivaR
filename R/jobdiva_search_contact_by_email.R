#' Search Contact Profiles
#' 
#' This function will return the HTTP content from searching for a contact profile
#' 
#' @param contact_email (type: string) -- a string with the email address to query the contact by 
#' @return Content from the HTTP content of the search                     
#' @export

jobdiva_search_contact_by_email = function(contact_email)
{
  
  email = as.character(str_replace(contact_email,'@', '%40'))
  base_url = 'https://api.jobdiva.com/api/jobdiva/searchContacts?email='
  
  request = httr::POST(url = paste0(base_url, contact_email)
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  results = httr::content(request)
  return(results)
  
}
