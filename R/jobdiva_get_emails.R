#' Query emails for a specific contact or candidate
#' 
#' This function will return the HTTP response from get methods
#' in Nutshell
#' 
#' @param entity (type: string) -- a string indicating which entity to get 
#'                         (Candidate or Contact)
#' @param entity_id (type: string) -- Entity of the contact/candidate you want to query emails for
#' @return Content from the HTTP response of the search                     
#' @export

jobdiva_get_emails = function(entity, entity_id)
{
  if (toupper(entity) == 'CONTACT')
  {
    base_url = "https://api.jobdiva.com/api/bi/ContactEmailRecords?"
    
  } else if (toupper(entity) == 'CANDIDATE')
  {
    base_url = "https://api.jobdiva.com/api/bi/CandidateEmailRecords?"
  } else
  {
    stop("ERROR: Please check the entity you input. Only contacts and candidates can be queried.")
  }
  
  full_method = paste0(tolower(entity), 'Id=', entity_id, '&alternateFormat=true')
  
  request = httr::GET(url = paste0(base_url, full_method)
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  results = try(results[[2]], silent = TRUE)
  if(class(results)[1] != 'try-error')
  {
    result_df = try(dplyr::bind_rows(results), silent = TRUE)
  }
  
  return(results)
  
}
