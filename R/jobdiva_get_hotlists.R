#' Query all contact or candidate hotlists in JD
#' 
#' This function will return the dataframe of contact or candidate hotlists based on the input
#' 
#' @param entity (type: string) -- a string indicating which entity to get 
#'                         (Candidate or Contact)
#' @return Content from the HTTP response of the search                     
#' @export

jobdiva_get_hotlists = function(entity)
{
  if (toupper(entity) == 'CONTACT')
  {
    base_url = "https://api.jobdiva.com/apiv2/bi/ContactHotlistList"
    
  } else if (toupper(entity) == 'CANDIDATE')
  {
    base_url = "https://api.jobdiva.com/apiv2/bi/CandidateHotlistList"
  } else
  {
    stop("ERROR: Please check the entity you input. Only contacts and candidates can be queried.")
  }
  
  request = httr::GET(url = base_url
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  results = try(results[[2]], silent = TRUE)
  if(class(results)[1] != 'try-error')
  {
    result_df = try(dplyr::bind_rows(results), silent = TRUE)
    return(result_df)
  }
  
  return(results)
  
}
