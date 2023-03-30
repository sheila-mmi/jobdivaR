#' Run search methods from JobDiva API
#' 
#' This function will return the HTTP response from search methods
#' in JobDiva
#' 
#' @param entity (type: string) -- a string indicating which entity to get 
#'                         (Company, Candidate,Contacts, etc.)
#' @param offset (type: integer) -- number of records to skip in query (useful for loops)
#' @return Content from the HTTP response of the search                     
#' @export

jobdiva_search = function(entity, offset = "")
{
  base_url = "https://api.jobdiva.com/api/jobdiva/"
  
  full_method = paste0("search", stringr::str_to_title(entity))
  full_method = stringr::str_replace_all(full_method, " ", "")
  print(offset)
  if (offset == "")
  {
    request = httr::GET(url = paste0(base_url, full_method)
                        , add_headers("Authorization" = jobdiva_login())
                        , encode = "json"
                        , httr::verbose())
    if (request$status_code == 404)
    {
      full_method = paste0("Search", stringr::str_to_title(entity))
      full_method = stringr::str_replace_all(full_method, " ", "")
      request = httr::GET(url = paste0(base_url, full_method)
                          , add_headers("Authorization" = jobdiva_login())
                          , encode = "json"
                          , httr::verbose()) 
    }
  }
  
  else
  {
    request = httr::GET(url = paste0(base_url, full_method, "?offset=", offset)
                        , add_headers("Authorization" = jobdiva_login())
                        , encode = "json"
                        , httr::verbose()) 
  }
  
  results = httr::content(request)
  
  return(results)
  
}
