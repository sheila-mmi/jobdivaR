#' JobDiva Create Candidate Hotlist
#' 
#' This function will create a candidate hotlist in JobDiva and return new hotlist's id
#' 
#' @param name (type: string) -- a string indicating with the client id given by JobDiva
#' @param description (type: string) -- a string to describe the hotlist
#' @param shared (type: vector) -- a vector of users to share the hotlist with 
#' @return A string that is the id of the newly created hotlist.
#' @export


jobdiva_create_candidate_hotlist = function(name
                                  , description = ""
                                  , shared = c())
{
  name = str_replace_all(name, ' ', '%20')
  description = str_replace_all(as.character(description), ' ', '%20')
  
  name = str_replace_all(name, '\\|', '%7C')
  description = str_replace_all(as.character(description),  '\\|', '%7C')
  
  name = str_replace_all(name, '&', '%26')
  description = str_replace_all(as.character(description),  '&', '%26')
  
  shared = c(shared,'1525852')
  
  url = paste0('https://api.jobdiva.com/api/hotlist/createCandidateHoltilst?name='
               , name
               , '&description='
               , description
               , '&userIds='
               , paste0(shared, collapse = '&userIds='))
  
  results = httr::POST(url
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  cont = httr::content(results)
  cont = as.numeric(cont)
  return(cont)
}

