#' JobDiva API Login
#' 
#' This function will return the key for authentication into JobDiva's API
#' 
#' @param client_id (type: string) -- a string indicating with the client id given by JobDiva
#' @param username (type: string) -- email address of the designated API account
#' @param pass (type: string) -- password of the designated API account
#' @return A string that is the key for authentication into JobDiva's API
#' @export


jobdiva_create_hotlist = function(name
                                  , active = TRUE
                                  , priv = FALSE
                                  , description
                                  , shared = c())
{
  name = str_replace_all(name, ' ', '%20')
  description = str_replace_all(as.character(description), ' ', '%20')
  shared = c(shared,'1525852')
  
  url = paste0('https://api.jobdiva.com/api/hotlist/createContactHotlist?name='
               , name
               , '&active='
               , tolower(active)
               , '&isPrivate='
               , tolower(priv)
               , '&description='
               , description
               , '&sharedwithIds='
               , paste0(shared, collapse = '&sharedwithIds='))
  
  results = httr::POST(url
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  cont = httr::content(results)
  cont = as.numeric(cont)
  return(cont)
}

