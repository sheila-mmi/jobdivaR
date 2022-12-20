#' JobDiva API Login
#' 
#' This function will return the key for authentication into JobDiva's API
#' 
#' @param client_id (type: string) -- a string indicating with the client id given by JobDiva
#' @param username (type: string) -- email address of the designated API account
#' @param pass (type: string) -- password of the designated API account
#' @return A string that is the key for authentication into JobDiva's API
#' @export


jobdiva_login = function(client_id = 2425
                         , username = 'API.jobdiva@itmmi.com'
                         , pass = 'ItmmI550$')
{
  url = paste0('https://api.jobdiva.com/api/authenticate?clientid='
               , client_id
               , '&username='
               , username
               , '&password='
               , pass)
  
  results = httr::GET(url)
  key = httr::content(results, type = 'text')
  
  auth = httr::authenticate("api.jobdiva@itmmi.com"
                            , key) 
  
  return(auth)
}
  
