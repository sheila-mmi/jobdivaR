#' JobDiva Create Hotlist
#' 
#' This function will create a hotlist in JobDiva and return new hotlist's id
#' 
#' @param name (type: string) -- a string indicating with the client id given by JobDiva
#' @param active (type: boolean) --  A boolean of whether or not the hotlist should be active
#' @param priv (type: boolean) -- A boolean of whether or not the hotlist should be private (only viewable to the person who created it/those who it is shared with)
#' @param description (type: string) -- a string to describe the hotlist
#' @return A string that is the id of the newly created hotlist.
#' @export


jobdiva_create_hotlist = function(name
                                  , active = TRUE
                                  , priv = FALSE
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
  shared = unique(shared)
  
  url = paste0('https://api.jobdiva.com/api/hotlist/createContactHotlist?name='
               , name
               , '&active='
               , tolower(active)
               , '&isPrivate='
               , tolower(priv)
               , '&description='
               , description
               , '&sharedWithIds='
               , paste0(shared, collapse = '&sharedWithIds='))
  
  results = httr::POST(url
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  cont = httr::content(results)
  cont = as.numeric(cont)
  return(cont)
}

