  
#' Query users from JD
#' 
#' This function will return the list of users in JobDiva
#' 
#' @return a Dataframe with users and their information from JobDiva                     
#' @export

jobdiva_hotlist_add_contact = function(hotlist_id, contact_id)
{
  base_url = "https://api.jobdiva.com/api/hotlist/addContactToHotlist?hotListid="
  
  full_url = paste0(base_url
                    , hotlist_id
                    , '&contactId='
                    , contact_id)
  request = httr::POST(url = full_url
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose()
                      )
  
  status = httr::content(request)
  return(status)
  
}
