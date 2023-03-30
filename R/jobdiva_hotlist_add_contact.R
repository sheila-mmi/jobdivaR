#' JobDiva Hotlist Add Contact
#' 
#' This function will add the contact to the specified hotlist in JD.
#' 
#' @return a status indicating either success or failure                   
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
