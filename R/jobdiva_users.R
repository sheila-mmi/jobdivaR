#' Query users from JD
#' 
#' This function will return the list of users in JobDiva
#' 
#' @return a Dataframe with users and their information from JobDiva                     
#' @export

jobdiva_users = function()
{
  base_url = "https://api.jobdiva.com/api/bi/UsersList?alternateFormat=true"
  
  request = httr::GET(url = base_url
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose()
                      )
  
  users = httr::content(request)
  users = users[[2]]
  users = dplyr::bind_rows(users)
  
  return(users)
  
}
