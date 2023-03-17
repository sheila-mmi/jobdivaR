#' Query User-Defined Fields (UDFs) from JobDiva
#' 
#' This function will return the list of UDFs in JobDiva
#' 
#' @return a Dataframe with all UDFs in JobDiva
#' @export

jobdiva_udfs = function()
{
  base_url = "https://api.jobdiva.com/api/bi/UserfieldsList?alternateFormat=true"
  
  request = httr::GET(url = base_url
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose()
  )
  
  user_fields = httr::content(request)
  user_fields = user_fields[[2]]
  user_fields = dplyr::bind_rows(user_fields)
  
  return(user_fields)
  
}
