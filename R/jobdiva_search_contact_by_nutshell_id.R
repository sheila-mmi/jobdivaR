#' Search Contact Profiles
#' 
#' This function will return the HTTP content from searching for a contact profile
#' 
#' @param contact_email (type: string) -- a string with the email address to query the contact by 
#' @return Content from the HTTP content of the search                     
#' @export

jobdiva_search_contact_by_nutshell_id = function(nutshell_id)
{
  
  # email = as.character(str_replace(contact_email,'@', '%40'))
  base_url = 'https://api.jobdiva.com/apiv2/jobdiva/SearchContacts'
  
  # udf = paste0('{"searchForUdfs": [{%0A  "udfName": '
  #              , '"Nutshell_ID"'
  #              , ',%0A  "udfValue": "'
  #              , nutshell_id 
  #              , '"%0A}]','}')
  # 
  # udf = paste0('{"searchForUdfs": [{"udfName": '
  #              , '"Nutshell_ID"'
  #              , ', "udfValue": "'
  #              , nutshell_id
  #              , '"}]}')
  
  searchForUdfs = list('udfName' = 'Nutshell_ID', 'udfValue' = nutshell_id)
  
  request = httr::POST(url = paste0(base_url)
                      , add_headers("Authorization" = jobdiva_login())
                      , body = list('searchForUdfs' = list(searchForUdfs))
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  
  x_list = list()
  x_list = lapply(results, function(x){
    step_1 = lapply(x, as.character)
    col_names = names(x)
    step_2 = data.frame(t(step_1))
    # step_3 = lapply(step_2, data.frame)
    # step_4 = lapply(step_3, cbind)
    x_df = step_2
    names(x_df) = col_names
    x_df
  })
  
  x_df = dplyr::bind_rows(x_list)
  results = x_df
  return(results)
  
}
