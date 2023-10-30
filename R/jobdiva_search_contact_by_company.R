#' Search Contact Profiles -- Company
#' 
#' This function will return the HTTP content from searching for a contact profile
#' 
#' @param company (type: string) -- a string with the company name to query the contact by 
#' @return Content from the HTTP content of the search                     
#' @export

jobdiva_search_contact_by_company = function(company)
{
  
  email = as.character(str_replace(company,' ', '%20'))
  base_url = 'https://api.jobdiva.com/api/jobdiva/SearchContacts?company='
  offset = 0
  
  request = httr::GET(url = paste0(base_url, company)
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  
  final_x_df = data.frame()
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
  final_x_df = rbind(final_x_df, x_df)
  
  while(nrow(x_df) == 200)
  {
    offset = offset + 200
    
    request = httr::GET(url = paste0(base_url, company, '&offset=', offset)
                        , add_headers("Authorization" = jobdiva_login())
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
    final_x_df = rbind(final_x_df, x_df)
    
  }
  
  return(final_x_df)
  
}