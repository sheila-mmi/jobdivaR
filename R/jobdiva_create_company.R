#' JobDiva Create Company
#' 
#' This function will create a company in JobDiva if it does not already exist.
#' 
#' @param company_name (type: string) -- a string indicating with the client id given by JobDiva
#' @param company_website (type: string) -- email address of the designated API account
#' @param phx_normalized_company_id (type:string) -- the Phoenix Normalized Company Id to match the record to
#' @return The JobDiva company id of the created company
#' @export


jobdiva_create_company = function(company_name
                                  , company_website = ""
                                  , phx_normalized_company_id = "")
{
  name = trimws(company_name)
  name = stringr::str_replace_all(name, ' ', '%20')
  name = stringr::str_replace_all(name, '@', '%40')
  name = stringr::str_replace_all(name, '\\(', '%28')
  name = stringr::str_replace_all(name, '\\)', '%29')
  name = str_replace_all(name, '\\|', '%7C')
  name = str_replace_all(name, '&', '%26')
  name = str_replace_all(name, ',', '%2C')
  name = str_replace_all(name, 	'\\{', '%7B')
  name = str_replace_all(name, 	'\\}', '%7D')
  name = str_replace_all(name, 	'\\:', '%3A')
  
  url = paste0('https://api.jobdiva.com/api/jobdiva/createCompany?companyname='
               , name
               , '&url='
               , tolower(company_website))
  
  results = httr::POST(url
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  # Return Company ID
  {
    status = as.numeric(status_code(results))
    
    # 200 code: successful
    # 500 code: company already exists
    # anything else: ERROR
    
    if (status == 200)
    {
      cont = httr::content(results)
      cont = as.numeric(cont)
    }else if (status == 500)
    {
      cont = httr::content(results)
      cont = cont$message
      cont = stringr::str_extract(cont, regex("id\\:(.*)\\)"))
      cont = substring(cont, 4,nchar(cont)-1)
      
    }else {
      cont = character(0)
      return(cont)
    }
  }
  
  # Add PHX ID to company
  {
    new_comp_id = cont
    if (phx_normalized_company_id != ''
        && phx_normalized_company_id != " "
        && !is.null(phx_normalized_company_id)
        && !is.na(phx_normalized_company_id))
    {
      update_comp = try(jobdiva_update_phx_normalized_company_id(new_comp_id
                                                      , phx_normalized_company_id), silent = TRUE)
    }
  }
  
  return(cont)
}

