#' JobDiva Contact Creation
#' 
#' This function will insert information into JobDiva
#' 
#' @param first_name (type: string) --
#' @param last_name (type: string) --
#' @param title (type: string) --
#' @param company_name (type: string) --
#' @param email (type: string) --
#' @param alternate_email (type: string) --
#' @param type (type: string) -- The type of contact
#' @param phone_numbers (type: string) -- The properly formatted string for phone number creation (action = 0
#' , phone = phone number, ext = "" and only for work phones, type: w = work, h = home, c = cell/mobile, f = fax, p = pager)
#' @param phx_employee_id (type: string) -- The PHX Employee_Id of a contact from Phoenix
#' @param linkedin (type: string) -- The LinkedIn URL of the contact's LinkedIn profile
#' @param addresses (type: string) -- The properly formatted string for address creation (action = 0 for creation and action = 1 for updating)
#' @return The JobDiva contact id of newly created contact
#' @export


jobdiva_create_contact = function(first_name
                                  , last_name
                                  , title = ""
                                  , company_name = ""
                                  , email = ""
                                  , alternate_email = ""
                                  , type = ""
                                  , phone_numbers = ""
                                  , phx_employee_id = ""
                                  , linkedin = ""
                                  , addresses = "")
{
  
  # Clean variables
  {
    vars = {c(first_name
             , last_name
             , title
             , company_name
             , email
             , alternate_email
             , type )}
    
    clean_vars = c()
    for (i in 1:length(vars))
    {
      tmp = as.character(vars[i])
      tmp = stringr::str_replace_all(tmp, ' ', '%20')
      tmp = stringr::str_replace_all(tmp, '@', '%40')
      tmp = stringr::str_replace_all(tmp, '\\(', '%28')
      tmp = stringr::str_replace_all(tmp, '\\)', '%29')
      tmp = str_replace_all(tmp, '\\|', '%7C')
      tmp = str_replace_all(tmp, '&', '%26')
      tmp = str_replace_all(tmp, ',', '%2C')
      tmp = str_replace_all(tmp, 	'\\{', '%7B')
      tmp = str_replace_all(tmp, 	'\\}', '%7D')
      tmp = str_replace_all(tmp, 	'\\:', '%3A')
      
      clean_vars = c(clean_vars, as.character(tmp))
    }
  }
  
  # Fields Query
  {
    fields = {c('firstname'
               , 'lastname'
               , 'title'
               , 'company'
               , 'email'
               , 'alternateemail'
               , 'types')}
    
    field_df = data.frame(fields, clean_vars)
    # Drop rows w/ blanks 
    field_df = field_df[nchar(as.character(field_df[,2])) > 0,]
    
    if (nrow(field_df) != 0)
    {
      fields = apply(field_df, 1, function(x){
        udf = paste0('&'
                     , tolower(as.character(x[1]))
                     , "="
                     , as.character(x[2]))
      })
      
      field_query = paste0(fields, collapse = '')
      field_query = str_replace_all(field_query, ' ', '%20')
      field_query = str_replace_all(field_query, '@', '%40')
      field_query = str_replace_all(field_query, '&', '%26')
      field_query = str_replace_all(field_query, ',', '%2C')
      field_query = str_replace_all(field_query,  '\\)', '%29')
      field_query = str_replace_all(field_query,  '\\(', '%28')
      field_query = str_replace_all(field_query,  '\\|', '%7C')
      field_query = str_replace_all(field_query, 	'\\{', '%7B')
      field_query = str_replace_all(field_query, 	'\\}', '%7D')
      field_query = str_replace_all(field_query, 	'\\:', '%3A')
      
      
    }
    else
    {
      stop("ERROR: Mandatory fields not found.")
    }
  }
  
  # Build Query
  {
    url = paste0('https://api.jobdiva.com/api/jobdiva/createContact?'
                 , field_query)
    
    if(phone_numbers != "" & !is.null(phone_numbers) & !is.na(phone_numbers))
    {
      url = paste0(url, phone_numbers)
    }
    
    if(addresses != "" & !is.null(addresses) & !is.na(addresses))
    {
      url = paste0(url, addresses)
    }
  }
  
  # Create Contact
  {
    results = httr::POST(url
                         , add_headers("Authorization" = jobdiva_login())
                         , encode = "json"
                         , httr::verbose())
    
    status = as.numeric(results$status_code)
    print(status)
    cont = httr::content(results)
    print(cont)

    if(status == '200')
    {
      new_contact_id = as.numeric(cont)
    } else if (status == '500')
    {
      msg = cont$message
      
      if(grepl('company', tolower(msg)))
      {
        # Create company
        new_comp = try(jobdiva_create_company(clean_vars[4]), silent = TRUE)
        if(class(new_comp)[1] != 'try-error')
        {
          # Retry creation
          results = httr::POST(url
                               , add_headers("Authorization" = jobdiva_login())
                               , encode = "json"
                               , httr::verbose())
          
          status = as.numeric(results$status_code)
          cont = httr::content(results)
          if(status == '200')
          {
            new_contact_id = as.numeric(cont)
          } else
          {
            stop("ERROR: Creation failed twice")
          }
        }
      }else
      {
        stop(paste0("ERROR: ", msg))
      }
    }
    else
    {
      msg = cont$message
      stop(paste0("ERROR: ", msg))
    }
    print(new_contact_id)
  }
  
  print(phx_employee_id)
  # Add PHX ID to contact
  {
    if (phx_employee_id != ''
        && phx_employee_id != " "
        && !is.null(phx_employee_id)
        && !is.na(phx_employee_id))
    {
      update_emp = try(jobdiva_update_phx_employee_id(new_contact_id
                                                  , phx_employee_id), silent = TRUE)
      print(update_emp)
    }
  }
  
  # Add LinkedIn to contact
  {
    if (linkedin != ''
        && linkedin != " "
        && !is.null(linkedin)
        && !is.na(linkedin))
    {
      update_emp = try(jobdiva_update_contact_social_link_v2(new_contact_id
                                                      , linkedin, "LinkedIn"), silent = TRUE)
    }
  }
  
  return(new_contact_id)
}

