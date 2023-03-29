#' JobDiva Bulk Contact Creation
#' 
#' This function will create contacts in JobDiva in bulk
#' 
#' @param creation_df (type: dataframe) -- a dataframe w/ the columns to create the contact
#'  (must at least contain first and last names otherwise the contact will not be created. Currently does not handle phone numbers) 
#' @return A string that is the JobDiva contact id of newly created contact
#' @export


jobdiva_create_contact_bulk = function(creation_df, type)
{
  contact_ids = c()
  colnames(creation_df) = toupper(colnames(creation_df))
  
  for (i in 1:nrow(creation_df))
  {
    x = creation_df[i, ]
    first_name = x$FIRST_NAME
    last_name =x$LAST_NAME
    title = as.character(x$TITLE)
    company_name = x$COMPANY_NAME

    if ('EMAIL' %in% toupper(colnames(creation_df)))
    {
      email = unlist(str_split(x$EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$EMAIL, ';'))[2]
    } else if ('WORK_EMAIL' %in% toupper(colnames(creation_df)) & 'PERSONAL_EMAIL' %in% toupper(colnames(creation_df)))
    {
      email = unlist(str_split(x$WORK_EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$WORK_EMAIL, ';'))[2]
      if(is.null(alternate_email))
      {
        alternate_email = unlist(str_split(x$PERSONAL_EMAIL, ';'))[1]
      }
      
      if (is.null(email)
          || is.na(email))
      {
        email = ""
      }
      
      if(is.null(alternate_email)
         || is.na(alternate_email))
      {
        alternate_email = ""
      }
      
    } else {
      email = ""
      alternate_email = ""
    }
    
    phones = c()
    
    if ('WORK_PHONE' %in% toupper(colnames(creation_df)))
    {
      work_phones = unlist(str_split(x$WORK_PHONE, ';'))
      # print(work_phones)
      # print(x)
      if (length(work_phones) > 0 & work_phones != "")
      {
        work_phone_queries = sapply(work_phones, function(phone){
          print(phone)
          paste0('&phones={%0A  "action": '
                 , 0
                 , ',%0A  "phone": "'
                 , phone
                 , '",%0A "type": "w"%0A,%0A  "ext": ""%0A}')
        })
        
        work_phone_queries = paste0(work_phone_queries, collapse = '')
        work_phone_queries = as.character(work_phone_queries)
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\{', '%7B')
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\}', '%7D')
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\:', '%3A')
        work_phone_queries = str_replace_all(work_phone_queries, '\\,', '%2C')
        work_phone_queries = str_replace_all(work_phone_queries, '"', '%22')
        work_phone_queries = str_replace_all(work_phone_queries, ' ', '%20')
        work_phone_queries = str_replace_all(work_phone_queries, ' ', '%40')
        # work_phone_queries = str_replace_all(work_phone_queries, '\\(', '%28')
        # work_phone_queries = str_replace_all(work_phone_queries, '\\)', '%29')
        phones = c(phones, work_phone_queries)
      }
    } else if ('WORK_NUMBERS' %in% toupper(colnames(creation_df)))
    {
      work_phones = unlist(str_split(x$WORK_PHONE, ';'))
      # print(work_phones)
      # print(x)
      if (length(work_phones) > 0 & work_phones != "")
      {
        work_phone_queries = sapply(work_phones, function(phone){
          print(phone)
          paste0('&phones={%0A  "action": '
                 , 0
                 , ',%0A  "phone": "'
                 , phone
                 , '",%0A "type": "w"%0A,%0A  "ext": ""%0A}')
        })
        
        work_phone_queries = paste0(work_phone_queries, collapse = '')
        work_phone_queries = as.character(work_phone_queries)
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\{', '%7B')
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\}', '%7D')
        work_phone_queries = str_replace_all(work_phone_queries, 	'\\:', '%3A')
        work_phone_queries = str_replace_all(work_phone_queries, '\\,', '%2C')
        work_phone_queries = str_replace_all(work_phone_queries, '"', '%22')
        work_phone_queries = str_replace_all(work_phone_queries, ' ', '%20')
        work_phone_queries = str_replace_all(work_phone_queries, ' ', '%40')
        # work_phone_queries = str_replace_all(work_phone_queries, '\\(', '%28')
        # work_phone_queries = str_replace_all(work_phone_queries, '\\)', '%29')
        phones = c(phones, work_phone_queries)
      }
    }
    
    if ('MOBILE_PHONE' %in% toupper(colnames(creation_df)))
    {
      mobile_phones = unlist(str_split(x$MOBILE_PHONE, ';'))
      if (length(mobile_phones) > 0 & mobile_phones != "")
      {
        mobile_phone_queries = sapply(mobile_phones, function(phone){
          paste0('&phones={%0A  "action": '
                 , 0
                 , ',%0A  "phone": "'
                 , phone
                 , '",%0A  "type": "c"%0A}')
        })
        
        mobile_phone_queries = paste0(mobile_phone_queries, collapse = '')
        mobile_phone_queries = as.character(mobile_phone_queries)
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\{', '%7B')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\}', '%7D')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\:', '%3A')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\,', '%2C')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '"', '%22')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%20')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%40')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\(', '%28')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\)', '%29')
        phones = c(phones, mobile_phone_queries)
      }
      
    } else if ('PERSONAL_PHONE' %in% toupper(colnames(creation_df)))
    {
      mobile_phones = unlist(str_split(x$PERSONAL_PHONE, ';'))
      if (length(mobile_phones) > 0 & mobile_phones != "")
      {
        mobile_phone_queries = sapply(mobile_phones, function(phone){
          paste0('&phones={%0A  "action": '
                 , 0
                 , ',%0A  "phone": "'
                 , phone
                 , '",%0A  "type": "c"%0A}')
        })
        
        mobile_phone_queries = paste0(mobile_phone_queries, collapse = '')
        mobile_phone_queries = as.character(mobile_phone_queries)
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\{', '%7B')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\}', '%7D')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\:', '%3A')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\,', '%2C')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '"', '%22')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%20')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%40')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\(', '%28')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\)', '%29')
        phones = c(phones, mobile_phone_queries)
      }
      
    }
    else if ('MOBILE_NUMBERS' %in% toupper(colnames(creation_df)))
    {
      mobile_phones = unlist(str_split(x$MOBILE_NUMBERS, ';'))
      if (length(mobile_phones) > 0 & mobile_phones != "")
      {
        mobile_phone_queries = sapply(mobile_phones, function(phone){
          paste0('&phones={%0A  "action": '
                 , 0
                 , ',%0A  "phone": "'
                 , phone
                 , '",%0A  "type": "c"%0A}')
        })
        
        mobile_phone_queries = paste0(mobile_phone_queries, collapse = '')
        mobile_phone_queries = as.character(mobile_phone_queries)
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\{', '%7B')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\}', '%7D')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, 	'\\:', '%3A')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\,', '%2C')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, '"', '%22')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%20')
        mobile_phone_queries = str_replace_all(mobile_phone_queries, ' ', '%40')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\(', '%28')
        # mobile_phone_queries = str_replace_all(mobile_phone_queries, '\\)', '%29')
        phones = c(phones, mobile_phone_queries)
      }
      
    }
    
    if(length(phones) == 0)
    {
      phones = NULL
    }else
    {
      phones = paste0(phones, collapse = "")
    }
    
    phx_employee_id = x$EMPLOYEE_ID
    if (is.null(phx_employee_id) & ('LEGACY_ID' %in% toupper(colnames(creation_df))))
    {
      phx_employee_id = x$LEGACY_ID
    }
    
    linkedin = x$LINKEDIN

    # print(phones)
  
    # print(paste(ifelse(is.null(first_name), "", first_name)
    #             , ifelse(is.null(last_name),"",last_name)
    #             , ifelse(is.null(title), "", title)
    #             , ifelse(is.null(company_name), "",company_name)
    #             , ifelse(is.null(email), "",email)
    #             , ifelse(is.null(alternate_email), "",alternate_email)
    #             , ifelse(is.null(phx_employee_id), "",phx_employee_id)))
    
    single = try(jobdiva_create_contact(first_name = ifelse(is.null(first_name), "", first_name)
                                    , last_name = ifelse(is.null(last_name),"",last_name)
                                    , title = ifelse(is.null(title), "", title)
                                    , company_name = ifelse(is.null(company_name), "",company_name)
                                    , email = ifelse(is.null(email), "",email)
                                    , type = type
                                    , alternate_email =  ifelse(is.null(alternate_email), "",alternate_email)
                                    , phx_employee_id = ifelse(is.null(phx_employee_id), "",phx_employee_id)
                                    , phone_numbers = ifelse(is.null(phones), "",phones)
                                    , linkedin = ifelse(is.null(linkedin), "",linkedin)), silent = TRUE)
    if (class(single)[1] != 'try-error')
    {
      contact_ids = c(contact_ids, single)
    }
    else
    {
      contact_ids = c(contact_ids, "")
    }
    }
  
  return(contact_ids)
}

