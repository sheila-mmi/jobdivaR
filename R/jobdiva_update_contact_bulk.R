#' JobDiva Bulk Update Contacts
#' 
#' This function will update contact information in bulk in JobDiva
#' 
#' @param update_df (type: dataframe) -- dataframe with the contact information to be updated in JD. There must be a JobDiva_Contact_Id.
#' @return 
#' @export


jobdiva_update_contact_bulk = function(update_df)
{
  updated_success_count = 0
  
  for (i in 1:nrow(update_df))
  {
    temp_df = update_df[i,]
    jd_id = temp_df$JOBDIVA_CONTACT_ID
    
    clean_update_df = data.frame('FIELD' = character(0)
                                 , 'CONTENT' = character(0))

    for (i in 1:ncol(temp_df))
    {
      content = as.character(temp_df[,i])
      field = colnames(temp_df)[i]
      
      if (toupper(field) == 'STATE'
          || toupper(field) == 'STATE_PROVINCE'
          || toupper(field) == 'CITY'
          || toupper(field) == 'EMAIL'
          || toupper(field) == 'WORK_EMAIL'
          || toupper(field) == 'PERSONAL_EMAIL'
          || toupper(field) == 'WORK_PHONE'
          || toupper(field) == 'WORK_NUMBERS'
          || toupper(field) == 'MOBILE_PHONE'
          || toupper(field) == 'PERSONAL_PHONE'
          || toupper(field) == 'PERSONAL_NUMBERS')
      {
        next
      }
      
      if (content != 'NULL')
      {
        if (grepl('DATE',field ,ignore.case =  TRUE))
        {
          content = format(as.Date(content), '%m/%d/%Y %H:%M')
        }
        
        if (grepl('EMPLOYEE_ID', toupper(field)) || grepl('LEGACY_ID', toupper(field)))
        {
          field = 'PHX_Employee_Id'
        }
        
        tmp = data.frame('FIELD' = field
                         , 'CONTENT' = content)
        
        clean_update_df = rbind(clean_update_df, tmp)
      }
    }
    
    # Address
    {
      if ('STATE' %in% toupper(colnames(update_df)))
      {
        state = temp_df$STATE
      } else if ('STATE_PROVINCE' %in% toupper(colnames(update_df)))
      {
        state = temp_df$STATE_PROVINCE
      } else
      {
        state = NULL
      }
      
      if ('CITY' %in% toupper(colnames(update_df)))
      {
        city = temp_df$CITY
      } else
      {
        city = ""
      }
      
      if(!is.null(state))
      {
        addresses = paste0('&addresses={%0A  "action": '
                           , 0
                           , ',%0A'
                           , '  "address1": "",%0A'
                           , '  "address2": "",%0A'
                           , '  "city": "'
                           , city
                           , '",%0A'
                           , '  "countryId": "",%0A'
                           , '  "defaultAddress": "true",%0A'
                           , '  "deleted": "false",%0A'
                           , '  "freeText": "",%0A'
                           , '  "id": 0,%0A'
                           , '  "state": "'
                           , state
                           , '",%0A'
                           , '  "address2": "",%0A'
                           , '  "zipCode": ""%0A}')
        
        addresses = paste0(addresses, collapse = '')
        addresses = as.character(addresses)
        addresses = str_replace_all(addresses, 	'\\{', '%7B')
        addresses = str_replace_all(addresses, 	'\\}', '%7D')
        addresses = str_replace_all(addresses, 	'\\:', '%3A')
        addresses = str_replace_all(addresses, '\\,', '%2C')
        addresses = str_replace_all(addresses, '"', '%22')
        addresses = str_replace_all(addresses, ' ', '%20')
        addresses = str_replace_all(addresses, ' ', '%40')
      } else
      {
        addresses = ""
      }
      
      if (addresses != '')
      {
        tmp = data.frame('FIELD' = c('addresses')
                         , 'CONTENT' = c(addresses))
        
        clean_update_df = rbind(clean_update_df, tmp)
      }
    }
    
    # Contact Information
    {
      if ('EMAIL' %in% toupper(colnames(update_df)))
      {
        email = unlist(str_split(temp_df$EMAIL, ';'))[1]
        alternate_email = unlist(str_split(temp_df$EMAIL, ';'))[2]
      } else if ('WORK_EMAIL' %in% toupper(colnames(update_df)) & 'PERSONAL_EMAIL' %in% toupper(colnames(update_df)))
      {
        email = unlist(str_split(temp_df$WORK_EMAIL, ';'))[1]
        alternate_email = unlist(str_split(temp_df$WORK_EMAIL, ';'))[2]
        if(is.null(alternate_email))
        {
          alternate_email = unlist(str_split(temp_df$PERSONAL_EMAIL, ';'))[1]
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
      
      tmp = data.frame('FIELD' = c('email', 'alternateemail')
                       , 'CONTENT' = c(email, alternate_email))
      
      clean_update_df = rbind(clean_update_df, tmp)
      
      phones = c()
      
      if ('WORK_PHONE' %in% toupper(colnames(update_df)))
      {
        work_phones = unlist(str_split(temp_df$WORK_PHONE, ';'))
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
      } else if ('WORK_NUMBERS' %in% toupper(colnames(update_df)))
      {
        work_phones = unlist(str_split(temp_df$WORK_PHONE, ';'))
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
      
      if ('MOBILE_PHONE' %in% toupper(colnames(update_df)))
      {
        mobile_phones = unlist(str_split(temp_df$MOBILE_PHONE, ';'))
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
        
      } else if ('PERSONAL_PHONE' %in% toupper(colnames(update_df)))
      {
        mobile_phones = unlist(str_split(temp_df$PERSONAL_PHONE, ';'))
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
      else if ('MOBILE_NUMBERS' %in% toupper(colnames(update_df)))
      {
        mobile_phones = unlist(str_split(temp_df$MOBILE_NUMBERS, ';'))
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
        
        tmp = data.frame('FIELD' = 'phones'
                         , 'CONTENT' = phones)
        
        clean_update_df = rbind(clean_update_df, tmp)
      }
    }
    
    # Drop any empty content
    clean_update_df = clean_update_df[nchar(as.character(clean_update_df$CONTENT)) > 0,]
    
    update_recs = try(jobdiva_update_contact(jd_id, clean_update_df), silent = TRUE)
    if (class(updated_recs)[1] != 'try-error')
    {
      if(update_recs == 'SUCCESS')
      {
        updated_success_count = updated_success_count + 1
      }
    }
  }
  
  return(updated_success_count)
}