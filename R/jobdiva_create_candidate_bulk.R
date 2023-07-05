#' JobDiva Bulk Candidate Creation
#' 
#' This function will create candidates in JobDiva in bulk
#' 
#' @param creation_df (type: dataframe) -- a dataframe w/ the columns to create the candidate
#'  (must at least contain first and last names otherwise the candidate will not be created. Currently does not handle phone numbers) 
#' @return A string that is the JobDiva candidate id of newly created candidate
#' @export


jobdiva_create_candidate_bulk = function(creation_df)
{
  candidate_ids = c()
  colnames(creation_df) = toupper(colnames(creation_df))
  
  for (i in 1:nrow(creation_df))
  {
    x = creation_df[i, ]
    first_name = x$FIRST_NAME
    last_name =x$LAST_NAME
    title = as.character(x$TITLE)

    if ('EMAIL' %in% toupper(colnames(x)))
    {
      email = unlist(str_split(x$EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$EMAIL, ';'))[2]
    } else if ('WORK_EMAIL' %in% toupper(colnames(x)) || 'PERSONAL_EMAIL' %in% toupper(colnames(x)))
    {
      email = unlist(str_split(x$WORK_EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$WORK_EMAIL, ';'))[2]
      
      email = ifelse(email == '', NA, email)
      alternate_email = ifelse(alternate_email == '', NA, alternate_email)
      
      if((is.null(email) || is.na(email)) && (is.null(alternate_email) || is.na(alternate_email)))
      {
        email = unlist(str_split(x$PERSONAL_EMAIL, ';'))[1]
        alternate_email = unlist(str_split(x$PERSONAL_EMAIL, ';'))[2]
      } else if (!is.null(email))
      {
        if(is.null(alternate_email))
        {
          alternate_email = unlist(str_split(x$PERSONAL_EMAIL, ';'))[1]
        }
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
    
    phx_employee_id = x$EMPLOYEE_ID
    if (is.null(phx_employee_id) & ('LEGACY_ID' %in% toupper(colnames(creation_df))))
    {
      phx_employee_id = x$LEGACY_ID
    }
    
    linkedin = x$LINKEDIN
    
    # Address
    {
      if ('STATE' %in% toupper(colnames(creation_df)))
      {
        state = x$STATE
      } else if ('STATE_PROVINCE' %in% toupper(colnames(creation_df)))
      {
        state = x$STATE_PROVINCE
      } else
      {
        state = NULL
      }
      
      if ('CITY' %in% toupper(colnames(creation_df)))
      {
        city = x$CITY
      } else
      {
        city = ""
      }
      
    }
    
    # print(phones)
    
    # print(paste(ifelse(is.null(first_name), "", first_name)
    #             , ifelse(is.null(last_name),"",last_name)
    #             , ifelse(is.null(title), "", title)
    #             , ifelse(is.null(company_name), "",company_name)
    #             , ifelse(is.null(email), "",email)
    #             , ifelse(is.null(alternate_email), "",alternate_email)
    #             , ifelse(is.null(phx_employee_id), "",phx_employee_id)))
    
    single = try(jobdiva_create_candidate(first_name = ifelse(is.null(first_name), "", first_name)
                                        , last_name = ifelse(is.null(last_name),"",last_name)
                                        , title = ifelse(is.null(title), "", title)
                                        , email = ifelse(is.null(email), "",email)
                                        , alternate_email =  ifelse(is.null(alternate_email), "",alternate_email)
                                        , phx_employee_id = ifelse(is.null(phx_employee_id), "",phx_employee_id)
                                        , work_phone = ""
                                        , cell_phone = ""
                                        , linkedin = ifelse(is.null(linkedin), "",linkedin)
                                        , city = ifelse(is.null(city), "",city)
                                        , state = ifelse(is.null(state), "",state)
                                        ), silent = TRUE)
    if (class(single)[1] != 'try-error')
    {
      candidate_ids = c(candidate_ids, single)
      
      # Then add the phone numbers
      {
        phone_df = x[,grepl('PHONE',toupper(colnames(x)))]
        clean_update_df = data.frame('FIELD' = character(0)
                                     , 'CONTENT' = character(0))
        
        phones = c()
        
        if ('WORK_PHONE' %in% toupper(colnames(x)))
        {
          work_phones = unlist(str_split(x$WORK_PHONE, ';'))
          # print(work_phones)
          # print(x)
          if (length(work_phones) > 0 & work_phones != "")
          {
            work_phone_queries = sapply(work_phones, function(phone){
              print(phone)
              
              paste0('&phones={%0A  "action": '
                     , 1
                     , ',%0A  "ext": ""'
                     , ',%0A  "phone": "'
                     , phone
                     , '",%0A  "type": "w"%0A}')
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
        } else if ('WORK_NUMBERS' %in% toupper(colnames(x)))
        {
          work_phones = unlist(str_split(x$WORK_PHONE, ';'))
          # print(work_phones)
          # print(x)
          if (length(work_phones) > 0 & work_phones != "")
          {
            work_phone_queries = sapply(work_phones, function(phone){
              print(phone)
              paste0('&phones={%0A  "action": '
                     , 1
                     , ',%0A  "ext": ""'
                     , ',%0A  "phone": "'
                     , phone
                     , '",%0A  "type": "w"%0A}')
              
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
        
        if ('MOBILE_PHONE' %in% toupper(colnames(x)))
        {
          mobile_phones = unlist(str_split(x$MOBILE_PHONE, ';'))
          if (length(mobile_phones) > 0 & mobile_phones != "")
          {
            mobile_phone_queries = sapply(mobile_phones, function(phone){
              paste0('&phones={%0A  "action": '
                     , 1
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
          
        } else if ('PERSONAL_PHONE' %in% toupper(colnames(x)))
        {
          mobile_phones = unlist(str_split(x$PERSONAL_PHONE, ';'))
          if (length(mobile_phones) > 0 & mobile_phones != "")
          {
            mobile_phone_queries = sapply(mobile_phones, function(phone){
              paste0('&phones={%0A  "action": '
                     , 1
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
        else if ('MOBILE_NUMBERS' %in% toupper(colnames(x)))
        {
          mobile_phones = unlist(str_split(x$MOBILE_NUMBERS, ';'))
          if (length(mobile_phones) > 0 & mobile_phones != "")
          {
            mobile_phone_queries = sapply(mobile_phones, function(phone){
              paste0('&phones={%0A  "action": '
                     , 1
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
        } else 
        {
          update_phones = try(jobdiva_update_candidate_profile(single, clean_update_df))
          
        }
      }
      
    }
    else
    {
      candidate_ids = c(candidate_ids, "")
    }
  }
  
  return(candidate_ids)
}

