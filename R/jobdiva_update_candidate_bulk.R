#' JobDiva Bulk Update candidates
#' 
#' This function will update candidate information in bulk in JobDiva
#' 
#' @param update_df (type: dataframe) -- dataframe with the candidate information to be updated in JD. There must be a JobDiva_candidate_Id.
#' @return 
#' @export


jobdiva_update_candidate_bulk = function(update_df)
{
  updated_success_count = 0
  
  for (i in 1:nrow(update_df))
  {
    temp_df = update_df[i,]
    jd_id = temp_df$JOBDIVA_CANDIDATE_ID
    
    clean_update_df = data.frame('FIELD' = character(0)
                                 , 'CONTENT' = character(0))
    
    for (i in 1:ncol(temp_df))
    {
      content = as.character(temp_df[,i])
      field = colnames(temp_df)[i]
      
      # print(paste0(field, ':', content))
      
      if (toupper(field) == 'EMAIL'
          || toupper(field) == 'WORK_EMAIL'
          || toupper(field) == 'PERSONAL_EMAIL'
          || toupper(field) == 'WORK_PHONE'
          || toupper(field) == 'WORK_NUMBERS'
          || toupper(field) == 'MOBILE_PHONE'
          || toupper(field) == 'PERSONAL_PHONE'
          || toupper(field) == 'PERSONAL_NUMBERS'
          || toupper(field) == 'JOBDIVA_CANDIDATE_ID')
      {
        next
      }

      if (content != 'NULL')
      {
        if (grepl('DATE',field ,ignore.case =  TRUE) && !grepl('CANDIDATE', field, ignore.case = TRUE))
        {
          content = try(format(as.Date(content), '%m/%d/%Y %H:%M'), silent = TRUE)
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
    
    # Candidate Information
    {
      if ('EMAIL' %in% toupper(colnames(temp_df)))
      {
        email = unlist(str_split(temp_df$EMAIL, ';'))[1]
        alternate_email = unlist(str_split(temp_df$EMAIL, ';'))[2]
      } else if ('WORK_EMAIL' %in% toupper(colnames(temp_df)) || 'PERSONAL_EMAIL' %in% toupper(colnames(temp_df)))
      {
        email = unlist(str_split(temp_df$WORK_EMAIL, ';'))[1]
        alternate_email = unlist(str_split(temp_df$WORK_EMAIL, ';'))[2]
        
        if(is.null(email) && is.null(alternate_email))
        {
          email = unlist(str_split(temp_df$PERSONAL_EMAIL, ';'))[1]
          alternate_email = unlist(str_split(temp_df$PERSONAL_EMAIL, ';'))[2]
        } else if (!is.null(email))
        {
          if(is.null(alternate_email))
          {
            alternate_email = unlist(str_split(temp_df$PERSONAL_EMAIL, ';'))[1]
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
      
      tmp = data.frame('FIELD' = c('email', 'alternateemail')
                       , 'CONTENT' = c(email, alternate_email))
      
      clean_update_df = rbind(clean_update_df, tmp)
      
      phones = c()
      
      if ('WORK_PHONE' %in% toupper(colnames(temp_df)))
      {
        work_phones = unlist(str_split(temp_df$WORK_PHONE, ';'))
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
      } else if ('WORK_NUMBERS' %in% toupper(colnames(temp_df)))
      {
        work_phones = unlist(str_split(temp_df$WORK_PHONE, ';'))
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
      
      if ('MOBILE_PHONE' %in% toupper(colnames(temp_df)))
      {
        mobile_phones = unlist(str_split(temp_df$MOBILE_PHONE, ';'))
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
        
      } else if ('PERSONAL_PHONE' %in% toupper(colnames(temp_df)))
      {
        mobile_phones = unlist(str_split(temp_df$PERSONAL_PHONE, ';'))
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
      else if ('MOBILE_NUMBERS' %in% toupper(colnames(temp_df)))
      {
        mobile_phones = unlist(str_split(temp_df$MOBILE_NUMBERS, ';'))
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
    
    # Don't update First + Last Name
    clean_update_df = clean_update_df[!clean_update_df$FIELD %in% c('firstName', 'lastName'), ]
    
    # Fix the fields
    {
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'FIRST_NAME', 'firstName')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'FIRSTNAME', 'firstName')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'LAST_NAME', 'lastName')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'LASTNAME', 'lastName')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'CITY', 'city')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'STATE_PROVINCE', 'state')
      clean_update_df$FIELD = str_replace_all(clean_update_df$FIELD
                                              , 'STATE', 'state')
    }
    
    updated_recs_1 = try(jobdiva_update_candidate_profile(jd_id, clean_update_df), silent = TRUE)
    updated_recs_2 = try(jobdiva_update_candidate_udfs(jd_id, clean_update_df), silent = TRUE)
    
    if (class(updated_recs_1)[1] != 'try-error' 
        || class(updated_recs_2)[1] != 'try-error')
    {
      if (updated_recs_1 == 'SUCCESS' 
          || updated_recs_2 == 'SUCCESS')
      {
        updated_success_count = updated_success_count + 1
      }
    }
    if ('LINKEDIN' %in% toupper(clean_update_df$FIELD))
    {
      linkedin_url = as.character(clean_update_df[which(toupper(clean_update_df$FIELD) == 'LINKEDIN'), c('CONTENT')])
      update_linkedin = try(jobdiva_update_candidate_social_link(jd_id
                                                                 , linkedin_url
                                                                 , 'LinkedIn'))
    }
  }
  
  return(updated_success_count)
}