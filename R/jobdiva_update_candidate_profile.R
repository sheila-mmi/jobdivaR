#' JobDiva Update Candidate Profile
#' 
#' This function will update standard candidate profile information in JobDiva
#' 
#' @param jobdiva_candidate_id (type: string) -- a string with the JobDiva candidate id of the candidate
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return status of whether or not the candidate was successfully updated.
#' @export


jobdiva_update_candidate_profile = function(jobdiva_candidate_id
                                  , update_df)
{
  update_df$FIELD = toupper(update_df$FIELD)
  
  # Field = standard JD fields
  {
    standard = {toupper(c('firstName'
                          , 'lastName'
                          , 'email'
                          , 'alternateemail'
                          , 'city'
                          , 'state'
                          , 'phones'))
      }
    standard_exceptions = c('phones')
    
    
    fields_df = update_df[update_df$FIELD %in% standard, ]
    fields_df = fields_df[!fields_df$FIELD %in% toupper(standard_exceptions), ]
    
    if (nrow(fields_df) != 0)
    {
      fields = apply(fields_df, 1, function(x){
        udf = paste0('&'
                     , tolower(as.character(x[1]))
                     , "="
                     , str_replace_all(as.character(x[2]), '&', '%26'))
      })
      
      field_query = paste0(fields, collapse = '')
      field_query = str_replace_all(field_query, ' ', '%20')
      field_query = str_replace_all(field_query, '@', '%40')
      field_query = str_replace_all(field_query, ',', '%2C')
      field_query = str_replace_all(field_query,  '\\)', '%29')
      field_query = str_replace_all(field_query,  '\\(', '%28')
      field_query = str_replace_all(field_query,  '\\|', '%7C')
      field_query = str_replace_all(field_query, 	'\\{', '%7B')
      field_query = str_replace_all(field_query, 	'\\}', '%7D')
      field_query = str_replace_all(field_query, 	'\\:', '%3A')
      
    } else
    {
      field_query = ""
    }
    
    standard_exception_df = update_df[update_df$FIELD %in% toupper(standard_exceptions), ]
    if (nrow(standard_exception_df) != 0)
    {
      standard_exception_query = paste0(as.character(standard_exception_df$CONTENT), collapse = '')
      standard_exception_query = as.character(standard_exception_query)
    } else
    {
      standard_exception_query = ""
    }
  }
  
  if (nchar(field_query) == 0
      && nchar(standard_exception_query) == 0)
  {
    return('No information to update')
  }
  url = paste0('https://api.jobdiva.com/api/jobdiva/updateCandidateProfile?candidateid='
               , jobdiva_candidate_id
               , field_query
               , standard_exception_query)
  
  results = try(httr::POST(url
                           , add_headers("Authorization" = jobdiva_login())
                           , encode = "json"
                           , httr::verbose()), silent = TRUE)
  
  if (class(results)[1] != 'try-error')
  {
    if(as.numeric(results$status_code) == 200)
    {
      return('SUCCESS')
    }
    
    else
    {
      return(paste0('FAILURE:', results))
    }
  }
  else
  {
    return(paste0('FAILURE:', results))
  }
  
  
}

