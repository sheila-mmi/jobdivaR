#' JobDiva Update Candidate User-Defined-Fields
#' 
#' This function will update candidate UDF information in JobDiva
#' 
#' @param jobdiva_candidate_id (type: string) -- a string with the JobDiva candidate id of the candidate
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return status of whether or not the candidate was successfully updated.
#' @export


jobdiva_update_candidate_udfs = function(jobdiva_candidate_id
                                            , update_df)
{
  # Query UDFs
  jd_udfs = jobdiva_udfs()
  
  # Drop irrelevant columns + rows
  cand_udfs = jd_udfs[jd_udfs$FIELDFOR == 'Candidates',]
  
  # Clean field names
  cand_udfs$FIELDNAME = str_replace_all(cand_udfs$FIELDNAME, ' ', '_')
  cand_udfs$FIELDNAME = toupper(cand_udfs$FIELDNAME)
  
  update_df$FIELD = toupper(update_df$FIELD)
  
  # UDF = User-defined JD fields
  {
    udfs_df = update_df[tolower(update_df$FIELD) %in% tolower(c(cand_udfs$FIELDNAME)), ]
    
    if (nrow(udfs_df) != 0)
    {
      udfs = apply(udfs_df, 1, function(x){
        udf_id = as.numeric(cand_udfs[match(x[1], cand_udfs$FIELDNAME), c('ID')])
        if(!is.na(udf_id))
        {
          udf = paste0('&Userfields={%0A  "userfieldId": '
                       , udf_id
                       , ',%0A  "userfieldValue": "'
                       , str_replace_all(as.character(x[2]), '&', '%26') 
                       , '"%0A}')
        }
      })
      
      # udf_query = jsonlite::toJSON(udfs)
      udf_query = paste0(udfs, collapse = '')
      udf_query = as.character(udf_query)
      udf_query = str_replace_all(udf_query, 'NULL','')
      udf_query = str_replace_all(udf_query, 	'\\{', '%7B')
      udf_query = str_replace_all(udf_query, 	'\\}', '%7D')
      udf_query = str_replace_all(udf_query, 	'\\:', '%3A')
      udf_query = str_replace_all(udf_query, '\\,', '%2C')
      udf_query = str_replace_all(udf_query, '"', '%22')
      udf_query = str_replace_all(udf_query, ' ', '%20')
      udf_query = str_replace_all(udf_query, ' ', '%40')
      udf_query = str_replace_all(udf_query,  '\\)', '%29')
      udf_query = str_replace_all(udf_query,  '\\(', '%28')
      udf_query = str_replace_all(udf_query,  '\\|', '%7C')
    }else
    {
      udf_query = ""
    }
    
  }
  
  url = paste0('https://api.jobdiva.com/api/jobdiva/updateCandidateUserfields?candidateid='
               , jobdiva_candidate_id
               , '&overwrite=true'
               , udf_query)
  
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

