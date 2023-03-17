#' JobDiva Update Company
#' 
#' This function will update company information in JobDiva
#' 
#' @param jobdiva_company_id (type: string) -- a string with the JobDiva company id of the company 
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return 
#' @export


jobdiva_update_company = function(jobdiva_company_id
                                  , update_df)
{
  # Query UDFs
  jd_udfs = jobdiva_udfs()
  
  # Drop irrelevant columns + rows
  comp_udfs = jd_udfs[jd_udfs$FIELDFOR == 'Companies',]
  
  # Clean field names
  comp_udfs$FIELDNAME = str_replace_all(comp_udfs$FIELDNAME, ' ', '_')
  comp_udfs$FIELDNAME = toupper(comp_udfs$FIELDNAME)
  
  update_df$FIELD = toupper(update_df$FIELD)
  
  # Format 
  
  udfs = apply(update_df, 1, function(x){
    udf_id = as.numeric(comp_udfs[match(x[1], comp_udfs$FIELDNAME), c('ID')])
    # jsonlite::toJSON(list(userfieldId = udf_id
    #           , userfieldValue = as.character(x[2])))
    udf = paste0('&Userfields={%0A  "userfieldId": '
                 , udf_id
                 , ',%0A  "userfieldValue": "'
                 , as.character(x[2])
                 , '"%0A}')
  })
  
  # udf_query = jsonlite::toJSON(udfs)
  udf_query = paste0(udfs, collapse = '')
  udf_query = as.character(udf_query)
  udf_query = str_replace_all(udf_query, 	'\\{', '%7B')
  udf_query = str_replace_all(udf_query, 	'\\}', '%7D')
  udf_query = str_replace_all(udf_query, 	'\\:', '%3A')
  udf_query = str_replace_all(udf_query, '\\,', '%2C')
  udf_query = str_replace_all(udf_query, '"', '%22')
  udf_query = str_replace_all(udf_query, ' ', '%20')

  
  
  url = paste0('https://api.jobdiva.com/api/jobdiva/updateCompany?companyid='
               , jobdiva_company_id
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
      return('FAILURE')
    }
  }
  else
  {
    return('FAILURE')
  }
  

}

