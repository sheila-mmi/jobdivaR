#' JobDiva Update Contact
#' 
#' This function will update contact information in JobDiva
#' 
#' @param jobdiva_contact_id (type: string) -- a string with the JobDiva contact id of the contact
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return 
#' @export


jobdiva_update_contact = function(jobdiva_contact_id
                                  , update_df)
{
  # Query UDFs
  jd_udfs = jobdiva_udfs()
  
  # Drop irrelevant columns + rows
  cont_udfs = jd_udfs[jd_udfs$FIELDFOR == 'Contacts',]
  
  # Clean field names
  cont_udfs$FIELDNAME = str_replace_all(cont_udfs$FIELDNAME, ' ', '_')
  cont_udfs$FIELDNAME = toupper(cont_udfs$FIELDNAME)
  
  update_df$FIELD = toupper(update_df$FIELD)
  
  # Field = standard JD fields
  {
    standard = {toupper(c('firstname'
                          , 'lastname'
                          , 'title'
                          , 'companyid'
                          , 'department'
                          , 'reportsto'
                          , 'active'
                          , 'primary'
                          , 'email'
                          , 'alternateemail'
                          , 'types'
                          , 'assistantname'
                          , 'assistantemail'
                          , 'assistantphone'
                          , 'assistantphoneextension'
                          , 'subguidelines'
                          , 'maxsubmittals'
                          , 'references'
                          , 'drugtest'
                          , 'backgroundcheck'
                          ,'securityclearance'
                          ,'phones'
                          ,'addresses'
                          ,'owners'))}
    
    fields_df = update_df[update_df$FIELD %in% standard, ]
    
    if (nrow(fields_df) != 0)
    {
      fields = apply(fields_df, 1, function(x){
        udf = paste0('&'
                     , tolower(as.character(x[1]))
                     , "="
                     , as.character(x[2]))
      })
      
      field_query = paste0(fields, collapse = '')
      field_query = str_replace_all(field_query, ' ', '%20')
      field_query = str_replace_all(field_query, '@', '%40')
    }
    else
    {
      field_query = ""
    }
  }
  
  # UDF = User-defined JD fields
  {
    udfs_df = update_df[!update_df$FIELD %in% standard, ]
    
    if (nrow(udfs_df) != 0)
    {
      udfs = apply(udfs_df, 1, function(x){
        udf_id = as.numeric(cont_udfs[match(x[1], cont_udfs$FIELDNAME), c('ID')])
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
      udf_query = str_replace_all(udf_query, ' ', '%40')
    }
    
    else
    {
      udf_query = ""
    }
    
  }
  
  url = paste0('https://api.jobdiva.com/api/jobdiva/updateContact?contactid='
               , jobdiva_contact_id
               , field_query
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

