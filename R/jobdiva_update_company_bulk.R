#' JobDiva Bulk Update Companies
#' 
#' This function will update company information in bulk in JobDiva
#' 
#' @param update_df (type: dataframe) -- dataframe with the company information to be updated in JD. There must be a JobDiva_Company_Id.
#' @return 
#' @export

jobdiva_update_company_bulk = function(update_df)
{
  updated_success_count = 0
  
  for (i in 1:nrow(update_df))
  {
    temp_df = update_df[i,]
    jd_id = temp_df$JOBDIVA_COMPANY_ID
    temp_df = temp_df[,!colnames(temp_df) %in% c('DATE_CREATED'
                                                 , 'COMPANY_ID'
                                                 , 'COMPANY_NAME'
                                                 , 'NORMALIZED_COMPANY_NAME'
                                                 , 'JOBDIVA_COMPANY_ID'
                                                 , 'COMPANY_WEBSITE')]
    
    clean_update_df = data.frame('FIELD' = character(0)
                                 , 'CONTENT' = character(0))
    
    for (i in 1:ncol(temp_df))
    {
      content = as.character(temp_df[,i])
      if (content != 'NULL')
      {
        if (grepl('DATE',colnames(temp_df)[i] ,ignore.case =  TRUE))
        {
          content = format(as.Date(content), '%m/%d/%Y %H:%M')
        }
        tmp = data.frame('FIELD' = colnames(temp_df)[i]
                         , 'CONTENT' = content)
        
        clean_update_df = rbind(clean_update_df, tmp)
      }
    }
    
    update_recs = try(jobdiva_update_company(jd_id, clean_update_df), silent = TRUE)
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