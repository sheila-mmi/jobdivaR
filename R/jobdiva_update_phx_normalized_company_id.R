#' JobDiva Update Company PHX Normalized Company id
#' 
#' This function will update contact PHX_Normalized_Company_ID in JobDiva
#' 
#' @param jobdiva_company_id (type: string) -- The company id
#' @param phx_normalized_company_id (type: string) -- The Normalized_Company id of the company in PHX to upload to JobDiva
#' @return status of whether or not the company's phx_normalized_company_id was successfully updated.
#' @export


jobdiva_update_phx_normalized_company_id = function(jobdiva_company_id
                                          , phx_normalized_company_id)
{
  tmp = data.frame('FIELD' = c('PHX_Normalized_Company_Id')
                   , 'CONTENT' = c(phx_normalized_company_id))
  
  updated = try(jobdiva_update_company(jobdiva_company_id
                                       , tmp), silent = TRUE)
  
  return(updated)
  
}

