#' JobDiva Bulk Company Creation
#' 
#' This function will create companies in JobDiva in bulk
#' 
#' @param creation_df (type: dataframe) -- a dataframe w/ the columns to create the company (must at least contain company_name, but can also take company_website and normalized_company_id, columns MUST be named as such, but is case insensitive)
#' @return A string that is the JobDiva contact id of newly created contact
#' @export


jobdiva_create_company_bulk = function(creation_df)
{
  company_ids = c()
  colnames(creation_df) = toupper(colnames(creation_df))
 
  for (i in 1:nrow(creation_df))
  {
    name = creation_df$COMPANY_NAME[i]
    if(is.null(name))
    {
      company_ids = c(company_ids, 'ERROR: Company Name not found.')
      next
    }
    url = creation_df$COMPANY_WEBSITE[i]
    phx_norm_comp = creation_df$NORMALIZED_COMPANY_ID[i]
    
    tmp_comp = jobdiva_create_company(name
                                      , url
                                      , phx_norm_comp)
    company_ids = c(company_ids, tmp_comp)
  }
  
  return(company_ids)
}

