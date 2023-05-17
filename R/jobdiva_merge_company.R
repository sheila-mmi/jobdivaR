#' JobDiva Merge Company
#' 
#' This function will merge the dup Company to the master Company (and all of their activity) in JD.
#' 
#' @param dup_id The Company id for the record to be merged into the main/master record
#' @param master_id The Company id for the record to inherit all of the notes and information from the dup record.
#' @return a status indicating either success or failure                   
#' @export

jobdiva_merge_company = function(dup_id, master_id)
{
  base_url = "https://api.jobdiva.com/apiv2/jobdiva/mergeCompany?mainCompanyId="
  
  full_url = paste0(base_url
                    , master_id
                    , '&mergedCompanyId='
                    , dup_id)
  request = httr::POST(url = full_url
                       , add_headers("Authorization" = jobdiva_login_v2())
                       , encode = "json"
                       , httr::verbose()
  )
  
  status = httr::content(request)
  return(status)
  
}
