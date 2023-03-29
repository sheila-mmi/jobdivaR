#' JobDiva Update Contact Social Network Link
#' 
#' This function will update contact Social Network links information in JobDiva
#' 
#' @param jobdiva_contact_id (type: string) -- a string with the JobDiva contact id of the contact
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return 
#' @export


jobdiva_update_contact_social_link_v2 = function(jobdiva_contact_id
                                  , social_network_link, social_network_name)
{
  
  query = list()
  query[['id']] = jobdiva_contact_id
  query[['socialnetworks']] = list(list("link" = social_network_link
                                    , "name" = social_network_name))

  url = "https://api.jobdiva.com/apiv2/jobdiva/updateContactSNLinks"
  
  results = try(httr::POST(url
                           , add_headers("Authorization" = jobdiva_login_v2())
                           , encode = "json"
                           , body = query
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
