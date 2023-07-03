#' JobDiva Update Candidate Social Network Link
#' 
#' This function will update candidate Social Network links information in JobDiva
#' 
#' @param jobdiva_candidate_id (type: string) -- a string with the JobDiva candidate id of the candidate
#' to be updated
#' @param update_df (type: dataframe) -- dataframe with the first column as 'FIELD' and second as 'CONTENT'
#' @return 
#' @export


jobdiva_update_candidate_social_link = function(jobdiva_candidate_id
                                                 , social_network_link, social_network_name)
{
  query = paste0('&socialnetworks={%0A  "link": "'
                     , social_network_link
                     , '",%0A'
                     , '  "name": "'
                     , social_network_name
                     , '"%0A}')
  
  query = as.character(query)
  query = str_replace_all(query, 	'\\{', '%7B')
  query = str_replace_all(query, 	'\\}', '%7D')
  query = str_replace_all(query, 	'\\:', '%3A')
  query = str_replace_all(query, '\\,', '%2C')
  query = str_replace_all(query, '"', '%22')
  query = str_replace_all(query, ' ', '%20')
  query = str_replace_all(query, ' ', '%40')
  
  url = paste0("https://api.jobdiva.com/api/jobdiva/updateCandidateSNLinks?candidateid="
               , jobdiva_candidate_id
               , query)
  
  results = try(httr::POST(url
                           , add_headers("Authorization" = jobdiva_login_v2())
                           , encode = "json"
                           , httr::verbose()), silent = TRUE)
  print(content(results))
  
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
