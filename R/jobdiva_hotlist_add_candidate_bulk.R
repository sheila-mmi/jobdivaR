#' JobDiva Hotlist Add Candidate (bulk)
#' 
#' This function will bulk add candidates to a hotlist 
#' 
#' @param jobdiva_candidate_id_vector (type: vector) -- The vector of candidate ids of candidates to be added to the given hotlist
#' @param hotlist_id (type: string) -- The id of the CANDIDATE hotlist that the candidates should be added to
#' @return A dataframe of entity details for the given entities.
#' @export

jobdiva_hotlist_add_candidate_bulk = function(jobdiva_candidate_id_vector, hotlist_id)
{
  
  jobdiva_candidate_id_vector = as.character(jobdiva_candidate_id_vector)
  clean_jobdiva_candidate_id_vector = lapply(jobdiva_candidate_id_vector, function(x) {
    x = paste0(x, collapse = paste0('&candidateIds='))
    x =  paste0('&candidateIds=', x)
  })
  
  clean_jobdiva_candidate_id_vector = paste0(clean_jobdiva_candidate_id_vector, collapse = '')
  
  results = data.frame()
  
  base_url = "https://api.jobdiva.com/api/hotlist/addCandidatesToHotlist?hotListid="
  
  request = httr::POST(url = paste0(base_url, hotlist_id,clean_jobdiva_candidate_id_vector)
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
  
  results = httr::content(request)
  
  return(results)
}
