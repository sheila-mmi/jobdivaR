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
  jobdiva_candidate_id_vector = split(jobdiva_candidate_id_vector, ceiling(seq_along(jobdiva_candidate_id_vector)/100))
  
  clean_jobdiva_candidate_id_vector = lapply(jobdiva_candidate_id_vector, function(x) {
    x = paste0(x, collapse = paste0('&candidateIds='))
    x =  paste0('&candidateIds=', x)
  })
  
  # clean_jobdiva_candidate_id_vector = paste0(clean_jobdiva_candidate_id_vector, collapse = '')
  
  results = list()
  
  base_url = "https://api.jobdiva.com/api/hotlist/addCandidatesToHotlist?hotListid="
  
  for (i in 1:length(clean_jobdiva_candidate_id_vector))
  {
    request = try(httr::POST(url = paste0(base_url, hotlist_id,clean_jobdiva_candidate_id_vector[i])
                         , add_headers("Authorization" = jobdiva_login())
                         , encode = "json"
                         , httr::verbose()), silent = TRUE)
    
    # Error Log Check
    {
      user = 'swarrick01'
      key_3_0 = phx_db_login(user, '1BeanzPaddle!', 'aobi', 'Phoenix_3_0')
      error_log_check = try(jobdiva_error_log('jobdiva_hotlist_add_candidate_bulk'
                                              , request
                                              , key_3_0
                                              , user
                                              , '~/jobdiva_error_log_temp.csv')
                            , silent = TRUE)
    }
    
    # Remove existing candidates from 
    curr_code = as.numeric(status_code(request))
    curr_vec = clean_jobdiva_candidate_id_vector[i]
    while (curr_code == 500)
    {
      curr_mesg = content(request)
      curr_mesg = curr_mesg$message
      candidate_to_remove = str_sub(curr_mesg, str_locate(curr_mesg, '\\[')[1] + 1, str_locate(curr_mesg, '\\]')[1] -1)
      new_vec = str_remove(curr_vec, paste0('&candidateIds=', candidate_to_remove))
                           
      request = try(httr::POST(url = paste0(base_url, hotlist_id,new_vec)
                               , add_headers("Authorization" = jobdiva_login())
                               , encode = "json"
                               , httr::verbose()), silent = TRUE)
      
      curr_code = as.numeric(status_code(request))
      curr_vec = new_vec
    }
    
    temp_results = httr::content(request)
    results[[i]] = temp_results
  }
  
  return(results)
}
