#' JobDiva Merge Contact
#' 
#' This function will merge the dup contact to the master contact (and all of their activity) in JD.
#' 
#' @param dup_id_vec The contact id for the record to be merged into the main/master record
#' @param master_id_vec The contact id for the record to inherit all of the notes and information from the dup record.
#' @return a status indicating either success or failure                   
#' @export

jobdiva_merge_contact_bulk = function(dup_id_vec, master_id_vec)
{
  success_count = 0
  for (i in 1:length(dup_id_vec))
  {
    dup_id = dup_id_vec[i]
    master_id = master_id_vec[i]
    
    merge_action = try(jobdiva_merge_contact(dup_id, master_id), silent = TRUE)
    if (class(merge_action)[1] != 'try-error')
    {
      if(merge_action == TRUE)
      {
        success_count = success_count + 1
      }
    }
  }
  
  return(success_count)
}
