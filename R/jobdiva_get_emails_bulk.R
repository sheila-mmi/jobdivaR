#' Query emails for a dataframe of contacts or candidates
#' 
#' This function will return the HTTP response from get methods
#' in Nutshell
#' 
#' @param entity (type: string) -- a string indicating which entity to get 
#'                         (Candidate or Contact)
#' @param entity_id (type: string) -- Entity of the contact/candidate you want to query emails for
#' @return Content from the HTTP response of the search                     
#' @export

jobdiva_get_emails_bulk = function(creation_df, entity)
{
  creation_df = as.data.frame(creation_df)
  colnames(creation_df) = toupper(colnames(creation_df))
  final_list = list()
  
  for (i in 1:nrow(creation_df))
  {
    tmp = creation_df[i,]
    idcol = paste0(toupper(entity), 'ID')
    id = as.character(tmp[idcol])
    tmp = try(jobdiva_get_emails(entity, id), silent = TRUE)
    if (length(tmp) > 0)
    {
      final_list[[i]] = tmp
    }
  }
}
