#' JobDiva Hotlist Add Contact (bulk)
#' 
#' This function will bulk add contacts to a hotlist 
#' 
#' @param entity (type: string) -- a string indicating which entity to find 
#'                         (Company, Candidate, Contact, etc.)
#' @param id_entity (type: string) --  what id are you using to identify the records
#'                          (i.e. for companynotesdetail: entity = 'companynotes', id_entity = 'company')
#' @param clean_entity_df (type: dataframe) -- a dataframe that must have a column named 'id' with the entity ids to query details for
#' @param udfs (type: vector) -- a vector of the UDFs to include
#' @param bulk (type: boolean) -- a boolean indicating whether we are upating 1 record or multiple
#' @return A dataframe of entity details for the given entities.
#' @export

jobdiva_hotlist_add_contact_bulk = function(jobdiva_contact_id_vector, hotlist_id)
{
  # id_entity (string: what id are you using to identify 
  # (i.e. for companynotesdetail: entity = 'companynotes', id_entity = 'company')
  
  jobdiva_contact_id_vector = as.character(jobdiva_contact_id_vector)
  clean_jobdiva_contact_id_vector = lapply(jobdiva_contact_id_vector, function(x) {
    x = paste0(x, collapse = paste0('&contactIds='))
    x =  paste0('&contactIds=', x)
  })
    
  clean_jobdiva_contact_id_vector = paste0(clean_jobdiva_contact_id_vector, collapse = '')
  
  results = data.frame()
  
  base_url = "https://api.jobdiva.com/api/hotlist/addContactsToHotlist?hotListid="
  
  request = httr::POST(url = paste0(base_url, hotlist_id,clean_jobdiva_contact_id_vector)
                      , add_headers("Authorization" = jobdiva_login())
                      , encode = "json"
                      , httr::verbose())
  
  results = httr::content(request)
  
  return(results)
}
