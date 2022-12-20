#' JobDiva Entity Detail 
#' 
#' This function will return the dataframe that corresponds the details for the entities submitted.
#' 
#' @param entity (type: string) -- a string indicating which entity to find 
#'                         (Company, Candidate, Contact, etc.)
#' @param id_entity (type: string) --  what id are you using to identify the records
#'                          (i.e. for companynotesdetail: entity = 'companynotes', id_entity = 'company')
#' @param clean_entity_df (type: dataframe) -- a dataframe that must have a column named 'id' with the entity ids to query details for
#' @return A dataframe of entity details for the given entities.
#' @export

jobdiva_detail = function(entity, id_entity, clean_entity_df)
{
  # id_entity (string: what id are you using to identify 
  # (i.e. for companynotesdetail: entity = 'companynotes', id_entity = 'company')
  entity_ids = clean_entity_df$id
  entity_ids = paste0(entity_ids, collapse = ',')
  
  base_url = "https://api.jobdiva.com/api/bi/"
  
  full_method = paste0(stringr::str_to_title(entity), "Detail")
  full_method = stringr::str_replace_all(full_method, " ", "")
  request = httr::GET(url = paste0(base_url, full_method)
                      , add_headers("Authorization" = jobdiva_login())
                      , query = list(
                        paste0("'", tolower(id_entity), 'Id', "'") =  entity_ids
                        , 'alternateFormat' = TRUE
                      )
                      , encode = "json"
                      , httr::verbose()) 
  results = httr::content(request)
  
  return(results)
}