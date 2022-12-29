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

jobdiva_detail = function(entity, id_entity, clean_entity_df, bulk = TRUE)
{
  # id_entity (string: what id are you using to identify 
  # (i.e. for companynotesdetail: entity = 'companynotes', id_entity = 'company')
  entity_ids = clean_entity_df$id
  if (bulk == TRUE)
  {
    entity_name = paste0(id_entity, 'Ids')
  }
  
  else
  {
    entity_name = paste0(id_entity, 'Id')
  }
  
  entity_ids = split(entity_ids, ceiling(seq_along(entity_ids)/100))
  entity_ids = lapply(entity_ids, function(x) {
    x = paste0(x, collapse = paste0('&', entity_name, '='))
    x =  paste0('?', entity_name, '=', x)
  })

  results = data.frame()
  
  base_url = "https://api.jobdiva.com/api/bi/"
  
  full_method = paste0(stringr::str_to_title(entity), "Detail")
  full_method = stringr::str_replace_all(full_method, " ", "")
  
  for (i in 1:length(entity_ids))
  {
    request = httr::GET(url = paste0(base_url, full_method, entity_ids[[i]], '&alternateFormat=true', '&userFieldsName=PHX_Normalized_Company_Id')
                        , add_headers("Authorization" = jobdiva_login())
                        , encode = "json"
                        , httr::verbose()) 
    temp_results = dplyr::bind_rows(httr::content(request)[[2]])
    results = rbind(results, temp_results)
  }
  
  return(results)
}