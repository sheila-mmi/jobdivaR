#' JobDiva New Updated Records v2
#' 
#' This function will return the dataframe that corresponds the recently updated records for the given entity and timeframe (from VERSION 2 of JobDiva's API)
#' 
#' @param entity (type: string) -- a string indicating which entity to find 
#'                         (Company, Candidate, Contact, etc.)
#' @param from_date (type: string) -- the earliest (starting) date for when the entity records where updated (by default it is 1 day before the system date)
#' @param to_date (type: string) -- the lastest (ending) date for when the entity records where updated (by default it is the system date)
#' @return A dataframe of new updated records based on the given timeframe.
#' @export


jobdiva_new_updated_records_v2 = function(entity
                                       , from_date = as.character(format(Sys.Date() - 1, '%m/%d/%Y %H:%M:%S'))
                                       , to_date = paste0(as.character(format(Sys.Date(), '%m/%d/%Y')),  ' 23:59:59'))
{
  if(is.null(from_date)|| from_date == ' ' || from_date == '')
  {
    from_date = as.character(format(Sys.Date() - 2, '%m/%d/%Y %H:%M:%S'))
  }
  
  if(is.null(to_date) || to_date == ' ' || to_date == '')
  {
    to_date = paste0(as.character(format(Sys.Date(), '%m/%d/%Y')),  ' 23:59:59')
  }
  print(from_date)
  print(to_date)
  
  base_url = "https://api.jobdiva.com/apiv2/bi/"
  
  full_method = paste0("NewUpdated", stringr::str_to_title(entity), "Records")
  full_method = stringr::str_replace_all(full_method, " ", "")
  print(full_method)
  request = httr::GET(url = paste0(base_url, full_method)
                      , add_headers("Authorization" = jobdiva_login())
                      , query = list(
                        "fromDate" = from_date
                        , 'toDate' = to_date
                        , 'alternateFormat' = TRUE
                      )
                      , encode = "json"
                      , httr::verbose()) 
  results = httr::content(request)
  results = results[[2]]
  result_df = try(dplyr::bind_rows(results), silent = TRUE)
  
  if(class(result_df)[1] != 'try-error')
  {
    return(result_df)
  } else
  {
    return(results)
    
  }
  
  
}
