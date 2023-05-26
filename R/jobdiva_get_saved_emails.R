#' JobDiva Saved Emails
#' 
#' This function will return the dataframe that corresponds the saved emails during the given timeframe.
#' 

#' @param from_date (type: string) -- the earliest (starting) date for when the entity records where updated (by default it is 1 day before the system date)
#' @param to_date (type: string) -- the lastest (ending) date for when the entity records where updated (by default it is the system date)
#' @return A dataframe of new updated records based on the given timeframe.
#' @export


jobdiva_get_saved_emails = function(from_date = as.character(format(Sys.Date() - 1, '%m/%d/%Y %H:%M:%S'))
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
  
  base_url = "https://api.jobdiva.com/api/bi/SavedEmails"
  
  request = httr::GET(url = base_url
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
