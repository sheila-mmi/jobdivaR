#' Report JobDiva API HTTP errors to Phoenix 
#' 
#' This function will insert a record of an error from a JobDiva API query into the JobDiva_Import_Error_Log table in Phoenix.
#' 
#' @param r_function_name (type--string): The name of the function in R that called the API query. 
#' @param http_response (type--list/response): The raw HTTP response directly from the query
#' @param phx_key (type-- Snowflake connector): The snowflake connector for login
#' @param phx_user (type-- string): The Phoenix username of the person using the function
#' @param path (type- string): The path to download temporary CSV files
#' @return a Dataframe with the outcome of the error upload                
#' @export

jobdiva_error_log = function(r_function_name, http_response, phx_key, phx_user, path)
{
 status = httr::status_code(http_response) 
 
 status = as.numeric(status)
 if (status == 200)
 {
   return('No error to log.')
 } else {
   resp_message = content(http_response)
   resp_message = resp_message[[1]]
   resp_message = resp_message$message
   print(resp_message)
   
   if(is.null(resp_message))
   {
     resp_message = 'No error message available'
   } else if(is.na(resp_message))
   {
     resp_message = 'No error message available'
   }
   
   error_record = data.frame('r_function_name' = r_function_name
                             , 'http_status_code' = status
                             , 'http_message' = resp_message)
   
   print(error_record)
   
   colnames(error_record) = toupper(colnames(error_record))
   error_record = phx_complete(phx_key, 'JobDiva_Import_Error_Log', error_record, phx_user)
   error_record = phx_add_default(error_record, 'JobDiva_Import_Error_Log')
   error_record_insert = phx_check_insert(phx_key, 'JobDiva_Import_Error_Log', phx_write_csv(error_record, path), phx_user)
   
   return(error_record_insert)
 }

}
