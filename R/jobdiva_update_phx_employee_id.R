#' JobDiva Update Contact PHX Employee id
#' 
#' This function will update contact PHX_Employee_ID in JobDiva
#' 
#' @param jobdiva_contact_id (type: string) -- The contact id
#' @param phx_employee_id (type: string) -- The 
#' @return 
#' @export


jobdiva_update_phx_employee_id = function(jobdiva_contact_id
                                  , phx_employee_id)
{
  tmp = data.frame('FIELD' = c('PHX_Employee_Id')
                   , 'CONTENT' = c(phx_employee_id))
  
  updated = try(jobdiva_update_contact(jobdiva_contact_id
                                  , tmp), silent = TRUE)
  
  return(updated)
  
}

