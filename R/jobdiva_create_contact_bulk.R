#' JobDiva Bulk Contact Creation
#' 
#' This function will create contacts in JobDiva in bulk
#' 
#' @param creation_df (type: dataframe) -- a dataframe w/ the columns to create the contact
#'  (must at least contain first and last names otherwise the contact will not be created. Currently does not handle phone numbers) 
#' @return A string that is the JobDiva contact id of newly created contact
#' @export


jobdiva_create_contact_bulk = function(creation_df, type)
{
  contact_ids = c()
  
  for (i in 1:nrow(creation_df))
  {
    x = creation_df[i, ]
    first_name = x$FIRST_NAME
    last_name =x$LAST_NAME
    title = as.character(x$TITLE)
    company_name = x$COMPANY_NAME

    if ('EMAIL' %in% toupper(colnames(creation_df)))
    {
      email = unlist(str_split(x$EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$EMAIL, ';'))[2]
    } else if ('WORK_EMAIL' %in% toupper(colnames(creation_df)) & 'PERSONAL_EMAIL' %in% toupper(colnames(creation_df)))
    {
      email = unlist(str_split(x$PERSONAL_EMAIL, ';'))[1]
      alternate_email = unlist(str_split(x$WORK_EMAIL, ';'))[1]
      
    } else {
      email = ""
      alternate_email = ""
    }
    
    
    phx_employee_id = x$EMPLOYEE_ID
    
    print(paste(ifelse(is.null(first_name), "", first_name)
                , ifelse(is.null(last_name),"",last_name)
                , ifelse(is.null(title), "", title)
                , ifelse(is.null(company_name), "",company_name)
                , ifelse(is.null(email), "",email)
                , ifelse(is.null(alternate_email), "",alternate_email)
                , ifelse(is.null(phx_employee_id), "",phx_employee_id)))
    
    single = try(jobdiva_create_contact(first_name = ifelse(is.null(first_name), "", first_name)
                                    , last_name = ifelse(is.null(last_name),"",last_name)
                                    , title = ifelse(is.null(title), "", title)
                                    , company_name = ifelse(is.null(company_name), "",company_name)
                                    , email = ifelse(is.null(email), "",email)
                                    , type = type
                                    , alternate_email =  ifelse(is.null(alternate_email), "",alternate_email)
                                    , phx_employee_id = ifelse(is.null(phx_employee_id), "",phx_employee_id)), silent = TRUE)
    if (class(single)[1] != 'try-error')
    {
      contact_ids = c(contact_ids, single)
    }
    else
    {
      contact_ids = c(contact_ids, "")
    }
    }
  
  return(contact_ids)
}

