#' JobDiva Create Contact Note
#' 
#' This function will create a note for a contact in JobDiva
#' 
#' @param contact_id (type: string) -- a string with the contact id to add the note to
#' @param note_body (type: string) -- the body of the note
#' @param note_owner_id (type:string) -- the recruiter to be added as the note/activity owner 
#' @param action (type: string) -- the note type (email, meeting, email opened, etc)
#' @return (type: boolean) The function will return TRUE if the note was successfully created and FALSE otherwise.
#' @export


jobdiva_create_contact_note = function(contact_id
                                  , note_body = ""
                                  , note_owner_id = ""
                                  , action = "")
{
  vars = c(contact_id
           , note_body
           , note_owner_id
           , action)
  
  for(i in 1:length(vars))
  {
    tmp = vars[i]
    
    tmp = trimws(tmp)
    tmp = stringr::str_replace_all(tmp, ' ', '%20')
    tmp = stringr::str_replace_all(tmp, '@', '%40')
    tmp = stringr::str_replace_all(tmp, '\\(', '%28')
    tmp = stringr::str_replace_all(tmp, '\\)', '%29')
    tmp = str_replace_all(tmp, '\\|', '%7C')
    tmp = str_replace_all(tmp, '&', '%26')
    tmp = str_replace_all(tmp, ',', '%2C')
    tmp = str_replace_all(tmp, 	'\\{', '%7B')
    tmp = str_replace_all(tmp, 	'\\}', '%7D')
    tmp = str_replace_all(tmp, 	'\\:', '%3A')
    
    vars[i] = tmp
  }
  
  url = paste0('https://api.jobdiva.com/api/jobdiva/createContactNote?contactid='
               , vars[1]
               , '&note='
               , vars[2]
               , '&recruiterid='
               , note_owner_id
               , '&action='
               , vars[4])
  
  results = try(httr::POST(url
                       , add_headers("Authorization" = jobdiva_login())
                       , encode = "json"
                       , httr::verbose())
                , silent = TRUE)
  if (class(results)[1] != 'try-error')
  {
    cont = httr::content(results)
    return(cont)
  } else
  {
    return(results)
  }
  
}

