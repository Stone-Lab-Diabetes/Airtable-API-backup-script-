# -------------------------------------------------------------------------
# Airtable Backup Script
#
# Downloads all tables from an Airtable base and saves them as CSV files
# in a dated local backup directory.
# -------------------------------------------------------------------------

token <- #your airtable access token, generated with airtable API. It is recommended to create a "read only" token and load it into R from a secure location on your computer. 
base <- #your airtable base ID. Can be found in the URL: "https://airtable.com/app..." the part starting with "app" is your base. 
local_root <- "~/Documents/Airtable Backup" #or whichever folder you want to store your local backup in

library(httr2)
library(jsonlite)
library(dplyr)
  
get_tables <- function(base_id, api_key) {
  
  url <- paste0("https://api.airtable.com/v0/meta/bases/", base_id, "/tables")
  
  resp <- request(url) %>%
    
    req_headers(Authorization = paste("Bearer", api_key)) %>%
    
    req_perform()
  
  json <- resp_body_json(resp)
  
  tibble(
    
    table_name = sapply(json$tables, function(x) x$name),
    
    table_id   = sapply(json$tables, function(x) x$id)
    
  )
  
}

fetch_airtable_table <- function(base_id, table_id, api_key) {
  
  url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_id)
  
  flatten_record <- function(rec) {
    
    fields <- rec$fields
    
    fields <- lapply(fields, function(x) {
      
      if (is.list(x)) {
        
        if (length(x) == 0) return(NA)
        
        paste(unlist(x), collapse = ", ")
        
      } else {
        
        as.character(x)
        
      }
      
    })
    
    as_tibble(fields) %>% mutate(record_id = rec$id)
    
  }
  
  all_records_df <- tibble()
  
  offset <- NULL
  
  repeat {
    
    req <- request(url) %>%
      
      req_headers(Authorization = paste("Bearer", api_key))
    
    if (!is.null(offset)) {
      
      req <- req_url_query(req, offset = offset)
      
    }
    
    resp <- req_perform(req)
    
    json <- resp_body_json(resp, simplifyVector = FALSE)
    
    records <- Filter(function(x) is.list(x) && !is.null(x$fields), json$records)
    
    page_df <- bind_rows(lapply(records, flatten_record))
    
    all_records_df <- bind_rows(all_records_df, page_df)
    
    offset <- json$offset
    
    if (is.null(offset)) break
    
  }
  
  all_records_df
  
}

fetch_all_tables <- function(base_id, tables_df, api_key) {
  
  result <- list()
  
  for (i in seq_len(nrow(tables_df))) {
    
    table_name <- tables_df$table_name[i]
    table_id   <- tables_df$table_id[i]
    
    message("Fetching: ", table_name)
    
    df <- fetch_airtable_table(base_id, table_id, api_key)
    
    # name the list element after table_name
    result[[table_name]] <- df
  }
  
  result
}

tables <- get_tables(base, token)

all_data <- fetch_all_tables(
  base_id = base,
  tables_df = tables,
  api_key = token
)

sanitize_filename <- function(x) {
  
  gsub("[/\\\\]", "_", x)
  
}
get_base_name <- function(base_id, api_key) {
  
  url <- paste0("https://api.airtable.com/v0/meta/bases/", base_id)
  
  resp <- request(url) %>%
    req_headers(Authorization = paste("Bearer", api_key)) %>%
    req_perform()
  
  json <- resp_body_json(resp)
  
  json$name
}

build_paths <- function(base_name, local_root) {
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  list(
    
    backup_dir = file.path(local_root, base_name, date_str)
    
  )
  
}

ensure_dirs <- function(paths) {
  
  dir.create(paths$backup_dir,
             
             recursive = TRUE,
             
             showWarnings = FALSE)
  
  invisible(TRUE)
  
}

write_tables <- function(all_data, day_dir) {
  
  lapply(names(all_data), function(nm) {
    
    safe_name <- sanitize_filename(nm)
    
    file_path <- file.path(day_dir, paste0(safe_name, ".csv"))
    
    write.csv(
      
      all_data[[nm]],
      
      file_path,
      
      row.names = FALSE
      
    )
    
  })
  
  invisible(TRUE)
  
}

airtable_backup <- function(base_id,
                            api_key,
                            all_data,
                            local_root) {
  
  # Get base name
  base_name <- get_base_name(base_id, api_key)
  
  # Build directory
  paths <- build_paths(base_name, local_root)
  
  # Create directory
  ensure_dirs(paths)
  
  # Write tables
  write_tables(all_data, day_dir = paths$backup_dir)
  
  message("Backup complete:")
  message(paths$backup_dir)
  
  invisible(paths)
  
}

airtable_backup(
  
  base_id = base,
  
  api_key = token,
  
  all_data = all_data,
  
  local_root = local_root
  
)

