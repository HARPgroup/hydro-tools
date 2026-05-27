
## This function just returns generic results for each of the table options
## Saves a bunch of if statements in the get()
fn_get_table_names <- function(entity_type) {
  
  out <- list()
  
  if (entity_type == "facility") {
    
    out$pk_col <- "[CEDS Facility Id]"
    
    out$pk_colname <- "CEDS Facility Id" ## Without the brackets needed to reference in R
    
    out$tbl_name <- "ceds.[CEDS Facilities]"
    
    out$gis_tbl <- "ceds.[CEDS Facilities Geospatial Data]"
    
    out$gis_col_names <- "Latitude, Longitude, [Geographic City County] AS County"
    
    out$contact_tbl <- "ceds.[CEDS Contacts]"
    
    out$contact_pkcol <- "[CEDS Facility ID]"
    
  } else if (entity_type == "WWR") {
    
    out$pk_col <- "Permit_Id"
    
    out$pk_colname <- out$pk_col
    
    out$tbl_name <- "water.Water_Withdrawal_Reg_Vw"
    
    out$permit_num_col <- "Registration_Number"
    
    out$facility_field <- "CEDS_Facility_Id"
    
    out$gis_tbl <- "water.Water_Withdrawal_GIS_Vw gis INNER JOIN water.Water_Withdrawal_Reg_Vw wwr
                      ON gis.Permit_Number = wwr.Registration_Number"
    
    out$gis_col_names <- "Latitude, Longitude, County"
    
    out$contact_tbl <- "ceds.[CEDS Contacts]"
    
    out$contact_pkcol <- "[CEDS Facility ID]"
    
  } else if (entity_type == "GWP") {
    
    out$pk_col <- "Permit_Id"
    
    out$pk_colname <- out$pk_col
    
    out$tbl_name <- "water.GWP_Permits_Vw"
    
    out$permit_num_col <- "GWP_Permit_Number"
    
    out$sort_col <- "Permit_Status"
    
    out$facility_field <- "CEDS_Facility_Id"
    
    out$gis_tbl <- "water.GWP_GIS_Vw gis INNER JOIN water.GWP_Permits_Vw gwp
                      ON gis.Permit_Number = gwp.GWP_Permit_Number"
    
    out$gis_col_names <- "Latitude, Longitude, County"
    
    out$contact_tbl <- "water.GWP_Permit_Contacts_Vw"
    
    out$contact_pkcol <- "Permit_Id"
    
  } else if (entity_type == "VWP") {
    
    out$pk_col <- "[Permit Id]"
    
    out$pk_colname <- "Permit Id"
    
    out$tbl_Name <- "water.[VWP Permits]"
    
    out$permit_num_col <- "[Permit Number]"
    
    out$sort_col <- "Classification"
    
    out$facility_field <- "[CEDS Facility Id]"
    
    out$gis_tbl <- "vwp.[VWP Geospatial Data]"
    
    out$gis_col_names <- "Latitude, Longitude, Geographic City County AS County"
    
    out$contact_tbl <- "water.VWP_Permit_Contacts_Vw"
    
    out$contact_pkcol <- "Permit_Id"
    
  } else if (entity_type == "VPDES") {
    
    out$pk_col <- "[Permit Id]"
    
    out$pk_colname <- "Permit Id"
    
    out$tbl_name <- "water.[Water Permits]"
    
    out$permit_num_col <- "[Permit Number]"
    
    out$sort_col <- "Classification"
    
    out$facility_field <- "[CEDS Facility Id]"
    
    out$contact_tbl <- "water.[Water Contacts]"
    
    out$contact_pkcol <- "[Permit Id]"
  }
  ## Repeat for the rest, just using this as an example
  
  
  return(out)
  
}

## Creates a query to be used for the gis tables
getGIS <- function(pkid, tbl_info, ds) {
  
  sql_select <- paste("SELECT", tbl_info$gis_col_names)
  sql_from <- paste("FROM", tbl_info$gis_tbl)
  sql_where <- paste("WHERE", tbl_info$pk_col, "=", pkid)
  
  gis_sql <- paste(sql_select,sql_from,sql_where)
  
  coords <- dbGetQuery(ds$connection, gis_sql)
  
  return(coords)
}

