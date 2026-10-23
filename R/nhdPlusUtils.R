#'@title process_foundation
#'@description Process foundation data to get intake capacity and facility
#'  classification
#' @importFrom rlang .data
#'@param onedrive_location Character, location of OneDrive
#'@param conn_ODS Pool, connection to ODS
#'@return a simple feature sf of foundation intakes with averages and exemption
#'  information
process_foundation <- function(onedrive_location, conn_ODS){
  found_data <- utils::read.csv(paste0(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\2026\\foundation_dataset_mgy.csv"))
  ## Create an annual foundation dataset that indicates if a given facility is
  ## permitted, exempt, or neither. Only need to include the hydroid, name,
  ## geometry, and five year average
  permit_classification_df <- DBI::dbGetQuery(conn = conn_ODS, paste0("
        SELECT mp.GMP_ID, mp.Gmp_VaHydro_Hydroid, mp.FACILITY_ID, mp.FACILITY_NAME, mp.PERMIT_NO,
          CASE 
            WHEN ((sum(mp.GMP_I_INTAKE_CAPACITY) > 0 OR sum(mp.GMP_I_PRE_1990_MAX) > 0) AND STRING_AGG(mp.PERMIT_NO,',') LIKE '%WWR%') THEN 'Exempt'
            WHEN STRING_AGG(mp.PERMIT_NO,',') NOT LIKE '%WWR%' THEN 'Permitted'
            ELSE 'NPNE'
          END as classification,
          sum(mp.GMP_I_INTAKE_CAPACITY) as intake_capacity,
          sum(mp.GMP_I_PRE_1990_MAX) as pre1989
        FROM water.Measuring_Point_Vw as mp
        WHERE mp.MPS_DESCRIPTION = 'Surface Water'
        GROUP BY mp.GMP_ID, mp.Gmp_VaHydro_Hydroid, mp.FACILITY_ID, mp.FACILITY_NAME, mp.PERMIT_NO")
  )
  
  foundation_monthly <- sqldf::sqldf(
    "SELECT f.MP_CEDSid, f.MP_Hydroid, f.Source_Type, f.MP_Name, f.facility_hydroid,
         f.Facility, f.facility_status, f.Permit_Classification,pdf.Classification,
         f.fiveyr_avg_mgy, pdf.intake_capacity, pdf.pre1989,
         CONCAT('POINT (',f.Longitude,' ', f.Latitude,')') as wkt
         FROM found_data as f
         LEFT JOIN permit_classification_df as pdf
           ON pdf.GMP_ID = f.MP_CEDSid
         WHERE f.Source_Type = 'Surface Water'
         AND (f.Permit_Classification IS NULL OR f.Permit_Classification != 'History')
         AND (f.Latitude IS NOT NULL AND f.Longitude IS NOT NULL)
         AND f.facility_status = 'Active'
         GROUP BY f.MP_Hydroid"
  )
  
  foundation_monthly <- foundation_monthly |> 
    dplyr::mutate(
      fiveyr_avg_mgd = .data$fiveyr_avg_mgy / 365,
      wd_intakecap_mgd = dplyr::case_when(
        classification == "Exempt" & (!is.na(.data$intake_capacity) & .data$intake_capacity > 0) ~ .data$intake_capacity,
        TRUE ~ fiveyr_avg_mgd
      ),
      wd_pre1989_mgd = dplyr::case_when(
        classification == "Exempt" & (!is.na(.data$pre1989) & .data$pre1989 > 0) ~ (.data$pre1989 / 365),
        TRUE ~ .data$fiveyr_avg_mgd
      )
    )
  
  found_sf <- sf::st_as_sf(foundation_monthly, wkt = "wkt", crs = 4326)
  
  return(found_sf)
}

#'@title get_consumption
#'@description Get estimated consumption for features of hydroids based on
#'  available model information stored in conumption or conumption_monthly
#'  properties
#'@param ds RomDataSource with connection to dbase
#'@param hydroids Integer, hydroids of features of interest to seearch for
#'  consumption
#'@return a data.frame of with maximum and average consumption for each hydroid
#'  input by user as well as the month the maximum consumption occurred
get_consumption <- function(ds, hydroids){
  consumption <- DBI::dbGetQuery(
    ds$connection,
    paste0(
      "WITH json_elements AS ( 
           SELECT p.entity_id, prop.propname,
             (json_tbl.json_element->>0)::varchar AS month,
             (json_tbl.json_element->>1)::numeric AS frac
           FROM field_data_field_dh_matrix p
           CROSS JOIN jsonb_array_elements(p.field_dh_matrix_json) AS json_tbl(json_element)
           LEFT JOIN dh_properties prop 
             ON p.entity_id = prop.pid
           WHERE prop.propname = 'consumption_monthly' 
             AND json_tbl.json_element->>1 NOT IN 
               ('xFrac','xMonth', 'xFactor','consumption_current','consumption_proposed','1.12161692293888e-05')
         )
      
         SELECT f.hydroid, modelprop.pid, modelprop.propname, 
            rseg.propcode as rseg, 
            j.month as max_frac_month,
            COALESCE(max_frac, CAST(sc.propcode AS float)) AS max_frac,
            COALESCE(avg_frac, CAST(sc.propcode AS float)) AS avg_frac
         FROM dh_feature as f
         LEFT JOIN dh_properties as modelprop
           ON modelprop.featureid = f.hydroid
           AND modelprop.propcode = 'vahydro-1.0'
         LEFT JOIN dh_properties as rseg
           ON rseg.featureid = modelprop.pid
           AND rseg.propname = 'riverseg'
         LEFT JOIN dh_properties as p
           ON p.featureid = modelprop.pid 
           AND p.propname = 'consumption_monthly'
         --Any static consumption set on property
         LEFT JOIN dh_properties as sc
           ON sc.featureid = modelprop.pid
           AND sc.propname = 'consumption'
           AND sc.propcode != 'consumption_monthly'
           
         LEFT JOIN json_elements j
           ON p.pid = j.entity_id
         --But we only care about the max row for each 
         LEFT JOIN (
           SELECT entity_id, MAX(frac) AS max_frac
           FROM json_elements
           GROUP BY entity_id
         ) get_max
           ON get_max.entity_id = j.entity_id 
           AND get_max.max_frac = j.frac
         --Average consumption
         LEFT JOIN (
           SELECT entity_id, AVG(frac) as avg_frac
           FROM json_elements
           GROUP BY entity_id
         ) get_avg
           ON get_avg.entity_id = j.entity_id
      
        WHERE f.hydroid IN (",paste0(hydroids,collapse = ","),")
          AND ( (get_max.max_frac = j.frac) OR (j.frac IS NULL))
          AND modelprop.propcode IS NOT NULL"
    ))
  return(consumption)
}

#'@title make_network_df
#' @importFrom rlang .data
#'@description Simplify and standardize facility withdrawal and point source
#'  data frames
#'@param entity_df data.frame, contains info derived from
#'  \code{nhdPlusRouting$get_intake_wshd_dfs}
#'@param entity_type Character. "wd", "ps", or "nhdplus" to represent the input
#'  data frame type which determines processing
#'@param consumption_df data.frame. Contains consumtpion factors from
#'  \code{nhdPlusRouting$get_intake_wshd_dfs} for entity_type "ps" only
#'@param baseline_factor Integer. The amount of baseline flow to use in
#'  scenariosused for entity_type "nhdplus" only
#'@return a processed data frame
make_network_df <- function(entity_df, entity_type = "wd",
                            consumption_df = NULL,
                            baseline_factor = NULL){
  #Drop geometry if present
  entity_df <- sf::st_drop_geometry(entity_df)
  #If analyzing point source, join consumption if available
  if(entity_type == "ps"){
    entity_df <- entity_df |> 
      dplyr::mutate(rseg = gsub("vahydrosw_wshed_","",.data$feat_hydrocode)) |>
      dplyr::left_join(consumption_df,
                       by = c("facility_hydroid" = "hydroid",
                              "rseg" = "rseg")) |> 
      dplyr::mutate(max_frac = dplyr::case_when(
        is.na(.data$max_frac) ~ 1,
        TRUE ~ .data$max_frac
      )) |>
        dplyr::select(-"rseg")
  }else if(entity_type == "wd"){
    entity_df <- entity_df |> 
      dplyr::mutate(max_frac = NA)
  }
  
  if(entity_type == "nhdplus"){
    out <- entity_df |> 
      dplyr::select(
        tidyselect::all_of(
          c(
            "hydroid", "Hydroseq", "UpHydroseq", "DnHydroseq", "AreaSqKM", "TotDASqKM", 
            "feat_hydroid", "feat_hydrocode",
            "nhd_Qbaseline",
            "geometry" 
          )
        )
      ) |> 
      dplyr::mutate(
        Facility_Status = NA, Permit_Classification = NA,
        consumption_frac = NA,
        fiveyr_avg_mgd = NA, intake_capacity = NA, pre1989 = NA,
        object_class = "nhdplus"
      )
  }else{
    out <- entity_df |> 
      dplyr::mutate(
        Hydroseq_fac = paste0(entity_type,"_",.data$MP_Hydroid, .data$facility_hydroid),
        UpHydroseq = 0,
        geometry = NA,
        AreaSqKM = 0,
        TotDASqKM = 0,
        nhd_Qbaseline = 0,
        object_class = paste0("facility_",entity_type),
        hydroid = .data$MP_Hydroid,
        Hydroseq = .data$Hydroseq_fac,
        DnHydroseq = .data$Hydroseq,
        feat_hydroid = .data$hydroid, 
        consumption_frac = .data$max_frac
      )
  }
  
  #Depending on facility type, either calculate the scenario baseflows as the
  #withdrawal (negative) or the point source return (positive). if its a nhdplus
  #watershed, allow users to take a fraction of baseline flow
  if(entity_type == "wd"){
    out <- out |> 
      dplyr::mutate(
        consumption_frac = NA,
        qbase_fiveyravg = -(.data$fiveyr_avg_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400),
        qbase_intakecap = -(.data$wd_intakecap_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400),
        qbase_pre1989   = -(.data$wd_pre1989_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400)
      )
  }else if(entity_type == "ps"){
    out <- out |> 
      dplyr::mutate(
        qbase_fiveyravg = (1 - .data$max_frac) * (.data$fiveyr_avg_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400),
        qbase_intakecap = (1 - .data$max_frac) * (.data$wd_intakecap_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400),
        qbase_pre1989   = (1 - .data$max_frac) * (.data$wd_pre1989_mgd * 1000000 * 231 / 12 / 12 / 12/ 86400)
      )
  }else if(entity_type == "nhdplus"){
    out <- out |> 
      dplyr::mutate(
        qbase_fiveyravg = .data$nhd_Qbaseline * baseline_factor,
        qbase_intakecap = .data$nhd_Qbaseline * baseline_factor,
        qbase_pre1989   = .data$nhd_Qbaseline * baseline_factor
      )
  }
  #Select only the relevant fields to make data frame smaller
  out <- out |> 
    dplyr::select(
      tidyselect::all_of(
        c(
          "object_class", "hydroid", "Hydroseq",
          "UpHydroseq", "DnHydroseq",
          "geometry",
          "AreaSqKM", "TotDASqKM", 
          "feat_hydroid", "feat_hydrocode",
          "nhd_Qbaseline",
          "Facility_Status", "Permit_Classification",
          "consumption_frac",
          "fiveyr_avg_mgd", "intake_capacity", "pre1989",
          "qbase_fiveyravg", "qbase_pre1989", "qbase_intakecap"
        )
      )
    )
  
  return(out)
}
