# RomMetricAllocation ####
#' Network Data With Allocationing Object
#' @title RomMetricAllocation
#' @description Utility class for examining, querying, and manipulating
#'   network-style data and adding/subtracting additional allocations
#' @details This R6 object has standard methods for managing the data and meta
#'   data related to network-style data allocations like river networks loads.
#'   This initializes a \code{RomMetricNetwork()} object and creates an
#'   allocation data frame that adds and subtracts additional loads to initial
#'   values in network_data to assist in cumulative calculation scenarios.
#' @importFrom R6 R6Class  
#' @param value_col Character, defaults to NA. A field in network data from
#'   \code{RomMetricNetwork$network_data} that contains initial allocations
#' @return R6 Object of class RomMetricAllocation
#' @export RomMetricAllocation
#' @examples \dontrun{
#' basepath='/var/www/R';
#' source("/var/www/R/config.R")
#' self <- RomMetricAllocation$new(
#'   ds = ds,
#'   datasource = "om",
#'   metrics = c('Qout','wd_cumulative_mgd',
#'   'l90_Qout', 'Qavailable_90_mgd'),
#'   runids = c("runid_400", "runid_600"),
#'   featureid = 'all',
#'   bundle = 'all',
#'   ftype = 'all',
#'   model_version = 'vahydro-1.0',
#'   force_unique_src = TRUE,
#'   value_col = "Qavailable_90_mgd_runid_600"
#' )
#' View(self$network_data)
#' View(self$allocation_df)
#' 
#' self$add_node_allocation(nodes = "all", values = 1)
#' all.equal((self$network_data$Qavailable_90_mgd_runid_600 + 1),
#'  self$allocation_df$allocation)
#' 
#' self$add_distributed_allocation(nodes = "all", value = 20,
#'  dist_type = "flat_percent")
#' all.equal((self$network_data$Qavailable_90_mgd_runid_600 + 1) * 1.2,
#'  self$allocation_df$allocation)
#' 
#' og <- self$allocation_df$allocation[4:6]
#' self$add_node_allocation(nodes = self$allocation_df$src_node[4:6],
#'  values = 1:3)
#' new <- self$allocation_df$allocation[4:6]
#' all.equal(og + 1:3, new)
#' 
#' og <- self$allocation_df$allocation[4:6]
#' self$add_distributed_allocation(nodes = self$allocation_df$src_node[4:6],
#'  value = 20, dist_type = "flat_percent")
#' new <- self$allocation_df$allocation[4:6]
#' all.equal(og * 1.2, new)
#' 
#' chk <- self$add_distributed_allocation(
#'    value = 20,
#'    dist_type = "outlet_percent",
#'    outlet_node = 7260)
#' 
#' test <- self$calc_cumulative()
#' }
RomMetricAllocation <- R6::R6Class(
  "RomMetricAllocation",
  inherit = RomMetricNetwork,
  public = list(
    #' @field allocation_df A data frame containing an allocation for each node
    #'   in network_data as well as a list-col of upstream nodes from
    #'   \code{get_node_relation()}
    allocation_df = NA,
    
    #' @description
    #' Initialize a RomMetricAllocation object by loading in data following
    #' \code{RomMetricNetwork}. An allocation data frame will be created from
    #' values of the field value_col in network_data if provided (otherwise
    #' initialized as 0 for all nodes)
    #' @param value_col Character, defaults to NA. A field in network data from
    #'   \code{RomMetricNetwork$network_data} that contains initial allocations
    #' @param ... Other named arguments supplied to \code{RomMetricNetwork()}
    #' @return object instance, with fields populated by user values
    initialize = function(value_col = NA, ...){
      #Initialize a RomMetricNetwork
      super$initialize(...)
      #If a value column is specified, store the data locally
      if(!is.na(value_col)){
        allocation_initial <- self$network_data[,value_col]
      }else{
        allocation_initial <- 0
      }
      #Create an initial allocation_df to allow for modifications
      self$allocation_df <- data.frame(
        src_node = self$network_data[,self$src_node_col],
        dest_node = self$network_data[,self$dest_node_col],
        allocation = allocation_initial
      )
      
      #Set upstream nodes
      self$allocation_df$upstream_nodes = self$get_node_relation()
      
    },
    
    #' @description
    #' Add an allocation to allocation_df for 'all' nodes or a vector of nodes
    #' with provided values
    #' @param nodes Character. Either 'all' or a vector of nodes to apply values
    #'   allocations to in allocation_df
    #' @param values Character. A vector of new allocations to apply to
    #'   allocation_df for nodes. Must be either single length of the length of
    #'   nodes (will be applied in order of nodes).
    #' @return Invisibly returns allocation_df where new allocations are set
    add_node_allocation = function(nodes, values){
      if(any(nodes == "all")){
        if(length(values) != nrow(self$allocation_df) & length(values) != 1){
          message("Cannot add allocation. values argument must be a length
          equal to the number of rows in the aloocation_df or of length 1")
        }else{
          self$allocation_df$allocation <- self$allocation_df$allocation + values
        }
      }else{
        self$allocation_df$allocation[match(nodes, self$allocation_df[,self$src_node_col])] <- 
          self$allocation_df$allocation[match(nodes, self$allocation_df[,self$src_node_col])] + values
      }
      
      invisible(self$allocation_df)
    },
    #' @description
    #' Add an allocation to allocation_df for 'all' nodes or a vector of nodes
    #' with by increasing allocations by a flat percent or by incrementing
    #' allocations proportional to positive values at the outlet (e.g. for water
    #' availability, consume all outlet WA by incrementing loads proportional to
    #' current WA). The latter method requires outlet_node
    #' @param nodes Character. Either 'all' or a vector of nodes to apply values
    #'   allocations to in allocation_df
    #' @param value Numeric. A vector of PERCENTAGES to increase allocations.
    #'   Either used in flat multiplication in dist_type = 'flat_percent' or as
    #'   a percentage of outlet value in 'outlet_percent'.
    #' @param dist_type Character. Either 'flat_percent' or 'outlet_percent' to
    #'   determine if value should be applied as a percentage increase of
    #'   current allocation or as percent of outlet allocation (requires
    #'   outlet_node)
    #' @param outlet_node Character. The source ID of the target outlet node for
    #'   dist_type = 'outlet_percent'
    #' @return Invisibly returns allocation_df where new allocations are set
    add_distributed_allocation = function(nodes = "all", value,
                                          dist_type = "flat_percent",
                                          outlet_node = NA){
      if(dist_type == "flat_percent" && any(nodes == "all")){
        if(length(value) != nrow(self$allocation_df) & length(value) != 1){
          message("Cannot add allocation. values argument must be a length
          equal to the number of rows in the aloocation_df or of length 1")
        }else{
          dist_value <- value * self$allocation_df$allocation / 100
          self$add_node_allocation(nodes = "all", values = dist_value)
        }
      
      }else if(dist_type == "flat_percent"){
        dist_value <- value * self$allocation_df$allocation[match(nodes, self$allocation_df[,self$src_node_col])] / 100
        self$add_node_allocation(nodes = nodes, values = dist_value)
        
      }else if(dist_type == "outlet_percent"){
        if(is.na(outlet_node)){
          warning("Please provide a reference outlet node. New allocation
              will be distributed as a percent of the value at this location
              e.g. 20% will be outlet value * 0.2 distributed across upstream
              nodes weighed by current value")
        }
        #Use only nodes in the selected system
        wshd_data <- self$allocation_df[self$allocation_df[,self$src_node_col] %in%
                                          unlist(self$allocation_df$upstream_nodes[self$allocation_df[,self$src_node_col] == outlet_node]),]
        
        #Outlet metric
        value_to_allocate <- wshd_data$allocation[wshd_data[,self$src_node_col] == outlet_node]
        
        #If duplicates, warn user
        if(length(value_to_allocate) > 1){
          message("Multiple outlet nodes found, using sum of positive metric")
          value_to_allocate <- sum(value_to_allocate[value_to_allocate > 0], na.rm = TRUE)
        }
        
        #If no outlet value, warn user
        if(is.na(value_to_allocate)){
          warning("Outlet value found was NA. Please select a different outlet.")
        }
        #Sum of positive metric
        sum_ex_metric <- sum(wshd_data$allocation[wshd_data$allocation > 0], na.rm = TRUE)
        
        #Here, we distribute the new allocation to all watersheds based on
        #the percent value submitted by user and based on the ratio of
        #current to total only on watersheds with positive metrics
        wshd_data$new_distributed_mgd <- wshd_data$allocation
        wshd_data$new_distributed_mgd[wshd_data$allocation > 0] <- (value_to_allocate * value / 100) * wshd_data$new_distributed_mgd[wshd_data$allocation > 0] / sum_ex_metric
        
        #Add new distributed allocation
        self$add_node_allocation(nodes = wshd_data[,self$src_node_col], values = wshd_data$new_distributed_mgd)
      }
      invisible(self$allocation_df)
    },
    #' @description
    #' Joins new allocations in allocatoin_df to network_data and runs
    #' \code{RomMetricNetwork$calc_cumulative()} on network_data
    #' @param value_col Character. The value in network_data to sum across
    #'   upstream nodes
    #' @param out_col_name Character. Default "cumulative_sum" and represents
    #'   where output network cumulative sum should be stored.
    #' @return A data frame of network_data now containing a field named by
    #'   out_col_name for the cumulative sum of the values in value_col
    calc_cumulative = function(value_col = "allocation", out_col_name = "cumultive_sum"){
      #Join allocations onto network data
      self$network_data[,value_col] <- self$allocation_df[match(self$network_data[,self$src_node_col], self$allocation_df[,self$src_node_col]), value_col]
      #Run network allocation calculation
      super$calc_cumulative(value_col = value_col, out_col_name = "cumulative_allocation")
    }
  ) #End public fields/methods
)#End R6Class()
    

