RomMetricAllocation <- R6::R6Class(
  "RomMetricAllocation",
  inherit = RomMetricNetwork,
  public = list(
    allocation_df = NA,
    
    initialize = function(..., value_col = NA){
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
    },
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
        
        invisible(wshd_data)
      }
    },
    calc_cumulative = function(value_col = "allocation", out_col_name = "cumultive_sum"){
      #Join allocations onto network data
      self$network_data[,value_col] <- self$allocation_df[match(self$network_data[,self$src_node_col], self$allocation_df[,self$src_node_col]), value_col]
      #Run network allocation calculation
      super$calc_cumulative(value_col = value_col, out_col_name = "cumulative_allocation")
    }
  ) #End public fields/methods
)#End R6Class()
    


basepath='/var/www/R';
source("/var/www/R/config.R")
self <- RomMetricAllocation$new(
  ds = ds,
  datasource = "om",
  metrics = c('Qout','wd_cumulative_mgd','l90_Qout', 'Qavailable_90_mgd'),
  runids = c("runid_400", "runid_600"),
  featureid = 'all',
  bundle = 'all',
  ftype = 'all',
  model_version = 'vahydro-1.0',
  force_unique_src = TRUE,
  value_col = "Qavailable_90_mgd_runid_600"
)
View(self$network_data)
View(self$allocation_df)

self$add_node_allocation(nodes = "all", values = 1)
all.equal((self$network_data$Qavailable_90_mgd_runid_600 + 1), self$allocation_df$allocation)

self$add_distributed_allocation(nodes = "all", value = 20, dist_type = "flat_percent")
all.equal((self$network_data$Qavailable_90_mgd_runid_600 + 1) * 1.2, self$allocation_df$allocation)


og <- self$allocation_df$allocation[4:6]
self$add_node_allocation(nodes = self$allocation_df$src_node[4:6], values = 1:3)
new <- self$allocation_df$allocation[4:6]
all.equal(og + 1:3, new)


og <- self$allocation_df$allocation[4:6]
self$add_distributed_allocation(nodes = self$allocation_df$src_node[4:6], value = 20, dist_type = "flat_percent")
new <- self$allocation_df$allocation[4:6]
all.equal(og * 1.2, new)

chk <- self$add_distributed_allocation(value = 20, dist_type = "outlet_percent",
                                outlet_node = 7260)
test2 <- self$allocation_df


self <- RomMetricAllocation$new(
  ds = ds,
  datasource = "om",
  metrics = c('Qout','wd_cumulative_mgd','l90_Qout', 'Qavailable_90_mgd'),
  runids = c("runid_400", "runid_600"),
  featureid = 'all',
  bundle = 'all',
  ftype = 'all',
  model_version = 'vahydro-1.0',
  force_unique_src = TRUE
)

self$add_node_allocation(nodes = "all", values = 1)
test <- self$calc_cumulative()
all(lengths(test$upstream_nodes) == test$cumultive_sum )
