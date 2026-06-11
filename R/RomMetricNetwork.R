RomMetricNetwork <- R6::R6Class(
  "RomMetricNetwork",
  public = list(
    ds = NA,
    datasource = "om",
    entity_type = "dh_feature",
    entity_id_col = "hydroid",
    dest_node_col = NA,
    src_node_col = NA,
    network_data = NA,
    network_graph = NA
    
    
    #' @description
    #' The object
    #' @param ds
    #' @return object instance, with fields populated by user values
    initialize = function(
      ds = FALSE,
      datasource = self$datasource,
      metrics = NA, runids = NA,
      featureid = 'all',
      bundle = 'all',
      ftype = 'all',
      model_version = 'vahydro-1.0',
      dest_node_col = self$dest_node_col,
      src_node_col = self$src_node_col,
      entity_type = self$entity_type,
      entity_id_col = self$entity_id_col
    ){
      #Store data passed by user
      self$handle_config(
        list(
          ds = ds,
          datasource = datasource,
          dest_node_col = dest_node_col,
          src_node_col = src_node_col,
          entity_type = entity_type,
          entity_id_col = entity_id_col
        )
      )
      #If user requests datasource = om, use om_vahydro_metric_grid()
      if(inherits(self$datasource, "character") && self$datasource == "om"){
        #Call om_vahydro_metric_grid() and allow either data frame runids input
        #or standard metric+runids input. Pass all provided user values.
        om_data <- om_vahydro_metric_grid(
          metric = metrics, runids = runids, featureid = featureid,
          bundle = bundle, ftype = ftype, model_version = model_version,
          entity_type = self$entity_type,
          ds = self$ds
        )
        
        #Get network mapping for all returned features
        all_nodes <- self$get_om_rseg_node(df = om_data)
        om_data$src_node <- all_nodes$src_node
        om_data$dest_node <- all_nodes$dest_node
        #Set object on node
        self$network_data <- om_data
        self$dest_node_col <- "dest_node"
        self$src_node_col <- "src_node"
        
        
      }else if(inherits(self$datasource,"data.frame")){
        if(is.na(self$dest_node_col) | is.na(self$src_node_col)){
          warning("Source and destination nodes must be provided for datasource
                  = data frame.")
        }
        #Set object on node
        self$network_data <- self$datasource
      }
      
      # Create igraph directed network where each element in the vector flows to
      # the next element
      edges <- c(
        rbind(
          self$network_data[,self$src_node_col],
          self$network_data[,self$dest_node_col]
        )
      )
      #Create the directed graph from igraph
      self$network_graph <- igraph::make_graph(edges, directed = TRUE)
      
    },
    get_om_rseg_node = function(df, om_rseg_col = "riverseg",
                                om_hydrocode_col = "hydrocode",
                                om_propname_col = "propname",
                                om_featureid_col = "featureid",
                                ds = self$ds){
      if(((inherits(self$datasource, "character") && self$datasource == "om")) ||
              self$src_node_col == "riverseg"){
        #Get the bundle for each feature to use to distinguish watersheds
        data_bundles <- DBI::dbGetQuery(
          ds$connection,
          paste0(
            "SELECT ",self$entity_id_col,", bundle
             FROM ",self$entity_type,"
             WHERE ",self$entity_id_col," IN (",paste0("'",df[,om_featureid_col],"'",collapse = ", "),")"
          )
        )
        #Join bundle data to the input data frame and use to parse out
        #watersheds from facilities or other features
        join_data <- sqldf::sqldf(
          paste0(
            "SELECT *
            FROM df
            LEFT JOIN data_bundles as bund
            ON bund.",self$entity_id_col," = df.",om_featureid_col
          )
        )
        #For watershed features, the source watershed is the second series of
        #number and the destination is the third. However, the source of the 
        #facility feature should be the propname and the destination the source
        #river segment to indicate the model is on that node
        join_data$src_node <- gsub("(.+)_(.+)_(.+)","\\2",df[,om_rseg_col])
        join_data$dest_node <- gsub("(.+)_(.+)_(.+)","\\3",df[,om_rseg_col])
        
        #If data is missing a riversegment, destination node will not work so
        #set to hydrocode
        join_data$src_node[is.na(join_data$riverseg) | join_data$riverseg == ""] <- 
          join_data[is.na(join_data$riverseg) | join_data$riverseg == "",om_hydrocode_col]
        
        #For non-watersheds, the destination node is the riversegment its in
        #(src_node above) and the source is the name of the model
        join_data$dest_node[join_data$bundle != "watershed"] <- 
          join_data$src_node[join_data$bundle != "watershed"]
        join_data$src_node[join_data$bundle != "watershed"] <- 
          join_data[join_data$bundle != "watershed",om_propname_col]
        
        return(
          list(src_node = join_data$src_node, dest_node = join_data$dest_node)
        )
      }
    },
    set_numeric_nodes = function(){
      #Convert src_node_col and dest_node_col to numeric fields
      allids <- factor(c(self$network_data[,self$src_node_col],self$network_data[,self$dest_node_col]))
      allids_numeric <- as.integer(allids)
      
      # self$network_data$src_node_num <- allids_numeric[match(self$network_data[,self$src_node_col],allids)]
      # self$network_data$dest_node_num <- allids_numeric[match(self$network_data[,self$dest_node_col],allids)]
      self$network_data[,self$src_node_col] <- allids_numeric[match(self$network_data[,self$src_node_col],allids)]
      self$network_data[,self$dest_node_col] <- allids_numeric[match(self$network_data[,self$dest_node_col],allids)]
    },
    
    get_node_relation = function(direction = "upstream", value = NA){
      #If user has provided a value and the value is a column name in the
      #network data, assign and attribute to the graph matching to ensure proper
      #order
      g <- self$network_graph
      if(!is.na(value) && value %in% names(self$network_data)){
        # g <- igraph::set_vertex_attr(
        #   graph = g,
        #   name = value,
        #   value = self$network_data[match(igraph::as_ids(igraph::V(g)),
        #                                   self$network_data[,self$src_node_col]),
        #                             value]
        # )
        igraph::V(g)$value <- self$network_data[match(igraph::as_ids(igraph::V(g)),
                                                      self$network_data[,self$src_node_col]),
                                                value]
      }
      
      #Set the direction variable for a depth first search in igraph
      direction <- switch(direction, upstream = "in", downstream = "out")
      
      #For each vertex, find all upstream and downstream vertex
      allSearch <- mapply(
        FUN = function(target_node, direction, inetwork){
          #Find fastest way upstream
          depths <- igraph::dfs(inetwork, root = target_node,
                                mode = direction, unreachable = FALSE)
          #If value is set, return the value attribute set of the vertices of
          #the graph; otherwise, return the vertices
          if(!is.null(V(inetwork)$value)){
            out <- igraph::V(inetwork)$value[depths$order]
          }else{
            out <- igraph::as_ids(depths$order)
          }
          return(out)
        }, 
        target_node = self$network_data[,self$src_node_col],
        MoreArgs = list(inetwork = g, direction = direction)
      )
      
      return(allSearch)
    },
    
    
    #' @description
    #' Handles the config passed in initialize to set all fields on the object
    #' by calling \code{self$handle_config_item()}
    #' @param config A named list of public fields with values to set
    #' @return NULL
    handle_config = function(config = list()){
      mapply(config, names(config), FUN = self$handle_config_item)
    },
    #' @description
    #' For a given item in the user config, check to see if the name of the
    #' config list item is a field on this object. If so, set it to the value of
    #' the config_item
    #' @param config_item A value to set on the field in config_item_name, if it
    #'   exists on this object
    #' @param config_item_name A field to set with the value config_item, if the
    #'   field exists on this object
    #' @return The target field, may be NULL if it does not exist
    handle_config_item = function(config_item, config_item_name){
      #Try to extract only fields from self by eliminating functions and
      #environments
      all_self_fields_methods <- names(self)
      all_self_fields <- unlist(lapply(all_self_fields_methods, function(x) (!is.function(self[[x]]) && !is.environment(self[[x]]))))
      all_self_fields <- all_self_fields_methods[all_self_fields]
      
      #If the name of the config item is a field on this object, set it
      if(config_item_name %in% names(self)){
        self[[config_item_name]] <- config_item
      }
      return(self[[config_item_name]])
    }
  )
)


basepath='/var/www/R';
source("/var/www/R/config.R")
self <- RomMetricNetwork$new(
  ds = ds,
  datasource = "om",
  metrics = c('Qout','wd_cumulative_mgd','l90_Qout', 'Qavailable_90_mgd'),
  runids = c("runid_400", "runid_600"),
  featureid = 'all',
  bundle = 'all',
  ftype = 'all',
  model_version = 'vahydro-1.0',
)
View(self$network_data)
#Need to check a few of these against fn_upstream.ALL and fn_downstream.ALL
#Need to also consider this is only considering itself. It DOES NOT consider the
#full riversegment network
test1 <- self$get_node_relation(value = "riverseg")
self$set_numeric_nodes()
test2 <- self$get_node_relation(value = "riverseg")

#Proof equality:
names(test1) <- NULL
all.equal(unlist(test1)[order(unlist(test1))], unlist(test2)[order(unlist(test2))])

