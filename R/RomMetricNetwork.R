# RomMetricNetwork ####
#' Network Data Object
#' @title RomMetricNetwork
#' @description Utility class for examining, querying, and manipulating
#'   network-style data
#' @details This R6 object has standard methods for managing the data and meta
#'   data related to network-style data like river networks or model metric
#'   returned from \code{om_vahydro_metric_grid()}.
#'   This object can query data from \code{om_vahydro_metric_grid()} or take in
#'   a data frame and help the user compile a network graph from source ids or
#'   riversegments. Methods on this object allow for cumulative calculations
#'   across the network, downstream/upstream extraction, and diagram creation.
#' @importFrom R6 R6Class  
#' @param ds datasource object of class \code{RomDataSource} often created in DEQ
#'   config files
#' @param datasource Either 'om' or a data frame input inidicating how this
#'   object instance should derive the metric data. If 'om' is selected,
#'   \code{om_vahydro_metric_grid()} will be used to get model metrics and
#'   methods will assume a riverseg column is present that contains watersheds
#'   IDs in the form JU3_6690_7280 where 6690 identifies this segment and 7280
#'   is the segment (source node) to which it flows (destination node)
#' @param metric character vector of requested metrics
#' @param runids Either a character vector of model run ids (in the form
#'   'runid_600', for example) or a dataframe with model_version, runid, metric,
#'   and runlabel specified. The data frame is generally deprecated in favor of
#'   using the individual arguments on this function.
#' @param featureid integer or 'all', representing which specific features to
#'   query for models
#' @param bundle character vector representing feature types. Only these 
#'  features will be queried for models of \code{model_version}. May be 'all' to
#'  query all bundle types. Defaults to 'watershed'
#' @param ftype character vector representing feature types, i.e. the bundle
#'   subclass. Only these features will be queried for models of
#'   \code{model_version}. May be 'all' to query all ftypes. Defaults to 'vahydro'
#' @param model_version character vector with default 'vahydro-1.0'. This will
#'   be used to determine which models to find on \code{entity_type}
#' @param dest_node_col If datasource is a dataframe, which field identifies the
#'   destination node that this node flows to?
#' @param src_node_col If datasource is a dataframe, which field is the source
#'   node that identifies this node?
#' @param entity_type character (single-length) to represent the table to query
#'   for models, defaults to \code{dh_feature_fielded}
#' @param entity_id_col Character; defaults to 'hydroid' and represents the primary key
#'   field of the entity type table (defaults dh_feature_fielded)
#' @param force_unique_src Logical, defaults to FALSE. If non-unique source ids
#'   are found, should this object append these ids to force all nodes to be
#'   unique? In the instance of om river segments, all data should have unique
#'   IDs but model errors can, rarely, interfere with this and this setting may
#'   be useful.
#' @return R6 Object of class RomMetricNetwork
#' @export RomMetricNetwork
#' @examples \dontrun{
#'self <- RomMetricNetwork$new(
#'  ds = ds,
#'  datasource = "om",
#'  metrics = c('Qout','wd_cumulative_mgd','l90_Qout', 'Qavailable_90_mgd'),
#'  runids = c("runid_400", "runid_600"),
#'  featureid = 'all',
#'  bundle = 'all',
#'  ftype = 'all',
#'  model_version = 'vahydro-1.0',
#'  force_unique_src = FALSE
#')
#'View(self$network_data)
#'upstream_nodes <- self$get_node_relation(value = "riverseg")
#'self$set_numeric_nodes()
#'upstream_nodes_num <- self$get_node_relation(value = "riverseg")
#'
#'self$network_data$newdist <- 1
#'cumulative_sum <- self$calc_cumulative("newdist")
#'all(lengths(cumulative_sum$upstream_nodes) == cumulative_sum$cumultive_sum )
#'
#'wshd <- self$extact_node_network(src_node = self$network_data$src_node[1])
#'wshd <- self$extact_node_network(src_node = self$network_data$src_node[1],
#'                                 direction = "downstream")
#'
#'netSF <- self$get_spatial_network(value_col = "Qavailable_90_mgd_runid_600",
#' value_label = "WA_MGD")
#'plot(netSF$dh_geofield,col = netSF$color)
#'
#'self$render_force_diagram()
#' }
RomMetricNetwork <- R6::R6Class(
  "RomMetricNetwork",
  public = list(
    #' @field ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    ds = NA,
    #' @field datasource Either 'om' or a data frame input inidicating how this
    #'   object instance should derive the metric data. If 'om' is selected,
    #'   \code{om_vahydro_metric_grid()} will be used to get model metrics and
    #'   methods will assume a riverseg column is present that contains watersheds
    #'   IDs in the form JU3_6690_7280 where 6690 identifies this segment and 7280
    #'   is the segment (source node) to which it flows (destination node)
    datasource = "om",
    #' @field entity_type character (single-length) to represent the table to query
    #'   for models, defaults to \code{dh_feature_fielded}
    entity_type = "dh_feature_fielded",
    #' @field entity_id_col Character; defaults to 'hydroid' and represents the primary key
    #'   field of the entity type table (defaults dh_feature_fielded)
    entity_id_col = "hydroid",
    #' @field dest_node_col If datasource is a dataframe, which field identifies the
    #'   destination node that this node flows to?
    dest_node_col = NA,
    #' @field src_node_col If datasource is a dataframe, which field is the source
    #'   node that identifies this node?
    src_node_col = NA,
    #' @field network_data A data frame of data with a source and destination
    #'   node field that identifies how data travels upstream and downstream
    #'   within the network represented by the data frame
    network_data = NA,
    #' @field network_graph An \code{igraph} style object from the package of
    #'   the same name with vertices defined by the source node column
    network_graph = NA,
    #' @description
    #' Initialize a RomMetricNetwork object by loading in either data from
    #' \code{om_vahydro_metric_grid()} or by loading in a source column and
    #' defining the approriate source and destination nodes. If datasource 'om'
    #' is used, this method will apply \code{get_om_rseg_node()} to identify
    #' potential source and destination node IDs and a bundle and ftype will be
    #' joined in by featureid. The user will be warned if duplicate nodes are
    #' encountered and it will be recommended to use
    #' \code{force_unique_src_ids()} if the user wishes to get a unique network
    #' only
    #' @param ds datasource object of class \code{RomDataSource} often created in DEQ
    #'   config files
    #' @param datasource Either 'om' or a data frame input inidicating how this
    #'   object instance should derive the metric data. If 'om' is selected,
    #'   \code{om_vahydro_metric_grid()} will be used to get model metrics and
    #'   methods will assume a riverseg column is present that contains watersheds
    #'   IDs in the form JU3_6690_7280 where 6690 identifies this segment and 7280
    #'   is the segment (source node) to which it flows (destination node)
    #' @param metrics character vector of requested metrics
    #' @param runids Either a character vector of model run ids (in the form
    #'   'runid_600', for example) or a dataframe with model_version, runid, metric,
    #'   and runlabel specified. The data frame is generally deprecated in favor of
    #'   using the individual arguments on this function.
    #' @param featureid integer or 'all', representing which specific features to
    #'   query for models
    #' @param bundle character vector representing feature types or 'all'
    #'   (default). Only these features will be queried for models of
    #'   \code{model_version}. May be 'all' to query all bundle types. Defaults
    #'   to 'watershed'
    #' @param ftype character vector representing feature types or 'all'
    #'   (default), i.e. the bundle subclass. Only these features will be
    #'   queried for models of \code{model_version}. May be 'all' to query all
    #'   ftypes. 
    #' @param model_version character vector with default 'vahydro-1.0'. This will
    #'   be used to determine which models to find on \code{entity_type}
    #' @param dest_node_col If datasource is a dataframe, which field identifies the
    #'   destination node that this node flows to?
    #' @param src_node_col If datasource is a dataframe, which field is the source
    #'   node that identifies this node?
    #' @param entity_type character (single-length) to represent the table to query
    #'   for models, defaults to \code{dh_feature_fielded}
    #' @param entity_id_col Character; defaults to 'hydroid' and represents the primary key
    #'   field of the entity type table (defaults dh_feature_fielded)
    #' @param force_unique_src Logical, defaults to FALSE. If non-unique source ids
    #'   are found, should this object append these ids to force all nodes to be
    #'   unique? In the instance of om river segments, all data should have unique
    #'   IDs but model errors can, rarely, interfere with this and this setting may
    #'   be useful.
    #' @return object instance, with fields populated by user value and
    #'   network_data and network_graph populated
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
      entity_id_col = self$entity_id_col, 
      force_unique_src = FALSE
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
        if(entity_type == "dh_feature_fielded"){
          entity_type <- "dh_feature"
        }
        om_data <- om_vahydro_metric_grid(
          metric = metrics, runids = runids, featureid = featureid,
          bundle = bundle, ftype = ftype, model_version = model_version,
          entity_type = entity_type,
          ds = self$ds
        )
        if(!inherits(om_data, "data.frame") || nrow(om_data) == 0){
          warning("No data retrieved from om_vahydro_metric_grid")
        }
        #Join bundle and ftype onto data
        hydroFeats <- DBI::dbGetQuery(
          paste0("SELECT f.hydroid, f.bundle, f.ftype
                  FROM ", entity_type," as f
                  WHERE f.hydroid IN (",paste0(om_data[,"featureid"],collapse = ","),")"),
          conn = ds$connection)
        
        #Join in the results data
        om_data <- sqldf::sqldf(
            "SELECT feat.bundle, feat.ftype, om.*
           FROM hydroFeats as feat
           LEFT JOIN om_data as om
           ON om.featureid = feat.hydroid"
        )
        
        #Get network mapping for all returned features
        all_nodes <- self$get_om_rseg_node(df = om_data)
        om_data$src_node <- all_nodes$src_node
        om_data$dest_node <- all_nodes$dest_node
        #Set object on node
        self$dest_node_col <- "dest_node"
        self$src_node_col <- "src_node"
        
      }else if(inherits(self$datasource,"data.frame")){
        #Store data frame
        om_data <- self$datasource
        #If src_node_col is riverseg, try standard riverseg node parsing
        if(self$src_node_col == "riverseg"){
          #Get node ids and set
          all_nodes <- self$get_om_rseg_node(df = om_data)
          om_data$src_node <- all_nodes$src_node
          om_data$dest_node <- all_nodes$dest_node
          self$src_node_col <- "src_node"
          self$dest_node_col <- "dest_node"
        }
        
        if(is.na(self$dest_node_col) | is.na(self$src_node_col)){
          #User must set destination and src node columns or have them
          #calculated by setting src_node_col to riverseg
          warning("Source and destination nodes must be provided for datasource
                  = data frame. If src_col set to 'riverseg', this object will try 
                  standard om network eveluation")
        }
      }else{
        warning("Non-standard datasource set. Use either 'om' or a data.frame")
      }
      #Set object on node
      self$network_data <- om_data
      #Force unique ids if user wants
      if(force_unique_src){
        self$force_unique_src_ids()
      }else if(any(duplicated(self$network_data[,self$src_node_col]))){
        message("Non-unique source nodes found. Use the
        self$force_unique_src_ids() method to force uniqueness. With
        non-unique src_ids, some cumulative calculations may be counter-inituitive")
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
    #' @description
    #' Take in data from \code{om_vahydro_metric_grid()} or some equivalent data
    #' frame with river segments, hydrocode, property names, and feature ids and
    #' extract id columns to use in this object to define source and destination
    #' nodes. This method uses bundle to detect non-river segments to assign
    #' proper source ids (e.g. facility names used in place of river segment
    #' source id on a facility node)
    #' @param df A data frame of potential network data with an OM style rseg
    #'   that can be parsed to source and destination nodes, with non-watersheds
    #'   characterized by propname
    #' @param om_rseg_col Character, default to "riverseg". The field containing
    #'   riversegments akin to JL3_6680_6970
    #' @param om_hydrocode_col Character, default to "hydrocode". The field that
    #'   contains the hydrocode of the entity table.
    #' @param om_propname_col Chacater, default to propname. The name of the
    #'   node. Used to name source node IDs for non-watersheds.
    #' @param om_featureid_col Character, default is "featureid". The field in
    #'   df that contains the primary key id relating entries to the entity
    #'   table
    #' @param ds A \code{RomDataSource} object, defaulting to that stored on
    #'   this object via \code{initialize()}
    #' @return A list with src_nodes defining the source nodes and dest_nodes
    #'   defining the destination of each node
    get_om_rseg_node = function(df, om_rseg_col = "riverseg",
                                om_hydrocode_col = "hydrocode",
                                om_propname_col = "propname",
                                om_featureid_col = "featureid",
                                ds = self$ds){
      if(((inherits(self$datasource, "character") && self$datasource == "om")) ||
              self$src_node_col == "riverseg"){
        #Join in bundle if not present from initialize (which can happen if a
        #data frame was loaded in data source)
        if(!("bundle" %in% names(df))){
          if(entity_type == "dh_feature_fielded"){
            entity_type <- "dh_feature"
          }else{
            entity_type <- self$entity_type
          }
          #Get the bundle for each feature to use to distinguish watersheds
          data_bundles <- DBI::dbGetQuery(
            ds$connection,
            paste0(
              "SELECT ",self$entity_id_col,", bundle
             FROM ",entity_type,"
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
        }else{
          join_data <- df
        }
        #For watershed features, the source watershed is the second series of
        #number and the destination is the third. However, the source of the 
        #facility feature should be the propname and the destination the source
        #river segment to indicate the model is on that node
        join_data$src_node <- gsub("(.+)_(.+)_(.+)","\\2",df[,om_rseg_col])
        join_data$dest_node <- gsub("(.+)_(.+)_(.+)","\\3",df[,om_rseg_col])
        
        #For old school naming convention like OR3_7740_8271_catawba we append
        #remaining characters onto the src id and ensure proper dest id
        join_data$src_node[grepl("(.+)_(.+)_(.+)_(.+)", df[,om_rseg_col])] <- 
          gsub("(.+)_(.+)_(.+)_(.+)", "\\2_\\4", df[grepl("(.+)_(.+)_(.+)_(.+)", df[,om_rseg_col]) ,om_rseg_col])
        join_data$dest_node[grepl("(.+)_(.+)_(.+)_(.+)", df[,om_rseg_col])] <- 
          gsub("(.+)_(.+)_(.+)_(.+)", "\\3", df[grepl("(.+)_(.+)_(.+)_(.+)", df[,om_rseg_col]) ,om_rseg_col])
        
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
    #' @description
    #' Ensures all nodes ids in src_node_col are unique by appending an
    #' iterator on duplicates such that
    #' \code{c("nodeA","nodeA","nodeB","nodeB","nodeA")} becomes
    #' \code{c("nodeA","nodeA1","nodeB","nodeB1","nodeA2")}
    #' @return Nothing, but sets network_data to have unique source nodes by
    #'   appending an iterator on the ID to signify a duplicate
    force_unique_src_ids = function(){
      #If user wants to ensure all source ids are unique, add iterator to node ids
      it <- 1
      while(any(duplicated(self$network_data[,self$src_node_col]))){
        self$network_data[,self$src_node_col][duplicated(self$network_data[,self$src_node_col])] <- 
          paste0(self$network_data[,self$src_node_col][duplicated(self$network_data[,self$src_node_col])], "_", it)
        it <- it + 1
      }
    },
    #' @description
    #' Converts source and destination node columns in network_data and
    #' network_graph to be numeric integers such that each unique node is given
    #' a unique integer value. Can optionally be run to force unique source IDs.
    #' @param force_unique_src_ids Logical, default FALSE. Should
    #'   \code{force_unique_src_ids()} method be used to ensure unique IDs?
    #' @return Nothing, but sets network_data and network_graph to use the now
    #'   integer source IDs.
    set_numeric_nodes = function(force_unique_src_ids = FALSE){
      #Convert src_node_col and dest_node_col to numeric fields
      allids <- factor(c(self$network_data[,self$src_node_col],self$network_data[,self$dest_node_col]))
      allids_numeric <- as.integer(allids)
      #Update source and destination node columns with integer ids
      self$network_data[,self$src_node_col] <- allids_numeric[match(self$network_data[,self$src_node_col],allids)]
      self$network_data[,self$dest_node_col] <- allids_numeric[match(self$network_data[,self$dest_node_col],allids)]
      
      #Update the igraph with numeric ids
      edges <- c(
        rbind(
          self$network_data[,self$src_node_col],
          self$network_data[,self$dest_node_col]
        )
      )
      #Create the directed graph from igraph
      self$network_graph <- igraph::make_graph(edges, directed = TRUE)
    },
    #' @description
    #' Creates a list of all nodes upstream or downstream of target source node
    #' and can optionally return aliases for node IDs
    #' @param direction Character, default "upstream". Either "upstream" or
    #'   "downstream" and determines if node searching should look for all
    #'   upstream or downstream nodes from target source nodes.
    #' @param value Character, default NA. A field in network_data containing
    #'   node aliases to return. For instance, may be 'riverseg' for OM data
    #'   from \code{om_vahydro_metric_grid()} to return upstream/downstream
    #'   riversegments (rather than just source segment id e.g. to return
    #'   JL3_6680_7280 instead of 6680)
    #' @param src_nodes Character, defaults to "all". Either "all" or a
    #'   character vector of source node IDs. Related nodes will only be
    #'   returned for these target source nodes.
    #' @return A list of the nodes or value in target direction from all or
    #'   src_nodes
    get_node_relation = function(direction = "upstream", value = NA, src_nodes = "all"){
      #If user has provided a value and the value is a column name in the
      #network data, assign and attribute to the graph matching to ensure proper
      #order
      g <- self$network_graph
      if(!is.na(value) && value %in% names(self$network_data)){
        g <- igraph::set_vertex_attr(
          graph = g,
          name = value,
          value = self$network_data[match(igraph::as_ids(igraph::V(g)),
                                          self$network_data[,self$src_node_col]),
                                    value]
        )
        self$network_graph <- g
      }
      
      #Set the direction variable for a depth first search in igraph
      direction <- switch(direction, upstream = "in", downstream = "out")
      #If user puts in a set of target node, find relations to only those nodes.
      #Otherwise, return all.
      if(any(src_nodes == "all")){
        target_node <- self$network_data[,self$src_node_col]
      }else{
        target_node <- src_nodes
      }
      #For each vertex, find all upstream and downstream vertex
      allSearch <- mapply(
        FUN = function(target_node, direction, inetwork, value){
          #Find fastest way upstream
          depths <- igraph::dfs(inetwork, root = target_node,
                                mode = direction, unreachable = FALSE)
          #If value is set, return the value attribute set of the vertices of
          #the graph; otherwise, return the vertices
          if(!is.null(igraph::vertex_attr(g,value))){
            out <- igraph::vertex_attr(g,value)[depths$order]
          }else{
            out <- igraph::as_ids(depths$order)
          }
          return(out)
        }, 
        target_node = target_node,
        MoreArgs = list(inetwork = g, direction = direction, value = value)
      )
      #Return a list of nodes for each node in network_data
      return(allSearch)
    },
    #' @description
    #' A subset of network_data that contains all data from nodes in target
    #' direction of source node (with optional destination node)
    #' @param src_node Character. The starting node to look for related nodes
    #'   and data in direction. This may optionally require a dest_node.
    #' @param dest_node Character. Default NA. The dest_node of src_node in case
    #'   of non-unique source IDs
    #' @param direction Character. Default "upstream". Either "upstream" or
    #'   "downstream" and represent the direction along the network with which
    #'   to search for related data.
    #' @return A data frame of the extracted node network from src_node (with
    #'   dest_node) in direction
    extact_node_network = function(src_node, dest_node = NA, direction = "upstream"){
      node_relation <- self$get_node_relation(direction, src_nodes = src_node)
      #Store data frame with network data
      network_data <- self$network_data
      
      if(is.na(dest_node)){
        #If now dest_node specified, assume src_node is unique and return first
        #instance of matching src node only but warn user if multiple found
        thisNode <- self$network_data[self$network_data[,self$src_node_col] == src_node,]
        if(nrow(thisNode) > 1){
          thisNode <- thisNode[1,]
          message("Multiple src_nodes found with id ",src_node," using only the first")
        }
      }else{
        #If a destination node is specified, find the node with the matching src
        #and destination nodes
        thisNode <- self$network_data[self$network_data[,self$src_node_col] == src_node &
                                        self$network_data[,self$dest_node_col] == dest_node,]
      }
      #Return data contained in the network by returning all data with source
      #AND destination nodes in node relation OR where it IS the source node
      #with optional destination node
      thisNetwork <- network_data[(network_data[,self$src_node_col] %in% node_relation &
                                  network_data[,self$dest_node_col] %in% node_relation) |
                                  (network_data[,self$src_node_col] == thisNode[,self$src_node_col] &
                                     network_data[,self$dest_node_col] == thisNode[,self$dest_node_col]),]
      
      return(thisNetwork)
    },
    #' @description
    #' Calculate the cumulative sum of a value defined by value_col from all
    #' upstream nodes of each node (defined by \code{get_node_relation()})
    #' @param value_col Character. The value in network_data to sum across
    #'   upstream nodes
    #' @param out_col_name Character. Default "cumulative_sum" and represents
    #'   where output network cumulative sum should be stored.
    #' @return A data frame of network_data now containing a field named by
    #'   out_col_name for the cumulative sum of the values in value_col
    calc_cumulative = function(value_col, out_col_name = "cumultive_sum"){
      #Store data frame with network data
      network_data <- self$network_data
      #Create columns for a cumulative allocation and a new WA (in mgd)
      network_data[,out_col_name] <- 0
      
      # Find the upstream segments for each nod in network_data. Store as a list
      # col on network_data
      network_data$upstream_nodes <- self$get_node_relation()
      #Create a column that indicates if a node is completed
      network_data$calculations_completed <- FALSE
      it <- 0
      #Run a loop on each node that checks to see if that watershed has been
      #mark completed and, if not, iterates through it's upstream nodes
      #beginning at the top and moving downstream. This nested while loop within
      #the broader for loop allows network_data to be completely disorganized
      #and the network math will still be correct.
      for (j in 1:nrow(network_data)) {
        if(network_data$calculations_completed[j]){
          next
        }else{
          #If not completed, find just this whole watershed in network_data
          #incluing upstream segments. In case of non-unique source nodes, we
          #check that the node is in the upstream nodes and its destination node
          #is also in upstream nodes OR both the source and destination node are
          #equal to this row
          thisBasin <- network_data[(network_data[,self$src_node_col] %in% network_data$upstream_nodes[[j]] &
                                      network_data[,self$dest_node_col] %in% network_data$upstream_nodes[[j]]) |
                                      (network_data[,self$src_node_col] == network_data[j,self$src_node_col] &
                                         network_data[,self$dest_node_col] == network_data[j,self$dest_node_col]),]
          #While any row in thisBasin is not yet mark completed, run a for
          #loop that iterates through thisBasin and totals the newAlloation
          #and adjustedWA when all subwatersheds have been run. This will
          #first run for headwaters, then the while loop will iterate and the
          #for loop will effectively only run second order streams and so on
          while(any(!thisBasin$calculations_completed)){
            for (k in 1:nrow(thisBasin)) {
              #if thisBasin$upstream_nodes is only one segment and identical to
              #src_id, its a headwater and can be completed now
              if(identical(thisBasin$upstream_nodes[[k]],thisBasin[k,self$src_node_col])){
                thisBasin[k,out_col_name] <- thisBasin[k,value_col]
                thisBasin$calculations_completed[k] <- TRUE
              }else if(
                all(
                  thisBasin$calculations_completed[match(thisBasin$upstream_nodes[[k]], thisBasin[,self$src_node_col])] |
                  thisBasin$upstream_nodes[[k]] == thisBasin[k, self$src_node_col]
                )
              ){
                #Otherwise, only run a basin if all the segments in
                #upstream_nodes has either been mark completed already or is
                #this segment
                thisBasin[k,out_col_name] <- sum(thisBasin[thisBasin[,self$src_node_col] %in% thisBasin$upstream_nodes[[k]], value_col])
                thisBasin$calculations_completed[k] <- TRUE
              }
            }
            #Total number of while loop iterations, for record keeping
            #purposes but not used
            it <- it + 1
          }
          #Find where source and destination match in both data frames
          data_lookup <- match(paste0(thisBasin[,self$src_node_col], thisBasin[,self$dest_node_col]), paste0(network_data[,self$src_node_col],network_data[,self$dest_node_col]))
          #Store data in network_data
          network_data[data_lookup,out_col_name] <- thisBasin[,out_col_name]
          network_data$calculations_completed[data_lookup] <- thisBasin$calculations_completed
          
        }
      }
      return(network_data)
    },
    #' @description
    #' Get a spatial data frame by joining in entity spatial data defined by
    #' pkid_col with optional color and metadata columns calculated from
    #' value_col to allow for rapid plotting.
    #' @param pkid_col Character. Default "feature_id" and represents
    #'   how to join entity table onto data to retrieve spatial data
    #' @param value_col Character. The value in network_data to create color and
    #'   metadata columns for.
    #' @param value_label Character. A metadata label defining value_col.
    #' @return A SF data frame containing geometries and optional color and
    #'   metadata columns defining values in value_col
    get_spatial_network = function(pkid_col = "featureid", value_col = NA,
                                   value_label = "value"){
      network_data <- self$network_data
      # Join on river segment hydroids and then query dh_feature_fielded for
      # geometries Filter down the list of features to only match model
      # results
      hydroFeats <- DBI::dbGetQuery(
        paste0("SELECT f.hydroid, f.hydrocode, f.name, f.dh_geofield
                  FROM ", self$entity_type," as f
                  WHERE f.dh_geofield_geom IS NOT NULL
                    AND f.hydroid IN (",paste0(network_data[,pkid_col],collapse = ","),")
                 ORDER BY st_area(f.dh_geofield_geom) DESC
                 "),
        conn = ds$connection)
      
      #Join in the results data
      feat_results <- hydroFeats |> 
        dplyr::left_join(network_data, by = c("hydroid" = pkid_col))
        
      #If user has assigned a value_col, use this to populate a label, popup,
      #and color column that can be used by ggplot or leaflet
      if(!is.na(value_col)){
        feat_results$popup <- paste0('Hydrocode: ',feat_results$hydrocode,"<br>",
                                     "Name: ",feat_results$name,"<br>",
                                     value_label,round(feat_results[,value_col],4), "<br>" )

        feat_results$value_label <- paste0('Hydrocode: ',feat_results$hydrocode,"; ",
                                     value_label,round(feat_results[,value_col],4))
        #Assign a color to each value, scaled by:
        #((value + abs(min)) / (abs(min)+ max)) to ensure a scale of 0 to 1 for
        #colorRamp()
        rgb_color <- colorRamp(c("darkred", "steelblue"))((abs(min(feat_results[,value_col], na.rm = TRUE)) + feat_results[,value_col]) / (abs(min(feat_results[,value_col], na.rm = TRUE)) + max(feat_results[,value_col], na.rm = TRUE)))
        rgb_color[is.na(feat_results[,value_col]),] <- 127
        feat_results$color <- apply(rgb_color/255, 1, function(x) rgb(x[1], x[2], x[3]))
      }
      
      ## Making it spatial
      feat_results_sf <- sf::st_as_sf(feat_results, wkt = "dh_geofield", crs = 4326)
      feat_results_sf <- sf::st_make_valid(feat_results_sf)
      return(feat_results_sf)
    },
    #' @description
    #' Get an interactive force network diagram of network_data with IDs defined
    #' by name_col, colors by group_col, node size by size_col, and a
    #' default_size for nodes with no size_col value
    #' @param name_col Character. A field in network_data that can characterize
    #'   node names.
    #' @param group_col Character. A field in network_data that can be used to
    #'   color the data by broad groups e.g. like bundle from OM data.
    #' @param size_col Character. A numeric field in network_data from which the
    #'   size of each node may be calculated.
    #' @param default_size Integer. Default 5. Number of pixels each node should
    #'   be sized to by default if no size_col value provided.
    #' @return A \code{networkD3} interactive (JS) force diagram of the network
    #'   data with size and colors broadly controlled by user.
    render_force_diagram = function(
      name_col = NA, group_col = NA,
      size_col = NA,default_size = 5)
    {
      #Convert the network igrpah to d3 JS
      plot_data <- networkD3::igraph_to_networkD3(self$network_graph)
      #Assign an arbitrary edge width
      plot_data$links$value <- 1
      #IF user wants to group the data (e.g. color the data by this grouping
      #variable), assign a value to each node with the matching data from
      #self$network_data
      if(!is.na(group_col) && name_col %in% names(self$network_data)){
        plot_data$nodes$group <- self$network_data[match(plot_data$nodes$name, self$network_data$src_node),group_col]
      }else{
        plot_data$nodes$group <- "result"
      }
      #IF user wants to size the data (e.g. each node has a size based on this
      #column) assign a value to each node based on its value relative the max
      if(!is.na(size_col) && name_col %in% names(self$network_data)){
        plot_data$nodes$size <- abs(ceiling(10 * self$network_data[,size_col] / max(self$network_data[,size_col], na.rm = TRUE)))
        if(any(is.na(plot_data$nodes$size))){
          message("Some sizes from",size_col,"were calculated to be NA. These
          have been replaced by defualt size",default_size)
        }
        plot_data$nodes$size[is.na(plot_data$nodes$size)] <- default_size
      }else{
        plot_data$nodes$size <- default_size
      }
      
      #If user wants the nodes to have a name other than src_id, assign it to
      #nodes
      if(!is.na(name_col) && name_col %in% names(self$network_data)){
        plot_data$nodes$name <- self$network_data[match(plot_data$nodes$name, self$network_data$src_node),name_col]
      }
      
      #Create the network d3 force diagram
      p <- networkD3::forceNetwork(
        Links = plot_data$links, Nodes = plot_data$nodes, 
        Source = "source", Target = "target", Value = "value",
        NodeID = "name", Group = "group", Nodesize = "size",
        linkDistance = 50, charge = -30, fontSize = 7, fontFamily = "serif",
        linkWidth = "'1.5px'.toString()", opacityNoHover = 1,
        opacity = 1, zoom = TRUE,
        radiusCalculation = networkD3::JS("d.nodesize + 4"))
      
      return(p)
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
