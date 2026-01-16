# hydrotools

<!-- badges: start -->
[![R-CMD-check](https://github.com/HARPgroup/hydro-tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HARPgroup/hydro-tools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Description

The R developers at DEQ's Office of Water Supply Planning and Analysis have developed an R package called `hydrotools` to support data base connections to OWSPA data sources and ensure consistent handling of common data base requests. The `rom` objects offered in hydrotools serve as a quasi-REST service for the VA Hydro databases, offering DEQ staff convenient means of getting properties and features or posting such entities. Installation instructions are offered below. Development issues, pull requests, and more are tracked on the hydrotools [GitHub repository](https://github.com/HARPgroup/hydro-tools) or on the internal [GitLab](https://gitlab.deq.virginia.gov/) server. 

## Installation

### DEQ Users
This package is available for internal distribution on the TEST (UAT) DEQ Posit Package manager. Additional information on this may be found in the [DEQ Methods Encyclopedia](https://positconnect.deq.virginia.gov/DEQmethods/). Users will need to add the UAT package manager to their list of secondary repositories: https://positpackagemanager-uat.deq.virginia.gov/DEQmethods/latest. A config file may be provided by the package managers for database integration.  
```
install.packages("hydrotools")
```

### All Other Users
A config file may be provided by the package managers for database integration.
``` r
install.packages("devtools")
library("devtools")
#Make sure it hasn't been called, but if it has we can unload it
unloadNamespace('hydrotools')
#Get the master branch deployment of the package
devtools::install_github("HARPgroup/hydro-tools")
# alternate, install a development branch for testing:
devtools::install_github("HARPgroup/hydro-tools", ref = "odbcnew", force=TRUE)

#EXAMPLE FUNCTION DOCUMENTATION
??om_vahydro_metric_grid
```

## Revelant Data Base Terms
**dh_feature** = A table containing physically grounded "features" that include wells, intakes, and more. Each entry in *dh_feature* should have an actual lat/long or geometry associated with it.   
Features have several descriptors:  
*hydroid* = A unique identifier for each feature  
*hydrocode* = A code that may relate this feature to other databases or data sources e.g. a feature used to represent a USGS gage may have a hydrocode related to its USGS gage number. This field was also once a unique identifier in previous databases. Generally encouraged this field be unique, but not required.
*bundle* = A general description of the type of feature that this entry may represent. Bundles are used to group similar data together. For instance, features representing drainages or watersheds are all assigned a `bundle` of "watershed"  
*ftype* = Bundle "subclasses" that help group similar features under the same bundle e.g. USGS gage watershed features have a "usgs_full_drainage" `ftype` under the "watershed" bundle  
*name* = The name of the feature. This may be imported from its primary data source or be assigned by DEQ staff upon creation. A USGS gage, for instance, will have a `name` equivalent to the USGS gage name. But, a well may have a name from the appropriate DEQ GW-2  
*fstatus* = A description for the status of the feature, including "active", "inactive", "abandoned", etc. This is used to track the life of a feature and if it is still in use.


**dh_adminreg_feature** = A table containing "features" that have no physical grounding, such as permits, Water Supply Plan (WSP) information, or web submittals. Each entry will not have a lat/long or coordinates, but may be associated with a physical feature that does. The fields in this table are equivalent to dh_feature, the only difference being the name of the primary key and admincode field   
*adminid* = Unique identifier for each adminreg_feature. This and hydroid are defined by the same autonumber, so each hydroid and adminid are unique to each other.  
*admincode* = Equivalent to hydrocode. This field provides information about a record that may relate to its origins in a different database or relation to another record. This field is not typically used in any way other than reference.  
*bundle/ftype* = Equivalent to dh_feature fields, but with a unique set of bundle/ftypes.


**dh_properties** = A table of supporting information about features, adminreg_features, other properties, or timeseries. This table contains any information (other than time series) for a record that is not captured in the base fields of feature tables. With the propcode and propvalue fields, this table can contain both numeric or string data, including up to paragraphs of text. Properties are generally directly connected to dh_feature or dh_adminreg_feature but can be connected to other records of dh_properties, creating a nested property that can be any number of levels deep. This is used for modeling, where a single facility can be a part of several models each with different runs with unique inputs.  
*pid* = A unique identifier for each property. This is for the property record itself, not any record it is in relation to  
*featureid* = The primary key field for the record that this property is tied to. If its a feature, this would be the hydroid, if its a property, it would be the pid. There can be overlap between the hydroid number and the pid, so this is used in combination with the entity_type to find the exact record.  
*entity_type* = Informs what table to look for the record, such as 'dh_feature' or 'dh_properties'  
*varid* = The variable id (from the dh_variabledefition table) that informs what this property is describing. This is generally repetitive information with the propname, but for some broad modeling variables the propname provides more details  
*propname* = A string describing what information the property is storing.   
*propvalue* = Numeric information of the property.   
*propcode* = Character information of the property.  
*startdate/enddate* = If the property has a start and end date, such as an expiration date, it would be captured here. While properties can include a start and an end date, any sort of regular or repeating data is captured in the dh_timeseries table.   
*bundle* = The class of information stored in the property. The majority of properties have a bundle of 'dh_properties', but there are a few special bundles used when a large number of properties exist with a single purpose, such as WSP projections.  


**dh_timeseries** = A table of supporting information about features or properties. dh_properties has a start and end time field, so this table is only for time series, repeating data where each point has a time element. Many of the fields from this table are similar to dh_properties, such as the featureid, entity_type, and varid to indicate what the record is attached to and what its data represents. 
*tid* = Unique identifier for each timeseries record  
*tstime* = The time associated with this record. If this record indicates a stretch of time, such as a week, the tstime represents the beginning of that period  
*tsendtime* = The end time of this record. If this record represents an instantaneous event or the end point is irrelevant, this can be left blank.  


**dh_variabledefinition** = This table defines the variables used in both the dh_properties and dh_timeseries table (linked by the varid field). This table essentially works to explain what the values in dh_properties and timeseries represent  
*hydroid* = Unique ID of the variable. This is also labeled hydroid, but is independent of the primary key of dh_feature  
*varname* = The name of the variable. This is a descriptor field that generally lines up with the propname field in dh_properties, but they can vary for object modelling properties  
*vardesc* = A short description of the variable   
*varkey* = Abbreviated alias of what this variable represents. This will match up with propname in dh_properties (except in the above discussed special cases)  


**field_data_dh_geofield** = Often referred to conversationally as dh_geofield, this is the table that links features (from dh_feature) to their physical location. It has the syntax of a linking table, but it links features directly to their geometry  
*entity_type* = The origin table of the record  
*entity_id* = The primary key of the feature (the hydroid)  
*dh_geofield_geom* = The geometry of the feature. This is in WKB, and can be a multipolygon of any size  
*dh_geofield_lat/dh_geofield_lon* = Latitude and Longitude. For points, i.e. wells, these are the coordinates. For polygons, i.e. watersheds, this is the centroid coordinates.

	
**hydroid** = Primary key of the dh_feature table. This table contains facilities, measuring points, and watersheds, the most commonly referenced records.  
**pk** = Primary Key, the unique identifier for a table.  
**hydrocode** =  A code that may relate a feature from dh_feature to other databases or data sources e.g. a feature used to represent a USGS gage may have a hydrocode related to its USGS gage number. This is an important field in features like watersheds or USGS gages, but is not commonly used for other feature types like facilities and measuring points. This field was once a unique identifier in previous databases. Generally encouraged this field be unique, but not required (can also be blank).  


Graft Hydrotools VA Hydro Views Structure (General)



## Contributing

Additional functionality suggested by DEQ R users is always welcome in the hydrotools package. If you have a process you believe would benefit other users, please contact Rob Burgholzer or Connor Brogan to begin the process of adding your function(s) to the package.

### Process for updating package

Begin by cloning the [hydrotools package](https://github.com/HARPgroup/hydro-tools). Next create a branch for your changes. Update, add, edit, etc. the relevant functions. If you add package dependencies to the repository that aren't leveraged in existing functions, you must include these in the `imports:` section of the DESCRIPTION file. Next, before committing the changes to your branch, you should use `devtools::document()` to update the .Rd file documentation associated with your function. These files are located in the man folder and are used when someone uses `?` to request documentation for your function. When ready, push your commit(s) to your branch, submit a merge request, and assign this request to Rob Burgholzer for review.

## Authors and acknowledgment

Connor Brogan ([connor.brogan\@deq.virginia.gov](mailto:connor.brogan@deq.virginia.gov){.email}) <br/>
Rob Burgholzer ([robert.burgholzer\@deq.virginia.gov](mailto:robert.burgholzer@deq.virginia.gov){.email})<br/> 


## Project status

This package is in active development.

## Release notes
### 1.0.11 01/07/2026
1. Updated the documentation on `RomFeature$get_raster_ts()` to be more accurate
for the starttime and endtime arguments. Additionally, added a `touched` argument
to leverage the new options in PostGIS `ST_Clip()`
2. Added an `outaszoo` argument to `om_get_rundata()` to allow data frames of
model data to be returned.

### 1.0.10 12/15/2025
1. Update elfgen wrappers to allow for more dynamic generation of NHDPlus flows.
2. Added an `na.rm = TRUE` to the mean flows call in `om_flow_table()`

### 1.0.9 12/01/2025
1. Added wrapper functions for elfgen package. `simple_nhdPlusFlows()` finds the NHDPlus segment (and flows) contained in a VA Hydro feature. The output code can be given to `simple_elfgen()` to allow for easy generation of ecologic limit functions from imported EDAS data

### 1.0.8 11/25/2025
1. Added the ability to query and summarize data via PostGIS from the `dh_timeseries_weather` table via the method `get_raster_ts()` on `RomFeature()`
2. New `fn_handletimestamp()` that performs basic checks on potential date/time data before converting to date using `lubridate` functions

### 1.0.7 11/13/2025
1. Fixed a variable name error in the `xQy()` function that was preventing the code from being able to handle non-standard flow data frames
2. Enabled examples in `xQy()`, `group1()`, `group2()`, and `fn_iha_7q10()`
3. Updated `fn_iha_7q10()` to use `xQy()` for consistency

### 1.0.6 10/06/2025
1. Bug fix to variable plugin for timeseries file objects by setting the correct object class to send to OM.

### 1.0.5 10/06/2025
1. Added 90Q10 to the default output of the `xQy()` function and did some minor debugging on function to ensure proper outputs of percentiles.
2. Added a new plug-in for timeseries file objects to allow for their import from/to OM.

### 1.0.4 09/30/2025
1. Removed rapportools dependence to ensure a clean environment for calling mean, median, max, min, etc. Updated all functions that relied on rapportools to no longer need the package and imported is.empty() from rapportools to hydrotools.
2. Added VPDES xQy low flow functions to hydrotools to support Shydrology (Shiny dashboard for USGS and VA Hydro hydrology calculations).

### 1.0.3 09/22/2025
1. Added an optional input to `RomDataSource$get_token()` to allow a user to use pool or dbi connections. This was added to aid in Shiny development while following DEQ R Team guidance.
2. Added a simple GIS mapping function for us in VWP summaries, `simple_wshed_map()`. See example for details.

### 1.0.2 09/12/2025
1. Documented and exported `om_flow_table()` from fac_utils.R.
2. Documented and exported `om_cu_table_data()` to create a data.frame of percent or numeric differences akin to those used in the display `flextable` in `om_cu_table()`.

### 1.0.1 09/05/2025
1. Removed the need for host_site in the `fn_get_runfile` to ensure clarity with update deq1 apache settings.

### 1.0.0 08/29/2025
1. Updated *fn_get_runfile()* to no longer require an HTTP connection when determining the host site. Instead, it now determines the URL prefix based on the site passed in by the user.

### 0.0.0.1 Initial Release


