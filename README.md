# hydrotools

## Description

The R developers at DEQ's Office of Water Supply Planning and Analysis have developed an R package called `hydrotools` to support data base connections to OWSPA data sources and ensure consistent handling of common data base requests. The `rom` objects offered in hydrotools serve as a quasi-REST service for the VA Hydro databases, offering DEQ staff convenient means of getting properties and features or posting such entities. Installation instructions are offered below. Development issues, pull requests, and more are tracked on the hydrotools [GitHub repository](https://github.com/HARPgroup/hydro-tools) or on the internal [GitLab](https://gitlab.deq.virginia.gov/) server. 

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



## Installation

### DEQ Users (coming soon!)
This package will soon be available for internal distribution on the DEQ Posit Package manager. Additional information on this may be found in the [DEQ Methods Encyclopedia](https://positconnect.deq.virginia.gov/DEQmethods/). A config file may be provided by the package managers for database integration.

### All Other Users
A config file may be provided by the package managers for database integration.
``` r
install.packages("devtools")
library("devtools")
#Make sure it hasn't been called, but if it has we can unload it
unloadNamespace('hydrotools')
#Get the master branch deployment of the package
install_github("HARPgroup/hydro-tools")
# alternate, install a development branch for testing:
install_github("HARPgroup/hydro-tools", ref = "odbc", force=TRUE)

#EXAMPLE FUNCTION DOCUMENTATION
??om_vahydro_metric_grid
```

## Project status

This package is in active development.

## Release notes

### 0.0.0.1 Initial Release


