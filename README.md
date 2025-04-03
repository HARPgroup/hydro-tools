# hydrotools

## Description

The R developers at DEQ's Office of Water Supply Planning and Analysis have developed an R package called `hydrotools` to support data base connections to OWSPA data sources and ensure consistent handling of common data base requests. The `rom` objects offered in hydrotools serve as a quasi-REST service for the VA Hydro databases, offering DEQ staff convenient means of getting properties and features or posting such entities. Installation instructions are offered below. Development issues, pull requests, and more are tracked on the hydrotools [GitHub repository](https://github.com/HARPgroup/hydro-tools) or on the internal [GitLab](https://gitlab.deq.virginia.gov/) server. 

## Revelant Data Base Terms
**dh_feature** = A table containing physically grounded "features" that include wells, intakes, and more. Each entry in *dh_feature* should have an actual lat/long or geometry associated with it.   
Features have several descriptors:  
*hydroid* = A unique identifier for each feature  
*hydrocode* = A code that may relate this feature to other databases or data sources e.g. a feature used to represent a USGS gage may have a hydrocode related to its USGS gage number  
*bundle* = A general description of the type of feature that this entry may represent. Bundles are used to group similar data together. For instance, features representing drainages or watersheds are all assigned a `bundle` of "watershed"  
*ftype* = Bundle "subclasses" that help group similar features under the same bundle e.g. USGS gage watershed features have a "usgs_full_drainage" `ftype` under the "watershed" bundle  
*name* = The name of the feature. This may be imported from its primary data source or be assigned by DEQ staff upon creation. A USGS gage, for instance, will have a `name` equivalent to the USGS gage name. But, a well may have a name from the appropriate DEQ GW-2  

**dh_properties** = 
**dh_variabledefinition**
**hydroid**
**pk**
**hydrocode**

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


