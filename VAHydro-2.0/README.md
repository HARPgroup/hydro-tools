functions for vahydro-2.0 entities


1) rest_token()   -function to retrieve REST token
2) getProperty()  -function that returns dataframe of properties matching supplied *varkey, *featureid, *entity_type 
3) postProperty() -function that searches for existing properties matching supplied *varkey, *featureid, *entity_type 
      1. If none exist, property is created 
      2. If one exists, property is updated 
      3. If more than one exist, execution is halted
4) getVarDef() -function for retrieving dh variable definition
5) getFeature() -function for retrieving dh features
6) vahydro_fe_data() -function for formating dh eco hydro queries 
7) vahydro_prop_matrix() -function for retrieving dh property matrix data 
