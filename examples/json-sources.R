library(hydrotools)
token <- om_vahydro_get_token()
jurl = "http://deq2.bse.vt.edu/d.dh/dh-properties-openmi-json/5031577"
jurl1 <- 'http://deq2.bse.vt.edu/d.dh/dh-feature-json/146'

model_json <- om_auth_read(jurl, token)
model_json <- om_auth_read(jurl, token, ctype = "text/json", delim=',', enc="json")
model_obj <- jsonlite::fromJSON(as.character( model_json[[1]]) )
model_obj <- model_obj[[1]]
names(model_obj)

# as csv/xml
model_json1 <- om_auth_read(jurl1, token)
# native json import
model_json1 <- om_auth_read(jurl1, token, ctype = "text/json", delim=',', enc="json")
model_obj1 <- model_json1[[1]][[1]]$entity 
names(model_obj1)

jurl2 <- "http://deq2.bse.vt.edu/d.dh/dh-properties-openmi-json/5031577"
model_json2 <- om_auth_read(jurl2, token, ctype = "text/json", delim=',', enc="json")
model_obj2 <- jsonlite::fromJSON(as.character( model_json2[[1]]) )
model_obj2 <- model_obj2[[1]]
names(model_obj2)

# 
