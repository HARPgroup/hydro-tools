# test user
odbc_name = 'jofam_odbc'
dso <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'dbname03',
  host = "my.data.base",
  port = 5431,
  user = odbc_name,
  password =  getPass::getPass("REST Password: ") # 
)

sqldf("select count(*) from dh_timeseries", connection=dso)

sqldf("select current_role", connection=dso)
sqldf("select * from field_data_dh_geofield where bundle = 'monitoringpoint' limit 1", connection=dso)
sqldf("select * from field_data_dh_geofield where bundle = 'intake' limit 1", connection=dso)
sqldf("ALTER USER jofam_odbc PASSWORD 'b@d!dea';", connection=dso)
sqldf("ALTER USER jofam_odbc PASSWORD 'b3Tt3r!dea';", connection=dso)
