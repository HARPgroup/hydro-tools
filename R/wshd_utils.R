#'@name usgs_bankfull_properties
#'@title usgs_bankfull_properties
#'@description Uses USGS bankfull regional curves for each VA geomorphic
#'  province to calculate bankfull channel stage, width, full width, and side
#'  slope
#'@details See USGS bankfull regression reports for additional information. The
#'  empirical bankfull equations are used to calculate channel properties based
#'  on which province the channel is in and the drainage area at that point.
#'  This data is often used in OM to set local_channel properties. Returns a list 
#'  with bank full stage "max height before floodplain", bank full width "max width before floodplain",
#'  base width "width at lowest stage", side slope of channel
#'@param prov Integer 1 - 4. Geomorphic province 1 = appalachian plateau, 2 =
#'  valley and ridge, 3 = Piedmont, 4 = coastal plain
#'@param da Numeric. The drainage area of your channel.
#'@return A list with the bank full stage (h), bank full width (bf), base width
#'  (b), and side slope (z)
#'@example 
#'#usgs_bankfull_properties(prov = 1,da = 10)
#'@export
usgs_bankfull_properties <- function(prov, da) {
  #----Provincial Channel Geometry----
  if (prov == 1){
    #Appalachian Plateau
    hc = 2.030 # "c" = coefficient for regional regression eqn
    he = 0.2310 # "e" = exponent for regional regression eqn
    bfc = 12.175
    bfe = 0.4711
    bc = 5.389
    be = 0.5349
    n = 0.036 # Manning's n
    nf = 0.055 # Floodplain n
  } else if (prov == 2){
    #Valley and Ridge
    hc = 1.435
    he = 0.2830
    bfc = 13.216
    bfe = 0.4532
    bc = 4.667
    be = 0.5489
    n = 0.038
    nf = 0.048
  }else if (prov == 3){
    #Piedmont
    hc = 2.137
    he = 0.2561
    bfc = 14.135
    bfe = 0.4111
    bc = 6.393
    be = 0.4604
    n = 0.095
    nf = 0.063
  }else if (prov == 4){
    #Coastal Plain
    hc = 2.820
    he = 0.2000
    bfc = 15.791
    bfe = 0.3758
    bc = 6.440
    be = 0.4442
    n = 0.040
    nf = 0.06
  }
  
  # Regional Regression Eqn's:
  #bank full stage "max height before floodplain":
  h = hc * (da**he)
  #bank full width "max width before floodplain":
  bf = bfc * (da**bfe)
  #base width "width @ lowest stage":
  b = bc * (da**be)
  #side slope of channel:
  z = 0.5 * (bf - b ) / h
  
  return(
    list(
      h = h,
      bf = bf,
      b = b,
      Z = z
    )
  )
}
