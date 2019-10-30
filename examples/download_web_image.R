library('png') #required for readPNG()

image_url <- 'https://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile157244242118784.png'
download.file(image_url,'image.png', mode = 'wb')


image_nativeRaster <- readPNG("image.png",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(image_nativeRaster,0,0,1,1)


