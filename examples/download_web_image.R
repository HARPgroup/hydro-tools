library('png') #required for readPNG()

image_url <- 'https://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile157244242118784.png'
download.file(image_url,'image.png', mode = 'wb') #downloads image to current directory

#Display image in R:
image_nativeRaster <- readPNG("image.png",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(image_nativeRaster,0,0,1,1)


df <- file.info(list.files("https://deq1.bse.vt.edu/drought/state/images/maps/", full.names = T))
rownames(df)[which.max(df$mtime)]

tmpshot <- fileSnapshot("https://deq1.bse.vt.edu/drought/state/images/maps")
rownames(tmpshot$info[which.max(tmpshot$info$mtime),])

list.files("https://deq1.bse.vt.edu/drought/")