library(raster)
library(PET)
library(imager)

im <- load.image("Dualsatellites.jpg")
im.gray<-grayscale(im)
a<-matrix(as.data.frame(im.gray)$value,nrow = 600, ncol = 400, byrow = TRUE)
plot(im.gray)

im.blurry <- isoblur(im.gray,10) #Blurry image
dev.new()
plot(im.blurry)
###
im.xedges <- deriche(im.gray,2,order=2,axis="x") #Edge detector along x-axis
dev.new()
xfil<-plot(im.xedges)
###
im.yedges <- deriche(im.gray,2,order=2,axis="y") #Edge detector along y-axis
dev.new()
yfil<-plot(im.yedges)

