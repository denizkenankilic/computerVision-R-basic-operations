#install.packages("plyr")
#library(plyr)
#library(magrittr)
#library(readbitmap)
install.packages("imager")
library(imager)
require(imager)
install.packages("grDevices")
library(grDevices)
install.packages("biOps")
library(biOps)
install.packages("raster")
library(raster)
install.packages("installr")
library(installr)

install.ImageMagick(URL ="http://www.imagemagick.org/script/binary-releases.php")

#######################################################################################################################
# "R Colors" graphic
# 1a. Plot matrix of R colors, in index order, 25 per row.
# This example plots each row of rectangles one at a time.
SetTextContrastColor <- function(color)
{
  ifelse( mean(col2rgb(color)) > 127, "black", "white")
}
# Define this array of text contrast colors that correponds to each
# member of the colors() array.
TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )

colCount <- 25 # number per row
rowCount <- 27
plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
      axes=FALSE, ylim=c(rowCount,0))
title("R colors")

for (j in 0:(rowCount-1))
{
  base <- j*colCount
  remaining <- length(colors()) - base
  RowSize <- ifelse(remaining < colCount, remaining, colCount)
  rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
       border="black",
       col=colors()[base + (1:RowSize)])
  text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
       col=TextContrastColor[base + (1:RowSize)])
}
######################################################################################################################

######################################################################################################################
#The inverse operation is called imappend: it takes a list of 
#images and concatenates them along the dimension of your choice.
#Sample functions and turn them into separate R,G,B channels
R <- as.cimg(function(x,y) sin(cos(3*x*y)),100,100)
G <- as.cimg(function(x,y) sin(cos(3*x*y + pi/2)),100,100)
B <- as.cimg(function(x,y) exp(-.03*x),100,100)
trippy <- imappend(list(R,G,B),"c") #Bind the three channels into one image
plot(trippy)
######################################################################################################################

im <- load.image("space.jpg")
plot(im)
dim(im) #The four dimensions are labelled x,y,z,c. The first two are the usual spatial 
#dimensions, the third one will usually correspond to depth or time, and the fourth one is colour. 
#Remember the order, it will be used consistently in imager. width, height, depth, spectrums
imsplit(im,"c") %>% laply(mean) # seperating to channels and find mean pixel value in each channel
imsplit(im,"x") %>% laply(mean) %>% head #Mean pixel value in each line (across all channels)

# Convert imagedata to raster
rst.blue <- raster(im[,,1])
rst.green <- raster(im[,,2])
rst.red <- raster(im[,,3])
# rst.red$layer
###
plot(rst.blue)
contour(rst.blue, add=TRUE)
###
# Plot single raster images and RGB composite
dev.new()
plot(stack(rst.blue, rst.green, rst.red), 
     main = c("Blue band", "Green band", "Red band"))
dev.new()
plotRGB(stack(rst.blue, rst.green, rst.red))
###
im.blurry <- isoblur(im,10) #Blurry image
dev.new()
plot(im.blurry)
###
im.xedges <- deriche(im,2,order=2,axis="x") #Edge detector along x-axis
dev.new()
xfil<-plot(im.xedges)
###
im.yedges <- deriche(im,2,order=2,axis="y") #Edge detector along y-axis
dev.new()
yfil<-plot(im.yedges)
###
#Chain operations using the pipe operator (from magrittr)
deriche(im,2,order=2,axis="x") %>% deriche(2,order=2,axis="y") %>% plot
###
#Another example of chaining: image gradient along x and y axes
layout(matrix(1:2,1,2));
grayscale(im) %>% get_gradient(axes="xy") %>% l_ply(plot)
###
#The file format is defined by the extension. Here we save as JPEG
imager::save.image(yfil,"linedety.png")

######################################################################################################################
#Split, apply, combine
#Often what one wants to do is to split the image 
#along a certain axis (e.g. colour), apply a transformation separately and recombine the result. iiply does that:
iiply(im,"c",function(v) v/max(v))  %>% plot
imsplit(im,"c") %>% add %>% plot(main="Adding colour channels")
#Use different levels of blur on the same image
blur.layers <- llply(seq(1,15,l=5),function(v) isoblur(im,v))

blur.layers %>% parmin %>% plot(main="Min across blur levels")
blur.layers %>% parmax %>% plot(main="Max across blur levels")
blur.layers %>% average %>% plot(main="Average across blur levels")
######################################################################################################################

######################################################################################################################
# Some Basics
imsub(im,x < 30) #Only the first 30 rows
imsub(im,y < 30) #Only the first 30 rows
imsub(im,x < 30,y < 30) #First 30 columns and rows
imsub(im, sqrt(x) > 8) #Can use arbitrary expressions
imsub(im,x > height/2,y > width/2)  #height and width are defined based on the image
imsub(im,cc==1) #Colour axis is "cc" not "c" here because "c" is an important R function
# If you need to access a specific colour channel, use any of the following:
R(im) 
G(im)
B(im)
#R(parrots) is equivalent to channel(parrots,1)

#Set all channels to 0 except red
im.cp <- load.image("space.jpg")
G(im.cp) <- 0
B(im.cp) <- 0
plot(im.cp)
#If you need pixel values along rows and columns use:
imrow(R(im),10) %>% plot(main="Red channel along 10th row",type="l")
imcol(B(im),10) %>% plot(main="Blue channel along 10th line",type="l")
# Individual pixels can be accessed using at and color.at:
at(im,x=20,y=20,cc=1:3)
color.at(im,x=20,y=20) #same as above
im2 <- imfill(4,4)
dim(im2) #4 dimensional, but the last two dimensions are singletons
im[,1,,] <- 1:4 #Assignment the standard way
im[,1] <- 1:4 #Shortcut
######################################################################################################################

######################################################################################################################
#DENOISING
# Denoising can be performed using basic filters that average over space:
im.noisy <- (im + 80*rnorm(prod(dim(im)))) #product of vector elements, generate random numbers
layout(t(1:2))
plot(im.noisy,main="Original")
isoblur(im.noisy,5) %>% plot(main="Blurred") #Blur image isotropically.
#Blurring removes some of the noise but also blurs away the contours. CImg 
#provides an anisotropic blur that does not have that problem:
layout(t(1:2))
plot(im.noisy,main="Original")
blur_anisotropic(im.noisy,ampl=1e5,sharp=1) %>% plot(main="Blurred (anisotropic)")
######################################################################################################################

######################################################################################################################
# COLOR SPACES
#To convert from RGB to HSL/HSV/HSI/YUV/YCbCR, run RGBto[.], as in the following example:
im.hsl <- RGBtoHSL(im) #hue, saturation, lightness; for converting wiki can be examined
chan <- channels(im.hsl) #Extract the channels as a list of images
names(chan) <- c("H","S","L")
#Plot
layout(matrix(1:3,1,3))
l_ply(names(chan),function(nm) plot(chan[[nm]],main=nm))
grayscale(im) %>% spectrum #Image has only one channel (luminance)
grayscale(im) %>% add.colour %>% spectrum #Image is still in gray tones but has R,G,B channels 
######################################################################################################################

######################################################################################################################
# Resizing, rotation, etc.
thmb <- resize(im,round(width(im)/10),round(height(im)/10))
plot(thmb,main="Thumbnail") #Pixellated im
#Same as above: negative arguments are interpreted as percentages
thmb <- resize(im,-10,-10)
imrotate(im,30) %>% plot(main="Rotating")
imshift(im,40,20) %>% plot(main="Shifting")
imshift(im,100,100,boundary=1) %>% plot(main="Shifting (Neumann boundaries)")
imshift(im,100,100,boundary=2) %>% plot(main="Shifting (circular)")
# You can pad an image using "pad":
pad(im,axes="y",140) %>% plot
pad(im,axes="y",140,pos=-1) %>% plot
#The argument to autocrop is the colour of the background it needs to remove
pad(im,axes="y",140,pos=-1) %>% autocrop(c(0,0,0)) %>% plot
######################################################################################################################

######################################################################################################################
# Warping 
# Warping maps the pixels of the input image to a different 
#location in the output. Scaling is a special case of warping, so is shifting. Warping relies on a map: M(x,y)=(x',y')
#Shifting M(x,y)=(x+deltax,y+deltay)
map.shift <- function(x,y) list(x=x+10,y=y+30)
imwarp(im,map=map.shift) %>% plot
map.scale <- function(x,y) list(x=1.5*x,y=1.5*y)
imwarp(im,map=map.scale) %>% plot(main="Forward mode")
map.scale.bw <- function(x,y) list(x=x/1.5,y=y/1.5)
imwarp(im,map=map.scale.bw,direction="backward") %>% plot(main="Backward mode")
map <- function(x,y) list(x=exp(y/600)*x,y=y*exp(-sin(x/40)))
imwarp(im,map=map,direction="forward") %>% plot()
######################################################################################################################

######################################################################################################################
# Lagged Operator
# To compute the difference between successive images in a video, you can use the shift operator:
#Compute difference between two successive frames (at lag 1)
plot((imshift(im,5,5)-im))
# For video
#Compute difference between two successive frames (at lag 1)
(imshift(tennis,delta_z=1)-tennis) %>% plot(frame=2,main="Difference betw. frames 2 and 1")
######################################################################################################################

######################################################################################################################
# imager has the usual correlate and convolve operations:
flt <- as.cimg(matrix(1,4,4)) #4x4 box filter #Create an image from a data.frame
# Computes a correlation matrix and runs hypothesis tests with corrections for multiple comparisons, like cor
grayscale(im) %>% correlate(flt) %>% plot(main="Filtering with box filter")
#Here the filter is symmetrical so convolution and correlation should be the same. Check:
a <- grayscale(im) %>% correlate(flt)
b <- grayscale(im) %>% imager::convolve(flt)
all.equal(a,b)
# CImg includes fast implementations of Gaussian (and derivative-of-Gaussian) 
#filters. They are available via the "deriche" and "vanvliet" functions.
im3 <- grayscale(im)
layout(t(1:2))
deriche(im3,sigma=4,order=2,axis="y") %>% plot(main="2nd deriv of Gaussian along y")
vanvliet(im3,sigma=4,order=2,axis="y") %>% plot(main="2nd deriv of Gaussian along y")
# The Vanvliet-Young filter is typically a better approximation. We can establish this by 
#looking at the impulse response (which should be Gaussian). Here's the one-dimensional case:
n <- 1e3
xv <- seq(0,1,l=n) #1D Grid
imp <- imdirac(c(n,1,1,1),n/2,1) #1D signal: Impulse at x = n/2
sig <- 80
#impulse response of the Deriche filter
imp.dr <- deriche(imp,sigma=sig) %>% as.vector
#impulse response of the Vanvliet-Young filter
imp.vv <- vanvliet(imp,sigma=sig) %>% as.vector 
imp.true <- dnorm(xv,sd=sig/n,m=.5) #True impulse response
plot(xv,imp.true/max(imp.true),type="l",lwd=2,xlab="x",ylab="Impulse response")
lines(xv,imp.vv/max(imp.vv),col="blue",lwd=2)
lines(xv,imp.dr/max(imp.dr),col="red",lwd=2)
# The ideal filter is in black, the Vanvliet-Young filter in blue, the Deriche filter in red. 
#Vanvliet-Young is clearly more accurate, but slightly slower:
im <- imfill(3e3,3e3)
system.time(deriche(im,3))
system.time(vanvliet(im,3))
######################################################################################################################

######################################################################################################################
# FFTs and the periodic/smooth decomposition
# FFTs can be computed via the FFT function:
im4 <- as.cimg(function(x,y) sin(x/5)+cos(x/4)*sin(y/2),128,128)
ff <- FFT(im4)
plot(ff$real,main="Real part of the transform")
plot(ff$imag,main="Imaginary part of the transform")
sqrt(ff$real^2+ff$imag^2) %>% plot(main="Power spectrum")

rff <- as.matrix(im4) %>% fft
Re(rff) %>% as.cimg %>% plot(main="Real part of the transform (R's native code)")
# Important: both FFT and fft will attempt to perform a multi-dimensional FFT of the input, 
#with dimensionality defined by the dimensionality of the array. If you want to compute a 2D FFT 
#for every frame of a video, use a split (imsplit or ilply).
# The FFT works best for periodic signals. One way of making signals periodic is via zero-padding 
#(use the pad function), another is to use the periodic-smooth decomposition of Moisan (2011):
layout(t(1:2))
periodic.part(im) %>% plot(main="Periodic part of im")
(im- periodic.part(im)) %>% plot(main="Smooth residual of im")
######################################################################################################################