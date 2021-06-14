# imager supports JPEG, PNG and BMP natively - for other formats you'll need to install ImageMagick.
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(imager)
library(raster)

im <- load.image("Dualsatellites.jpg")
plot(im)
class(im)
im
grayscale(im)
# An object of class cimg is actually just a thin interface over a regular 4D array:
dim(im)
# We'll see below how images are stored exactly. For most intents and purposes, 
#they behave like regular arrays, meaning the usual arithmetic operations work:
log(im)+3*sqrt(im)
mean(im)
sd(im)

#####################################################################################################################
# Example 1: Histogram equalisation
dev.new()
grayscale(im) %>% hist(main="Luminance values in im picture")
# Since images are stored essentially as arrays, here we're just using R's regular hist function, 
# which treats our array as a vector of values. If we wanted to look only at the red channel, we could use:
dev.new()
R(im) %>% hist(main="Red channel values in im picture")
#Equivalently:
#channel(boats,1) %>% hist(main="Red channel values in boats picture")
# Another approach is to turn the image into a data.frame, and use ggplot to view all channels at once:
bdf <- as.data.frame(im)
head(bdf,3)
bdf <- plyr::mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
dev.new()
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)
# What we immediately see from these histograms is that the middle values are in a sense over-used:
# there's very few pixels with high or low values. Histogram equalisation solves the problem by making 
# histograms flat: each pixel's value is replaced by its rank, which is equivalent to running the data 
# through their empirical cdf.

# As an illustration of what this does, see the following example:
x <- rnorm(100)
layout(t(1:2))
hist(x,main="Histogram of x")
# Compute an empirical cumulative distribution function, with several methods for plotting, 
# printing and computing with such an "ecdf" object.
f <- ecdf(x)
hist(f(x),main="Histogram of ecdf(x)")
# We can apply it directly to images as follows:
im.g <- grayscale(im)
f <- ecdf(im.g)
plot(f,main="Empirical CDF of luminance values")
# Again we're using a standard R function (ecdf), which returns another function corresponding 
# to the ECDF of luminance values in im.g.
# If we run the pixel data back through f we get a flat histogram:
f(im.g) %>% hist(main="Transformed luminance values")
# Now the only problem is that ecdf is base R, and unaware of our cimg objects. 
# The function f took an image and returned a vector:
f(im.g) %>% str
# If we wish to get an image back we can just use as.cimg:
f(im.g) %>% as.cimg(dim=dim(im.g)) %>% plot(main="With histogram equalisation") # doesn't give same as grayscale

# So far we've run this on a grayscale image. If we want to do this on RGB data, we need to 
# run the equalisation separately in each channel. imager enables this using its split-apply-combine tricks:
#Hist. equalisation for grayscale
hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
#Split across colour channels, 
cn <- imsplit(im,"c")
cn #we now have a list of images
cn.eq <- llply(cn,hist.eq) #run hist.eq on each
imappend(cn.eq,"c") %>% plot(main="All channels equalised") #recombine and plot
# There's even a one-liner to do this:
iiply(im,"c",hist.eq) # Split an image, apply function, recombine the results as an image
# We can use it to check that all channels have been properly normalised:
#Generate aesthetic mappings that describe how variables in the data are mapped to visual properties 
# (aesthetics) of geoms. This function also standardise aesthetic names by performs partial name matching, 
#converting color to colour, and old style R names to ggplot names (eg. pch to shape, cex to size)
iiply(im,"c",hist.eq) %>% as.data.frame %>% ggplot(aes(value))+geom_histogram(bins=30)+facet_wrap(~ cc) # not equal actually 
#####################################################################################################################

#####################################################################################################################
# Example 2: Edge detection
# Edge detection relies on image gradients, which imager returns via:
layout(t(1:2))
imgradient(im.g,"x") %>% plot(main="Gradient along x")
imgradient(im.g,"y") %>% plot(main="Gradient along y")
# To be more specific, noting I(x,y) the image intensity at location x,y what imager 
# returns is an approximation of: (partial/partialx)I in first panel and (partial/partialy)I in second panel
# The magnitude of the gradients thus tell us how fast the image changes around a certain point. Image edges 
#correspond to abrubt changes in the image, and so it's reasonable to estimate their location based on the norm 
#of the gradient sqrt(((partial/partialx)I)^2+((partial/partialy)I)^2)
# In imager:
dx <- imgradient(im.g,"x")
dy <- imgradient(im.g,"y")
grad.mag <- sqrt(dx^2+dy^2)
dev.new()
plot(grad.mag,main="Gradient magnitude")
# Here's a handy shortcut:
dev.new()
imgradient(im.g,"xy") %>% enorm %>% plot(main="Gradient magnitude (again)")
# The first function returns a list of images:
l <- imgradient(im.g,"xy")
str(l) # internal structure of an R object, a diagnostic function and an alternative to summary
# And the second takes a list of images and computes the Euclidean norm pixel-wise, i.e.:
enorm(list(3,2))
sqrt(3^2+2^2)
#####################################################################################################################

#####################################################################################################################
# imager and ggplot2
# To plot your image data using ggplot2, use as.data.frame and geom_raster:
df <- grayscale(im) %>% as.data.frame
p <- ggplot(df,aes(x,y))+geom_raster(aes(fill=value))
p
# We're not quite there, mainly because or y axis is reversed. Here's a fix:
p + scale_y_continuous(trans=scales::reverse_trans())
# The grey margin around the plot should be eliminated as well:
p <- p+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())
p
# Finally, ggplot has a blue colour scale by default, but we might want to keep our original grays:
p+scale_fill_gradient(low="black",high="white")

#Colour images are a bit trickier. We could plot each channel separately:
df <- as.data.frame(im) 
p <- ggplot(df,aes(x,y))+geom_raster(aes(fill=value))+facet_wrap(~ cc)
p+scale_y_reverse()
# Plotting channels separately may be useful on occasion, but usually we'd want the original colours. 
# We can tell as.data.frame to return a "wide" format:
as.data.frame(im,wide="c") %>% head
# The three colour channels are now stacked along columns, which lets us do the following:
df <- as.data.frame(im,wide="c") %>% mutate(rgb.val=rgb(c.1/255,c.2/255,c.3/255))
head(df,3)
# We can now plot our image using ggplot's identity scale:
p <- ggplot(df,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
p+scale_y_reverse() ##???
#####################################################################################################################

#####################################################################################################################
# Blob detection/extraction of local maxima, denoising, scale-space

#Our goal will be to find the coordinates of the galaxies in this picture 
hub <- load.image("hubble.jpg") %>% grayscale
plot(hub,main="Hubble Deep Field")
#Before we can work with the real image we'll try synthetic data. Here's how to generate an image with 
# a few randomly placed blobs:
layout(t(1:2))
set.seed(2)
points <- rbinom(100*100,1,.001) %>% as.cimg #Create an image from a data.frame, rbinom(n, size, prob)
# The data frame must be of the form (x,y,value) or (x,y,z,value), or (x,y,z,cc,value). 
# The coordinates must be valid image coordinates (i.e., positive integers).
blobs <- isoblur(points,5)
plot(points,main="Random points")
plot(blobs,main="Blobs")
# blobs are obtained from random points convolved with a blur kernel of size 5 pixels. Note the shortcut in:
rbinom(100*100,1,.001) %>% as.cimg
# where a vector of length 100^2 is turned into an image of dimension 100x100. That's just a guess on 
# imager's part and it's reported with a warning (we could be dealing with an image of dimension 10x1000, 
# for instance). To get rid of the warning you have to be explicit about the dimensions you want:
rbinom(100*100,1,.001) %>% as.cimg(x=100,y=100)
# Suppose our task is to find the location of the center of the blobs. There are several way of doing that, 
# but one that's convenient is to go through image hessians. Blobs are local maxima in the image, and local 
# maxima are usually associated with a hessian matrix that's positive definite (the well-known second-order 
# optimality condition). A matrix that's positive definite has positive determinant, which we can compute via:
# det(H)=I_{xx}x I_{yy}-I_{xy}^2 where I_{xx} is second derivative of image along x, etc. 
# See wikipedia on blob detection for more.
# In imager we can use:
imhessian(blobs)
# to get the derivatives we need, and:
Hdet <- with(imhessian(blobs),(xx*yy - xy^2))
plot(Hdet,main="Determinant of Hessian")
# To get only the pixels with the highest values, we threshold the image:
threshold(Hdet,"99%") %>% plot(main="Determinant: 1% highest values") #Threshold grayscale image, look details
# The thresholded image now contains discrete image regions, and if we can compute the center of these 
# regions we'll have our locations. The first step is to label these regions:
lab <- threshold(Hdet,"99%") %>% label
plot(lab,main="Labelled regions")
# label is a utility that fills each white region with a unique pixel value (the background stays at 0). 
# We can extract the labelled regions in the form of a data.frame:
df <- as.data.frame(lab) %>% subset(value>0)
head(df,3)
unique(df$value) #10 regions
# And now all we need to do is to split the data.frame into regions, and compute the mean coordinate values 
# in each. We'll show two solutions, one using plyr, the other using the more recent dplyr variant:
centers <- ddply(df,.(value),summarise,mx=mean(x),my=mean(y))
centers <- dplyr::group_by(df,value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
# As an exercise you can try extracting other summary values for the regions (area, for example, or aspect ratio).

# We now overlay the results on the original image:
plot(blobs)
with(centers,points(mx,my,col="red"))
# That's pretty good, but to make things a bit harder we'll add noise to the image:
nblobs <- blobs+.001*imnoise(dim=dim(blobs))
plot(nblobs,main="Noisy blobs")
# If we try the same thing again it fails completely:
get.centers <- function(im,thr="99%")
{
  dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
}

plot(nblobs)
get.centers(nblobs,"99%") %$% points(mx,my,col="red")
# We need an extra denoising step. Simple blurring will do here:
nblobs.denoised <- isoblur(nblobs,2)
plot(nblobs.denoised)
get.centers(nblobs.denoised,"99%") %$% points(mx,my,col="red")

# We're ready to move on to the Hubble image. Here's a first naive attempt:
plot(hub)
get.centers(hub,"99.8%") %$% points(mx,my,col="red")
# Our detector is mostly picking up small objects. Adding blur results in:
plot(hub)
isoblur(hub,5) %>% get.centers("99.8%") %$% points(mx,my,col="red")
# and the detector is now picking up large objects only. What if we want to detect objects 
# at various scale? The solution is to aggregate the results over scale, which is what multiscale approaches do.
#Compute determinant at scale "scale". 
hessdet <- function(im,scale=1) isoblur(im,scale) %>% imhessian %$% { scale^2*(xx*yy - xy^2) }
#Note the scaling (scale^2) factor in the determinant
plot(hessdet(hub,1),main="Determinant of the Hessian at scale 1")
# To view the results at different scales, we can use ggplot:
#Get a data.frame with results at scale 2, 3 and 4
dat <- ldply(c(2,3,4),function(scale) hessdet(hub,scale) %>% as.data.frame %>% mutate(scale=scale))
p <- ggplot(dat,aes(x,y))+geom_raster(aes(fill=value))+facet_wrap(~ scale)
p+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())
# Scale-space theory suggests that we look for blobs across scales. It's easy:
scales <- seq(2,20,l=10)

d.max <- llply(scales,function(scale) hessdet(hub,scale)) %>% parmax
plot(d.max,main="Point-wise maximum across scales")
#parmax is another example of a reduction function, one that here takes the maximum value for each pixel 
#across all scales. To find out which scale had the maximum value point-wise, we can use which.parmax:
i.max <- llply(scales,function(scale) hessdet(hub,scale)) %>% which.parmax
plot(i.max,main="Index of the point-wise maximum across scales")
# So far this isn't too informative. It will be once we have labelled regions:
#Get a data.frame of labelled regions
labs <- d.max %>% threshold("96%") %>% label %>% as.data.frame
#Add scale indices
labs <- mutate(labs,index=as.data.frame(i.max)$'value')
regs <- dplyr::group_by(labs,value) %>% dplyr::summarise(mx=mean(x),my=mean(y),scale.index=mean(index))
p <- ggplot(as.data.frame(hub),aes(x,y))+geom_raster(aes(fill=value))+geom_point(data=regs,aes(mx,my,size=scale.index),pch=2,col="red")
p+scale_fill_gradient(low="black",high="white")+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())
#####################################################################################################################