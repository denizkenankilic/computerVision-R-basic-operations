install.packages("png")
library(png)
install.packages("EBImage")
library(EBImage)
install.packages("RCurl")
library(RCurl)
install.packages("PET")
library(PET)
install.packages("som")
library(som)

#im <- readPNG( getBinaryURL( "http://upload.wikimedia.org/wikipedia/en/thumb/e/e5/Shepp_logan.png/170px-Shepp_logan.png") )[,,1]
#im <- load.image("Shepp_logan.png")
im <- readPNG("Shepp_logan.png")[,,1]
plot(im)
rad = radon(t(im))$rData
# Normalize intensity values from 0-1
rad = normalize(rad)   

display(t(im))  
display(t(rad))
