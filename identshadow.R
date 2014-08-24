#Code for reading in an image, comparing to a mask image to identify sample areas, and dividing the sample areas to light and shadow

#install.packages("jpeg")
#install.packages("reshape2")
#you may well have the jpeg library or reshape2 already

library(jpeg)
library(reshape2)
library(grDevices)

maskimage <- readJPEG("mask.jpg")
dataimage <- readJPEG("IMG_0538.jpg") #may 12th


#mask image
colnames(maskimage) <- 1:800
rownames(maskimage) <- 1:600
redmask <- maskimage[,,1]
greenmask <- maskimage[,,2]
bluemask <-  maskimage[,,3]
ri = melt(redmask)
names(ri) <- c("r","c","red")
gi <- melt(greenmask)
names(gi) <- c("r","c","green")
bi <- melt(bluemask)
names(bi) <- c("r","c","blue")
mask = ri
mask$green = gi$green
mask$blue = bi$blue
mask$group = rep(NA)
mask$group[mask$red > 0.1] = "Control"
mask$group[mask$green > 0.1] = "Low"
mask$group[mask$blue > 0.1] = "High"

#data image
colnames(dataimage) <- 1:800
rownames(dataimage) <- 1:600
rd <- dataimage[,,1]
gn <- dataimage[,,2]
bl <- dataimage[,,3]
rdp = melt(rd)
names(rdp) <- c("r","c", "red")
gnp <- melt(gn)
names(gnp) <- c("r","c","green")
blp <- melt(bl)
names(blp) <- c("r","c","blue")
pic = rdp
pic$green = gnp$green
pic$blue = blp$blue
pic$group = mask$group
pichsv= rgb2hsv(pic$red, pic$green, pic$blue, maxColorValue = 1)
pic$h = pichsv[1,]
pic$s = pichsv[2,]
pic$v = pichsv[3,]

instudy = pic[pic$group %in% c("Control","High","Low"),]

km = kmeans (instudy$v, 2)

#the graphs take a while and make a png file in the working directory
png("myplot.png", width=300, height=900, units="px", res=72)
par(mfrow=c(3,1))
par(mar=c(2,2,2,2))
plot(pic$c, pic$r, col = rgb(pic$red,pic$green,pic$blue), main="before", ylim=c(0,600), xlim=c(0,800))
#WARNING top and bottom reversed compared to orginal photo,
plot(instudy$c, instudy$r, col = rgb(instudy$red,instudy$green,instudy$blue), main="study area only", ylim=c(0,600), xlim=c(0,800))
plot(instudy$c, instudy$r, col = km$cluster, main="detecting shadow", ylim=c(0,600), xlim=c(0,800))
par(mfrow=c(1,1))
dev.off()