# I put these two in a file called R code sources
source("G:/Rachel Internship work/R code sources (needed for all maps)/exifproc.r")
source("G:/Rachel Internship work/R code sources (needed for all maps)/mapping.r")

#EXTRACTING TAGS AND COUNT DATA

setwd("G:/Rachel Internship work")
infolder <- "F:/Barnes/TaggedImages"
outfile <-"C:/Users/Rachel/Desktop/ZSL PTES/Barnes/BarnesExifData.csv"
extract.exif(infolder, outfile)

imgdat <- read.csv(outfile)
imgdat <- subset(imgdat, !grepl("._IMG", SourceFile))
str(imgdat)
nrow(imgdat)


cols <- c("SourceFile", "EventNumber", "Sequence", "MoonPhase", "AmbientTemperature")
dat <- extract.images(imgdat, "Keywords", "CreateDate", cols)
summary(dat)
dat$AmbientTemperature <- as.numeric(sub(" C", "", dat$AmbientTemperature))
subset(dat, is.na(placeID))

Barnescntdat <- extract.contacts(dat)
summary(Barnescntdat)
View(Bushycntdat)
write.csv(Barnescntdat, "G:/Rachel Internship work/Barnes/cntdat1.csv")
Barnescntdat<-read.csv("C:/Users/Rachel/Desktop/ZSL PTES/Barnes/Barnescntdata.csv")

table(Barnescntdat$species)
counts <- table(Barnescntdat[,c("placeID", "species")])


#MAPPING CAMERAS AND TRAP RATES

plcdat <- read.csv("C:/Users/Rachel/Desktop/ZSL PTES/Barnes/Barnes_cam_locations.csv")
summary(plcdat)

basemap <- loadmap("C:/Users/Rachel/Desktop/ZSL PTES/Barnes/Barnesmap.JPG", "C:/Users/Rachel/Desktop/ZSL PTES/Barnes/Barnescorners.kml")
plotmap(basemap)
coords <- data.frame(long=plcdat$Long, lat=plcdat$Lat)
addshape(basemap, coords, type="point", pch=16, col="white")

# previous way of plotting traprates
traprates <- as.data.frame(counts / plcdat$Days.active)
tr <- subset(traprates, species=="Badger")$Freq
cl <- ifelse(tr==0, "white", "Blue")
plotmap(basemap)
addshape(basemap, coords, type="point", pch=16, col=cl, cex=tr+0.5)

#marcus's new way (less errors)
i <- match(plcdat$Camera.No., rownames(counts))
cnts <- counts[i,]
cnts[is.na(cnts)] <- 0
traprates <- as.data.frame(cnts / plcdat$Days.active)
tr <- subset(traprates, species=="Badger")$Freq
cl <- ifelse(tr==0, "white", "Blue")
plotmap(basemap)
addshape(basemap, coords, type="point", pch=16, col=cl, cex=(tr*10)+1.0)

BadgerCoords<-read.csv("C:/Users/Rachel/Desktop/ZSL PTES/Barnes/BadgerCoords.csv")
addshape(basemap,BadgerCoords,type="point", pch=22, col="Red", cex=1.0)



# QUALITY OF CAMERA PLACEMENT MAP
plcdat <- read.csv("F:/BushyPark/Barnes_camera_locations.csv")
basemap <- loadmap("BushyPark_cropped.jpg", "Corners1.kml")
plotmap(basemap)
coords <- data.frame(long=plcdat$Long, lat=plcdat$Lat)
camstat<- data.frame(status=plcdat$Cam_Status)
camcol<- ifelse(camstat=="good", "green", "white")
addshape(basemap, coords, type="point", pch=16, col=camcol)

