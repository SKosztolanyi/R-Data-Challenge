instagram <- read.csv("instagram_data.csv", sep="\t", stringsAsFactors = F)
str(instagram)
summary(instagram)

osm_data <- read.csv("osm_data.csv", sep=";")
str(osm_data)
summary(osm_data)

library(ggplot2)
library(OpenStreetMap)
library(ggmap)

# Lookup Karlin lat long online:
# https://www.openstreetmap.org/export#map=15/50.0959/14.4585

# see what parameters to use
?get_openstreetmap

# generate map of karlin for further use
karlin_osm <- get_openstreetmap(bbox = c(left =14.4345, bottom = 50.0830, right = 14.4781, top= 50.1040), scale = 10000, format = c("png", "jpeg","svg", "pdf", "ps"), messaging = FALSE, urlonly = FALSE,filename = "ggmapKarlin", color = c("color", "bw"))
ggmap(karlin_osm)
# the most important factor is scale, because it determines the level of detail in the map
# This transforms all factor variables to null, so it is not a good approach
osm_data$LAT <- as.numeric(as.character(osm_data$LAT))

# creating a data frame for Lat-Lon frequencies
osm_data <- read.csv("osm_data.csv", sep=";")
LatLonCounts <- as.data.frame(table(osm_data$LAT, osm_data$LON))
LatLonCounts2 <- subset(LatLonCounts, LatLonCounts$Freq > 0)
str(LatLonCounts2)

LatLonCounts2$Lat <- as.numeric(as.character(LatLonCounts2$Var1))
# the problem is comma in latitude: I need to substitute it for a dot:

LatLonCounts2$Var1 <- gsub(",", ".", LatLonCounts2$Var1)
LatLonCounts2$Var2 <- gsub(",", ".", LatLonCounts2$Var2)
str(LatLonCounts2)

#Finally I can change the character to numeric without losing the values:
LatLonCounts2$Lat <- as.numeric(as.character(LatLonCounts2$Var1))
LatLonCounts2$Lon <- as.numeric(as.character(LatLonCounts2$Var2))
head(LatLonCounts2)

# let's plot some of the acitivities for a starter to see if we are on a good track:
ggmap(karlin_osm) + geom_point(data=LatLonCounts2[1:100,], aes(x=Lon, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")
str(osm_data)

ggmap(karlin_osm) + geom_point(data=LatLonCounts2, aes(x=Lon, y = Lat))

ggmap(karlin_osm) + geom_point(data=instagram, aes(x=longitude, y = latitude))

str(instagram)

# Let's look at the top most visited places
head(sort(table(instagram$location_name),  decreasing = T), n=20L)
# and compare them to latitude + longitude pairs
head(sort(table(instagram$longitude, instagram$latitude, instagram$location_name), decreasing = T), n=20L)

# We see that in the top positions, they are almost identical, 
#so there is a nice overlap of places and their lat/lon according to photos.
# I am now confident in plotting them on the map

Inst_LatLonCounts <- as.data.frame(table(instagram$longitude, instagram$latitude))
str(Inst_LatLonCounts)
Inst_LatLonCounts <- subset(Inst_LatLonCounts , Inst_LatLonCounts$Freq > 0)
str(Inst_LatLonCounts)

Inst_LatLonCounts$Lat <- as.numeric(as.character(Inst_LatLonCounts$Var2))
Inst_LatLonCounts$Lon <- as.numeric(as.character(Inst_LatLonCounts$Var1))

str(Inst_LatLonCounts)

ggmap(karlin_osm) + geom_point(data=Inst_LatLonCounts, aes(x=Lon, y = Lat, color=Freq, size = Freq)) + scale_color_gradient(low = "orange", high = "red")

# There is only one red dot, which is a clear outlier - Forum Karlin with 1629 photos, while the second one has only 631.
# Let's remove this outlier as well as places that have less than 30 visits:

Inst_LatLonCounts_filter <- subset(Inst_LatLonCounts, Inst_LatLonCounts$Freq < 800 & Inst_LatLonCounts$Freq > 30)

ggmap(karlin_osm) + geom_point(data=Inst_LatLonCounts_filter, aes(x=Lon, y = Lat, size = Freq, color=Freq)) + scale_color_gradient(low="black", high = "blue")

# This is for the 
?stat_density2d()
ggmap(karlin_osm) + stat_density2d(data=Inst_LatLonCounts_filter, bins = 4, geom = "polygon", aes(x=Lon, y = Lat, size = Freq, color=Freq, fill = "green")) + scale_fill_gradient(low="orange", high = "red")

?fill
ggmap(karlin_osm, extent = "device") + geom_density2d(data=Inst_LatLonCounts_filter, aes(x = Lon, y = Lat), size = 0.3)+
      stat_density2d(data=Inst_LatLonCounts_filter, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
                                                                   guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)

str(instagram)
inst_subset <- instagram[, c("longitude", "latitude", "location_name")]
head(sort(table(inst_subset$location_name),  decreasing = T), n=40L)

# some places like vitkov have different diacritics or lowercase/uppercase - I want to change it


inst_lower = as.data.frame(sapply(inst_subset, tolower))
str(inst_lower)
table(inst_lower$location_name)

library(stringi)
inst_lower$location_name <- stri_trans_general(inst_lower$location_name, "Latin-ASCII")
count_of_20_locations <- head(sort(table(inst_lower$location_name),  decreasing = T), n=20L)
# not perfect, but better - there are still some english and czech versions of the same
# stemming could also help. But what we want now is to aggregate the data by the location name

str(count_of_20_locations)
barplot(count_of_20_locations, main= "Instagram Activity in Karlin",col = rainbow(20), horiz = F, cex.names = 0.7, las=2)

# I wanted to plot the names of x axes in 45 degree axes, but it is not possible in this format
# As the datatype is array, it doesn't have names I could change the angle of.
count_of_20_locations

loka <- rainbow(20)


barplot(count_of_20_locations, main= "Instagram Activity in Karlin", col = rainbow(20), xlab = "Karlin Locations", cex.names = 0.1)
labels <- paste(rownames(count_of_20_locations), sep = " ")

# not very succesfull improvements
text(1:20, cex=0.7, labels = labels, adj = 1.1, srt=35, xpd=T)
legend("topright", legend =  rownames(count_of_20_locations), fill = loka, ncol = 2, cex = 0.40)

labels <- paste(rownames(count_of_20_locations), sep = " ")
