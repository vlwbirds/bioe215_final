

#Load ARU data.
aru <- read.csv("../Data/EnergyDetector_GLMM.csv", stringsAsFactors = F)

#Load camera data.
cam <- read.csv("../Data/Camera_Positives.csv", stringsAsFactors = F)

#Load Station metadata.
stations <- read.csv("../Data/StationMetadata.csv", stringsAsFactors = F)

#Code to make Figure 4a.####

#Aggregate ARU data - total hours per weekday.
y <- aggregate(Hours ~ Weekday, data = aru, FUN = sum)

#Aggregate gunshots - total gunshots per weekday
z <- aggregate(Gunshots ~ Weekday, data = aru, FUN = sum)

#Merge.
z <- merge(z, y, by = 'Weekday')

rm(y)

#Calculate rate - gunshots per 10-hour (i.e. average) day.
z$rate <- z$Gunshots / z$Hours * 10 #Multiply by ten to approximate gunshots / day (10 hours/day)

#Calculate weekdays of camera data.
cam$Weekday <- weekdays(as.POSIXlt(cam$DateTime))

#Calculate camera hits by weekday.
w <- as.data.frame(table(cam$Weekday), stringsAsFactors = F)

#Rename columns.
colnames(w) <- c('Weekday', 'Hunters')

#Number of each weekday.
w$dayCount <- 13*21 #There were thirteen of each weekday, and the 21 cameras used here were operational
#for the full study period.

#Calculate rate - hunters per day.
w$rate <- w$Hunters / w$dayCount

plot(w$rate, z$rate) #Same scatterplot shown in Figure 4a.

#Code to make Figure 4b.####

aru2 <- merge(aru, stations[,c('Station', 'AccessPoint')], by = "Station")

cam2 <- merge(cam, stations[,c('Station', 'AccessPoint')], by = "Station")

#Remove ARU data not near access points with cameras.

aru2 <- aru2[aru2$AccessPoint %in% cam2$AccessPoint,]

#Aggregate ARU data - total hours per AccessPoint.
r <- aggregate(Hours ~ AccessPoint, data = aru2, FUN = sum)

#Aggregate gunshots - total gunshots per AccessPoint.
s <- aggregate(Gunshots ~ AccessPoint, data = aru2, FUN = sum)

#Merge.
s <- merge(s, r, by = 'AccessPoint')
rm(r)

#Calculate rate - gunshots per 10-hour (i.e. average) day.
s$rate <- s$Gunshots / s$Hours * 10 #Multiply by ten to approximate gunshots / day (10 hours/day)

#Calculate camera hits by Access Point.
q <- as.data.frame(table(cam2$AccessPoint), stringsAsFactors = F)

#Rename columns.
colnames(q) <- c('AccessPoint', 'Hunters')

#Number of stations at each access point.
g <- aggregate(Station ~ AccessPoint, data = cam2, FUN = function(x) (length(unique(x))))

#merge q and g.
q <- merge(q,g, by='AccessPoint')

rm(g)

#Calculate rate - hunters per access point per day. All stations were active for 91 days.
q$rate <- q$Hunters / q$Station / 91

plot(q$rate, s$rate) #Same scatterplot shown in Figure 4b.
