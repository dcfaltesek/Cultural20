library(ggmap)

#a sequence on portland
port<-geocode("Portland, Oregon")
portland<-get_map(port)
PDX<-ggmap(portland)
ggmap(portland)

#a nice little mappy
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data=donut)

#let's zoom this up a little
#call the map from google
portland<-get_map(port, zoom = 11)

#now we translate the basic plotting object into an intermediate var
PDX<-ggmap(portland)
PDX +
  geom_point(aes(x = lon, y = lat, colour = hip, size = flavor,stroke =5), data = donut)

#now a quick jitter
PDX +
  geom_jitter(aes(x = lon, y = lat, colour = hip, size = flavor), data = donut)

#with geom_text
PDX + 
  geom_text(aes(x = lon, y = lat, colour = hip, size = flavor, label = name), data = donut)

#and crime in Corvallis via Zac
#import corvallis noise and let's start cleaning
library(lubridate)
#parsed times
bonkers<-mdy_hms(corvallis_noise$DATE)
hour<-hour(bonkers)
month_day<-day(bonkers)
month<-month(bonkers)
year<-year(bonkers)
bh<-data.frame(hour, month_day, month, year)
noise<-bind_cols(corvallis_noise, bh)
View(noise)

#NOW we must be frugal
noiseA<-filter(noise, year > 2017)%>%
  filter(month > 8)

#make a dataframe with the street_addresses
df <- data.frame(street_address = noise$ADDRESS, stringsAsFactors = FALSE)

#mutate_geocode to put that list into a new dataframe
locations<-df %>% mutate_geocode(street_address)
write.csv(locations, "locations.csv", row.names = FALSE)

robby<-select(locations, street_address, lon, lat)

#inner_join that to the original
colnames(noise)[4]<-"street_address"
noise3 <- right_join(noise, robby)

#take a quick look
View(noise2)

#get a road map of town
corvallis<-get_googlemap("corvallis, oregon", zoom = 14, maptype = "road")

corvallis2<-ggmap(corvallis)

#and the fun begins...
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = NATURE.CODE), data = noise2)

#and the police really helped us here with formatting
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = NATURE.CODE), data = noise2)+facet_wrap(. ~month)

#from the Houston crimes example
corvallis2 +
  stat_bin2d(
    aes(x = lon, y = lat, colour = NATURE.CODE, fill = mean(hour, na.rm=TRUE)),
    size = .5, bins = 30, alpha = 1/2,
    data = noise2
  )
noise <- noise %>% mutate(street_address = paste0(street_address, ", Corvallis OR"))
