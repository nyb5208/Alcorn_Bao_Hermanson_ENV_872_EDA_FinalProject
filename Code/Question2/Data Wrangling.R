#Reduce our data to just one record for each location, computing mean and max daily AQI values 2010 and 2020

EPAair_PA_Lead_avg2010 <- epa.PAlead.2010 %>%
  group_by(COUNTY, SITE_LATITUDE, SITE_LONGITUDE) %>%
  summarize(meanPb = mean(Daily.Mean.Pb.Concentration),
            maxPb = max(Daily.Mean.Pb.Concentration))


EPAair_PA_Lead_avg2020 <- epa.PAlead.2020 %>%
  group_by(COUNTY, SITE_LATITUDE, SITE_LONGITUDE) %>%
  summarize(meanPb = mean(Daily.Mean.Pb.Concentration),
            maxPb = max(Daily.Mean.Pb.Concentration))

#Convert the dataset to a spatially enabled "sf" data frame
sf_PA_Lead_avg2010 <- st_as_sf(EPAair_PA_Lead_avg2010,
                               coords = c('SITE_LONGITUDE','SITE_LATITUDE'),
                               crs=4326)

sf_PA_Lead_high_2010<-filter(sf_PA_Lead_avg2010, maxPb > 0.15)
range(sf_PA_Lead_high_2010$meanPb)



sf_PA_Lead_avg2020 <- st_as_sf(EPAair_PA_Lead_avg2020,
                               coords = c('SITE_LONGITUDE','SITE_LATITUDE'),
                               crs=4326)

sf_PA_Lead_high_2020<-filter(sf_PA_Lead_avg2020, maxPb > 0.15)

#read in county shape file
counties_sf<- st_read('./Data/Spatial/cb_2018_us_county_20m.shp')%>%
  filter(STATEFP == 42) #Filter for just PA Counties
colnames(counties_sf)[2]<-"CNTY_FIPS"
colnames(counties_sf)[6]<-"COUNTY"

counties_sf$CNTY_FIPS<-as.numeric(counties_sf$CNTY_FIPS)


#read in CDC Data for each year and group by county
#2010
CDC_PA_2010<-read.csv("./Data/Raw/Pennsylvania_CDC_2010.csv", header = TRUE)

CDC_PA_2010$CNTY_FIPS<-as.numeric(CDC_PA_2010$CNTY_FIPS)

CDC_PA_2010_avg<-CDC_PA_2010%>%
  group_by(CNTY_FIPS) %>%
  summarize(meanPCI = mean(E_PCI),
            meanPOV = mean(E_P_POV),
            meanPOP = sum(TOTPOP))

range(CDC_PA_2010_avg$meanPCI)

#join CDC data and county shape file
PA_county_2010<-left_join(counties_sf, CDC_PA_2010_avg, by = c("CNTY_FIPS"))

#2018
CDC_PA_2018<-read.csv("./Data/Raw/Pennsylvania_CDC_2018.csv", header = TRUE)

CDC_PA_2018$CNTY_FIPS<-as.numeric(CDC_PA_2018$CNTY_FIPS)

CDC_PA_2018_avg<-CDC_PA_2018%>%
  group_by(COUNTY) %>%
  summarize(meanPCI = mean(E_PCI),
            meanPOV = mean(EP_POV),
            meanPOP = sum(E_TOTPOP))

CDC_PA_2018_avg<-mutate(CDC_PA_2018_avg, meanPOV = (meanPOV/100))
CDC_PA_2018_avg$CNTY_FIPS<-CDC_PA_2010_avg$CNTY_FIPS
#join CDC data and county shape file
PA_county_2018<-left_join(counties_sf, CDC_PA_2018_avg, by = c("CNTY_FIPS"))