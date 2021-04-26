#2010

ggplot() + 
  geom_sf(data=PA_county_2010,aes(fill = meanPOV),alpha=0.3) + 
  scale_fill_gradient2(low="blue",high="red", midpoint = .15) +
  labs(fill = "% Pop in Poverty")+
  geom_sf(data=sf_PA_Lead_avg2010, aes(size=meanPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2010 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2010,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient2(low="red",high="blue", midpoint = 28000, breaks=c(16000,28000, 40000)) + 
  labs(fill = "Per Capita 
    Income")+
  geom_sf(data=sf_PA_Lead_avg2010, aes(size=meanPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2010 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2010,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient(low="blue", high="red", breaks=c(16000,28000, 40000)) +
  labs(fill="Per Capita Income")+
  geom_sf(data=sf_PA_Lead_high_2010, aes(size=maxPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2010 Max Lead Levels over .15 (µg/m3)")

#2020

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

ggplot() + 
  geom_sf(data=PA_county_2018,aes(fill = meanPOV),alpha=0.3) + 
  scale_fill_gradient2(low="blue",high="red", midpoint = .15) +
  labs(fill = "% Pop in Poverty")+
  geom_sf(data=sf_PA_Lead_avg2020, aes(size=meanPb))+
  labs(size="Lead Values 
       (µg/m3)")+
  ggtitle("2020 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2018,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient2(low="red",high="blue", midpoint = 35000, breaks=c(21000,35000, 47000)) +
  labs(fill = "Per Capita
  Income")+
  geom_sf(data=sf_PA_Lead_avg2020, aes(size=meanPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2020 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2018,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient2(low="red",high="blue", midpoint = 35000, breaks=c(20000,35000, 45000)) +
  labs(fill="Per Capita Income")+
  geom_sf(data=sf_PA_Lead_high_2020, aes(size=maxPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2020 Max Lead Levels over .15 (µg/m3)")
