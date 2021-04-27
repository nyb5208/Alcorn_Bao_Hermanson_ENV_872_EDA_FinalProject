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
  scale_fill_gradient2(low="red",high="blue", midpoint = 28000, breaks=c(16000,28000, 40000)) +
  labs(fill="Per Capita 
    Income")+
  geom_sf(data=sf_PA_Lead_high_2010, aes(size=maxPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2010 Max Lead Levels over .15 (µg/m3)")


ggplot() + 
  geom_sf(data=PA_county_2014,aes(fill = meanPOV),alpha=0.3) + 
  scale_fill_gradient2(low="blue",high="red", midpoint = .15) +
  labs(fill = "% Pop in Poverty")+
  geom_sf(data=sf_PA_Lead_avg2015, aes(size=meanPb))+
  labs(size="Lead Values 
       (µg/m3)")+
  ggtitle("2015 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2014,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient2(low="red",high="blue", midpoint = 30000, breaks=c(18000,30000, 40000)) +
  labs(fill = "Per Capita
  Income")+
  geom_sf(data=sf_PA_Lead_avg2015, aes(size=meanPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2015 Mean Lead Values Across Pennsylvania")

ggplot() + 
  geom_sf(data=PA_county_2014,aes(fill = meanPCI),alpha=0.3) + 
  scale_fill_gradient2(low="red",high="blue", midpoint = 30000, breaks=c(18000,30000, 40000)) +
  labs(fill="Per Capita
  Income")+
  geom_sf(data=sf_PA_Lead_high_2015, aes(size=maxPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2015 Max Lead Levels over .15 (µg/m3)")


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
  scale_fill_gradient2(low="red",high="blue", midpoint = 35000, breaks=c(21000,35000, 47000)) +
  labs(fill="Per Capita
  Income")+
  geom_sf(data=sf_PA_Lead_high_2020, aes(size=maxPb))+
  labs(size="Lead Values
       (µg/m3)")+
  ggtitle("2020 Max Lead Levels over .15 (µg/m3)")