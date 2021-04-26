#### Interpolation
# Philadelphia-Camden-Wilmington, PA-NJ-DE-MD 
Philadelphia.days <-as.data.frame(seq(ymd("2010-01-02"), ymd("2020-12-29"),by="days"))
Philadelphia.days <-setNames(Philadelphia.days,c("Date"))
Phila.msa.Pb.filter <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==37980) %>%
  select(Date,Daily.Mean.Pb.Concentration)
Phila.msa.Pb<-left_join(Philadelphia.days,Phila.msa.Pb.filter)
Phila.msa.Pb.cleaned <-Phila.msa.Pb %>%
  mutate(Daily.Mean.Pb.Concentration.clean=
           zoo::na.approx(Daily.Mean.Pb.Concentration)) %>%
  add_column(MSA=c("Philadelphia-Camden-Wilmington, PA-NJ-DE-MD"))                        
# Pittsburgh, PA 
Pitt.days <-as.data.frame(seq(ymd("2010-01-02"), ymd("2020-12-29"),by="days"))
Pitt.days <-setNames(Pitt.days, c("Date"))
Pitt.msa.Pb.filter<-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==38300) %>%
  select(Date,Daily.Mean.Pb.Concentration)
Pitt.msa.Pb<-left_join(Pitt.days,Pitt.msa.Pb.filter)
Pitt.msa.Pb.cleaned <- Pitt.msa.Pb %>%
  mutate(Daily.Mean.Pb.Concentration.clean=
           zoo::na.approx(Daily.Mean.Pb.Concentration)) %>%
  add_column(MSA=c("Pittsburgh, PA"))
# Allentown-Bethlehem-Easton, PA-NJ 
Allentown.days <-as.data.frame(seq(ymd("2012-07-02"), ymd("2020-12-29"),by="days"))
Allentown.days<-setNames(Allentown.days,c("Date"))
Allentown.msa.Pb.filter<-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==10900)%>%
  select(Date,Daily.Mean.Pb.Concentration)
Allentown.msa.Pb<-left_join(Allentown.days,Allentown.msa.Pb.filter)
#Cleaned Allentown data
Allentown.msa.Pb.cleaned <-Allentown.msa.Pb %>%
  mutate(Daily.Mean.Pb.Concentration.clean=
           zoo::na.approx(Daily.Mean.Pb.Concentration))%>%
  add_column(MSA=c("Allentown-Bethlehem-Easton, PA-NJ"))
Allentown.msa.Pb.cleaned
#
# Scranton--Wilkes-Barre--Hazleton, PA
Scranton.days <-as.data.frame(seq(ymd("2010-01-02"), ymd("2018-09-29"),by="days"))
Scranton.days <-setNames(Scranton.days,c("Date"))
Scranton.msa.Pb.filter<-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==42540)%>%
  select(Date,Daily.Mean.Pb.Concentration)
Scranton.msa.Pb<-left_join(Scranton.days,Scranton.msa.Pb.filter)
#Cleaned Scranton data
Scranton.msa.Pb.cleaned <-Scranton.msa.Pb %>%
  mutate(Daily.Mean.Pb.Concentration.clean=
           zoo::na.approx(Daily.Mean.Pb.Concentration)) %>%
  add_column(MSA=c("Scranton--Wilkes-Barre--Hazleton, PA"))
Scranton.msa.Pb.cleaned
#
#Lancaster, PA
Lancaster.days <-as.data.frame(seq(ymd("2012-01-04"), ymd("2020-12-29"),by="days"))
Lancaster.msa.Pb.filter<- epa.PA.airPb.processed %>%
  filter(CBSA_CODE=="29540") %>%
  select(Date,Daily.Mean.Pb.Concentration)

Lancaster.days<-setNames(Lancaster.days,c("Date"))
#
Lancaster.msa.Pb <-left_join(Lancaster.days,Lancaster.msa.Pb.filter)
Lancaster.msa.Pb.cleaned <-Lancaster.msa.Pb %>%
  mutate(Daily.Mean.Pb.Concentration.clean=
           zoo::na.approx(Daily.Mean.Pb.Concentration))%>%
  add_column(MSA=c("Lancaster, PA"))
######
#I am looking at the  metropolitan areas with Air Pb Levels that exceeded the EPA primary and secondary NAAQs
#Pittsburgh
Pitt.msa.linear.interp.plot <-ggplot(Pitt.msa.Pb.cleaned)+
  geom_line(aes(x = Date,
                y = Daily.Mean.Pb.Concentration.clean),
            color = "chartreuse4", alpha=0.5) +
  labs(y="Daily Mean Air Pb Level (ug/m^3)",title="Daily Mean Air Pb Levels in the Pittsburgh, PA Metropolitan Area from 2010 to 2020")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2010-01-01"), as.Date("2020-01-01")),
               expand=c(0,0))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2016-12-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
print(Pitt.msa.linear.interp.plot)
#
Pitt.monthly.Pb.msa <-Pitt.msa.Pb.cleaned %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))%>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb=mean(Daily.Mean.Pb.Concentration.clean))
Pitt.monthly.Pb.msa <- Pitt.monthly.Pb.msa %>%
  mutate(Date=as.Date(Date_combined))
#Checking the first and last dates of data collection
first(Pitt.msa.Pb.cleaned$Date)
last(Pitt.msa.Pb.cleaned$Date)
#
#Time Series: daily
Pitt.msa.Pb.daily.ts <-ts(Pitt.msa.Pb.cleaned$Daily.Mean.Pb.Concentration.clean,
                          start=c(2010,1),
                          end=c(2020,12),
                          frequency=365)
#Time Series: monthly
Pitt.msa.Pb.monthly.ts <-ts(Pitt.monthly.Pb.msa$Mean.monthly.Pb,
                            start=c(2010,1),
                            end=c(2020,12),
                            frequency=12)
#
Pitt.msa.Pb.daily.decomp <- stl(Pitt.msa.Pb.daily.ts, s.window = "periodic")
plot(Pitt.msa.Pb.daily.decomp)

Pitt.msa.Pb.monthly.decomp <-stl(Pitt.msa.Pb.monthly.ts, s.window = "periodic")
plot(Pitt.msa.Pb.monthly.decomp)
#
Pitt.Pb.daily.trend.nonseasonal <-Kendall::MannKendall(Pitt.msa.Pb.daily.ts)
summary(Pitt.Pb.daily.trend.nonseasonal)
#
Pitt.Pb.monthly.trend.nonseasonal <-Kendall::MannKendall(Pitt.msa.Pb.monthly.ts)
summary(Pitt.Pb.monthly.trend.nonseasonal)
#Scranton-Wilkes-Barre-Hazleton
#Scranton linear interpolation
Scranton.msa.linear.interpolation<-ggplot(Scranton.msa.Pb.cleaned) +
  geom_line(aes(x = Date,
                y = Daily.Mean.Pb.Concentration.clean),
            color = "blue", alpha=0.5) +
  labs(y="Daily Mean Pb Air Concentration (ug/m^3)",title="Daily Mean Air Pb Levels in the Scranton-Wilkes-Barre-Hazleton Metropolitan Area from 2010 to 2018")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2010-01-01"), as.Date("2018-10-01")),
               expand=c(0,0))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2014-10-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
print(Scranton.msa.linear.interpolation)
#
Scranton.monthly.Pb.msa <-Scranton.msa.Pb.cleaned %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))%>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb=mean(Daily.Mean.Pb.Concentration.clean))
Scranton.monthly.Pb.msa <- Scranton.monthly.Pb.msa %>%
  mutate(Date=as.Date(Date_combined))
#Checking the first and last dates of data collection
first(Scranton.msa.Pb.cleaned$Date)
last(Scranton.msa.Pb.cleaned$Date)
#
#Time Series: daily
Scranton.msa.Pb.daily.ts <-ts(Scranton.msa.Pb.cleaned$Daily.Mean.Pb.Concentration.clean,
                              start=c(2010,1),
                              end=c(2018,9),
                              frequency=365)
#Time Series: monthly
Scranton.msa.Pb.monthly.ts <-ts(Scranton.monthly.Pb.msa$Mean.monthly.Pb,
                                start=c(2010,1),
                                end=c(2018,9),
                                frequency=12)
#
Scranton.msa.Pb.daily.decomp <- stl(Scranton.msa.Pb.daily.ts, s.window = "periodic")
plot(Scranton.msa.Pb.daily.decomp)

Scranton.msa.Pb.monthly.decomp <-stl(Scranton.msa.Pb.monthly.ts, s.window = "periodic")
plot(Scranton.msa.Pb.monthly.decomp)
#
Scranton.Pb.daily.trend.nonseasonal <-Kendall::MannKendall(Scranton.msa.Pb.daily.ts)
summary(Scranton.Pb.daily.trend.nonseasonal)
#
Scranton.Pb.monthly.trend.nonseasonal <-Kendall::MannKendall(Scranton.msa.Pb.monthly.ts)
summary(Scranton.Pb.monthly.trend.nonseasonal)
#Lancaster, PA
#Lancaster linear interpolation
Lancaster.msa.linear.interpolation<-ggplot(Lancaster.msa.Pb.cleaned) +
  geom_line(aes(x = Date,
                y = Daily.Mean.Pb.Concentration.clean),
            color = "orange", alpha=1)
labs(y="Daily Mean Air Pb Level (ug/m^3)",title="Daily Mean Air Pb Levels in the Lancaster, PA Metropolitan Area from 2012 to 2020")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2012-01-01"), as.Date("2021-01-01")),
               expand=c(0,0))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2014-10-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
print(Lancaster.msa.linear.interpolation)
#
Lancaster.monthly.Pb.msa <-Lancaster.msa.Pb.cleaned %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))%>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb=mean(Daily.Mean.Pb.Concentration.clean))
Lancaster.monthly.Pb.msa <- Lancaster.monthly.Pb.msa %>%
  mutate(Date=as.Date(Date_combined))
#Checking the first and last dates of data collection
first(Lancaster.msa.Pb.cleaned$Date)
last(Lancaster.msa.Pb.cleaned$Date)
#
#Time Series: daily
Lancaster.msa.Pb.daily.ts <-ts(Lancaster.msa.Pb.cleaned$Daily.Mean.Pb.Concentration.clean,
                               start=c(2012,1),
                               end=c(2020,12),
                               frequency=365)
#Time Series: monthly
Lancaster.msa.Pb.monthly.ts <-ts(Lancaster.monthly.Pb.msa$Mean.monthly.Pb,
                                 start=c(2012,1),
                                 end=c(2020,12),
                                 frequency=12)
#
Lancaster.msa.Pb.daily.decomp <- stl(Lancaster.msa.Pb.daily.ts, s.window = "periodic")
plot(Lancaster.msa.Pb.daily.decomp)

Lancaster.msa.Pb.monthly.decomp <-stl(Lancaster.msa.Pb.monthly.ts, s.window = "periodic")
plot(Lancaster.msa.Pb.monthly.decomp)
#
Lancaster.Pb.daily.trend.nonseasonal <-Kendall::MannKendall(Lancaster.msa.Pb.daily.ts)
summary(Lancaster.Pb.daily.trend.nonseasonal)
#
Lancaster.Pb.monthly.trend.nonseasonal <-Kendall::MannKendall(Lancaster.msa.Pb.monthly.ts)
summary(Lancaster.Pb.monthly.trend.nonseasonal)
Lancaster.Pb.monthly.trend.seasonal<-
  Kendall::SeasonalMannKendall(Lancaster.msa.Pb.monthly.ts)
summary(Lancaster.Pb.monthly.trend.seasonal)
#Allentown
Allentown.msa.linear.interpolation<-ggplot(Allentown.msa.Pb.cleaned) +
  geom_line(aes(x = Date,
                y = Daily.Mean.Pb.Concentration.clean),
            color = "aquamarine", alpha=1) +
  labs(y="Daily Mean Air Pb Level (ug/m^3)",title="Daily Mean Air Pb Levels in the Allentown-Bethlehem-Easton, PA-NJ Metropolitan Area from 2012 to 2020")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2012-07-01"), as.Date("2021-01-01")),
               expand=c(0,0))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2014-10-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
print(Allentown.msa.linear.interpolation)
#Checking the first and last dates of data collection
first(Allentown.msa.Pb.cleaned$Date)
last(Allentown.msa.Pb.cleaned$Date)
#
Allentown.monthly.Pb.msa <-Allentown.msa.Pb.cleaned %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))%>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb=mean(Daily.Mean.Pb.Concentration.clean))
Allentown.monthly.Pb.msa <- Allentown.monthly.Pb.msa %>%
  mutate(Date=as.Date(Date_combined))
#Time Series: daily
Allentown.msa.Pb.daily.ts <-ts(Allentown.msa.Pb.cleaned$Daily.Mean.Pb.Concentration.clean,
                               start=c(2012,7),
                               end=c(2020,12),
                               frequency=365)
Allentown.Pb.daily.trend.nonseasonal <-Kendall::MannKendall(Allentown.msa.Pb.daily.ts)
summary(Allentown.Pb.daily.trend.nonseasonal)
#Time Series: monthly
Allentown.msa.Pb.monthly.ts <-ts(Allentown.monthly.Pb.msa$Mean.monthly.Pb,
                                 start=c(2012,7),
                                 end=c(2020,12),
                                 frequency=12)
Allentown.Pb.monthly.trend.nonseasonal <- Kendall::MannKendall(Allentown.msa.Pb.monthly.ts)
summary(Allentown.Pb.monthly.trend.nonseasonal)
#Decomposition of the time series
Allentown.msa.Pb.daily.decomp <- stl(Allentown.msa.Pb.daily.ts, s.window = "periodic")
plot(Allentown.msa.Pb.daily.decomp)
#
Allentown.msa.Pb.monthly.decomp <- stl(Allentown.msa.Pb.monthly.ts, s.window = "periodic")
plot(Allentown.msa.Pb.monthly.decomp)
#Decided to look at Philadelphia as well
#Checking the first and last dates of data collection
first(Phila.msa.Pb.cleaned$Date)
last(Phila.msa.Pb.cleaned$Date)
#
Phila.monthly.Pb.msa <-Phila.msa.Pb.cleaned %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))%>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb=mean(Daily.Mean.Pb.Concentration.clean))
Phila.monthly.Pb.msa <- Phila.monthly.Pb.msa %>%
  mutate(Date=as.Date(Date_combined))
#Time Series: daily
Phila.msa.Pb.daily.ts <-ts(Phila.msa.Pb.cleaned$Daily.Mean.Pb.Concentration.clean,
                           start=c(2010,1),
                           end=c(2020,12),
                           frequency=365)
Phila.Pb.daily.trend.nonseasonal <-Kendall::MannKendall(Phila.msa.Pb.daily.ts)
summary(Phila.Pb.daily.trend.nonseasonal)
#Time Series: monthly
Phila.msa.Pb.monthly.ts <-ts(Phila.monthly.Pb.msa$Mean.monthly.Pb,
                             start=c(2010,1),
                             end=c(2020,12),
                             frequency=12)
Phila.Pb.monthly.trend.nonseasonal <- Kendall::MannKendall(Phila.msa.Pb.monthly.ts)
summary(Phila.Pb.monthly.trend.nonseasonal)
#Decomposition of the time series
Phila.msa.Pb.daily.decomp <- stl(Phila.msa.Pb.daily.ts, s.window = "periodic")
plot(Phila.msa.Pb.daily.decomp)
#
Phila.msa.Pb.monthly.decomp <- stl(Phila.msa.Pb.monthly.ts, s.window = "periodic")
plot(Phila.msa.Pb.monthly.decomp)
```

```{r spatial-analysis}
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