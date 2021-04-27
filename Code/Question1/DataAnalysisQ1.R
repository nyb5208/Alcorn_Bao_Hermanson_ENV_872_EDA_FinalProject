#first I joined all the linear interpolated data together
PA.msa.Pb.cleaned <-rbind(Phila.msa.Pb.cleaned,Pitt.msa.Pb.cleaned,Allentown.msa.Pb.cleaned,Scranton.msa.Pb.cleaned, Lancaster.msa.Pb.cleaned)
class(PA.msa.Pb.cleaned$Date)
#
PA.msa.Pb.cleaned.plot<-ggplot(PA.msa.Pb.cleaned, aes(x = Date,
                                                      y = Daily.Mean.Pb.Concentration.clean, color= MSA))+
  geom_line() +
  labs(y="Daily Average Air Pb Level (ug/m^3)",
       x="Date",
       title="Daily Average Air Pb Levels from 2010 to 2020 in Metropolitan Areas in PA",
       color="")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2010-01-01"), as.Date("2021-01-01")),
               expand=c(0,0))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2016-12-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)
print(PA.msa.Pb.cleaned.plot)

```

```{r time-series-analysis}
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