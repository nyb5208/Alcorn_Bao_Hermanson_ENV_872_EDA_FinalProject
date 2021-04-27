# Set your working directory
getwd()
# Load your packages
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(dplyr)
library(lubridate)
library(zoo)
library(trend)
library(sf)
library(Kendall)
library(knitr)
library(kableExtra)
#Load color palette packages
library(RColorBrewer)
library(wesanderson)
library(viridisLite)
library(viridis)
library(colormap)
library(mapview)
library(sf)
# Set your ggplot theme
project.theme <- theme_classic(base_size=12)+
  theme(plot.title= 
          element_text(size=14,
                       face="bold",
                       color= "black",
                       hjust=0.5),
        axis.text = 
          element_text(color = "black"),
        axis.title = 
          element_text(color= "black",face= "bold"),
        legend.position="top")
theme_set(project.theme)
#___________________

# Load EPA Air Lead Pollution datasets
epa.PAlead.2010 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_10_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2011 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_11_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2012 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_12_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2013 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_13_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2014 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_14_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2015 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_15_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2016 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_16_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2017 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_17_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2018 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_18_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2019 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_19_PA_lead.csv", stringsAsFactors = TRUE)
epa.PAlead.2020 <-read.csv( "./Data/Raw/EPA_Lead_PA/EPA_20_PA_lead.csv", stringsAsFactors = TRUE)
# Combined the EPA lead air data sets with rbind()
epa.PAair.lead <-rbind(epa.PAlead.2010, epa.PAlead.2011, epa.PAlead.2012, epa.PAlead.2013,
                       epa.PAlead.2014, epa.PAlead.2015, epa.PAlead.2016, epa.PAlead.2017,
                       epa.PAlead.2018, epa.PAlead.2019, epa.PAlead.2020)

#Change date variable from factor to date
epa.PAair.lead$Date <- as.Date(epa.PAair.lead$Date, format="%m/%d/%Y")
class(epa.PAair.lead$Date)
# Looked at dimensions, column names, and first few entries
dim(epa.PAair.lead)
colnames(epa.PAair.lead)
head(epa.PAair.lead)

#Cleaned and Processed EPA PA Air Lead Data adding month and year and combined month-year date variables
epa.PA.airPb.processed <- epa.PAair.lead %>%
  select(Date, Site.ID, 
         Daily.Mean.Pb.Concentration,
         Site.Name,AQS_PARAMETER_DESC,CBSA_NAME,CBSA_CODE,
         STATE,COUNTY, COUNTY_CODE) %>%
  mutate(Month = month(Date),
         Year=year(Date)) %>%
  mutate(Date_combined = my(paste0(Month,"-",Year)))
##### Looking at air lead level averages by metropolitan area 
#####
epa.PA.air.Pb.summary.msa <-epa.PAair.lead %>%
  select(Date, CBSA_NAME, CBSA_CODE, Daily.Mean.Pb.Concentration)%>%
  group_by(CBSA_NAME) %>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))
epa.PA.air.Pb.summary.msa
#Prior to linear interpolation of data 
#Looking at the top 5 Metropolitan Areas for air Pb concentrations from 2010 to 2020
#PA Metropolitan Area
PAmonthly_plot<-ggplot(Pa.monthly.Pb.MSA,aes(x = Date_combined,
                                             y = Mean.monthly.Pb, color= CBSA))+
  geom_line() +
  labs(y="Monthly Average Pb Air Concentration (ug/m^3)",
       x="Date",
       title="Monthly Average Pb Air Concentrations from 2010 to 2020 in PA Metro Areas")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y",
               limits=c(as.Date("2010-01-01"), as.Date("2021-01-01")),
               expand=c(0,0))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_hline(yintercept=0.15, linetype='dotted', col = 'black')+
  annotate("text", x = as.Date("2016-12-01", "%Y-%m-%d"), y = 0.15,
           label = "EPA Primary and Secondary 3-month Average Pb NAAQ Standard=0.15ug/m^3", vjust = -0.5)
print(PAmonthly_plot)
