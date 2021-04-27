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

########## Data Wrangling ######################
#
#
#Looking at the lead air concentrations for major metropolitan areas in Pennsylvania 
# Looking at the Combined Statistical Areas (CBSA) to identify the metropolitan areas included in this study
summary(epa.PA.airPb.processed$CBSA_NAME)
summary(as.factor(epa.PA.airPb.processed$CBSA_CODE))
#associated CBSA codes 
### Allentown-Bethlehem-Easton, PA-NJ =10900
### Chambersburg-Waynesboro =16540
### Erie=21500
### Indiana=26860
### Lancaster=29540
### Lewisburg=30260
### New Castle=35260
### Reading=39740
### Pittsburgh =38300
### Philadelphia-Camden-Wilmington, PA-NJ-DE-MD =39780
### Reading=39740
summary(epa.PA.airPb.processed$COUNTY)

#Need to determine the top 5 metropolitan areas by 2010 US Census population size (most populated):
## Loaded US census data to determine the most populated metropolitan areas from the CBSAs provided in the epa Air Lead data
PA.msa.population.size <-read.csv("./Data/Raw/PA_Metro_Area_data/cbsa-est2019-alldata.csv")
#Set CBSA variable as factor
as.factor(PA.msa.population.size$CBSA)
### Process the PA.msa.population.size data so I only have PA metro areas filtered by CBSA code
PA.msa.pop.processed <- PA.msa.population.size %>%
  filter(LSAD=="Metropolitan Statistical Area") %>%
  select(CBSA, NAME, CENSUS2010POP) %>%
  filter(CBSA %in% c("10900", "16540", "21500", "26860", "29540",
                     "30260", "35260", "37980",
                     "38300", "39740", "42540"))%>%
  arrange(desc(CENSUS2010POP))
#Picked the Top 5 Metropolitan Areas to look at based on 2010 US Census Population Size:
## 1. Philadelphia-Camden-Wilmington, PA-NJ-DE-MD 
## 2. Pittsburgh, PA  
## 3. Allentown-Bethlehem-Easton, PA-NJ 
## 4. Scranton--Wilkes-Barre--Hazleton, PA
## 5. Lancaster, PA 
#Looked at Monthly Lead Air concentrations of the Five major metropolitan areas in Pennsylvania 
#Philadelphia-Camden-Wilmington, PA-NJ-DE-MD 
epa.PA.monthly.Pb.Philly <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==37980)%>%
  select(Date,Date_combined, Daily.Mean.Pb.Concentration,) %>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))%>%
  add_column(CBSA="Philadelphia-Camden-Wilmington")
epa.PA.monthly.Pb.Philly
#
#Pittsburgh Metropolitan Area
epa.PA.monthly.Pb.Pitt <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==38300)%>%
  select(Date,Date_combined, Daily.Mean.Pb.Concentration,) %>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))%>%
  add_column(CBSA="Pittsburgh")
epa.PA.monthly.Pb.Pitt
#only have data from 2010 to 2017 for Pittsburgh area (Allegheny Co., PA)
#
# Allentown-Bethlehem-Easton, PA-NJ Metropolitan Area
epa.PA.monthly.Pb.Allentown <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==10900)%>%
  select(Date,Date_combined, Daily.Mean.Pb.Concentration,) %>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))%>%
  add_column(CBSA="Allentown-Bethlehem-Easton")
epa.PA.monthly.Pb.Allentown 
#Now looking at major mining county: Scranton-Wilkes-Barre-Hazleton Metropolitan Area (Luzerne Co, PA)
epa.PA.monthly.Pb.Scranton <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==42540)%>%
  select(Date,Date_combined,COUNTY, Daily.Mean.Pb.Concentration,) %>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))%>%
  add_column(CBSA="Scranton-Wilkes_Barre-Hazleton")
epa.PA.monthly.Pb.Scranton 
# Lancaster Metropolitan Area 
epa.PA.monthly.Pb.Lancaster <-epa.PA.airPb.processed %>%
  filter(CBSA_CODE==29540)%>%
  select(Date,Date_combined,COUNTY, Daily.Mean.Pb.Concentration,) %>%
  group_by(Date_combined)%>%
  dplyr::summarise(Mean.monthly.Pb = mean(Daily.Mean.Pb.Concentration))%>%
  add_column(CBSA="Lancaster")
epa.PA.monthly.Pb.Lancaster
#Created a monthly mean Pb air concentration in PA counties 
Pa.monthly.Pb.MSA<-rbind(epa.PA.monthly.Pb.Philly, epa.PA.monthly.Pb.Pitt, epa.PA.monthly.Pb.Allentown, epa.PA.monthly.Pb.Scranton, epa.PA.monthly.Pb.Lancaster)
Pa.monthly.Pb.MSA
class(Pa.monthly.Pb.MSA$Date_combined)
#
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
########### 
###########
# Created separate linear interpolation data frames for each metropolitan area in PA 
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
