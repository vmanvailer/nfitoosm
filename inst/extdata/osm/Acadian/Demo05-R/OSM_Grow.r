
library(ggplot2)
library(dplyr)

#Set working directory to this r source file from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#TODO: Update the exe path for your machine
OSMexe  <-"C:\\OSM\\OSMv2.25.1_Win64\\OSM.ConsoleApp.exe"

Variant <-"Acadian"
OSMcmds<-"OSM_Grow.osmc"

#Run OSM
system(paste(OSMexe,Variant,OSMcmds))

#View stand-level report projections---------------------------------------------
StandForecast <- read.csv("StandListProjections.csv")
View(StandForecast)

#Graph stand projections (ggplot2 library required)
ggplot(StandForecast, aes(x=Year, y=GMV))+geom_line()

#View tree-level report projections summarized to the stand level----------------
TreeForecast <- read.csv("TreeListProjections.csv")
View(TreeForecast)

#Summarize species basal area and volume at the stand level (dplyr library required)
TreeForecast.stand <- TreeForecast %>%
                      group_by(Year,Species) %>%
                      summarize(BA=sum(0.00007854*DBH^2*Stems),GMV=sum(GMV*Stems))

View(TreeForecast.stand)

ggplot(TreeForecast.stand, aes(x=Year, y=BA,col=Species))+geom_line()
ggplot(TreeForecast.stand, aes(x=Year, y=GMV,col=Species))+geom_line()


