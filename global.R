# Celiac app global with packages and data
# Contributor: Lindsay Hracs
# Date: 06-Dec-22
# R version 4.1.2

##########

# If you are adapting this code, please cite:

# Buie MJ, Quan J, Windsor JW, et al. Global hospitalization trends for Crohn's disease and ulcerative colitis in the 21st century:
# A systematic review with temporal analyses. Clin. Gastroenterol. Hepatol. 2022. doi: https://doi.org/10.1016/j.cgh.2022.06.030
# Shiny app: https://kaplan-gi.shinyapps.io/hospitalization/

# King JA, Bakal JA, Bing L, et al. Variation in testing for and incidence of celiac autoimmunity in Canada: A population-based study. 
# Gastroenterology. 2023. doi: https://doi.org/10.1053/j.gastro.2022.12.040
# Shiny app: https://kaplan-gi.shinyapps.io/CAVE/

# Windsor JW, Hracs LH, Gorospe J, et al. The global evolution of inflammatory bowel disease across four epidemiologic stages:
# A systematic review of incidence and prevalence studies over the past century. Gastroenterology. To appear.
# Shiny app: https://kaplan-gi.shinyapps.io/GIVES21/

##########

# load libraries
library(rsconnect)
library(shiny) 
library(shinyjs) 
library(shinythemes) 
library(shinyWidgets)
library(readr) # parsing date information
library(DT) 
library(tidyverse) 
library(ggplot2) 
library(ggtext) # HTML in ggplots
library(dplyr) # filter columns in df
library(ggiraph)
library(viridis)
library(plotly)
library(reshape2)
library(shinycssloaders)
library(leaflet) # version 2.0.4.1
library(leaflet.esri)
library(leaflet.extras)
library(sf)
library(geojsonsf)
library(fontawesome)

# data prep

inc_prov <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Provincial_Map_CDA_Incidence_2022-07-11.csv", header = T)
inc_zone <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Zone_Map_CDA_Incidence_2022-11-22.csv", header = T)
inc_HSA <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/HSA_Map_CDA_Incidence_2022-11-22.csv", header = T)
test_prov <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Provincial_Map_TTG_Testing_2022-08-12.csv", header = T)
test_zone <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Zone_Map_TTG_Testing_2022-11-22.csv", header = T)
test_HSA <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/HSA_Map_TTG_Testing_2022-11-22.csv", header = T)

indiv_prov <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Provincial_Map_TTG_Individual_Testing_2022-09-16.csv", header = T)
indiv_zone <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/Zone_Map_TTG_Individual_Testing_2022-09-16.csv", header = T)
indiv_HSA <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/HSA_Map_TTG_Individual_Testing_2022-11-24.csv", header = T)

can <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/geo_files/main/canada_prov_terr.json")
HSAs <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/geo_files/main/HSA.geojson")
zones <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/geo_files/main/Zone.geojson")


# create prov, zone, and HSA data frames

inc_prov_data <- inner_join(can, inc_prov, by = "name")
inc_zone_data <- inner_join(zones, inc_zone, by = "ZONE_NAME")
inc_HSA_data <- inner_join(HSAs, inc_HSA, by = "HSA_NAME")
test_prov_data <- inner_join(can, test_prov, by = "name")
test_zone_data <- inner_join(zones, test_zone, by = "ZONE_NAME")
test_HSA_data <- inner_join(HSAs, test_HSA, by = "HSA_NAME")
indiv_prov_data <- inner_join(can, indiv_prov, by = "name")
indiv_zone_data <- inner_join(zones, indiv_zone, by = "ZONE_NAME")
indiv_HSA_data <- inner_join(HSAs, indiv_HSA, by = "HSA_NAME")

inc_zone_data_met <- subset(inc_zone_data, MET == "yes")
inc_HSA_data_met <- subset(inc_HSA_data, MET == "yes")
test_zone_data_met <- subset(test_zone_data, MET == "yes")
test_HSA_data_met <- subset(test_HSA_data, MET == "yes")
indiv_zone_data_met <- subset(indiv_zone_data, MET == "yes")
indiv_HSA_data_met <- subset(indiv_HSA_data, MET == "yes")

inc_zone_data_nonmet <- subset(inc_zone_data, MET == "no")
inc_HSA_data_nonmet <- subset(inc_HSA_data, MET == "no")
test_zone_data_nonmet <- subset(test_zone_data, MET == "no")
test_HSA_data_nonmet <- subset(test_HSA_data, MET == "no")
indiv_zone_data_nonmet <- subset(indiv_zone_data, MET == "no")
indiv_HSA_data_nonmet <- subset(indiv_HSA_data, MET == "no")


# data for 3D plots

years_3D <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/3D_Age_Sex_Testing_Incidence_2022-10-17.csv", header = T)
bands_3D <- read.csv("https://raw.githubusercontent.com/kaplan-gi/Map_Data/main/3D_AgeBands_Sex_Testing_Incidence_2022-10-17.csv", header = T)
years_3D$SEX <- recode_factor(years_3D$SEX, "F" = "Female", "M" = "Male")
bands_3D$SEX <- recode_factor(bands_3D$SEX, "F" = "Female", "M" = "Male")
dataF_years <- subset(years_3D, SEX == "Female")
dataM_years <- subset(years_3D, SEX == "Male")
dataF_bands <- subset(bands_3D, SEX == "Female")
dataM_bands <- subset(bands_3D, SEX == "Male")

# create columns with distance values for error array which uses distance not axis value; lower CI = upper CI

years_3D[,"lowerCI_inc"] <- years_3D$INCIDENCE_RT - years_3D$INCIDENCE_L95
years_3D[,"upperCI_inc"] <- years_3D$INCIDENCE_U95 - years_3D$INCIDENCE_RT
years_3D[,"lowerCI_test"] <- years_3D$TEST_RT - years_3D$TEST_L95
years_3D[,"upperCI_test"] <- years_3D$TEST_U95 - years_3D$TEST_RT
dataF_years[,"lowerCI"] <- dataF_years$INCIDENCE_RT - dataF_years$INCIDENCE_L95
dataF_years[,"upperCI"] <- dataF_years$INCIDENCE_U95 - dataF_years$INCIDENCE_RT
dataM_years[,"lowerCI"] <- dataM_years$INCIDENCE_RT - dataM_years$INCIDENCE_L95
dataM_years[,"upperCI"] <- dataM_years$INCIDENCE_U95 - dataM_years$INCIDENCE_RT

bands_3D[,"lowerCI_inc"] <- bands_3D$INCIDENCE_RT - bands_3D$INCIDENCE_L95
bands_3D[,"upperCI_inc"] <- bands_3D$INCIDENCE_U95 - bands_3D$INCIDENCE_RT
bands_3D[,"lowerCI_test"] <- bands_3D$TEST_RT - bands_3D$TEST_L95
bands_3D[,"upperCI_test"] <- bands_3D$TEST_U95 - bands_3D$TEST_RT
dataF_bands[,"lowerCI"] <- dataF_bands$INCIDENCE_RT - dataF_bands$INCIDENCE_L95
dataF_bands[,"upperCI"] <- dataF_bands$INCIDENCE_U95 - dataF_bands$INCIDENCE_RT
dataM_bands[,"lowerCI"] <- dataM_bands$INCIDENCE_RT - dataM_bands$INCIDENCE_L95
dataM_bands[,"upperCI"] <- dataM_bands$INCIDENCE_U95 - dataM_bands$INCIDENCE_RT


# options

options(spinner.color="#52D6F4")#, warn = -1)
