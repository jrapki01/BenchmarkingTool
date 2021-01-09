library(readxl)
library(here)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(data.table)


####################
## CIN Census Data
####################

## This has been processed externally to save server useage time. We will read in the .rds file
dfCIN <- readRDS(here("cin_data_2020.rds"))

dfCIN <- dfCIN %>% 
  filter(Measure != "Number")

dfCIN_topics <- dfCIN %>% 
  filter(Measure != "Number") %>% 
  select(distinct_id, subset_category, metric) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(metric = as.factor(metric),
         distinct_id = as.factor(distinct_id),
         subset_category = as.factor(subset_category))

#####################
## 903 Data
#####################

## 903 Data
## This has been processed externally to save server useage time. We will read in the .rds file
dfLAC <- readRDS(here("lac_data_2020V2.rds"))

dfLAC <- dfLAC %>% 
  filter(Measure != "Number") 

dfLAC_topics <- dfLAC %>% 
  filter(Measure != "Number") %>% 
  select(distinct_id, subset_category, metric) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(metric = as.factor(metric),
         distinct_id = as.factor(distinct_id),
         subset_category = as.factor(subset_category))

#####################
## Fostering Data
#####################

## Fostering Data
## This has been processed externally to save server useage time. We will read in the .rds file
dfFOST <- readRDS(here("fostering_data_2020.rds"))

dfFOST <- dfFOST %>% 
  filter(Measure != "Number") 

dfFOST_topics <- dfFOST %>% 
  filter(Measure != "Number") %>% 
  select(distinct_id, subset_category, metric) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(metric = as.factor(metric),
         distinct_id = as.factor(distinct_id),
         subset_category = as.factor(subset_category))

#####################
## Workforce Data
#####################

## Workforce Data
## This has been processed externally to save server useage time. We will read in the .rds file
dfWORK <- readRDS(here("workforce_data_201819.rds"))

dfWORK <- dfWORK %>% 
  filter(Measure != "Number") 

dfWORK_topics <- dfWORK %>% 
  filter(Measure != "Number") %>% 
  select(distinct_id, subset_category, metric) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(metric = as.factor(metric),
         distinct_id = as.factor(distinct_id),
         subset_category = as.factor(subset_category))

####################
## LA Data
####################

## Local Authorities
LA <- read.csv(here("la_names.csv"), header = TRUE)

## CUSTOM 
LA_CUSTOM <- setNames(LA$old_la_code, LA$la_name)

## STAT NEIGH GROUPINGS (Pre 2019/20)
#"England", "East of England", "Bournemouth", "East Sussex", "Isle of Wight", "Kent", "Medway", "Plymouth", "Poole", "Sheffield", "Southend-on-Sea", "Swindon", "Telford and Wrekin"
STATNEIGH <- LA %>% 
  filter(old_la_code %in% c("999", "F", "837", "845", "921", "886", "887", "879", "836", "373", "882", "886", "894"))
STATNEIGH_GROUP <- setNames(STATNEIGH$old_la_code, STATNEIGH$la_name)

## STAT NEIGH GROUPINGS (2019/20 Onwards)
#"England", "East of England", "Bournemouth, Christchurch & Poole", "East Sussex", "Isle of Wight", "Kent", "Medway", "Plymouth", "Sheffield", "Southend-on-Sea", "Swindon", "Telford and Wrekin", "Torbay"
STATNEIGH_201920 <- LA %>% 
  filter(old_la_code %in% c("999", "F", "839", "845", "921", "886", "887", "879", "373", "882", "886", "894", "880"))
STATNEIGH_201920_GROUP <- setNames(STATNEIGH_201920$old_la_code, STATNEIGH_201920$la_name)

## IMG GROUPINGS
#"England","East of England", "Dudley", "Gateshead", "Havering", "Hillingdon", "Isle of Wight", "Medway", "Plymouth", "Redbridge", "Sheffield", "Slough", "Southend-on-Sea", "Wakefield", "Wirral"
IMD <- LA %>% 
  filter(old_la_code %in% c("999", "F", "332", "390", "311", "312", "921","887", "879", "317", "373", "871", "882", "384", "344"))
IMD_GROUP <- setNames(IMD$old_la_code, IMD$la_name)

## REGIONAL GROUPINGS
REGIONAL <- LA %>% 
  filter(old_la_code %in% c("999", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"))
REGIONAL_GROUP <- setNames(REGIONAL$old_la_code, REGIONAL$la_name)

## EASTERN REGION
EASTERN <- LA %>% 
  filter(old_la_code %in% c("999", "822", "873", "823", "881", "919", "821", "926", "874", "882", "935", "883"))
EASTERN_GROUP <- setNames(EASTERN$old_la_code, EASTERN$la_name)


##################
## Colour Paletes
##################

## This controls the colours of the plots based on whether the confidence intervals overlap with England
pal <- c(
  "Higher" = "#004C97",#"dodgerblue4",
  "Similar" = "darkorange2",
  "Lower" = "#009FDF", #"dodgerblue1",
  "England" = "grey"
)

## This controls the colours of the time series plots
pal2 <- c(
  "Higher" = "#004C97",#"dodgerblue4",
  "Similar" = "darkorange2",
  "Lower" = "#009FDF", #"dodgerblue1",
  "England" = "black"
)

## This controls the colours for the population plot
cols <- c("Male" = "#004C97",
          #"steelblue", 
          "Female" = "#009FDF",
          #"skyblue", 
          "England" = "Black")

###################
## Population Data
###################

## This has been processed externaly to save server usage time. We will read in the .rds file
dfPop <- readRDS(here("PopGender.rds"))