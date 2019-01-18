### PACKAGES
install.packages("readr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("dismo")
install.packages("gbm")
install.packages("usdm")
install.packages("pdp")


### LIBRARIES
library(readr)
library(tidyverse)
library(dplyr)
library(dismo)
library(gbm)
library(usdm)
library(pdp)


### GET SITUATED
setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Drought Sensitivity")
# setwd("D:/Shared/BackedUp/Caitlin/Drought Sensitivity") # If on goshawk
scratchdir <- ("//goshawk.sefs.uw.edu/Space_Lawler/Shared/Scratch/Workspace/Littlefield/DroughtSensitivity_mods")
# scratchdir <- ("D:/Shared/Scratch/Workspace/Littlefield/DroughtSensitivity_mods")
datadir <- ("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Drought Sensitivity/data_for_modeling")
# datadir <- ("D:/Shared/BackedUp/Caitlin/Drought Sensitivity/data_for_modeling")
outdir <- ("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/Drought Sensitivity/BRT_modeling/BRT_outputs")
# outdir <- ("D:/Shared/BackedUp/Caitlin/Drought Sensitivity/BRT_modeling/BRT_outputs")


# Read in data
D <- read.csv(paste0(datadir,"/","data_for_modeling.csv"))

# # Separate into forest and steppe datasets
# Forest <- D[which(D$forest.use==1),]
# Steppe <- D[which(D$steppe.use==1),]

# # Choose explanatory variables to model -- INITIAL
# explan.vars <- c("EAD",  # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "base_EVI","AGC",  # Biomass variables     
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI","HLI","shade_dens",  # Topo variables
#                  "WTD") # Water table depth
# 
# # Choose explanatory variables to model -- THIS PICKS LAND COVER, TOO!
# explan.vars <- c("NVC_LC", # Land cover type (factor)
#                  "EAD",  # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "base_EVI","AGC",  # Biomass variables
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI","HLI","shade_dens",  # Topo variables
#                  "WTD") # Water table depth
# 
# # Choose explanatory variables to model -- DROPS LC, HLI, SHADE_DENS
# explan.vars <- c("EAD", # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "base_EVI","AGC",  # Biomass variables
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI",  # Topo variables
#                  "WTD") # Water table depth
# 
# # Choose explanatory variables to model -- DROPS LC, HLI, SHADE_DENS, C
# explan.vars <- c("EAD", # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "base_EVI",  # Biomass variables
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI",  # Topo variables
#                  "WTD") # Water table depth
# 
# # Choose explanatory variables to model -- DROPS LC, HLI, SHADE_DENS, EVI
# explan.vars <- c("EAD", # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "AGC",  # Biomass variables
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI",  # Topo variables
#                  "WTD") # Water table depth

# Choose explanatory variables to model -- DROPS LC, HLI, SHADE_DENS, EVI, C
explan.vars <- c("EAD", # Exposure to ANY drought, but could instead use EMD and/or ESD
                 "AET","Deficit", # Climate normals
                 "soil_AWC","soil_BD",  # Soil variables
                 "elev","CTI",  # Topo variables
                 "WTD") # Water table depth

# # Choose explanatory variables to model -- DROPS base_EVI and AGC
# explan.vars <- c("NVC_LC", # Land cover type (factor)
#                  "EAD",  # Exposure to ANY drought, but could instead use EMD and/or ESD
#                  "AET","Deficit", # Climate normals
#                  "soil_AWC","soil_BD",  # Soil variables
#                  "elev","CTI","HLI","shade_dens",  # Topo variables
#                  "WTD") # Water table depth

