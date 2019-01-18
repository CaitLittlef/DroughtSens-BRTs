# Read in the data to be modeled using boosted regression trees (BRT)
# setwd("F:/NWCSC_REFUGIA/PROJECTS/Drought_sensitivity/tabular_outputs")
# setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/Backed Up/Caitlin/Drought Sensitivity/data_for_modeling")
# D <- read.csv("data_for_modeling.csv")

# Separate into forest and steppe datasets
# Forest <- D[which(D$forest.use==1),] # already done during set-up
# Steppe <- D[which(D$steppe.use==1),] # already done during set-up

# Choose explanatory variables to model.
explan.vars <- c("EAD",  # starting with exposure to ANY drought, but could instead use EMD and/or ESD
                 "AET","Deficit", # Climate normals
                 "base_EVI","AGC",  # Biomass variables     
                 "soil_AWC","soil_BD",  # Soil variables
                 "elev","CTI","HLI","shade_dens",  # Topo variables
                 "WTD") # Water table depth


##################################################################################
###      PRELIMINARY EXPLORATION OF COLLINEARITY                              ####
##################################################################################

# Examine pairwise correlations to reduce collinearity of explanatory vars:
# Note that due to large number of grid cells, even weak correlations will
# have significant P values.

Forest.explan.pairwise.cors <- data.frame()
Steppe.explan.pairwise.cors <- data.frame()

for (V1 in explan.vars){
  for (V2 in explan.vars){

    cor <- cor.test(Forest[[V1]], Forest[[V2]])
    row <- data.frame(V1, V2, cor=round(cor$estimate,4), P=round(cor$p.value, 4))
    Forest.explan.pairwise.cors <- rbind(Forest.explan.pairwise.cors, row)
    rm(cor, row)
    cor <- cor.test(Steppe[[V1]], Steppe[[V2]])
    row <- data.frame(V1, V2, cor=round(cor$estimate,4), P=round(cor$p.value, 4))
    Steppe.explan.pairwise.cors <- rbind(Steppe.explan.pairwise.cors, row)
    rm(cor, row)
  }
}

rm(V1, V2)



# Look at variable inflation factors (VIF) to determine degrees of collinearity
# A VIF >10 signals collinerity prob. 
library(usdm)

Forest.vars <- Forest[ , explan.vars]
Steppe.vars <- Steppe[ , explan.vars]

vif(Forest.vars)
vif(Steppe.vars)
# ^ See none have VIF > 10

# Alt:
vifcor(Forest.vars, 0.9) # sets threshold of r=0.9 for IDing collinear pairs.
vifstep(Forest.vars, 10) # sets threshold of VIF=10 for IDing collinear pairs

rm(Forest.vars, Steppe.vars)



