### This includes land cover type. Relevent lines:
# BRT_00_Setup.R: pick explan.vars that include NVC_LC
# lines 26: setting NCV_LC as a factor
# lines 169-179: var.mat, rel.inf.mat, colnames
# lines 183: csv file name has "inclLC"
# lines 215 & 218 may need to change indices depending on number of variables
# lines 273: watch for variable names
# plots at end may fail...

#######################################
### SELECT ECOSYSTEM & DROUGHT TYPE ###   
#######################################

## Select ecosystem (PICK ONLY 1!)
# Forest
data.eco <- D[which(D$forest.use==1),]
# Steppe
data.eco <- D[which(D$steppe.use==1),]


## Select drought type (PICK ONLY 1!)
# Moderate
data.eco.drought <- data.eco[ , c("forest.use", "steppe.use", "S_mod", "pixel_ID", explan.vars) ]
# Severe
data.eco.drought <- data.eco[ , c("forest.use", "steppe.use", "S_sev", "pixel_ID", explan.vars) ]

## Set land cover type as a factor (has been integer)
data.eco.drought$NCV_LC <- factor(data.eco.drought$NVC_LC)
# Remove integer version
data.eco.drought[,5] <- NULL

# Append eco type to drought type
if (sum(data.eco.drought$forest.use) > 0){
  colnames(data.eco.drought)[3] <- paste0(colnames(data.eco.drought)[3],"_forest")
} else {
  colnames(data.eco.drought)[3] <- paste0(colnames(data.eco.drought)[3],"_steppe")
}

# Check & drop extra columns
colnames(data.eco.drought)[3]
data.eco.drought[,c("forest.use", "steppe.use")] <- NULL

# Remove NAs or BRT will fail.
data.eco.drought <- data.eco.drought[which(!is.na(data.eco.drought[,1])),] # remove any grid cells with missing response variable 





#################
### BRT SETUP ###   
#################

## Set sample size.
# sample.size <- 100
# sample.size <- 1000
sample.size <- 10000
# sample.size <- 25000
# sample.size <- 50000

## Set learning rate
# if (sample.size < 1500){ LR<-0.002}
# if (sample.size > 4500){ LR<-0.01}
LR<-0.01
# LR<-0.02
# LR<-0.005

## Set tree complexity
TC <- 5
# TC <- 10

# How many bootstraps?
# num.loops <- 1
# num.loops <- 1:2
num.loops <- 1:20

# Create empty list to store models in; create vectors to store 
models <- list()
model.name <- NULL
BRT.perc.dev.expl <- NULL
CV.correlation <- NULL
trees <- NULL
var <- list()
rel.inf <- list()

# What kind of drought type are you about to run?
(drought.type <- paste0(colnames(data.eco.drought)[1],"_n",sample.size,"_LR",LR,"_TC",TC))


################
### BRT LOOP ###   
################

start <- Sys.time()
for (i in num.loops){

  # Pull random sample
  sample <- data.eco.drought[which(data.eco.drought$pixel_ID %in% sample(data.eco.drought$pixel_ID, sample.size)), ]
  
  models[[i]] <-gbm.step(data=sample, 
                  gbm.x = 3:ncol(sample), # skip col with S and pixel_ID
                  gbm.y = 1, # response is S          
                  family = "gaussian", # use normal probability distribution
                  tree.complexity = TC, # number of nodes in a tree
                  learning.rate = LR, # will vary with eco/drought type
                  bag.fraction = 0.5, # pretty universally used
                  n.trees=500, # start with this number of trees
                  step.size=500, # iteratively add this number of trees
                  max.trees=22500, # max out at this number of trees
                  verbose=TRUE) # show me what happens!
  # Capture model name (here, iteration i), percent deviance explained, CV corrleation, num trees used.
  model.name.temp <- paste0(colnames(sample)[1],"_",i)
  BRT.perc.dev.expl.temp <- round(1-(models[[i]]$self.statistics$mean.resid/models[[i]]$self.statistics$mean.null),2) # percent deviance explained
  CV.correlation.temp  <- round(models[[i]]$cv.statistics$correlation.mean,2) # cross-validated correlation
  tree.temp <-
    models[[i]]$trees.fitted[match(TRUE, models[[i]]$cv.values == min(models[[i]]$cv.values))]
  
  # Capture each variable & its relative influence
  var.temp <- paste0(models[[i]]$contributions$var) 
  rel.inf.temp <- paste0(models[[i]]$contributions$rel.inf)

  # Add temp of each metric to vector
  model.name <- c(model.name, model.name.temp)
  BRT.perc.dev.expl <- c(BRT.perc.dev.expl, BRT.perc.dev.expl.temp)
  CV.correlation <- c(CV.correlation, CV.correlation.temp)
  trees <- c(trees, tree.temp )
  var <- c(var, var.temp)
  rel.inf <- c(rel.inf, rel.inf.temp)

}
print(Sys.time() - start)



#####################
### 1 MODEL PLOTS ###   
#####################

## Recreate default holdout deviance vs. number of trees plot (automatic with gbm.step)
attach(models[[5]]) # Can't query model_1, named above. Dunno why. Call actual name from gbm.step

y.bar <- min(cv.values) 
y.min <- min(cv.values - cv.loss.ses)
y.max <- max(cv.values + cv.loss.ses)

par(mai=c(1,1,1,1), mfrow=c(1,1))
plot(trees.fitted, cv.values, type = 'l', ylab = "Holdout deviance", xlab = "Number of trees", ylim = c(y.min,y.max))
abline(h = y.bar, col = 3)

lines(trees.fitted, cv.values + cv.loss.ses, lty=2)  
lines(trees.fitted, cv.values - cv.loss.ses, lty=2)  

target.trees <- trees.fitted[match(TRUE,cv.values == y.bar)]
abline(v = target.trees, col=4)
title(paste0(colnames(sample)[1], ", n=", sample.size, ", LR=", LR))

detach(models[[5]])


## Partial deviance of each variable
par(mai=c(0.6,0.1,0.1,0.1), mfrow=c(3,2))
gbm.plot(models[[i]], write.title=FALSE, smooth=TRUE, common.scale = FALSE,
         main=paste0(drought.type)) # marginal response curves



#####################################################################
## COMPILE ALL MODEL PERFORMANCE STATS & VARIABLE INFLUENCE VALUES ##
#####################################################################

# how many variables?
length(explan.vars)
# change matrix function ncol and actual col names per that number.

var.mat <- matrix(unlist(var), ncol = 11, byrow = TRUE) 
rel.inf.mat <- matrix(unlist(as.numeric(rel.inf)), ncol = 11, byrow = TRUE) 

stats <- cbind.data.frame(model.name, CV.correlation, BRT.perc.dev.expl,
                 trees, var.mat, rel.inf.mat)
colnames <- c("model.name", "CV.correlation", "BRT.perc.dev.expl", "num.trees",
              "var.1", "var.2", "var.3", "var.4", "var.5", "var.6",
              "var.7", "var.8", "var.9", "var.10", "var.11",
              "rel.inf.1", "rel.inf.2", "rel.inf.3", "rel.inf.4", "rel.inf.5", "rel.inf.6",
              "rel.inf.7", "rel.inf.8", "rel.inf.9", "rel.inf.10", "rel.inf.11") 
colnames(stats) <- colnames              


# Save as csv
currentDate <- Sys.Date()
csvFileName <- paste0(drought.type,"_stats_woEVIwoC_wLCwHLIwSHADE_",currentDate,".csv")
write.csv(stats, paste0(outdir,"/",csvFileName))


######################################
## CREATE ORDERED STATS LIST BY VAR ##
######################################






# Create a list for storing re-named relative influence values. List b/c they'll be dataframes, technically.
stats.list <- list()

# How many rows should get looped through?
rows <- 1:nrow(stats)

# Run loop to rename values and store in list.
for (i in rows){
  # Pull out one row at a time from stats table
  stats.row <- stats[i,] # Don't have to specify rows hereafter b/c there's only 1.
  colnames(stats.row)
  
  # Take variables (as row entires) and unlist, in order. Unname takes off col names.
  col.to <- unname(unlist(stats.row[,5:15])) %>% as.character()
  
  # Define which columns I want to change: they'll go from FROM to TO.
  col.from <- colnames(stats.row[,16:26]) 
  
  # Use vars to specify columns for renaming. Send those FROM cols to TO.
  stats.row <- stats.row %>% rename_at(vars(col.from), ~col.to)
  # Alt: stats.row %>% rename_at(vars(col.from), function(x) col.to) %>% head(2)
  # Rename_at is applied to all selected columns. I.e., function(x) basically receives
  # the col.from values in x, then does stuff with 'em. Vars is selector.
  
  # Rel.inf values are now in columns with associated var names. Drop cols w/ var names.
  stats.row <- stats.row %>% dplyr::select(-(5:15)) 
  
  # Fill list with each new row of stats AND asso. col names. These are technically dataframes
  stats.list[[i]] <- stats.row
}

# Bind all the rows stored as separate list columns (done to maintain col names).
# dplyr's bind_rows automagically splices contents of lists per col names. 
stats.new <- bind_rows(stats.list) # bind_rows automatically splices contents of lists per col names.

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.new, paste0(outdir,"/",drought.type,"_stats_ORDERED_woEVIwoC_wLCwHLIwSHADE_",currentDate,".csv"))

# Get mean & sd of all stats and relative influences
stats.sum <- stats.new %>%
  dplyr::select(-model.name) %>%
  summarize_all(funs(mean, sd))

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.sum, paste0(outdir,"/",drought.type,"_stats_SUMMARY_woEVIwoC_wLCwHLIwSHADE_",currentDate,".csv"))

# Clean up
# remove(list = ls(pattern = "stats"), name, col.to, col.from, currentDate) 
# remove(var, var.mat1, var.mat2)
# remove(rel.inf, rel.inf.mat1, rel.inf.mat2)
# remove(list = ls(pattern = ".temp"))
# remove(sample, LR, num.loops, sample.size, start, currentDate,
#        model.name, CV.correlation, BRT.perc.dev.expl, csvFileName, stats)


################################################## BELOW HERE IS UNNECESSARY TO RE-CREATE BOXPLOTS OF REL INFL

# ####################################
# ## OVERLAY ALL ITERATIONS OF BRTS ##
# ####################################
# 
# par(mfrow=c(1,1))
# 
# predictors<-list(rep(NA,length(models))) ## space for data: however many models are run
# responses<-list(rep(NA,length(models)))
# 
# explan.vars
# explan.vars.names <- c("Land cover type",
#                        "Drought exposure (%)",
#                        "Actual evapotranspiration (mm)",
#                        "Climatic water deficit (mm)",
#                        # "Baseline EVI",
#                        #"Aboveground C (metric tons)",
#                        "Soil AWC (volumetric fraction)",
#                        "Soil bulk density (kg/m^3)",
#                        "Elevation (m)",
#                        "Compound topographic index",
#                        "Head load index",
#                        "Shade density",
#                        "Groundwater-table depth (m)")
# 
# # Create folder for plots
# currentDate <- Sys.Date()
# dir.create(paste0(outdir, "/", drought.type, "_woEVIwoC_wLCwHLIwSHADE_", currentDate))
# plotdir <- paste0(outdir, "/", drought.type, "_woEVIwoC_wLCwHLIwSHADE_", currentDate)
# 
# ## Create a list to store the plots in
# myplots <- list()
# 
# ## Loop through the variables and overlay loess fit for each model on one plot per variable
# 
# for (i in 1:length(explan.vars)){ 
#   
#   ## Loop through the models and populate the lists of predictors and responses -- these are marginal effects of selected variables.
#   ## Calculate the x and y limits for plotting and adjust the response scale as in dismo partial plots function (subtract mean)
#   for(j in 1:length(models)){
#     gbm.mod<-models[[j]]
#     r1 <- gbm::plot.gbm(gbm.mod, i.var = i, return.grid = TRUE) # return.grid only gives eval pts & avg predictions. no graphics. c.f. gbm.plot
#     predictors[[j]]<-r1[,1]
#     responses[[j]]<-r1[,2] - mean(r1[,2])
#     
#   }
#   
#   # currentDate <- Sys.Date()
#   # tiff(paste0(plotdir, "/", explan.vars[[i]], ".tif"))
#   pdf(paste0(plotdir, "/", explan.vars[[i]], ".pdf"))
#   par(mar=c(5.5,5.1,4.1,2.1))
#   
#   ymin=min(unlist(responses))
#   ymax=max(unlist(responses))
#   xmin=min(unlist(predictors))
#   xmax=max(unlist(predictors))
#   
#   ## Create first plot of first model (j = 1), then overlay next models
#   j <- 1
#   temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
#   plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1, 
#        xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
#   
#   ## Overlay models (starting with number 2 til the second to last
#   for(j in 2:(length(models)-1)){
#     ## variable i, model j
#     par(new=TRUE)
#     temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
#     plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1,
#          xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
#   }
#   
#   ## create final overlay with last model; add labels here.
#   ## variable i, j=last
#   j <- length(models)
#   par(new=TRUE)
#   temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
#   plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1,
#        ylab="Fitted function", xlab=explan.vars.names[i], ylim=c(ymin,ymax),xlim=c(xmin,xmax),
#        main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
#   
#   abline(a=0,b=0,lty=1)
#   
#   ## Store the plots in a list
#   myplots[[i]] <- recordPlot()
#   
#   dev.off()
# }  
# 
# ## FIXME
# # There are many functions for plottings ggplot stuff together (GROBjects).
# # It's less clear how to handle non-GROBs (e.g., gtable, cowplot's plot_grid, multiplot won't work).
# # I've recorded plots (recordPlot) but when I try to show each in sequence, multiple panels are overriden
# # I.e., I can call each plot, but each plot takes over whole area, seemingly overriding par(mfrow)
# 
# # par(mfrow=c(5,2))
# # for(i in length(myplots)){
# # myplots[[i]]
# # }
# # dev.off()
# # N.b., just calling plot is same as this:
# # replayPlot(myplots[[3]])
# 
# 
# 
# 
# ##########################################
# ### PREP TO AVERAGE ALL LINES TOGETHER ###   
# ##########################################
# 
# predictors<-list() 
# responses<-list()
# temp.lo <- list()
# all.lo <- NULL
# 
# ## Loop through the models and populate the lists of predictors and responses -- these are marginal effects of selected variables.
# ## Calculate the x and y limits for plotting and adjust the response scale as in dismo partial plots function (subtract mean)
# 
# # Can't just call vars in order of how they appear in model, b/c each model has diff order. 
# #So, specify which var to use as predictor in gbm::plot.gbm below 
# for(i in 1:length(explan.vars)){
#   for(j in 1:length(models)){
#     # store evaluation points (explan.var values) and the marginal effects. return.grid gives values, no graphics.
#     r1 <- gbm::plot.gbm(models[[j]], i.var = explan.vars[i], return.grid = TRUE) 
#     predictors[[j]]<-r1[,1] # store the predictor values
#     responses[[j]]<-r1[,2] - mean(r1[,2]) # store the predictor values, standardized by subtracting mean
#     # run smoothing (local weighted scatter smooth = lowess aka loess)
#     temp.lo[[j]] <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
#     # store smoothing predictor values, predictED values, and which variable. "unname" drops extra col from x.
#     df.lo <- data.frame(var = paste0(explan.vars[i]), x = unname(temp.lo[[j]]$x), y = temp.lo[[j]]$fitted)
#     # iteratively add these individ var loess dfs to one that includes all variables.
#     all.lo <- rbind(all.lo, df.lo)
#   }
# }
# 
# # Save these loess values for this type of drought so I can plot average across all droughts.
# currentDate <- Sys.Date()
# write.csv(all.lo, paste0(outdir, "/", drought.type, "_LO.PRED_woEVIwoC_wLCwHLIwSHADE_", currentDate,".csv"))
# 
# # Once all drougth types are run, proceed with BRT_05_PlotVarInf_AllDroughts.R 
# 
# #######################
# ### SAVE MODELS JIC ###   
# #######################
# 
# # Save models JIC. save() needs names and will look to global envi unless I say look to list.
# names(models) <- paste0("model_", 1:length(num.loops)) # First assign names to each of the models.
# currentDate <- Sys.Date()
# Rdata.name <- paste0(scratchdir, "/", drought.type, "_woEVIwoC_wLCwHLIwSHADE_", currentDate,".Rdata")
# save(list=names(models),
#      file=paste0(Rdata.name),
#      envir=as.environment(models))
# # load() # Insert that .Rdata here to reload models
# 
