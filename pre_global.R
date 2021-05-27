library(shiny)
library(shinyIncubator)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
library(foreign)
library(vegan)
library(raster)
library(leaflet)
library(rhandsontable)
library(Matrix)
library(plyr)
library(dplyr)
library(tidyr)
library(prioritizr)
library(here)

### Load rasters
raster.on <- TRUE

bound <- raster(here("data/final/bound/AOI_Raster.tif"))
bound.val <- bound[]

if(!file.exists(here("data/final/bound/bound.mat.rds"))) {
  bound.mat <- boundary_matrix(bound)
  saveRDS(bound.mat, here("data/final/bound/bound.mat.rds"))
} else {
  bound.mat <- readRDS(here("data/final/bound/bound.mat.rds"))
}

# #need to reload bound to make sure the right path is set on the server
# bound <- raster("./GIS/WHSA_Boundary.tif")


prot.rast <- raster(here("data/final/prot/ProtectedAreas.tif"))

# cost
f_cost <- list.files(path = here("data/final/cost/"), pattern = ".tif$", full.names = TRUE)
cost <- stack(f_cost, bound)
cost[[1]] <- cost[[1]] + 0.001
cost[[2]] <- cost[[2]] + 0.001

names(cost)[nlayers(cost)] <- "Area"

# features
features <- stack(list.files(path = here("data/final/"), pattern = ".tif$", full.names = TRUE))
features.df <- as.data.frame(features)
features.df <- features.df[!is.na(bound.val),]
features.df[is.na(features.df)] <- 0
features.df.sums <- colSums(features.df)

#calculate % protected per feature
# feat.prot <- features.df
# 
# for(ii in 1:ncol(feat.prot)){
#   feat.prot[,ii][feat.prot$Protected_Areas == 1] <- 0
# }
# feat.prot$Protected_Areas <- 0
# 
# feat.prot.sums <- colSums(feat.prot)
# 
# feat.sums.comb <- data.frame(All_cells = features.df.sums, Prot_rem = feat.prot.sums)
# feat_prot <- (feat.sums.comb$All_cells - feat.sums.comb$Prot_rem)/ feat.sums.comb$All_cells


in.raster <- bound
in.rast.val <- bound.val
in.rast.val[!is.na(bound.val) & bound.val == 1] <- 0

#Setup "Edit Target" table
nms <- names(features)
feat.lst <- data.frame(id=seq(1,length(nms)),
                       Percent=0,
                       name=nms,
                       #name=names(puvsf[[1]][,-1]),
                       stringsAsFactors =F)
l.feat.lst <- nrow(feat.lst)


#setup "Edit Scenarios" table
scen <- data.frame(scenario="template",
                   #time="curr",
                   cost="Area",
                   protected="locked",
                   BLM = 0,
                   Edge = 0,
                   #                   FTcutoff=0,
                   stringsAsFactors =F)
scen_col <- ncol(scen)
for(kk in (scen_col+1):(scen_col+nrow(feat.lst)))
  scen[,kk] <- 0	
names(scen)[(scen_col+1):ncol(scen)] <- feat.lst$name


####
#Test for df
####
pu <- data.frame(id=seq(1:sum(!is.na(bound.val))),cost=1,status=0)
#pu <- pu[!is.na(bound.val),]

rij.id <- data.frame(pu=pu$id)
rij <- data.frame(pu=vector(),species=vector(),amount=vector)

spp <- 1
spp.nm <- names(features)

for (ii in 1:ncol(features.df)){

  print(names(features)[ii])
  flush.console()

  rij.tmp <- data.frame(pu=rij.id$pu,species=spp,amount=features.df[,ii])

  spp <- spp + 1

  rij.tmp <- drop_na(rij.tmp)
  rij <- rbind(rij,rij.tmp)
  rm(rij.tmp)

  gc()
}

rij <- rij[with(rij, order(pu, species)), ]

#so that prioritzr is tricked into thinking pu and rij have the same length in id
#need to fix that error in prioritzr
#rij <- rbind(rij,c(max(pu$id),min(rij$species),0))

feat <- data.frame(id=seq(1,spp-1),name=spp.nm)

#save.image(paste0(owd,"/problem_setup.RData"))

#tt <- problem(pu, features, rij) %>%
# add_min_set_objective() %>%
# add_binary_decisions()

#ss <- solve(tt %>% add_relative_targets(0.17))
pu.tmp <- pu
gc()

problem_list <- list()
for(ii in 1:nlayers(cost)){
  problem_list[[ii]] <- problem(cost[[ii]], features)
  
}

names(problem_list) <- names(cost)

save.image("pre_global.RData")

