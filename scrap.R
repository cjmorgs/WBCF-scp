library(raster)
library(here)
library(magrittr)

setwd(here("data/raw/"))
fls <- list.files(pattern = "*.tif$")
base_raster <- raster(raster(fls[1]))

for(ii in 1:length(fls)){
  print(fls[ii])
  flush.console()
  tmp_r <- raster(fls[ii])
  tmp_r <- tmp_r %>% projectRaster(crs = proj4string(base_raster), method = "ngb") %>%
    resample(base_raster, method = "ngb")
  writeRaster(tmp_r, here("data/intermediate", fls[ii]))
  rm(tmp_r)
}

# ss <- stack(list.files(here("data/intermediate"), full.names = TRUE))

aoi <- raster(here("data/intermediate/AOI_Raster.tif"))
aoi_val <- aoi[]
aoi[] <- ifelse(!is.na(aoi_val), 1, aoi_val)
writeRaster(aoi, here("data/intermediate/AOI_Raster.tif"), overwrite = TRUE)


library(prioritizr)
library(gstat)
library(raster)


setwd("data")
fls <- list.files(pattern = "*.tif$")
ss <- stack(fls)

loc <- "D:/Work/UNBC/Ian_Curtis/"
out <- "D:/Work/UNBC/Ian_Curtis/Ian_Curtis/GIS/"

if(first){
  rr <- raster(paste(out,"footprint_raster.tif",sep=""))
  rr[] <- 0
  #rr[][is.na(bound[])] <- NA
  writeRaster(rr, filename=paste(out,"footprint_raster.tif",sep=""), format="GTiff", overwrite=TRUE)
  
  system(paste("gdalwarp -r near"
               ,paste(loc,"footprint_raster.tif",sep="")
               ,paste(out,"footprint_raster.tif",sep=""),sep=" "))
  
}


bound <- raster(paste0(out,"boundary_raster.tif"))
cost <- raster(paste0(out,"footprint_raster.tif"))

cost[][is.na(bound[])] <- NA

#fake 4 species
cost.val <- cost[]
spp1 <- spp2 <- spp3 <- spp4 <- cost

#g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)
#tt <- data.frame(coordinates(cost))
#names(tt) <- c('x','y')
#yy <- predict(g.dummy, newdata=tt, nsim=4)

spp1[][!is.na(cost.val)] <- round(rnorm(length(cost.val[!is.na(cost.val)]),5,1)*100,0)
spp2[][!is.na(cost.val)] <- round(rnorm(length(cost.val[!is.na(cost.val)]),5,1)*100,0)
spp3[][!is.na(cost.val)] <- round(rnorm(length(cost.val[!is.na(cost.val)]),5,1)*100,0)
spp4[][!is.na(cost.val)] <- round(rnorm(length(cost.val[!is.na(cost.val)]),5,1)*100,0)

spp1[][spp1[] > 525 | spp1[] < 475 ] <- 0
spp2[][spp2[] > 525 | spp2[] < 475 ] <- 0
spp3[][spp3[] > 525 | spp3[] < 475 ] <- 0
spp4[][spp4[] > 525 | spp4[] < 475 ] <- 0

writeRaster(spp1, filename=paste0(out,"Species1.tif"), format="GTiff", overwrite=TRUE)
writeRaster(spp2, filename=paste0(out,"Species2.tif"), format="GTiff", overwrite=TRUE)
writeRaster(spp3, filename=paste0(out,"Species3.tif"), format="GTiff", overwrite=TRUE)
writeRaster(spp4, filename=paste0(out,"Species4.tif"), format="GTiff", overwrite=TRUE)


features <- stack(spp1,spp2,spp3,spp4)
writeRaster(features, filename=paste0(out,"Features.tif"), format="GTiff", overwrite=TRUE)

cost.1 <- cost
cost.1[][!is.na(cost.val) & cost.val == 0] <- 0.0000001


#create base ILP problem with input data
prob <- problem(cost.1, features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() 

#change target values according to user input
prob.ta <- prob %>% add_relative_targets(0.20)

#
s.prob <- solve(prob.ta)

library(rgdal)
owd <- setwd("D:/Work/UNBC/Ian_Curtis/ILP_InputPackage_1/raster/")
files <- list.files(pattern = "*.tif$")
ss <- stack(files)






####
#Test for df
####
pu <- data.frame(id=seq(1:length(cost[[4]])),cost=1,status=0)
pu <- pu[!is.na(bound.val),]

rij.id <- data.frame(pu=pu$id)
rij <- data.frame(pu=vector(),species=vector(),amount=vector)

spp <- 1
spp.nm <- names(features)

for (ii in 1:nlayers(features)){

  print(names(features)[ii])
  flush.console()
  
  rij.tmp <- data.frame(pu=rij.id$pu,species=spp,amount=features[[ii]][][!is.na(bound.val)])
  
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

pu.tmp$cost <- cost[[1]][!is.na(bound.val)]
#create Marxan type problem entire extent
pweek.base <- problem(pu.tmp, feat, rij) %>%
  add_min_set_objective() %>%
  add_binary_decisions()

owd <- setwd("output")
file.remove(list.files())
for(ii in 1:nlayers(cost)){
  writeRaster(cost[[ii]], filename=paste0(names(cost)[ii],".tif"), overwrite=TRUE)
}
zip(zipfile = 'testZip', files = list.files())
setwd(owd)

names(cost)
writeRaster(cost, filename="multilayer.tif", options="INTERLEAVE=BAND", overwrite=TRUE)





if (!is.null(input$hot_multi)) {
  DF = hot_to_r(input$hot_multi)
  values[["hot_multi"]] = DF  
} else if (!is.null(input$scen_file)){
  DF = read.csv(input$scen_file$datapath,stringsAsFactors =F)
  values[["hot_multi"]] = DF  
} else if (!is.null(values[["hot_multi"]])) {
  DF = values[["hot_multi"]]
}

inr <- raster("D:/Work/UNBC/Jerrica_Mann/GIS/WHSA_Boundary.tif")
inr[] <- NA
writeRaster(inr, filename="D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", format="GTiff", datatype = "FLT4S", overwrite=TRUE)

inwd <- "D:/Work/UNBC/Jerrica_ILP/Tiffs/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/jerrica_features/"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}

inwd <- "D:/Work/UNBC/Jerrica_ILP/BECzones/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/jerrica_zones"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}


inwd <- "D:/Work/UNBC/Jerrica_ILP/Zones/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/jerrica_zones_old/"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}


inwd <- "D:/Work/UNBC/Jerrica_ILP/Additional/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/tata/"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}

bec50 <- raster("bec_50s.tif")
bec50df <- read.dbf("bec_50s.dbf")
bec50.out <- data.frame(VALUE = bec50[])
bec50.out <- join(bec50.out, bec50df, by = "VALUE")
bec50[] <- bec50.out$ZONE
writeRaster(bec50, filename="bec_50s.tif", format="GTiff", overwrite=TRUE)


bec80 <- raster("bec_80s.tif")
bec80df <- read.dbf("bec_80s.dbf")
bec80.out <- data.frame(VALUE = bec80[])
bec80.out <- join(bec80.out, bec80df, by = "VALUE")
bec80[] <- bec80.out$ZONE
writeRaster(bec80, filename="bec_80s.tif", format="GTiff", overwrite=TRUE)

bec_now <- raster("bec_now.tif")
bec_nowdf <- read.dbf("bec_now.dbf")
bec_now.out <- data.frame(VALUE = bec_now[])
bec_now.out <- join(bec_now.out, bec_nowdf, by = "VALUE")
bec_now[] <- bec_now.out$ZONE
writeRaster(bec_now, filename="bec_now.tif", format="GTiff", overwrite=TRUE)

inwd <- "D:/Work/UNBC/FisherData/FisherUpdated/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/fine_filter/"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
#do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}

####

inwd <- "D:/Work/UNBC/EcoregionZones/"
outwd <- "D:/Work/UNBC/Jerrica_Mann/GIS/EcoregionZones/"

setwd(inwd)
fl <- list.files(pattern = ".tif$")

setwd(outwd)
#do.call(file.remove, list(list.files()))

for(ii in 1:length(fl)){
  file.copy("D:/Work/UNBC/Jerrica_Mann/GIS/FLOAT.tif", fl[ii])
  system(paste("gdalwarp -r near",
               paste0(inwd,fl[ii]),
               paste0(outwd,fl[ii]),
               sep=" "))
  
}
