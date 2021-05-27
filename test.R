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

load("pre_global.RData")

input <- list()
input$cost <- "Area"
input$protected <- "locked_out" #"locked" #"avail" "locked_out"
input$MultiScen <- FALSE
input$blm <- 0
input$edge <- 0.5
input$portfolio <- 1 #number of runs

feat.temp <- feat.lst
feat.temp$Percent[1] <- 10

scnm <- paste0(substr(input$cost,1,1),substr(input$protected,1,1))#input$FTcutoff)
scen[1,] <- c(scnm, input$cost, input$protected, input$blm, input$edge, feat.temp$Percent)

sel.fr <- data.frame(pu$id) 

res.fr <- data.frame(scen=character(),
                     #time=character(),
                     cost=character(),
                     protected=character(),
                     BLM=vector(),
                     Edge=vector(),
                     area=numeric(),stringsAsFactors=F)

in_col <- ncol(res.fr)
for(kk in (in_col+1):(in_col+nlayers(features)))
  res.fr[,kk] <- numeric()	
names(res.fr)[(in_col+1):ncol(res.fr)] <- names(features)

in_col <- ncol(res.fr)
for(kk in (in_col+1):(in_col+nlayers(features)))
  res.fr[,kk] <- numeric()
names(res.fr)[(in_col+1):ncol(res.fr)] <- paste0(names(features),"_Tar")

ii <- 1

#pu_temp <- cost

pu.tmp$cost <- cost[[scen$cost[ii]]][][!is.na(bound.val)]

puvsfeat.temp <- features

#feat.temp <- sprintf("NPLCC_comm_feature_input_%s.csv",scale_temp)
feat.temp <- data.frame(id=seq(1,nlayers(features)),
                        Percent=as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)])),
                        name=names(features),
                        stringsAsFactors =F)

#debug
#write.csv(feat.temp,sprintf("./output/feat.temp_%s.csv",ii),row.names = F)
#prioritzr insert

#change target values according to user input
tmp.tar <- as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)]))/100
tmp.tar <- rep(0.2, length(tmp.tar))

if(scen$protected[ii] == "locked"){
  prob.ta <- problem(pu.tmp, feat, rij) %>%
    add_min_set_objective() %>%
    add_binary_decisions() %>% 
    add_relative_targets(tmp.tar) %>% 
    add_locked_in_constraints(pu.tmp$id[!is.na(prot.rast[][!is.na(bound.val)])])
  
} else if(scen$protected[ii] == "locked_out"){
  tmp.tar <- tmp.tar - feat_prot
  tmp.tar[tmp.tar < 0] <- 0
  prob.ta <- problem(pu.tmp, feat, rij) %>%
    add_min_set_objective() %>%
    add_binary_decisions() %>% 
    add_relative_targets(tmp.tar) %>%
    add_locked_out_constraints(pu.tmp$id[!is.na(prot.rast[][!is.na(bound.val)])])
  
} else {
  prob.ta <- problem(pu.tmp, feat, rij) %>%
    add_min_set_objective() %>%
    add_binary_decisions() %>% 
    add_relative_targets(tmp.tar) 
}

if(scen$BLM[ii] > 0){
  prob.ta <- prob.ta %>% add_boundary_penalties(as.numeric(scen$BLM[ii]), as.numeric(scen$Edge[ii]), boundary_data = bound.mat)
}

prob.ta <- prob.ta %>% add_cuts_portfolio(number_solutions = input$portfolio)
#
s.prob <- solve(prob.ta)


prob.ta <- problem(pu.tmp, feat, rij) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>% 
  add_relative_targets(tmp.tar) %>%
  add_boundary_penalties(input$blm, input$edge, boundary_data = bound.mat)

#
s.prob <- solve(prob.ta)

prob.ta2 <- prob.ta %>% add_cuts_portfolio(number_solutions = 10)
s.prob2 <- solve(prob.ta2)


#result <- fit.gurobi(pu=pu_temp, puvsfeat=features, feat=feat.temp)      

if (input$MultiScen == FALSE) {
  scnm <- paste0(substr(as.character(scen$cost[ii]),1,1),
                 substr(as.character(scen$protected[ii]),1,1))
  #                       as.numeric(scen$FTcutoff[ii]))
} else {
  scnm <- scen$scenario[ii]
}  

sel.fr <- cbind(sel.fr,rowSums(s.prob2[,grepl("solution_", names(s.prob2))], na.rm = TRUE)) ##**


names(sel.fr)[ncol(sel.fr)] <- scnm

#cst.tmp <- cost$dollar$cost
#if(scen$protected[ii] == "locked"){
#  cst.tmp[pu_temp$status ==2] <- 0
#}
#cst_doll <- round(sum(cst.tmp[result$x>0]),0)
tar <- vector()
for(jj in 1:nlayers(puvsfeat.temp)){
  
  val.tmp <- features.df[,jj]
  tar[jj] <- round(sum(val.tmp[s.prob$solution_1 == 1],na.rm=T) / features.df.sums[jj] * 100,2)
  
}

if(scen$protected[ii] == "locked_out"){
  tar <- round(tar + feat_prot * 100, 2)
}
## CALC HERE round(sum(cost$dollar$cost[result$x>0]),0)
res.fr[ii,] <- c(scnm, 
                 #scen$time[ii],
                 scen$cost[ii],
                 scen$protected[ii],
                 scen$BLM[ii],
                 scen$Edge[ii],
                 #                         scen$FTcutoff[ii],
                 #cst_doll,
                 round(sum(s.prob$solution_1)/length(s.prob$solution_1)*100,2),
                 tar,
                 feat.temp$Percent
)  

################################################
##create rasters
################################################

if (raster.on){
  r <- in.raster
  rv <- r[]
  ind.r.v <- data.frame(id=in.rast.val[!is.na(in.rast.val)])
  
  res <- cbind(ind.r.v,sel.fr[,-1])
  names(res)[-1] <- names(sel.fr)[-1]
  
  rout <- list()
  for(ii in 2:ncol(res)){
    
    rv[!is.na(rv)] <- res[,ii]
    
    r[] <- rv
    rout[[ii-1]] <- r
  }
  rst <- stack(rout)
  names(rst) <- names(res)[-1]
  

  
  
  tmpdir <- tempdir()
  setwd(tempdir())
  
  fs <- vector()
  for(ii in 1:nlayers(rst)){
    fs[ii] <- paste0(names(rst)[ii],".tif")
    writeRaster(rst[[ii]], filename = fs[ii], overwrite=TRUE)
  }
  
  zip(zipfile="solutions.zip", files=fs)
  
  
  
  
  prob.ta2 <- prob.ta %>% add_cuts_portfolio(number_solutions = 50)
  s.prob2 <- solve(prob.ta2)
  
  r <- in.raster
  rv <- r[]

  tt <- rowSums(s.prob2[ , grepl( "solution" , names( s.prob2 ) ) ])
  rv[!is.na(rv)] <- tt
    
  r[] <- rv
  
  rst <- stack(r)
  
  #pal <- colorFactor(c('#d7191c','#2c7bb6'),domain=factor(values(rst[[1]])),
   pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(rst[[1]]),
                     na.color = "transparent")
  
  outl <- leaflet() %>% addTiles() %>%
    # Base groups
    addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
  
  
  # Overlay groups
  for(ii in 1:length(rst@layers))
    outl <- addRasterImage(outl,rst[[ii]], colors=pal, opacity = 0.9, 
                           maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)
  
  outl <- addLegend(outl, pal = pal, values = values(rst[[1]]), title = "selected") %>%
    addLayersControl(
      baseGroups = c("StreetMap", "Aerial", "Terrain"),
      overlayGroups = names(rst),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  outl 
  
  
  
feat1 <- features.df
feat2 <- features.df

feat1.sums <- colSums(feat1)

for(ii in 2:ncol(feat2)){
  feat2[,ii][feat2$Protected_Areas == 1] <- 0
}

feat2.sums <- colSums(feat2)

tt <- data.frame(All_cells = feat1.sums, Prot_rem = feat2.sums)
tt$delta_perc <- tt$Prot_rem/tt$All_cells*100

write.csv(tt, "Quick_gap.csv")
