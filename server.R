##################################################################
##################################################################
## Shiny App for CDFCP priotirization ILP
##
## Author: Richard Schuster (mail@richard-schuster.com)
## 12 Novemeber 2016
##
## v0.20
##    - based on NPLCC.v0.13
##    - included FISH and HERP
## v0.22
##    - Output map working now
##################################################################
##################################################################


# Define server logic 
shinyServer(function(input, output, session) {
  
  #  setwd("/var/shiny-server/www/examples/calib.shiny.v2/")
  values = reactiveValues(
    hot_feat = feat.lst,
    #hot_tree = tree.lst,
    hot_multi = scen
  )
  #setHot = function(x) values[["hot"]] <<- x
  
  calc = reactive({
    # load initial values
    df1 = values[["hot_feat"]]
    df3 = values[["hot_multi"]]
    
    
    list(feat = df1,
         multi = df3)
  })
  
  #######################
  ## Edit Targets
  #######################
  output$hot_feat = renderRHandsontable({
    if (!is.null(input$hot_feat)) {
      DF = hot_to_r(input$hot_feat)
      values[["hot_feat"]] = DF      
    } else if (!is.null(values[["hot_feat"]])) {
      DF = values[["hot_feat"]]
    }
    
    #prevent rhandson from adding rows when user drags values
    if(nrow(DF) > l.feat.lst){
      DF <- DF[1:l.feat.lst,]
      values[["hot_feat"]] = DF      
    }
    
    #setHot(DF)
    rhandsontable(DF, readOnly = T) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(c("Percent"), readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (col == 1 && (value > 100 || value < 0)) {
               td.style.background = 'red';
               }
               }")
  })
  
  
  #######################
  ## Multiple Scenarios
  #######################
  output$hot_multi = renderRHandsontable({
    if (!is.null(input$scen_file)){
      DF = read.csv(input$scen_file$datapath,stringsAsFactors =F)
      values[["hot_multi"]] = DF  
    } else if (!is.null(input$hot_multi)) {
      DF = hot_to_r(input$hot_multi)
      values[["hot_multi"]] = DF  
    } else if (!is.null(values[["hot_multi"]])) {
      DF = values[["hot_multi"]]
    }
    
    rhandsontable(DF, readOnly = F) %>%
      #      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_table() %>%
      #      hot_col(col = "time", type = "dropdown", source = c("curr")) %>%
      hot_col(col = "cost", type = "dropdown", source = c("Hard.SoftFootprint", "HardFootprint", "Area")) %>%
      hot_col(col = "protected", type = "dropdown", source = c("locked","avail")) %>%
      #hot_validate_numeric(col = "maxRoadDns",  min = 0, max = 20) %>%
      #hot_validate_numeric(col = "minPropSz",  min = 0, max = 10) %>%
      #hot_validate_numeric(col = "maxAgrDns",  min = 0, max = 1) %>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (col > 5 && (value > 100 || value < 0)) {
               td.style.background = 'red';
               }
               }")
      #hot_col(c("include"), readOnly = FALSE) 
  })
  
  #######################
  ## Gurobi reactive
  #######################
  my.data <- reactive({ 
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.    
    if(input$mrun == 0)
      return(NULL)    
    
    return(isolate({
      
      if (input$MultiScen == FALSE) {
        feat.temp <- calc()$feat
        scnm <- paste0(substr(input$cost,1,1),substr(input$protected,1,1))#input$FTcutoff)
        scen[1,] <- c(scnm, input$cost, input$protected, input$blm, input$edge, feat.temp$Percent)
        #        scen[1,] <- c(scnm,input$time,input$cost,input$protected,input$FTcutoff,feat.temp$Percent)
      } else {
        scen <- calc()$multi
      }
      
      #debug
      #write.csv(scen,"./output/scenarios.csv",row.names = F)
      
      progress <- Progress$new(session)
      progress$set(message = 'Setting up Analysis inputs', detail = "Please be patient...", value = 0.01)
      
      #setup output frames
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
      
      res_rast <- list()
      #scenario loop
      for (ii in 1:nrow(scen)){
        
        pu_temp <- cost
        
        pu.tmp$cost <- cost[[scen$cost[ii]]][][!is.na(bound.val)]
        
        prob_temp <- problem_list[[scen$cost[ii]]]
        # prob_temp <- problem(pu.tmp, feat, rij, cost_column = "cost")
        
        #create Marxan type problem entire extent
        
        #if(scen$protected[ii] == "locked"){
        #  pu_temp$status <- prot$Prot
        #  pu_temp$cost[pu_temp$status ==2] <- 0 
        #} else {
        #  pu_temp$status <- 0
        #}
        
        puvsfeat.temp <- features
        
        #feat.temp <- sprintf("NPLCC_comm_feature_input_%s.csv",scale_temp)
        feat.temp <- data.frame(id=seq(1,nlayers(features)),
                                Percent=as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)])),
                                name=names(features),
                                stringsAsFactors =F)
        
        #debug
        #write.csv(feat.temp,sprintf("./output/feat.temp_%s.csv",ii),row.names = F)
        
        progress$set(message = 'Calculation in progress', detail = sprintf("Scenario %s/%s",ii,nrow(scen)), 
                     value = round(ii/nrow(scen)*0.8,1))
        
        
        #prioritzr insert
        
        #change target values according to user input
        tmp.tar <- as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)]))/100
        #prob.ta <- prob %>% add_relative_targets(tmp.tar)
        prob.ta <- prob_temp %>%
          add_min_set_objective() %>%
          add_binary_decisions() %>% 
          add_relative_targets(tmp.tar)
          
          
        
        if(scen$protected[ii] == "locked"){
          prob.ta <- prob.ta %>% 
            add_locked_in_constraints(prot.rast)
        }
          
        # } else if(scen$protected[ii] == "locked_out"){
        #   tmp.tar <- tmp.tar - feat_prot
        #   tmp.tar[tmp.tar < 0] <- 0
        # 
        #   prob.ta <- problem(pu.tmp, feat, rij, cost_column = "cost") %>%
        #     add_min_set_objective() %>%
        #     add_binary_decisions() %>% 
        #     add_relative_targets(tmp.tar) %>%
        #     add_locked_out_constraints(pu.tmp$id[!is.na(prot.rast[][!is.na(bound.val)])])
          
        # } else {
        #   prob.ta <- problem(pu.tmp, feat, rij, cost_column = "cost") %>%
        #     add_min_set_objective() %>%
        #     add_binary_decisions() %>% 
        #     add_relative_targets(tmp.tar) 
        # }
        # 
        if(scen$BLM[ii] > 0){
          prob.ta <- prob.ta %>% 
            add_boundary_penalties(as.numeric(scen$BLM[ii]), as.numeric(scen$Edge[ii]))
        }
        
        # prob.ta <- prob.ta %>% add_cuts_portfolio(number_solutions = input$portfolio)
        #
        s.prob <- solve(prob.ta)
        
        #result <- fit.gurobi(pu=pu_temp, puvsfeat=features, feat=feat.temp)      
        
        if (input$MultiScen == FALSE) {
          scnm <- paste0(substr(as.character(scen$cost[ii]),1,1),
                         substr(as.character(scen$protected[ii]),1,1))
          #                       as.numeric(scen$FTcutoff[ii]))
        } else {
          scnm <- scen$scenario[ii]
        }  
        
        # if(input$portfolio > 1){
        #   sel.fr <- cbind(sel.fr,rowSums(s.prob[,grepl("solution_", names(s.prob))], na.rm = TRUE)) ##**
        # } else {
        #   sel.fr <- cbind(sel.fr,s.prob[,grepl("solution_", names(s.prob))]) ##**
        # }
        s.prob.val <- s.prob
        # sel.fr <- cbind(sel.fr, s.prob.val[!is.na(s.prob.val)])
        # 
        # names(sel.fr)[ncol(sel.fr)] <- scnm
        
        #cst.tmp <- cost$dollar$cost
        #if(scen$protected[ii] == "locked"){
        #  cst.tmp[pu_temp$status ==2] <- 0
        #}
        #cst_doll <- round(sum(cst.tmp[result$x>0]),0)
        # tar <- vector()
        # for(jj in 1:nlayers(puvsfeat.temp)){
        #   
        #   val.tmp <- features.df[,jj]
        #   tar[jj] <- round(sum(val.tmp[s.prob$solution_1 == 1],na.rm=T) / features.df.sums[jj] * 100,2)
        #   
        # }
        tar <- round(feature_representation(prob.ta, s.prob)$relative_held * 100, 2)
        
        area <- sum(s.prob[], na.rm = TRUE)
        
        # if(scen$protected[ii] == "locked_out"){
        #   tar <- round(tar + feat_prot * 100, 2)
        #   area <- round(area + sum(features.df$Protected_Areas)/ nrow(features.df)* 100, 2)
        # }
        ## CALC HERE round(sum(cost$dollar$cost[result$x>0]),0)
        res.fr[ii,] <- c(scnm, 
                         #scen$time[ii],
                         scen$cost[ii],
                         scen$protected[ii],
                         scen$BLM[ii],
                         scen$Edge[ii],
                         #                         scen$FTcutoff[ii],
                         #cst_doll,
                         area,
                         tar,
                         feat.temp$Percent
        ) 
        #rm(cst.tmp,cst_doll)
        
        res_rast[[ii]] <- s.prob
        names(res_rast)[ii] <- scnm
      }
      
      ################################################
      ##create rasters
      ################################################
      progress$set(message = 'Post processing', detail = "This will take a few mins...", value = 0.9)
      
      if (raster.on){
        # r <- in.raster
        # rv <- r[]
        # ind.r.v <- data.frame(id=in.rast.val[!is.na(in.rast.val)])
        # 
        # res <- cbind(ind.r.v,sel.fr[,-1])
        # names(res)[-1] <- names(sel.fr)[-1]
        # 
        # rout <- list()
        # for(ii in 2:ncol(res)){
        #   
        #   rv[!is.na(rv)] <- res[,ii]
        #   
        #   r[] <- rv
        #   rout[[ii-1]] <- r
        # }
        rst <- stack(res_rast)
        # names(rst) <- names(res)[-1]
        
        rlist <- list(sel.fr=sel.fr,res.fr=res.fr,rst=rst,rstL=NULL)
        
      } else {
        rlist <- list(sel.fr=sel.fr,res.fr=res.fr,rst=NULL,rstL=NULL)
      }
      progress$set(value = 1)
      progress$close() 
      
      return(rlist)
    }))
    
  })
  
  observe ({  my.data()
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    "Marxan results"
  })
  
  output$cadMap <- renderLeaflet({
    if(input$mrun == 0) {
      #print("Run Marxan")
      return(NULL)
    }
    
    
    #[[1]] for now, should allow for multiple eventually
    rst <- my.data()$rst
    
    #pal <- colorFactor(c('#d7191c','#2c7bb6'),domain=factor(values(rst[[1]])),
    #                   #pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
    #                   na.color = "transparent")
    pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(rst[[1]]),
                        na.color = "transparent")
    
    outl <- leaflet() %>% addTiles() %>%
      # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
    
    
    # Overlay groups
    for(ii in 1:length(rst@layers))
      outl <- addRasterImage(outl,rst[[ii]], colors=pal, opacity = 0.7, 
                             maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)
    
    outl <- addLegend(outl, pal = pal, values = values(rst[[1]]), title = "selected") %>%
      addLayersControl(
        baseGroups = c("StreetMap", "Aerial", "Terrain"),
        overlayGroups = names(rst),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    outl 
    
    # Overlay groups
    #prog$set(value = 1)
    
    
    # end individual run attribute table
    ########################################
    #prog$close()     
  })  
  
  output$InMap <- renderLeaflet({
    feat.in
  })  
  
  output$TreeMap <- renderLeaflet({
    tree_curr
  })  
  
  
  output$TreeMapTool <- renderLeaflet({
    if(input$tree.update == 0) {
      tin_curr
    }
    
  })  
  
  ###############################
  # Summary Table + Download Results raster
  ###############################
  output$summary <- renderTable({ # to display in the "Summary" tab
    if(input$mrun == 0) {
      return(data.frame(Output="You need to run the prioritization first"))
    }
    
    my.data()$res.fr
    #data.frame(t(my.data()$res.fr))
    
  })
  
  output$downloadRAST <- downloadHandler(
    
    filename = function() {
      paste('solutions.zip', sep='')
    },
    content = function(fname) {
      rst <- my.data()$rst
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- vector()
      for(ii in 1:nlayers(rst)){
        fs[ii] <- paste0(names(rst)[ii],".tif")
        writeRaster(rst[[ii]], filename = fs[ii], overwrite=TRUE)
      }
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
  output$download_ssoln <- downloadHandler(
    
    filename = function() {
      paste('CDFCP_summary_results-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$res.fr, file, row.names=F)
    }
  )
  
  output$download_selfr <- downloadHandler(
    
    filename = function() {
      paste('CDFCP_property_selection-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$sel.fr, file, row.names=F)
    }
  )
  
  #Tabsets
  output$tabsets <- renderUI({
    tabs <- list(NULL)
    
    if(input$MultiScen == TRUE){
      tabs[[1]] <- tabPanel("Scenario List", rHandsontableOutput("hot_multi",width="100%",height="500px"))
      ii <- 1
    } else {
      tabs[[1]] <- tabPanel("Edit Target", rHandsontableOutput("hot_feat"))
      #tabs[[2]] <- tabPanel("Edit Trees", rHandsontableOutput("hot_tree"))
      ii <- 1
    }
    tabs[[ii+1]] <- tabPanel("Input Layers",leafletOutput("InMap",height=900))
    #tabs[[ii+2]] <- tabPanel("Tree Layers",leafletOutput("TreeMap",height=1000))
    #tabs[[ii+3]] <- tabPanel("Tree Community",leafletOutput("TreeMapTool",height=1000))
    tabs[[ii+2]] <- tabPanel("Results + Download",
                             helpText(HTML("<h4>Result Summary Table</h4>")),
                             tableOutput("summary"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Results download (PU selection):</h4>")),
                             downloadButton("download_selfr", label = "PU selection"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Results download (summary of outputs):</h4>")),
                             downloadButton("download_ssoln",label = "Results download"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Download the solution rasters:</h4>")),
                             downloadButton("downloadRAST",label = "Results download")
    )
    tabs[[ii+3]] <- tabPanel("Result Map",leafletOutput("cadMap",height=900))
    
    tabs$id <- "tab0"
    do.call(tabsetPanel, tabs)
  })  
  
  
  })


