
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#mobile.no@fssai



library(shiny)
  if(!require(Hmisc)){
    install.packages("Hmisc")
    library(Hmisc)
  }
  if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
  }
  if(!require(MASS)){
    install.packages("MASS")
    library(MASS)
  }
  
  library(dplyr)
  library(ggplot2)
  library(openxlsx)
  if(!require(scales)){
    install.packages("scales")
    library(scales)
  }
  library(stringr)



shinyServer(function(input, output) {
  load(file.path("data","13_L04_Short3.RData"))
  
  # load(file.path("data","13_L05_Data2.RData"))  
  # load(file.path("data","17_L05_Data3.RData"))  
  load(file.path("data","18_L05_Data3.RData"))  
  
  # load(file.path("data","15_HH_Summary_All.RData"))
  load(file.path("data","17_HH_Summary_All_Heme.RData"))
  
  load(file.path("data","15_L03_Data2 with StateDistMap.RData"))
  
  load(file.path("data","district_map.RData"))
  
  load(file.path("data","district_shp_df2.RData"))
  
  load(file.path("data","states_shp.RData"))
  
  load(file.path("data","fg_dbMapping.RData"))
  
  load(file.path("data","16_RDA TUL.RData"))
  
  load(file.path("data","desc.RData"))
  
  load(file.path("data","agristats_summary08to12_map.RData"))
  
  load(file.path("data","nfhs4_complete3.RData"))
  
  load(file.path("data","nfhs4_outcomelist.RData"))
  
  options(scipen = 999)
  district_map1 <- district_map[!duplicated(district_map$NSS_DID)& district_map$MM_in_NSS!="No"&!is.na(district_map$NSS_DID),c("MM_Unique","ST_CEN_CD","DISTRICT","NSS_DID","censuscode")]
  district_map1 <- district_map1[!is.na(district_map1$NSS_DID),]
  
  state_map1 <- district_map[!duplicated(district_map$ST_CEN_CD)&!is.na(district_map$ST_CEN_CD),c("ST_CEN_CD","NSS_State")]
  
  #Merging it upstream so that it is not caught between Reactives
  district_shp_df2 <- merge(district_shp_df2,state_map1,by="ST_CEN_CD")
  hh_summary_all <- hh_summary_all[hh_summary_all$Moisture.WATER!=0,]
  hh_summary_all.orig <- hh_summary_all
  
  observeEvent(input$goButton1,{
    fooditem1 = "rice - PDS"
    fooditem1 = input$fooditem1
    # print(fooditem1)
    # food1 = l05_data2[l05_data2$name==fooditem1,c("hhuid","finalweight","cq100","state_L05","state_dist_L05")]
    food1 = l05_data3[l05_data3$name==fooditem1,c("hhuid","finalweight","cq100","state_L05","state_dist_L05")]
        
    food1 <- merge(food1,district_map1,by.x="state_dist_L05",by.y="NSS_DID") 
    #Includes district name and censuscode
    food1 <- merge(food1,l03_data2[,c("hhuid","mpce_mrp","st_quintile","NSS_State")],by="hhuid")
    food1 <- merge(food1,l04_short3[,c("hhuid","consumerunits","hh_size_L04")],by="hhuid")
    food1$percon_qty <- with(food1,cq100/consumerunits)
    # food1 <- food1[food1$st_quintile %in% c(1,2,3),]
    food1 <- food1[food1$st_quintile %in% input$quintiles1,]
    
    #Summarising datasets for entire country by state, district
    summarydf_dist <- food1 %>% group_by(state_L05,NSS_State,state_dist_L05,DISTRICT,censuscode) %>% summarise(mean=wtd.mean(percon_qty*100,w=consumerunits*finalweight,na.rm=TRUE),sd_qty = sqrt(wtd.var(percon_qty*100,w=consumerunits*finalweight,na.rm=TRUE)),median=wtd.quantile(percon_qty*100,w=consumerunits*finalweight,probs=0.5,na.rm=TRUE))
    summarydf_state <- food1 %>% group_by(NSS_State,ST_CEN_CD) %>% summarise(mean=wtd.mean(percon_qty*100,w=consumerunits*finalweight,na.rm=TRUE),sd_qty = sqrt(wtd.var(percon_qty*100,w=consumerunits*finalweight,na.rm=TRUE)),median=wtd.quantile(percon_qty*100,w=consumerunits*finalweight,probs=0.5,na.rm=TRUE))
    
    # #Reactive dataset for selection of State
    map_df_dist <- reactive({
      if(input$state1=="India"){
       dataset <- merge(district_shp_df2,summarydf_dist[,c("median","censuscode","state_L05","state_dist_L05","DISTRICT")],by.x="id",by.y="censuscode",all.x=TRUE)
       dataset[order(dataset$order),]       
       }
      else{
       dataset <- merge(district_shp_df2[district_shp_df2$NSS_State==input$state1,],summarydf_dist[summarydf_dist$NSS_State==input$state1,c("median","censuscode","state_L05","state_dist_L05","DISTRICT")],by.x="id",by.y="censuscode",all.x=TRUE)
       dataset[order(dataset$order),]   
      }
    })
    # print("About to plot Map")
    # map_df_dist <- dataset[order(dataset$order),]
    #Plotting map
    output$mapPlot1<- renderPlot({
      mp1 <- ggplot() + geom_polygon(data=map_df_dist(),aes(x=long,y=lat,group=group,fill=median))
      # mp1 <- ggplot() + geom_polygon(data=map_df_dist,aes(x=long,y=lat,group=group,fill=median))
      mp1 <- mp1 + coord_map() + xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(fooditem1," Monthly Intake in grams per consumer unit- ",input$state1," by District")) + theme_grey()
      mp1 <- mp1 + scale_fill_distiller(name="Monthly Intake in grams", palette = "YlGnBu",direction=1)
      # print("Plotting Map")
      print(mp1)
      # mp1
    },height=600)
    
    if(input$state1=="India"){
      food2 <- food1
      
    }
    else{
      food2 <- food1[food1$NSS_State==input$state1,]

    }
    # print("About to plot Hist")
    #Plotting histogram
    output$distPlot1 <- renderPlot({
      dp1 <- ggplot(data=food2,aes(percon_qty*100,weight=consumerunits*finalweight)) + geom_histogram() +scale_y_continuous(labels = comma)
      dp1 <- dp1 + xlab("Food Intake") + ylab("Count") + theme(text = element_text(size=12)) + ggtitle(paste0("Distribution of Monthly Intake in grams per consumer unit in ",input$state1))
      # print("Plotting Hist")
      return(dp1)
      
    },height=600)
    
    summarytable <- reactive({
      if(input$state1=="India"){
        temp <- summarydf_state[,c("NSS_State","mean","sd_qty","median")]
        colnames(temp) <- c("State","Mean","SD","Median")
        temp
      }
      else{
        temp <- summarydf_dist[summarydf_dist$NSS_State==input$state1,c("NSS_State","DISTRICT","mean","sd_qty","median")]
        colnames(temp) <- c("State","District","Mean","SD","Median") 
        temp
      }
      
    })
    
    output$summary1 <- renderTable({
      print(summarytable())
    })
    
  })

  observeEvent(input$goButton2,{
    
    # "Total Saturated Fatty Acids\n(TSFA)", "Total Ascorbic Acid","Phytate",'Calcium(Ca)' ,'Magnesium(Mg)''Zinc(Zn)' "Protein" "Energy in KiloCal" "Total Fat"
    # "Vitamin B-12 "," Total Folates (B9)",'Iron(Fe)',"Total Polyphenols", "Vitamin A, RAE "
    # "Protein" "Energy in KiloCal" "Total Fat","Carbo-hydrate" "Vitamin A, RAE "
    nutrient2 = "Vitamin B-12 " # #Link with input
    nutrient2 = input$nutrient2
    print(nutrient2)
    var_nutrient <- fg_dbMapping[fg_dbMapping$nin.nutrient==nutrient2,"nin.var_nutrient"]
    # var_nutrient = "NonHeme.IronFe.FE", "Retinol.RETOL"
    unit_nutrient <- fg_dbMapping[fg_dbMapping$nin.nutrient==nutrient2,"nin.unit_nutrient"]
    # unit_nutrient = "mg"
    multiplier=1
    if(unit_nutrient=="µg"){
      multiplier=1000000
    }
    if(unit_nutrient=="mg"){
      multiplier=1000
    }
    # hh_summary_all <- hh_summary_all %>% group_by(hhuid) %>% mutate(totalfatprovided=sum(TotalPolyUnsaturatedFattyAcids.FAPU,TotalMonoUnsaturatedFattyAcids.FAMS,TotalSaturatedFattyAcids.FASAT,na.rm=TRUE))
    # hh_summary_all <- as.data.frame(hh_summary_all)
    bioavailability = 1
    quintiles = c(1,2,3)
    quintiles = as.numeric(input$quintiles2)
    
    hh_summary_all <- hh_summary_all.orig
    hh_summary_all <- hh_summary_all[hh_summary_all$st_quintile %in% quintiles,]
    # hh_summary_all <- hh_summary_all[hh_summary_all$w.st_quintile %in% quintiles,]
    dataset.nutrient2 <- reactive({
        if(is.null(input$type2)|input$type2==""){
          type2 = "Adult Women"
        }
        else{
          type2 = input$type2
        }

        var_no <- desc[desc$desc==type2,"variable2"]
        var_cu <- desc[desc$desc==type2,"cu"]
        
        # var_no <- "hh_size_L04"
        # var_cu <- "consumerunits"
        # # For consumer units
        
        
        hh_summary_all$nutrient.no <- hh_summary_all[,var_no]
        hh_summary_all$nutrient.cu <- hh_summary_all[,var_cu]
        hh_summary_all$nutrient.pd <- (hh_summary_all[,var_nutrient]*hh_summary_all$nutrient.cu)/(30*hh_summary_all$consumerunits*hh_summary_all$nutrient.no)
        # q99.9 <- quantile(hh_summary_all$nutrient.pd,probs=0.999,na.rm=TRUE)
        q99.9 <- with(hh_summary_all,wtd.quantile(nutrient.pd,w=finalweight*nutrient.no,probs=0.999,na.rm=TRUE))
        q00.1 <- with(hh_summary_all,wtd.quantile(nutrient.pd,w=finalweight*nutrient.no,probs=0.001,na.rm=TRUE))
        # hh_summary_all <- hh_summary_all[hh_summary_all$nutrient.pd<q99.9 & !is.na(hh_summary_all$nutrient.pd),]
        hh_summary_all2 <- hh_summary_all[hh_summary_all$nutrient.pd<q99.9&hh_summary_all$nutrient.pd>q00.1,]
        hh_summary_all2
    })
    
    
    # hh_summary_all.nutrient <- hh_summary_all2
    # 
    
    summarydf.r <- reactive({
      hh_summary_all.nutrient <- dataset.nutrient2()
      hh_summary_all.nutrient[!is.na(hh_summary_all.nutrient$nutrient.pd),] %>% summarise(mean_intake_hmisc_n=wtd.mean(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE))
    })
    
    # summarydf.r <- hh_summary_all.nutrient[!is.na(hh_summary_all.nutrient$nutrient.pd),] %>% summarise(mean_intake_hmisc_n=wtd.mean(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE))

    summarydf_state.r <- reactive({ 
      hh_summary_all.nutrient <- dataset.nutrient2()
      # q99.9 <- quantile(hh_summary_all.nutrient$nutrient.pd,probs=0.999,na.rm=TRUE)
      # hh_summary_all.nutrient[!is.na(hh_summary_all.nutrient$nutrient.pd)&hh_summary_all.nutrient$nutrient.pd<q99.9,]
      # View(hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9,])
      # summarydf_state.r <-
        hh_summary_all.nutrient[!is.na(hh_summary_all.nutrient$nutrient.pd),] %>% group_by(NSS_State,RDS_State) %>% summarise(mean_intake_hmisc_n=wtd.mean(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                                           sd_intake_hmisc_n=sqrt(wtd.var(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                                                                           q25_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.25),
                                                                                                                           median_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.5),
                                                                                                                           q75_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.75),
                                                                                                                           q90_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.9),
                                                                                                                           q99_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.99),
                                                                                                                           q99.9_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.999),
                                                                                                                           max_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=1),
                                                                                                                           no_households = n(),no_individuals=sum(nutrient.no,na.rm=TRUE))
    })
    summarydf_dist.r <- reactive({
      hh_summary_all.nutrient <- dataset.nutrient2()
      # q99.9 <- quantile(hh_summary_all.nutrient$nutrient.pd,probs=0.999,na.rm=TRUE)
      # summarydf_dist.r <-
      hh_summary_all.nutrient[!is.na(hh_summary_all.nutrient$nutrient.pd),] %>% group_by(NSS_State,RDS_State,DISTRICT,RDS_District,censuscode) %>% summarise(mean_intake_hmisc_n=wtd.mean(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                                                                           sd_intake_hmisc_n=sqrt(wtd.var(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                                                                                                           q25_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.25),
                                                                                                                                                           median_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.5),
                                                                                                                                                           q75_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.75),
                                                                                                                                                           q90_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.9),
                                                                                                                                                           q99_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.99),
                                                                                                                                                           q99.9_hmisc_n = wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=0.999),
                                                                                                                                                           max_hmisc_n=wtd.quantile(nutrient.pd*multiplier,w=nutrient.no*finalweight,na.rm=TRUE,probs=1),
                                                                                                                                                           no_households = n(),no_individuals=sum(nutrient.no,na.rm=TRUE))
  
    })
    if(is.null(input$type2)|input$type2==""){
      type2 = "Adult Women"
    }
    else{
      type2 = input$type2
    }
    
    

       
    output$mapPlot2<- renderPlot({  
      
      write.csv(summarydf.r(),file=paste0("National Summary-",Sys.Date(),"_",nutrient2,"_",type2,".csv"))
      write.csv(summarydf_state.r(),file=paste0("State Summary-",Sys.Date(),"_",nutrient2,"_",type2,".csv"))
      write.csv(summarydf_dist.r(),file=paste0("District Summary-",Sys.Date(),"_",nutrient2,"_",type2,".csv"))

      state2="India"
      state2 = input$state2
      
      if(is.null(input$type2)|input$type2==""){
        type2 = "Adult Women"
      }
      else{
        type2 = input$type2
      }
      summarydf_dist <- summarydf_dist.r()
      summarydf_state <- summarydf_state.r()
      if(state2=="India"){

        dataset.map2 <- merge(district_shp_df2,summarydf_dist[,c("median_hmisc_n","censuscode")],by.x="id",by.y="censuscode",all.x=TRUE)
        dataset.map2 <- dataset.map2[order(dataset.map2$order),]
        states_shp2 <- states_shp
      }
      else{
        dataset.map2 <- merge(district_shp_df2[district_shp_df2$NSS_State==state2,],summarydf_dist[summarydf_dist$NSS_State==state2,c("median_hmisc_n","censuscode")],by.x="id",by.y="censuscode",all.x=TRUE)
        dataset.map2 <- dataset.map2[order(dataset.map2$order),]
        states_shp2 <- states_shp[states_shp$NSS_State==state2,]
      }
      
      legend = paste0("Intake in ",unit_nutrient)

      title2 = paste0(type2," ",nutrient2," Intake- ",state2," by District")

      mp2 <-  ggplot() + geom_polygon(data=states_shp2,aes(x=long,y=lat,group=group),color="black",fill="white",size=0.2)
      mp2 <- mp2 + geom_polygon(data=dataset.map2,aes(x=long,y=lat,group=group,fill=median_hmisc_n),alpha=0.8)
      mp2 <- mp2 + coord_map() + xlab("Longitude") + ylab("Latitude") + ggtitle(title2) + theme_grey()
      mp2 <- mp2 + scale_fill_distiller(name=legend, palette = "YlGnBu",direction=1)
      print(mp2)
    
    
  },height=600)
    
  output$summary2 <- renderTable({
    summarydf_dist <- summarydf_dist.r()
    summarydf_state <- summarydf_state.r()
    if(is.null(input$type2)|input$type2==""){
      type2 = "Adult Women"
    }
    else{
      type2 = input$type2
    }
    
    state2 = "Gujarat"
    state2 = input$state2
    
    if(state2=="India"){
      temp <- summarydf_state[,c("NSS_State","mean_intake_hmisc_n","sd_intake_hmisc_n","q25_hmisc_n","median_hmisc_n","q75_hmisc_n")]
      colnames(temp) <- c("State","Mean","SD","Quartile 25","Median","Quartile 75")
      temp
    }
    else{
      temp <- summarydf_dist[summarydf_dist$NSS_State==state2,c("NSS_State","DISTRICT","mean_intake_hmisc_n","sd_intake_hmisc_n","q25_hmisc_n","median_hmisc_n","q75_hmisc_n")]
      colnames(temp) <- c("State","District","Mean","SD","Quartile 25","Median","Quartile 75")
      temp
    }
    
    print(temp)
      
      
  })   
      
  output$distPlot2 <- renderPlot({
    hh_summary_all.nutrient <- dataset.nutrient2()
    if(is.null(input$type2)|input$type2==""){
      type2 = "Adult Women"
    }
    else{
      type2 = input$type2
    }
    state2 = "Gujarat"
    state2 = input$state2
    # q99.9 <- quantile(hh_summary_all.nutrient$nutrient.pd,probs=0.999,na.rm=TRUE)
    
    if(state2=="India"){
      dataset <- hh_summary_all.nutrient
      # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9,]
    }
    else{
      dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$NSS_State==state2,]
      # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9 & hh_summary_all.nutrient$NSS_State==state2,]
    }
    
    
    dp2 <- ggplot(data=dataset,aes(nutrient.pd*multiplier,weight=nutrient.no*finalweight)) + geom_histogram() +scale_y_continuous(labels = comma)
    dp2 <- dp2 + xlab(paste0("Nutrient Intake in ",unit_nutrient)) + ylab("Count (excl. top 0.1%ile)") + theme(text = element_text(size=12)) + ggtitle(paste0("Distribution of Monthly Intake in ",unit_nutrient," in ",state2))
    print(dp2)
    
  },height=600)

  })
  
  
  observeEvent(input$goButton3,{
    # 'Calcium(Ca)' " Total Folates (B9)"
    nutrient3 = 'Calcium(Ca)' # #Link with input
    nutrient3 = input$nutrient3
    print(nutrient3)
    var_nutrient <- fg_dbMapping[fg_dbMapping$nin.nutrient==nutrient3,"nin.var_nutrient"]
    # var_nutrient = "NonHeme.IronFe.FE"
    unit_nutrient <- fg_dbMapping[fg_dbMapping$nin.nutrient==nutrient3,"nin.unit_nutrient"]
    # unit_nutrient = "mg"
    multiplier=1
    if(unit_nutrient=="µg"){
      multiplier=1000000
    }
    if(unit_nutrient=="mg"){
      multiplier=1000
    }
    bioavailability = 1

    
    #Nutrient intake from food = consumed quantity (in 100g units) * composition
    # fortificant = 0 #From input (same unit as nutrient per 100g)
    # fooditem3 = "All Rice" #Link with input
    fooditem3 = input$fooditem3
    # fortificant = 50
    fortificant = input$fortificant3
    # f.unit = 2
    f.unit = input$unit3
    if(f.unit==1){
      fortificant = fortificant/1000000
    }
    if(f.unit==2){
      fortificant = fortificant/1000
    }
    
    # fooditem3 = "All Rice"
    food = l05_data3[l05_data3$name==fooditem3,c("hhuid","finalweight","cq100")]
    food <- merge(food,l03_data2[,c("hhuid","state_L03","state_dist_L03","NSS_State","mpce_mrp","st_quintile","DISTRICT","RDS_District","RDS_State")],by="hhuid")
    quintiles3 <- c(1,2,3)
    quintiles3 <- as.numeric(input$quintiles3)
    food <- food[food$st_quintile %in% quintiles3 ,]
    coverage=100 #Percentage
    coverage = input$coverage3
    food <- food %>% dplyr::group_by(state_L03) %>% dplyr::sample_frac(size=coverage/100,weight=finalweight)
    food$totalnutrient <- fortificant*food$cq100
    food$state_L03 <- as.numeric(food$state_L03)
    print(input$scenario3_2)
    if(input$scenario3b==TRUE){
      
      # fooditem3_2 = "All Rice Products"
      fooditem3_2 = input$fooditem3_2
      # fortificant_2 = 10
      fortificant_2 = input$fortificant3_2
      # f.unit_2=2
      f.unit_2 = input$unit3_2
      if(f.unit_2==1){
        fortificant_2 = fortificant_2/1000000
      }
      if(f.unit_2==2){
        fortificant_2 = fortificant_2/1000
      }
      
      food_2 = l05_data3[l05_data3$name==fooditem3_2,c("hhuid","finalweight","cq100")]
      food_2 <- merge(food_2,l03_data2[,c("hhuid","state_L03","state_dist_L03","NSS_State","mpce_mrp","st_quintile","DISTRICT","RDS_District","RDS_State")],by="hhuid")
      quintiles3_2 <- c(1,2,3)
      quintiles3_2 <- as.numeric(input$quintiles3_2)
      
      food_2 <- food_2[food_2$st_quintile %in% quintiles3_2,]
      # coverage_2=100 #Percentage
      coverage_2 = input$coverage3_2
      food_2 <- food_2 %>% dplyr::group_by(state_L03) %>% dplyr::sample_frac(size=coverage_2/100,weight=finalweight)
      # food_2$totalnutrient_2 <- fortificant_2*food_2$cq100
      food_2$totalnutrient <- fortificant_2*food_2$cq100
      food_2$state_L03 <- as.numeric(food_2$state_L03)
    }
    
    if(input$scenario3c==TRUE){
      
      fooditem3_3 = input$fooditem3_3
      fortificant_3 = input$fortificant3_3
      f.unit_3=3
      f.unit_3 = input$unit3_3
      if(f.unit_3==1){
        fortificant_3 = fortificant_3/1000000
      }
      if(f.unit_3==2){
        fortificant_3 = fortificant_3/1000
      }
      
      
      food_3 = l05_data3[l05_data3$name==fooditem3_3,c("hhuid","finalweight","cq100")]
      food_3 <- merge(food_3,l03_data2[,c("hhuid","state_L03","state_dist_L03","NSS_State","mpce_mrp","st_quintile","DISTRICT","RDS_District","RDS_State")],by="hhuid")
      quintiles3_3 <- c(1,2,3)
      quintiles3_3 <- as.numeric(input$quintiles3_3)
      
      food_3 <- food_3[food_3$st_quintile %in% quintiles3_3,]
      coverage_3=50 #Percentage
      coverage_3 = input$coverage3_3
      food_3 <- food_3 %>% dplyr::group_by(state_L03) %>% dplyr::sample_frac(size=coverage_3/100,weight=finalweight)
      # food_3$totalnutrient_3 <- fortificant_3*food_3$cq100
      food_3$totalnutrient <- fortificant_3*food_3$cq100
      food_3$state_L03 <- as.numeric(food_3$state_L03)
    }
    
    
    food_all <- food[,c("hhuid","totalnutrient")]
    n=2
    if(input$scenario3b==TRUE){
      # food_all <- merge(food_all[,c("hhuid","totalnutrient")],food_2[,c("hhuid","totalnutrient_2")],by="hhuid",all=TRUE)
      # n = n+1
      food_all <- rbind(food_all,food_2[,c("hhuid","totalnutrient")])
    }
    if(input$scenario3c==TRUE){
      # food_all <- merge(food_all[,c("hhuid","totalnutrient")],food_3[,c("hhuid","totalnutrient_3")],by="hhuid",all=TRUE)
      # n = n+1
      food_all <- rbind(food_all,food_3[,c("hhuid","totalnutrient")])
    }
    
    # temp <- array(food_all[,c(2:n)],dim=c(length(food_all$hhuid),n-1))
    # food_all$fortificant <- base::rowSums(temp,na.rm=TRUE)
    # rm(temp)
    
    food_all <- food_all %>% group_by(hhuid) %>% summarise(fortificant=sum(totalnutrient,na.rm=TRUE))

    lambda_pop <- function(nutrient.a,finalweight,number){
      # quantile99.9 <- wtd.quantile(nutrient.a,weights=finalweight*number,probs=0.999,na.rm=TRUE)
      # nutrient.a1 <- nutrient.a[nutrient.a<quantile99.9]
      error.add <- min(nutrient.a[nutrient.a>0&!is.na(nutrient.a)])/1000
      nutrient.a1 <- nutrient.a + error.add
      # nutrient.a1 <- nutrient.a
      
      # lambda <- tryCatch({
      boxcox.pop <- as.data.frame({
        boxcox(nutrient.a1~1,lambda=seq(-7,7,by=0.2),plotit=FALSE)
      })
      lambda <- boxcox.pop[which.max(boxcox.pop$y),"x"]
      # )}
    #   error=function()
      return(lambda)
    }

    risk_estimation <- function(dataset.nutrient,lambda,grouping.var,method){
      # dataset.nutrient <- dataset
      # grouping.var <- grouping.var3
      if(!is.na(lambda)){
        dataset.nutrient$lambda <- lambda
  
        dataset.nutrient$bc.value <- with(dataset.nutrient,ifelse(lambda==0,log(value),(value^lambda-1)/lambda))
        
        dataset.nutrient$bc.RDA2g <- with(dataset.nutrient,ifelse(lambda==0,log(RDA2g),(RDA2g^lambda-1)/lambda))
        dataset.nutrient$bc.TUL2.1g <- with(dataset.nutrient,ifelse(lambda==0,log(TUL2.1g),(TUL2.1g^lambda-1)/lambda))
  
        dataset.nutrient <- dataset.nutrient %>% mutate(quantile.prob= as.numeric(cut(value,breaks=wtd.quantile(value,probs=seq(0,1,by=0.05),na.rm=TRUE)),right=FALSE,include.lowest=TRUE))
        bc.RDA2g <- unique(dataset.nutrient$bc.RDA2g)
        bc.TUL2.1g <- unique(dataset.nutrient$bc.TUL2.1g)

      }
#Method 1: EAR Cutpoint Method----
      if(method==1){
        # summary.dataset.nutrient <- dataset.nutrient[dataset.nutrient$value<dataset.nutrient$re.q99.9,]
        summary.dataset.nutrient <- dataset.nutrient[!is.na(dataset.nutrient$value),] %>%  group_by_(grouping.var[1],grouping.var[2]) %>% summarise(Mean=wtd.mean(value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                         SD=sqrt(wtd.var(value,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                                                         Median = wtd.quantile(value,w=nutrient.no*finalweight,probs=0.5,na.rm=TRUE),
                                                                                                         bc.Mean= wtd.mean(bc.value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                         bc.SD =sqrt(wtd.var(bc.value,w=nutrient.no*finalweight,na.rm=TRUE)))
        
        

        summary.dataset.nutrient <- summary.dataset.nutrient %>% mutate(inadequacy = round(pnorm(bc.RDA2g,mean=bc.Mean,sd=bc.SD),5))
        summary.dataset.nutrient <- summary.dataset.nutrient %>% mutate(risk = round(1-pnorm(bc.TUL2.1g,mean=bc.Mean,sd=bc.SD),5))  
        summary.dataset.nutrient <- summary.dataset.nutrient[,c(grouping.var,"Mean","SD","Median","inadequacy","risk")]
        
      }
#Method 2: Probability Approach----     
      if(method==2){

        dataset.nutrient <- dataset.nutrient %>% group_by_(grouping.var[1],grouping.var[2],"quantile.prob") %>% mutate(mean.quantile=wtd.mean(value,w=nutrient.no*finalweight,na.rm=TRUE))
        dataset.nutrient <- dataset.nutrient[!is.na(dataset.nutrient$value),]
        dataset.nutrient <- dataset.nutrient %>% ungroup()  %>% group_by_(grouping.var[1],grouping.var[2]) %>% mutate(inadequacy = 1-pnorm(mean.quantile,mean=RDA2g,sd=abs(RDA2g*0.1)),
                                                                                                                      risk = pnorm(mean.quantile,mean=TUL2.1g,sd=abs(TUL2.1g*0.1)),
                                                                                                                      total.weight = sum(nutrient.no*finalweight,na.rm=TRUE))
        # summary.dataset.nutrient <- dataset.nutrient[dataset.nutrient$value<dataset.nutrient$re.q99.9 & dataset.nutrient$value>0,]
        # summary.dataset.nutrient <- dataset.nutrient[dataset.nutrient$value<dataset.nutrient$re.q99.9,] 
        summary.dataset.nutrient <- dataset.nutrient %>% summarise(Mean=wtd.mean(value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                   SD=sqrt(wtd.var(value,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                   Median = wtd.quantile(value,w=nutrient.no*finalweight,probs=0.5,na.rm=TRUE),
                                                                   #bc.Mean= wtd.mean(bc.value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                   #bc.SD =sqrt(wtd.var(bc.value,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                   inadequacy = round(wtd.mean(inadequacy,w=nutrient.no*finalweight,na.rm=TRUE),5),
                                                                   risk = round(wtd.mean(risk,w=nutrient.no*finalweight,na.rm=TRUE),5))
        
        
        
      }
      
      if(method==3){
        
        dataset.nutrient$direct.inadequacy <- with(dataset.nutrient,ifelse(bc.value<bc.RDA2g,1,0))
        dataset.nutrient$direct.risk <- with(dataset.nutrient,ifelse(bc.value>bc.TUL2.1g,1,0))
        
        # proportion.inadequacy <- with(dataset.nutrient,wtd.mean(direct.inadequacy,w=finalweight*nutrient.no,na.rm=TRUE))
        # proportion.risk <- with(dataset.nutrient,wtd.mean(direct.risk,w=finalweight*nutrient.no,na.rm=TRUE))
        
        # summary.dataset.nutrient$inadequacy <- proportion.inadequacy
        # summary.dataset.nutrient$risk <- proportion.risk
        # summary.dataset.nutrient <- dataset.nutrient[dataset.nutrient$value<dataset.nutrient$re.q99.9 & dataset.nutrient$value>0,]
        summary.dataset.nutrient <- dataset.nutrient %>% summarise(Mean=wtd.mean(value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                                                                 SD=sqrt(wtd.var(value,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                                                                                                 Median = wtd.quantile(value,w=nutrient.no*finalweight,probs=0.5,na.rm=TRUE),
                                                                                                                                                 #bc.Mean= wtd.mean(bc.value,w=nutrient.no*finalweight,na.rm=TRUE),
                                                                                                                                                 #bc.SD =sqrt(wtd.var(bc.value,w=nutrient.no*finalweight,na.rm=TRUE)),
                                                                                                                                                 inadequacy = round(wtd.mean(direct.inadequacy,w=finalweight*nutrient.no,na.rm=TRUE),5),
                                                                                                                                                 risk = round(wtd.mean(direct.risk,w=finalweight*nutrient.no,na.rm=TRUE),5))
        
      }
      return(summary.dataset.nutrient)
    }

    
    
    dataset.nutrient3 <- reactive({
      
      if(is.null(input$type3)|input$type3==""){
        type3 = "Adult Women"
      }
      else{
        type3 = input$type3
      }
      
      RDA2g <- as.numeric(rda_tul[rda_tul$nin.var_nutrient==var_nutrient & rda_tul$desc==type3,"RDA2g"])
      TUL2.1g <- as.numeric(rda_tul[rda_tul$nin.var_nutrient==var_nutrient & rda_tul$desc==type3,"TUL2.1g"])
      
      var_no <- desc[desc$desc==type3,"variable2"]
      var_cu <- desc[desc$desc==type3,"cu"]
      hh_summary_all <- hh_summary_all.orig
      hh_summary_all$nutrient.no <- hh_summary_all[,var_no]
      hh_summary_all$nutrient.cu <- hh_summary_all[,var_cu]
      hh_summary_all$nutrient.pd <- (hh_summary_all[,var_nutrient]*hh_summary_all$nutrient.cu)/(30*hh_summary_all$consumerunits*hh_summary_all$nutrient.no)
 
      hh_summary_all <- merge(hh_summary_all,food_all[,c("hhuid","fortificant")],by="hhuid",all.x=TRUE)
      hh_summary_all$fortificant.pd <- with(hh_summary_all,(fortificant*nutrient.cu)/(30*consumerunits*nutrient.no))
      #There are 10,231 cases who do not consume any wheat
      #There are 5,074 cases who do not have 0 for no of Adult women
      hh_summary_all$fortificant.pd <- with(hh_summary_all,ifelse(is.na(fortificant.pd),0,fortificant.pd))
      # hh_summary_all$total.pd <- rowSums(hh_summary_all[,c("fortificant.pd","nutrient.pd")]) #Include NA
      hh_summary_all$total.pd <- rowSums(hh_summary_all[,c("fortificant.pd","nutrient.pd")],na.rm = TRUE) #Include NA
      # View(hh_summary_all[,c("hhuid","nutrient.pd","fortificant.pd","total.pd")])
      hh_summary_all$total.pd <- with(hh_summary_all,ifelse(is.na(nutrient.pd)&fortificant.pd==0,NA,total.pd))
      
      lambda.test = lambda_pop(hh_summary_all$nutrient.pd,hh_summary_all$finalweight,hh_summary_all$nutrient.no)
      hh_summary_all$RDA2g <- RDA2g
      hh_summary_all$TUL2.1g <- TUL2.1g
      q99.9 <- with(hh_summary_all,wtd.quantile(nutrient.pd,w=finalweight*nutrient.no,probs=0.999,na.rm=TRUE))
      q00.1 <- with(hh_summary_all,wtd.quantile(nutrient.pd,w=finalweight*nutrient.no,probs=0.001,na.rm=TRUE))
      
      # hh_summary_all3 <- hh_summary_all[hh_summary_all$nutrient.pd<q99.9 & !is.na(hh_summary_all$nutrient.pd),]
      hh_summary_all3 <- hh_summary_all[hh_summary_all$nutrient.pd<q99.9&hh_summary_all$nutrient.pd>q00.1 & !is.na(hh_summary_all$nutrient.pd),]
      hh_summary_all3

    })
    
    f.bioavailability = 1 #From input
    
    # hh_summary_all.nutrient <- hh_summary_all3

    
    
    
     #-------------------------------------------------------#
    
    output$distPlot3 <- renderPlot({
      hh_summary_all.nutrient <- dataset.nutrient3()
      # hh_summary_all.nutrient <- hh_summary_all3
      state3 <- "India"
      state3 <- input$state3
      # 
      if(is.null(input$type3)|input$type3==""){
        type3 = "Adult Women"
      }
      else{
        type3 = input$type3
      }
      
      # q99.9 <- with(hh_summary_all.nutrient,wtd.quantile(nutrient.pd,w=nutrient.no*finalweight,probs=0.999,na.rm=TRUE))
      
      if(state3 == "India"){
        # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9,c("NSS_State","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        dataset <- hh_summary_all.nutrient[,c("NSS_State","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        dataset <- melt(dataset,id.vars=c("NSS_State","hhuid","finalweight","nutrient.no","RDA2g","TUL2.1g"),measure.vars=c("nutrient.pd","total.pd"))
      }
      else{
        # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9 & hh_summary_all.nutrient$NSS_State==state3,c("NSS_State","DISTRICT","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$NSS_State==state3,c("NSS_State","DISTRICT","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        dataset <- melt(dataset,id.vars=c("NSS_State","DISTRICT","hhuid","finalweight","nutrient.no","RDA2g","TUL2.1g"),measure.vars=c("nutrient.pd","total.pd"))
        
      }
      
      dataset$fort <- with(dataset,ifelse(variable=="nutrient.pd","1. Before Fortification","2. After Fortification"))
      
      title = paste0("Intake distribution of ",nutrient3," in ",state3," for ",type3)
      legend = paste0("Intake in ",unit_nutrient)
      
      hp3 <- ggplot() + geom_histogram(data=dataset,aes(x=value*multiplier,weight=nutrient.no*finalweight,group=fort)) + facet_grid(~fort)
      hp3 <- hp3 + geom_vline(data=dataset,aes(xintercept=RDA2g*multiplier,group=fort),col="blue")
      hp3 <- hp3 + geom_vline(data=dataset,aes(xintercept=TUL2.1g*multiplier,group=fort),col="red")
      hp3 <- hp3 + xlab(legend) + ylab("Count (excl top 0.1%ile)") + scale_y_continuous(labels = comma)
      hp3 <- hp3 + ggtitle(title) + theme(text = element_text(size=15))
      print(hp3)
    })
    
    output$summary3 <- renderTable({
      
      hh_summary_all.nutrient <- dataset.nutrient3()
      # hh_summary_all.nutrient <- hh_summary_all3
      state3 <- "India"
      state3 <- input$state3
      
      #Adult women is default 
      if(is.null(input$type3)|input$type3==""){
        type3 = "Adult Women"
      }
      else{
        type3 = input$type3
      }
      
      #Calculate 99.9%ile for outlier detection
      # q99.9 <- with(hh_summary_all.nutrient,wtd.quantile(nutrient.pd,w=nutrient.no*finalweight,probs=0.999,na.rm=TRUE))
      lambda = lambda_pop(hh_summary_all.nutrient$nutrient.pd,hh_summary_all.nutrient$finalweight,hh_summary_all.nutrient$nutrient.no)
      #Eliminate outliers
      if(state3 == "India"){
        dataset <- hh_summary_all.nutrient[,c("NSS_State","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9,c("NSS_State","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        grouping.var3=c("NSS_State","fort")
      }
      else{
        dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$NSS_State==state3,c("NSS_State","DISTRICT","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        # dataset <- hh_summary_all.nutrient[hh_summary_all.nutrient$nutrient.pd<q99.9 & hh_summary_all.nutrient$NSS_State==state3,c("NSS_State","DISTRICT","hhuid","finalweight","nutrient.no","nutrient.pd","total.pd","RDA2g","TUL2.1g")]
        grouping.var3=c("DISTRICT","fort")
      }
      
      dataset <- melt(dataset,measure.vars=c("nutrient.pd","total.pd"))
      dataset$fort <- with(dataset,ifelse(variable=="nutrient.pd","1. Before Fortification","2. After Fortification"))
      # dataset <- dataset[!is.na(dataset$value),]
      

      
      method3 = 1
      if(var_nutrient=="IronFe.FE"|is.null(lambda)|is.na(lambda)){
        method3 = 2
      }
      
      summary.fortification <- dataset %>% group_by_(grouping.var3[1],grouping.var3[2]) %>% risk_estimation(.,grouping.var=grouping.var3,lambda=lambda,method=method3)

      # summary.fortification <- dataset %>% risk_estimation(.,lambda=lambda,method=2) #Matches
      # summary.fortification <- dataset %>% group_by(NSS_State) %>% risk_estimation(.,lambda=lambda,method=1) #Matches
      summary.fortification[,3:5] <- round(summary.fortification[,3:5]*multiplier,2)
      summary.fortification[,6:7] <- round(summary.fortification[,6:7],3)*100
      
      colnames(summary.fortification)[3:5] <- paste0(colnames(summary.fortification)[3:5]," (in ",unit_nutrient,")")
      colnames(summary.fortification)[6:7] <- paste0(colnames(summary.fortification)[6:7]," (%)")
      
      
      print(summary.fortification)
    })
    
    
  })
  
  observeEvent(input$goButton4,{
    crop4 = "Paddy"
    crop4 = input$crop4
    
    
    map_df_dist <- reactive({
      # state2 = "Gujarat"
      state4="India"
      state4 = input$state4
      
      year4=2008
      year4 = input$year4
      
      statistic4 = "Area"
      statistic4 = input$statistic4
      
      if(state4=="India"){
        dataset <- merge(district_shp_df2,agristats.summary[agristats.summary$mag.Year2==year4 & agristats.summary$mag.CROP==crop4,c(statistic4,"censuscode")],by.x="id",by.y="censuscode",all.x=TRUE)
        dataset <- dataset[order(dataset$order),]
        dataset
      }
      else{
        dataset <- merge(district_shp_df2[district_shp_df2$NSS_State==state4,],agristats.summary[agristats.summary$mag.Year2==year4 & agristats.summary$mag.CROP==crop4 & agristats.summary$NSS_State==state4,c(statistic4,"censuscode")],by.x="id",by.y="censuscode",all.x=TRUE)
        dataset <- dataset[order(dataset$order),]
        dataset
      }
    })
    
    
    # map_df_dist <- dataset
    
    output$mapPlot4<- renderPlot({
      state4="India"
      state4 = input$state4
      
      year4=2010
      year4 = input$year4
      
      statistic4 = "Production"
      statistic4 = input$statistic4
      
      unit = "Tonnes"
      if(statistic4=="Area"){
        unit = "Hectare"
      }
      if(statistic4=="Yield"){
        unit = "Tonnes per Hectare"
      }
      
      crop4 = "All Pulses"
      
      mp2 <- ggplot() + geom_polygon(data=map_df_dist(),aes(x=long,y=lat,group=group,fill=eval(parse(text=statistic4))))
      # mp2 <- ggplot() + geom_polygon(data=map_df_dist,aes(x=long,y=lat,group=group,fill=eval(parse(text = statistic4))))
      mp2 <- mp2 + coord_map() + xlab("Longitude") + ylab("Latitude") + ggtitle(paste0(statistic4," of ",crop4," - ",state4," by District")) + theme_grey()
      mp2 <- mp2 + scale_fill_distiller(name=paste0(statistic4," in ",unit), palette = "YlGnBu",direction=1)
      print(mp2)
    },height=600)
    

    
    output$summary4 <- renderTable({
      state4="India"
      state4 = input$state4
      
      year4=2008
      year4 = input$year4
      
      crop4 = "Rice"
      crop4 = input$crop4
      
      if(state4=="India"){
        agristats.summary_state <- agristats.summary[agristats.summary$mag.CROP==crop4&agristats.summary$mag.Year2==year4,] %>% group_by(NSS_State) %>% summarise(Area=sum(Area,na.rm=TRUE),Production=sum(Production,na.rm=TRUE))
        agristats.summary_state$Yield <- with(agristats.summary_state,ifelse(Area==0,0,Production/Area))
        
        colnames(agristats.summary_state) <- c("State","Production in Tonnes","Area in Hectares","Yield in Tonnes per Hectare")
        print(agristats.summary_state)
      }
      else{
        
        agristats.summary_dist <- agristats.summary[agristats.summary$mag.CROP==crop4&agristats.summary$mag.Year2==year4&agristats.summary$NSS_State==state4,c("NSS_State","DISTRICT","Production","Area","Yield")]
        
        colnames(agristats.summary_dist) <- c("State","District","Production in Tonnes","Area in Hectares","Yield in Tonnes per Hectare")
        print(agristats.summary_dist)
      }
    })
    
    
    
  })
  
  observeEvent(input$goButton5,{
    # outcome5 = "77. Non-pregnant women age 15-49 years who are anaemic (<12.0 g/dl) (%)"
    outcome5 = input$outcome5
    
    outcome.variable = outcomelist[outcomelist$Description==outcome5,"variable.ITEMID2"]
    
    map_df_dist <- reactive({
      # state5 = "Andhra Pradesh"
      state5="India"
      state5 = input$state5
      
      area5 = "Total"
      area5 = input$area5
      
      if(state5=="India"){
        dataset <- merge(district_shp_df2,nfhs4.complete3[nfhs4.complete3$variable.ITEMID2==outcome.variable,c(area5,"censuscode")],by.x="id",by.y="censuscode",all.x=TRUE)
        dataset <- dataset[order(dataset$order),]
        dataset
      }
      else{
        dataset <- merge(district_shp_df2[district_shp_df2$NSS_State==state5,],nfhs4.complete3[nfhs4.complete3$variable.ITEMID2==outcome.variable & nfhs4.complete3$NSS_State==state5,c(area5,"censuscode","DISTRICT")],by.x="id",by.y="censuscode",all.x=TRUE)
         dataset <- dataset[order(dataset$order),]
        dataset
      }
    })
    

    output$mapPlot5<- renderPlot({
      state5="India"
      state5 = input$state5

      area5 = "Total"
      area5 = input$area5
      
      title = paste0(substr(outcome5,4,str_length(outcome5))," - ",state5," by District")
      # title = paste0(substr(outcome5,4,str_length(outcome5))," - Kurnool, AP")
      
      # mp2 <- ggmap(india) #+ geom_polygon(data=states_shp,aes(x=long,y=lat,group=group),color="black",fill="white",size=0.2,alpha=0.8)
      mp2 <- ggplot() + geom_polygon(data=states_shp[states_shp$id==28,],aes(x=long,y=lat,group=group),color="black",fill="white",size=0.2,alpha=0.8)
      mp2 <- mp2 + geom_polygon(data=map_df_dist(),aes(x=long,y=lat,group=group,fill=eval(parse(text=area5))))
      
      # mp2 <- ggplot() + geom_polygon(data=map_df_dist,aes(x=long,y=lat,group=group,fill=eval(parse(text = area5))))
      # mp2 <- mp2 + geom_text(data=label_dist,aes(long,lat,label=DISTRICT),size=2)
      mp2 <- mp2 + coord_map() + xlab("Longitude") + ylab("Latitude") + ggtitle(title) + theme_grey()
      mp2 <- mp2 + scale_fill_distiller(name=title, palette = "RdYlGn",direction=-1,limits=c(10,80))
      print(mp2)
    },height=600)
    

    
    
  })
  
  
  
  
})