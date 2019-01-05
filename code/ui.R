library(shiny)

if(!require(shinydashboard)){
  install.packages("shinydashboard)")
  library(shinydashboard)
}
library(openxlsx)
# 
nutrientlist.nutrients <- c("Vitamin A, RAE ",'Iron(Fe)',"Vitamin B-12 "," Total Folates (B9)",'Calcium(Ca)',"Energy in KiloCal","Total Fat" ,"Total Ascorbic Acid","Phytate" ,"Retinol",'Zinc(Zn)' ,'Aluminium(Al)' ,'Potassium(K)','Magnesium(Mg)',"Total Dietary Fibre","Total Polyphenols","Protein")
nutrientlist.fortificants <- c("Vitamin A, RAE ",'Iron(Fe)'," Total Folates (B9)",'Calcium(Ca)')
states <- read.xlsx(file.path("data","Food Grouping_Short.xlsx"),sheet=5,colNames = TRUE)
states <- states[!is.na(states$NSS_ID)&!duplicated(states$NSS_ID),c("NSS_ID","NSS_State")]

load(file.path("data","18_foodlist.RData"))
load(file.path("data","16_RDA TUL.RData"))
load(file.path("data","croplist.RData"))
load(file.path("data","nfhs4_outcomelist.RData"))



dashboardPage(
  dashboardHeader(
    title= "FSSAI"
  ),
  dashboardSidebar(
      conditionalPanel(condition="input.selectedpanel==1",
                       selectInput("fooditem1", label = "Select Food",choices = foodlist$name),
                       checkboxGroupInput("quintiles1","Select wealth quintile target group",
                                          c("Lowest 20%"=1,
                                            "Quintile 2"=2,
                                            "Quintile 3"=3,
                                            "Quintile 4"=4,
                                            "Highest 20%"=5)),
                       actionButton("goButton1", "Go!")),
      
      conditionalPanel(condition="input.selectedpanel==2",
                       selectInput("nutrient2", label = "Select Nutrient",choices = nutrientlist.nutrients),
                       checkboxGroupInput("quintiles2","Select wealth quintile target group",
                                          c("Lowest 20%"=1,
                                            "Quintile 2"=2,
                                            "Quintile 3"=3,
                                            "Quintile 4"=4,
                                            "Highest 20%"=5)),
                       actionButton("goButton2", "Go!")),
      
      conditionalPanel(condition="input.selectedpanel==3",
                       selectInput("nutrient3", label = "Select Nutrient",choices = nutrientlist.fortificants),
                       selectInput("fooditem3", label = "Select Food",choices = foodlist$name),
                       checkboxGroupInput("unit3","Select unit of fortification",
                                          c("µg"=1,
                                            "mg"=2,
                                            "g"=3)),
                       numericInput("fortificant3",label="Input amount of fortificant (unit per 100g)",min = 0,max = 100,value=0),
                       checkboxGroupInput("quintiles3","Select wealth quintile target group",
                                          c("Lowest 20%"=1,
                                            "Quintile 2"=2,
                                            "Quintile 3"=3,
                                            "Quintile 4"=4,
                                            "Highest 20%"=5)),
                       sliderInput("coverage3",label = "Select percentage of population covered in target group",min = 0,max=100,step = 0.5,value = 0.5),
                       ########################################################################
                       checkboxInput("scenario3b", "Select to add food 2",value=FALSE),
                       conditionalPanel(
                         condition = "input.scenario3b == true",
                         selectInput("fooditem3_2", label = "Select Food",choices = foodlist$name),
                         checkboxGroupInput("unit3_2","Select unit of fortification",
                                            c("µg"=1,
                                              "mg"=2,
                                              "g"=3)),
                         numericInput("fortificant3_2",label="Input amount of fortificant (unit per 100g)",min = 0,max = 100,value=0),
                         checkboxGroupInput("quintiles3_2","Select wealth quintile target group",
                                            c("Lowest 20%"=1,
                                              "Quintile 2"=2,
                                              "Quintile 3"=3,
                                              "Quintile 4"=4,
                                              "Highest 20%"=5)),
                         sliderInput("coverage3_2",label = "Select percentage of population covered in target group",min = 0,max=100,step = 0.5,value = 0.5)
                       ),
                       ##########################################################################
                       checkboxInput("scenario3c", "Select to add food 3",value=FALSE),
                       conditionalPanel(
                         condition = "input.scenario3_3 == true",
                         selectInput("fooditem3_3", label = "Select Food",choices = foodlist$name),
                         checkboxGroupInput("unit3_3","Select unit of fortification",
                                            c("µg"=1,
                                              "mg"=2,
                                              "g"=3)),
                         numericInput("fortificant3_3",label="Input amount of fortificant (unit per 100g)",min = 0,max = 100,value=0),
                         checkboxGroupInput("quintiles3_3","Select wealth quintile target group",
                                            c("Lowest 20%"=1,
                                              "Quintile 2"=2,
                                              "Quintile 3"=3,
                                              "Quintile 4"=4,
                                              "Highest 20%"=5)),
                         sliderInput("coverage3_3",label = "Select percentage of population covered in target group",min = 0,max=100,step = 0.5,value = 0.5)
                       ),
                       actionButton("goButton3", "Go!")),
      
      conditionalPanel(condition="input.selectedpanel==4",
                       selectInput("crop4",label="Select Crop",choices=croplist),
                       actionButton("goButton4", "Go!")),
      
      conditionalPanel(condition="input.selectedpanel==5",
                       selectInput("outcome5",label="Select Outcome",choices=outcomelist$Description),
                       actionButton("goButton5", "Go!"))
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Crop Production Statistics",value=4,
               selectInput("state4","Select State",choices=states$NSS_State),
               selectInput("statistic4","Select Statistic",choices=c("Area","Production","Yield")),
               sliderInput("year4",label = "Select year",min = 2008,max=2011,step = 1,value = 2008,sep=""),
               box(width=12,plotOutput("mapPlot4"),height=700),
               box(width=6,tableOutput("summary4"))),
      
      tabPanel("Food Intake",value=1,
               # selectInput("state1","Select State",choices=states$NSS_State),
               selectInput("state1","Select State",choices="India"),
               #Since it is based on consumer units
               #selectInput("type1","Select Classification",choices=rda_tul$desc),
               fluidRow(
                 box(title="Intake of Food in grams per CU per month",solidHeader=TRUE,status="danger",width=8,height=800,
                     plotOutput("mapPlot1"))
                 # http://rstudio.github.io/shinydashboard/appearance.html#icons
                ),
               # hr(),
               
                 box(title="Distribution of Intake",solidHeader=TRUE,status="warning",width=6,height=1000,
                     plotOutput("distPlot1")),
                 box(title="Intake in grams",solidHeader=TRUE,status="warning",width=6,
                     tableOutput("summary1"))
               
              ),
      
      tabPanel("Nutrient Intake",value=2,
               # selectInput("state2","Select State",choices=states$NSS_State),
               selectInput("state2","Select State",choices="India"),
               selectInput("type2","Select Classification",choices=rda_tul$desc),
               fluidRow(
                 box(title="Intake of Nutrient per person per day",solidHeader=TRUE,status="danger",width=8,height=800,
                     plotOutput("mapPlot2"))
               ),
               fluidRow(
                 box(title="Distribution of Intake",solidHeader=TRUE,status="warning",width=6,height=1000,
                     plotOutput("distPlot2")),
                 box(title="Summary of Intake",solidHeader=TRUE,status="warning",width=6,
                     tableOutput("summary2"))
               )),
      tabPanel("Fortification Simulation",value=3,
               # selectInput("state3","Select State",choices=states$NSS_State),
               selectInput("state3","Select State",choices="India"),
               selectInput("type3","Select Classification",choices=rda_tul$desc),
               box(width=12,plotOutput("distPlot3")),
               # plotOutput("mapPlot3"),
               box(width=12,height=5000,tableOutput("summary3"))),
    
      tabPanel("NFHS 4 Outcomes",value=5,
               selectInput("state5","Select State",choices=states$NSS_State),
               selectInput("area5","Select Area",choices=c("Rural","Urban","Total")),
               box(width=12,plotOutput("mapPlot5"),height=700)
               # box(width=8,tableOutput("summary5"))
               ),
      
      id = "selectedpanel"
    ))
  
  
)



#References:
# http://frantzmd.info/Alternative%20Medicine/Tolerable%20Upper%20Limits.htm