#
#    http://shiny.rstudio.com/
#
#Reading in libraries 
library(shiny)
library(faraway)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(giscoR)
library(zoo)
library(corrplot)
library(reshape2)
library(DT)
library(caret)
library(stargazer)
library(olsrr)


#link data to github
#data23=read.csv("")
colnames(data23)=c("City","Latitude","Longitude","Month","Year","Rainfall",
                   "Elevation","Climate","Temperature","Humidity","dates")

#data23$dates=as.yearmon(paste(data23$Year, data23$Month), "%Y %m")

# UI for application that 
ui <- dashboardPage(
    skin="blue",
    dashboardHeader(
        title = "Rainfall Insights on German Cities ",
        titleWidth = 350,
        dropdownMenuOutput("messageMenu")),
    
    dashboardSidebar(
        sidebarMenu(
            id="SideBar_Names",
            width = 350,
            menuItem("Data Overview",tabName = "DataO",icon=icon("globe")),
            menuItem("Data Exploration",tabName="DE",icon=icon("search")),
            menuItem("Linear Regression Model",tabName="LRM",
                     icon=icon("chart-line"))
        )
    ),
    dashboardBody(#rows have width of 12
        
        tags$head(
            tags$style(HTML("
                .info-box .info-box-number{font-size:25;}
                .info-box .info-box-text{font-size:40px;}
                ") 
                )
        ),
 
      
        
        tabItems(
            
        ##DATA OVERVIEW 
            tabItem(tabName = "DataO",h2("Data Overview"),
                    
                    br(),
                    
                    fluidRow(
                        valueBox(length(colnames(data23[,-11])),
                                 "Total Variables",icon=icon("hashtag", style="color:white;opacity:0.5;"),width=12,color="navy")
                    ),
                    
                    fluidRow(
                       div(
                        DTOutput("IndicatorChart"),
                        style=("margin:20px;")
                   )),
                   
                   br(),
                   
                    fluidRow(
                      infoBoxOutput("NABox",width = 6),
                      infoBoxOutput("factoredVars",width = 6)
                    ),
                   
                   
                   h3("Germany Map"),
                   
                   
                   fluidRow(
                       box(title="German Cities in the Dataset",width = 12,solidHeader = T, status = "primary",
                           leafletOutput("Leafmap1"), paste("Cities on the map:",paste(unique(data23$City),collapse = ", ")))
                   ),
                   
                   
                   
                   fluidRow(
                       infoBox(length(unique(data23$City)),"Cities" , icon=icon("building",style="opacity:0.5;"),width=12,color="navy")
                   ),
                   
                   
                   
                    fluidRow(
                         valueBox(length(unique(data23$Climate)),"Climates",icon=icon("cloud-sun", style="color:white;opacity:0.5;"),color="navy"),
                         valueBox(length(unique(data23$Month)),"Months" , icon=icon("clock", style="color:white;opacity:0.5;"),color="navy"),
                         valueBox(length(unique(data23$Year)),"Years", icon=icon("clock", style="color:white; opacity:0.5;"),color="navy")
                    ),
                   
                   
                    h3("Elevation"),
                   
                    fluidRow(
                         box(title="Elevation Points by City",width = 12,solidHeader = T, status = "primary",
                             plotOutput("ElevationMap",click="plotClick"))
                      ),
                   
                    fluidRow(
                         valueBoxOutput("ChangeName",width=12)   
                       ),
                   
                   
                    fluidRow(
                        valueBoxOutput("temper"),
                        valueBoxOutput("rainfall"),
                        valueBoxOutput("humid")
                    )
                   
                   ),
            
            
         ##DATA EXPLORATION      
   
   
            tabItem(tabName = "DE",h2("Data Exploration"),
                    fluidRow(
                          box(title="Choose From Dates, Cities and Variables Below",width = 12,solidHeader = T, status = "primary",
                              plotOutput("RainfallCityPlot")),
                      ),
                      
                     fluidRow(
                          box(status = "primary", height = 100,
                              dateRangeInput("DateRange",label = "Select Date Range:", min=min(as.Date(data23$dates)),
                                             max=max(as.Date(data23$dates)),startview="year",format="mm/dd/yyyy")),
                          box(status = "primary",height = 100,
                              selectInput("CityChoose",label = "Choose a City:",choices = unique(data23$City))),
                     ),
                    
                    fluidRow(
                        div(style = "text-align: center;",
                            box(status = "primary",height = 100, width = 6,
                                selectInput("VariableChoose",label = "Choose a Variable to Model on the Y Axis:",choices = colnames(data23[,c(6,9,10)])))   
                        ),
                           box(status = "primary",height = 100, width = 6,
                               actionButton("ClickMe",label="Render Plot",style="width:100%;;background-color:#001f3f;color:white; height:80px; font-size:20px;")
                           )
                        
                    ),
                    

                    
                    br(),
                    
                    h3("Bivariate Plots"),
                    fluidRow(
                        column(width=6, 
                                box(status = "primary",width = NULL,height=200,
                                selectInput("VarchooseBXPY",label = "Choose a Variable to Model on the Y Axis:",
                                            choices = colnames(data23[,c(6,9,10)]))),
                               
                               box(status = "primary",height=200,
                                   width = NULL, selectInput("VarchooseBXPX", label = "Choose a Variable to Model on the X Axis:",
                                                                  choices = colnames(data23[,c(1,8)]))), 
                        ),
                        
                        box(title="Choose From Variables on the Left",width = 6,solidHeader = T, status = "primary",
                                plotOutput("Boxplot1"))
                        
                    ),
                    
                    br(),
                    
                    h3("Total Average for Each City"),
                    
                    fluidRow(
                      box(title="Choose From Variables on the Right",width = 6,solidHeader = T, status = "primary",
                          plotOutput("Histogram1")),
                      
                      column(width=6, 
                            box(status = "primary",width = NULL,height=200,
                                 selectInput("HistVarY",label = "Choose a Variable to Model on the Y Axis:",
                                             choices = colnames(data23[,c(6,9,10)]))),
                             
                            box(status = "primary",height=200,
                                 width = NULL, selectInput("HistVarX", label = "Choose a Variable to Model on the X Axis:",
                                                           choices = colnames(data23[,c(1,8)]))), 
                      ),
                    ),
                    
                    
                    
                    h3("Correlation Matrix"),
                    
                    fluidRow(
                        box(title = "Correlation Matrix of Variables",width = 12,solidHeader = T,status = "primary",
                            plotOutput("CorrMat"))
                    ),
                    
                    fluidRow(
                        infoBoxOutput("corrBox",width = 12)
                    )
                    
                ),
            
      ##MODEL FITTING Linear Regression           
            tabItem(tabName = "LRM",h2("Robust Model Fitting With Variable Selection: Linear Regression Model"),
                    
                    fluidRow(
                      column(width=3, 
                             box(status = "primary",width = NULL,height=200,
                               selectizeInput("response",label="Choose a Variable to model as the response:",
                                              choices=colnames(data23[,c(6,9,10)]),selected = colnames(data23[,6]))),
                             
                             box(status = "primary",width = NULL,height=200,
                               selectizeInput("predictor",label="Choose Variables to model as predictors:",
                                              choices=colnames(data23[,-c(11,5,4)]),multiple=TRUE,selected= colnames(data23[,c(1:3,7:10)]))),
                             
                             box(status = "primary",width = NULL,height=200,
                                 sliderInput("slider",label = "Adjust the Test/Train Split % here:",min = 0, max=100,value =70),
                                 textOutput("TrainText"), 
                                 textOutput("TestText"))
                             ),
                      
                      
                      tabBox(id="tabBoxLM",width=9,
                        tabPanel("Data", 
                                 DTOutput("DataChart")),
                        tabPanel("Summary Statistics", br(),
                                 div(style = "text-align: center;",
                                   verbatimTextOutput("sum"), br(),
                                   verbatimTextOutput("summa"))),
                        tabPanel("Model",
                                 verbatimTextOutput("Lmod"), br(), 
                                 div(style = "text-align: center;",
                                    verbatimTextOutput("varimp")),br(),
                                 verbatimTextOutput("alias")
                                 ),
                        tabPanel("Diagnostics", plotOutput("residsPlot"),title = "Diagnostic Plots"),
                        tabPanel("Prediction")
                      )
                      
                    )
                  
                  )
                      
        ),
    )
)
    








# Server logic 
server <- function(input, output, session) {
    react=reactiveValues(messy=list(
        messageItem("Admin","Hello, Welcome to the Dashboard!",time=format(Sys.time(),"%H:%M:%S"))),
        count=list(DataO=0,DE=0,LRM=0)
        )
    observeEvent(input$SideBar_Names,{
        react$count[[input$SideBar_Names]]=react$count[[input$SideBar_Names]]+1
        if(react$count[[input$SideBar_Names]]==1){
        react$tabCount=input$SideBar_Names
            mess=messageItem("ReactiveBot",paste("You found a tab! Explore the",input$SideBar_Names,"tab!"),
                             time=format(Sys.time(), "%H:%M:%S"))
            react$messy=c(react$messy,list(mess))}
        })
    output$messageMenu=renderMenu({
        dropdownMenu(type = "messages", .list = react$messy)
    })
    
    output$Leafmap1=renderLeaflet({
        lat=data23$Latitude
        lon=data23$Longitude
        
        leaflet(data23) %>% 
            addTiles() %>% 
            addMarkers(lng = lon, lat=lat, popup=data23$City) %>% 
            addMiniMap(width = 150, height = 150)
    })
    
    output$ElevationMap=renderPlot({
        germany_counties <- gisco_get_nuts(
            year = "2021", #only 2016,2021, 2024 available, data from 2015-2023, most recent
            epsg = "4326", #gets coordinates
            resolution = "10",
            country = "DE", 
            nuts_level = "3" #regions
        )
        Elevation_m=data23$Elevation
        ggplot()+
            geom_sf(data=germany_counties,color = "black",fill="white")+
            geom_point(data=data23,size=5,shape = 16, fill = NA,
                       aes(Longitude,Latitude,color=Elevation_m))+
            scale_color_viridis_c()+
            coord_sf()+
            labs(title = "Elevation in 10 German Cities", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")
        
    })
    
    output$ChangeName=renderValueBox({
        valueBox(value="Click a Point","To see the elevation of the cities", 
                 icon=icon("caret-up",style = "color:white; opacity:0.5;"),color="navy")
    })
    
    output$temper=renderValueBox({
        valueBox(value="Click a Point","To see the average temperature of the cities", 
                 icon=icon("temperature-low",style = "color:white; opacity:0.5;"),color="navy")
    })
    
    output$rainfall=renderValueBox({
        valueBox(value="Click a Point","To see the average rainfall of the cities", 
                 icon=icon("cloud-showers-heavy",style = "color:white; opacity:0.5;"),color="navy")
    })
    
    output$humid=renderValueBox({
        valueBox(value="Click a Point","To see the average humidity of the cities", 
                 icon=icon("cloud",style = "color:white; opacity:0.5;"),color="navy")
    })
    
    observeEvent(input$plotClick,{
        clicker=nearPoints(data23, input$plotClick, xvar="Longitude",yvar="Latitude",threshold = 13)
        if (nrow(clicker) == 0){return()}
        if (nrow(clicker>0)){
            output$ChangeName=renderValueBox({
                valueBox(paste(clicker$Elevation[1]," m"), paste("Elevation of",clicker$City[1]), 
                             icon=icon("caret-up",style = "color:white; opacity:0.5;"),color="navy")
            })
            
            output$temper=renderValueBox({
                valueBox(paste(round(mean(clicker$Temperature),1),"\u00B0C"), paste("Total Avg. Temperature of",clicker$City[1]), 
                         icon=icon("temperature-low",style = "color:white; opacity:0.5;"),color="navy")
            })
            
            output$rainfall=renderValueBox({
                valueBox(paste(round(mean(clicker$Rainfall),1),"mm"), paste("Total Avg. Rainfall of",clicker$City[1]), 
                         icon=icon("cloud-showers-heavy",style = "color:white; opacity:0.5;"),color="navy")
            })
            
            output$humid=renderValueBox({
                valueBox(round(mean(clicker$Humidity),1), paste("Total Avg. Humidity of",clicker$City[1]), 
                         icon=icon("cloud",style = "color:white; opacity:0.5;"),color="navy")
            })
        }
    })
    
    
    output$IndicatorChart=renderDT({
        indicatorNames=colnames(data23[,-11])
        indicatorInfo=c("Name of the city","City's latitude in degrees","City's longitude in degrees","The month number (1-12)",
                        "The year of the data","Rainfall amount in millimeters","City’s elevation above sea level in meters",
                        "The climate classification of the city", "Average temperature for the month in Celsius",
                        "Average humidity level for the month in percentage")
        indicator=as.data.frame(cbind(indicatorNames,indicatorInfo))
        colnames(indicator)=c("Variables","Description")
        datatable(indicator,options=list(
            dom='t',paging=FALSE
        ),
        rownames = FALSE
        )
    })
    
    output$RainfallCityPlot=renderPlot({
        pchMonth=substr(format(data23$dates, "%b"),1,2)
        data23$PchVal=pchMonth[1:length(data23$City)]
        
        Datasub=data23 %>% 
            filter(City=="Berlin")
        
        monthLetters=factor(Datasub$PchVal)
        
        ggplot(Datasub)+
            aes(x=dates,Rainfall)+
            geom_line()+
            geom_text(aes(label = PchVal,color= monthLetters,vjust=-1))+
            ggtitle(paste0(Datasub$City[1]," Rainfall over ",min(Datasub$Year),"-",max(Datasub$Year)))
    })
    
    
    
    
    observeEvent(input$ClickMe,{
      req(input$DateRange)
        dataTemp=data23 %>% 
            filter(
                 data23$City==input$CityChoose,
                 as.Date(data23$dates)>=as.Date(input$DateRange[1]),
                 as.Date(data23$dates)<=as.Date(input$DateRange[2]))
        
        output$RainfallCityPlot=renderPlot({
            minyear=as.Date(input$DateRange[1])
            minyr=format(minyear,"%Y")
            maxyear=as.Date(input$DateRange[2])
            maxyear=format(maxyear,"%Y")
            pchMonth=substr(format(dataTemp$dates, "%b"),1,2)
            dataTemp$pchM=pchMonth[1:length(dataTemp$dates)]
            monthLetter=factor(dataTemp$pchM)
            Y_Variable=dataTemp[[input$VariableChoose]]
            
            
          ggplot(dataTemp)+
                aes(x=dates,y=Y_Variable)+
                geom_line()+
                geom_text(aes(label =pchM ,color=(monthLetter),vjust=-1)) +
                ggtitle(paste0(dataTemp$City[1]," ",input$VariableChoose , " over ",minyr,"-",maxyear))
        })
    })
    
    
    
    observeEvent(input$VarchooseBXPY,{
        output$Boxplot1=renderPlot({
            x=data23[[input$VarchooseBXPX]]
            y=data23[[input$VarchooseBXPY]]
            ggplot(data23)+
                aes(x,y)+
                geom_boxplot(aes(fill=x))+
                ggtitle(paste0(input$VarchooseBXPX," & ",input$VarchooseBXPY ," plot"))
        })
    })
    
    
    observeEvent(input$VarchooseBXPX,{
        output$Boxplot1=renderPlot({
            x=data23[[input$VarchooseBXPX]]
            y=data23[[input$VarchooseBXPY]]
            ggplot(data23)+
                aes(x,y)+
                geom_boxplot(aes(fill=x))+
                ggtitle(paste0(input$VarchooseBXPX," & ",input$VarchooseBXPY ," plot"))
        })
    })


    output$CorrMat=renderPlot({
        corData=cor(data23[,-c(1,8,11)])
        corrplot(corData,method="circle",type="upper",
                 title="Correlation Matrix",
                 mar=c(0,0,2,0))
    })
    
    output$corrBox=renderInfoBox({
        corData=cor(data23[,-c(1,8,11)])
        melty=melt(corData)
        absVar=abs(melty$value)>0.6
        newCorrMat=melty[absVar,]
        finalCorMat=newCorrMat[newCorrMat$value<1,]
    
        infoBox(length(finalCorMat$Var1),subtitle=paste("The number of variables with moderate to high correlation. They are ",
                      finalCorMat[1,c(1,2)]$Var1," and ",finalCorMat[1,c(1,2)]$Var2 ),
                 icon=icon("chevron-right",style = "color:white; opacity:0.5;"),color="navy")
    })
    
    
    output$NABox=renderInfoBox({
        NAVal=sum(is.na(data23))
        infoBox(NAVal,"NA Values" , 
                icon=icon("creative-commons-zero",style="opacity:0.5;"),width=12,color="navy")
    })
    
    output$factoredVars=renderInfoBox({
      tempvar=c()
      for(i in colnames(data23)){
        if(!is.null(levels(data23[,i]))){
          tempvar=c(tempvar,i)
        }}
        infoBox(length(tempvar),"Factored Variables with Levels" , 
                icon=icon("plus",style="opacity:0.5;"),width=12,color="navy")
    })
    
    
    observeEvent(input$HistVarY,{
      dataTempMat=data23 %>% 
        group_by_at(input$HistVarX) %>% 
        summarise(Avg=mean(!!sym(input$HistVarY)))
      
      Histodata=as.data.frame(dataTempMat)
      
      output$Histogram1=renderPlot({
          X_Var=Histodata[,1]
          ggplot(Histodata)+
              aes(X_Var,Avg,fill=X_Var)+
              geom_bar(stat = "identity")+
              ggtitle(paste0(input$HistVarX," & ",input$HistVarY ," plot"))
      })
    })
    
    observeEvent(input$HistVarX,{
      dataTempMat=data23 %>% 
        group_by_at(input$HistVarX) %>% 
        summarise(Avg=mean(!!sym(input$HistVarY)))
      
      Histodata=as.data.frame(dataTempMat)
      
      output$Histogram1=renderPlot({
        X_Var=Histodata[,1]
        ggplot(Histodata)+
          aes(X_Var,Avg,fill=X_Var)+
          geom_bar(stat = "identity")+
          ggtitle(paste0(input$HistVarX," & ",input$HistVarY ," plot"))
      })
    })
    
  
    ##Observe event (reactive) from linear regression model; tab connects to all tabs
    observeEvent(input$slider,{
      datatemp=data23[,-11]
      pc=input$slider/100
      set.seed(123)
      indx=createDataPartition(datatemp[[input$response]],times=1,p=pc,list=FALSE)
      train=datatemp[indx,]
      test=datatemp[-indx,]
      
      
      output$DataChart=renderDT({
        if (is.null(input$predictor)){
          datatable(datatemp,options = list(lengthMenu = c(15, 20, 50, 70),scrollX=TRUE))
        }
        else{
          datatable(train[,c(input$response,input$predictor)],options = list(lengthMenu = c(15, 20, 50, 70),scrollX=TRUE))
        }
      })
      
      output$TrainText=renderText({
        paste("Train:",length(train[,1]),"obs.")
      })
      
      output$TestText=renderText({
        paste("Test:",length(test[,1]),"obs.")
      })
      
      output$sum=renderPrint({
        if(nrow(train)==0){
          print("Add observations")
        }
        else {
          stargazer(train[,c(input$response,input$predictor)],type="text",
                    title = "Descriptive statistics",
                    digits = 1)
        }
      })
      
      output$summa=renderPrint({
        summary(train[,c(input$response,input$predictor)])
      })
      
      output$Lmod=renderPrint({
       if(length(input$predictor)==0){
         print("Add Predictors")
       }
        
        
       else if(length(input$predictor) == 1 && input$predictor == input$response &&
                nrow(train)==0){
          print("Add observations & other predictors ")
        }
        
       else if(length(input$predictor) == 1 && input$predictor == input$response){
          lmod2=lm(as.formula(paste(input$response, "~1")),data = train)
          summary(lmod2)
        }
        
       else if (nrow(train)==0){
          print("Add observations")
        } 
        
       else{
         newPredictors = setdiff(input$predictor, input$response)
         preds=paste(newPredictors, collapse = " + ")
         lmodString=as.formula(paste(input$response,'~',preds))
         resps=as.formula(paste(input$response,'~1'))
         lmod1=lm(lmodString,data=train)
         summary(lmod1)
       }

        
     
      })
      
      
      output$varimp=renderPrint({
        if(length(input$predictor)==0 || 
           length(input$predictor) == 1 && input$predictor == input$response) {
           print("Choose predictors to model on the left")
        }
        
        else if(nrow(train)==0){
          print("Add observations")
        }
        
        else{
          newPredictors = setdiff(input$predictor, input$response)
          preds=paste(newPredictors, collapse = " + ")
          lmodString=as.formula(paste(input$response,'~',preds))
          lmod1=lm(lmodString,data=train)
          importance=as.data.frame(varImp(lmod1))
          importance$names=rownames(importance)
          ordered1=importance[order(importance[,1], decreasing = T),]
          rownames(ordered1)=NULL
          ordered1
          
        }
      })
      
      output$alias=renderPrint({
        if(length(input$predictor)==0 || 
           length(input$predictor) == 1 && input$predictor == input$response){
           print("Choose predictors to model on the left")
        }
        
        else if(nrow(train)==0){
          print("Add observations")
        }
        
        else {
          newPredictors = setdiff(input$predictor, input$response)
          preds=paste(newPredictors, collapse = " + ")
          lmodString=as.formula(paste(input$response,'~',preds))
          lmod1=lm(lmodString,data=train)
          ali=alias(lmod1)$Complete
          
          if (is.null(ali)==TRUE){
            print("Nothing to show")
          }
          else {
            indx=which(abs(ali)>1,arr.ind = TRUE)
            rowq=rownames(ali)[indx[,1]]
            colq=colnames(ali)[indx[,2]]
            cat("Frequency of Redundant Variables")
            table(cbind(rowq,colq))
            
          }
        }
        
        
      })
      
      
      output$residsPlot=renderPlot({
        if(length(input$predictor)==0 || 
           length(input$predictor) == 1 && input$predictor == input$response){
          return(NULL)
        }
        
        else if(nrow(train)==0){
          return(NULL)
        }
        
        else{
          newPredictors = setdiff(input$predictor, input$response)
          preds=paste(newPredictors, collapse = " + ")
          lmodString=as.formula(paste(input$response,'~',preds))
          lmod1=lm(lmodString,data=train)
          par(mfrow = c(2, 2))
          plot(lmod1)
          par(mfrow = c(1, 1))
        }

      })
        
      
      
      
      
    })
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
