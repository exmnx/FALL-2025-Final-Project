#
#
## Emily Mendez
#
##installing packages: 
#packages=c("shiny","shinydashboard","tidyverse","leaflet","giscoR","zoo","corrplot","reshape2","DT","caret","stargazer","olsrr")
#dif=setdiff(packages,rownames(installed.packages()))
#if(length(dif)>0){install.packages(dif)}


#Reading in libraries 
library(shiny)
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


#linked data to github
#Example DATASET
data23=read.csv("https://raw.githubusercontent.com/exmnx/FALL-2025-Final-Project/refs/heads/main/ProjectContents/Rainfall_Data.csv",stringsAsFactors = TRUE)

#added extra column for calculating time easier, later changed to add days (starts on 1).
data23$dates=as.yearmon(paste(data23$Year, data23$Month), "%Y %m")

#Changed names of variables since inconvenient variable names
colnames(data23)=c("City","Latitude","Longitude","Month","Year","Rainfall",
                   "Elevation","Climate","Temperature","Humidity","dates")


# UI for application 
ui <- dashboardPage(
          skin="blue", #changing color of app 
          
          
          #title of dashboard app
          dashboardHeader( 
              title = "Rainfall Insights on German Cities ",
              titleWidth = 350,
              
             # created a message board that sends messages for changing tabs (server) 
              dropdownMenuOutput("messageMenu")),
          
          
          #Creating tabs included in the dashboard
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
          
          
          #Dashboard body includes all tabs 
          dashboardBody(#rows have width of 12
              
            #Global in dashboard body for all info boxes in HTML
              tags$head(
                  tags$style(HTML("
                      .info-box .info-box-number{font-size:25;}
                      .info-box .info-box-text{font-size:40px;}
                      ") 
                      )
              ),
       
            
              #Tab items begin here 
              tabItems(
                  
                
                
          ##DATA OVERVIEW 
                  tabItem(tabName = "DataO",h2("Data Overview"),
                          
                          br(),
                          
                          #Value box to explain the # of variables 
                          fluidRow(
                              valueBox(length(colnames(data23[,-11])),
                                       "Total Variables",icon=icon("hashtag", style="color:white;opacity:0.5;"),width=12,color="navy")
                          ),
                          
                          #Manually inputted from Data description in server to create a table.
                          fluidRow(
                             div(
                              DTOutput("IndicatorChart"),
                              style=("margin:20px;"))
                          ),
                         
                         br(),
                         
            
                         #Info box to explain the NA count & factored variables 
                          fluidRow(
                            infoBoxOutput("NABox",width = 6),
                            infoBoxOutput("factoredVars",width = 6)
                          ),
                         
                         
                         #header for map
                         h3("Germany Map"),
                         
                         #Map from leaflet package with text on the bottom stating the city names
                         fluidRow(
                             box(title="German Cities in the Dataset",width = 12,solidHeader = T, status = "primary",
                                 leafletOutput("Leafmap1"), paste("Cities on the map:",paste(unique(data23$City),collapse = ", ")))
                         ),
                         
                         
                         #Info box to state the City count 
                         fluidRow(
                             infoBox(length(unique(data23$City)),"Cities" , icon=icon("building",style="opacity:0.5;"),width=12,color="navy")
                         ),
                         
                         
                         #Value Boxes to state the climate, month and year count
                         fluidRow(
                               valueBox(length(unique(data23$Climate)),"Climates",icon=icon("cloud-sun", style="color:white;opacity:0.5;"),color="navy"),
                               valueBox(length(unique(data23$Month)),"Months" , icon=icon("clock", style="color:white;opacity:0.5;"),color="navy"),
                               valueBox(length(unique(data23$Year)),"Years", icon=icon("clock", style="color:white; opacity:0.5;"),color="navy")
                          ),
                         
                         
                         #Starting Elevation Data Part
                         h3("Elevation"),
                         
                         #Plot of country; reactive plot that allows for you to click a city and look at values(Server)
                         fluidRow(
                               box(title="Elevation Points by City",width = 12,solidHeader = T, status = "primary",
                                   plotOutput("ElevationMap",click="plotClick"))
                            ),
                         
                         #Value box returning the elevation and city name of the data
                         fluidRow(
                               valueBoxOutput("ChangeName",width=12)   
                             ),
                         
                         ##Value box returning the avg total temperature,rainfall,and humidity and city name of the data
                         fluidRow(
                              valueBoxOutput("temper"),
                              valueBoxOutput("rainfall"),
                              valueBoxOutput("humid")
                          )
                         
                         ),
                  
                  
                  
                  
                  
          ##DATA EXPLORATION TAB
               
                  tabItem(tabName = "DE",h2("Data Exploration"),
                          
                          #Plot of Rainfall or any selected y variable renders with dates on the bottom 
                          #The plot point or label is the first 2 letters of the month, since factoring the same level 
                          #gave me less colors so J would only be one color for each J month. so 2 letters.
                          fluidRow(
                                box(title="Choose From Dates, Cities and Variables Below",width = 12,solidHeader = T, status = "primary",
                                    plotOutput("RainfallCityPlot")),
                            ),
                          
                          
                          #These are reactive inputs for my plot from above, they select dates ranges and cities 
                          #include january 1st to include it january, etc for months since the date-time starts then
                          fluidRow(
                                box(status = "primary", height = 100,
                                    dateRangeInput("DateRange",label = "Select Date Range:", min=min(as.Date(data23$dates)),
                                                   max=max(as.Date(data23$dates)),startview="year",format="mm/dd/yyyy")),
                                box(status = "primary",height = 100,
                                    selectInput("CityChoose",label = "Choose a City:",choices = unique(data23$City))),
                           ),
                          
                          
                          #These are more reactive inputs for my plot from above, here we can choose a variable to model on the y axis
                          # There is an action button here to render the plot
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
                          
                          
                          #Here i have box and whisker plots that model relationships between 2 variables 
                          h3("Bivariate Plots"),
                          
                          #We choose variables here to model on the plot
                          fluidRow(
                              column(width=6, 
                                      box(status = "primary",width = NULL,height=200,
                                      selectInput("VarchooseBXPY",label = "Choose a Variable to Model on the Y Axis:",
                                                  choices = colnames(data23[,c(6,9,10)]))),
                                     
                                     box(status = "primary",height=200,
                                         width = NULL, selectInput("VarchooseBXPX", label = "Choose a Variable to Model on the X Axis:",
                                                                        choices = colnames(data23[,c(1,8)]))), 
                              ),
                              
                              
                              #The plot will be created and put into this row on the right 
                              box(title="Choose From Variables on the Left",width = 6,solidHeader = T, status = "primary",
                                      plotOutput("Boxplot1"))
                              
                          ),
                          
                          br(),
                          
                          
                          #Next plot is the total average like in the data overview tab from the bottom plot's valueboxes
                          #These plots will be histograms to see the total averages
                          h3("Total Average for Each City"),
                          
                          
                          #Plot UI information, and using reactive input widgets to allow for variable selection on plots
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
                          
                          
                          #correlation matrix to view correlations between variables upper triangle plot filled with circles 
                          h3("Correlation Matrix"),
                          
                          fluidRow(
                              box(title = "Correlation Matrix of Variables",width = 12,solidHeader = T,status = "primary",
                                  plotOutput("CorrMat"))
                          ),
                          
                          fluidRow(
                              infoBoxOutput("corrBox",width = 12)
                          )
                          
                      ),
                  
               
               
            ##MODEL FITTING Linear Regression TAB
            
                  
                  #Tab ui header name
                  tabItem(tabName = "LRM",h2("Robust Model Fitting With Variable Selection: Linear Regression Model"),
                          
                          
                          #These are the important input widgets that will be used to model our OLS model
                          #Here we create a select input for our response and predictors, with the removal of month and year
                          #Users have the opportunity to select the training and testing partition this was inspired by rifayat showrav dashboard design/ code: 
                          #Machine learning app with shiny. this article will walk through how to… | by rifayat showrav | medium. (n.d.). https://medium.com/@rshowrav/machine-learning-app-with-shiny-8c088f2f4646 
                          
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
                            
                            #Here we have the different tabs needed for linear regression 
                            #the user can choose what variables to look at even if that includes looking at inaccurate outcomes due to singularities caused by  unidentifiability. 
                            #But the other tabs will help users identify problems such as the model, diagnostics, and prediction tab  
                            
                            
                            tabBox(id="tabBoxLM",width=9,
                              tabPanel("Data", 
                                       DTOutput("DataChart")),
                              
                              tabPanel("Summary Statistics", br(),
                                       div(style = "text-align: center;",verbatimTextOutput("sum"),
                                       verbatimTextOutput("summa"))
                                       ),
                              
                              #Centering the text output
                              tabPanel("Model",
                                       div(style="text-align: center;width=100%",
                                       verbatimTextOutput("Lmod"), br(),
                                       verbatimTextOutput("varimp"), br(),
                                       verbatimTextOutput("alias")
                                       )),
                              
                              tabPanel("Diagnostics", plotOutput("residsPlot"),title = "Diagnostic Plots", 
                                       div(style = "text-align: center;" ,verbatimTextOutput("Vif"))),
                              
                              tabPanel("Prediction",plotOutput("predictPlot"),br(),
                                       verbatimTextOutput("TrainError"))
                            )
                            
                          )
                        
                        )
                            
            ),
          )
)
    
##UI ends here 








# Server Starts here 


server <- function(input, output, session) {
  
              
     #Dashboard and sidebar 
  
                #This allows to create a dropdown message menu that reacts to clicking tabs
                react=reactiveValues(messy=list(
                    messageItem("Admin","Hello, Welcome to the Dashboard!",time=format(Sys.time(),"%H:%M:%S"))),
  
                    #stores the number of times clicked starting at 0: Useful for allowing  me to send messages to the user
                    #whenever they click on a tab.
                    count=list(DataO=0,DE=0,LRM=0)
                    )
                
                
                #Creating the event listed above observing when sidebar names are clicked then..
                observeEvent(input$SideBar_Names,{
                  
                    #Allowing whenever the sidebar is clicked it increments by 1 
                    react$count[[input$SideBar_Names]]=react$count[[input$SideBar_Names]]+1
                    
                    #first time clicking allows this to activate: 
                    if(react$count[[input$SideBar_Names]]==1){
                  
                    #A message is created saying you found a new tab and prints the system time. 
                      react$tabCount=input$SideBar_Names
                          mess=messageItem("ReactiveBot",paste("You found a tab! Explore the",input$SideBar_Names,"tab!"),
                                         time=format(Sys.time(), "%H:%M:%S"))
                          react$messy=c(react$messy,list(mess))}
                    })
                
                #Creating the menu. 
                output$messageMenu=renderMenu({
                    dropdownMenu(type = "messages", .list = react$messy)
                })
                
                
                
                
      #Data Overview          
                
                #Rendering the leaflet map of the country of interest where lat and long are variables
                output$Leafmap1=renderLeaflet({
                    lat=data23$Latitude
                    lon=data23$Longitude
                    
                    leaflet(data23) %>% 
                        addTiles() %>% 
                        addMarkers(lng = lon, lat=lat, popup=data23$City) %>% 
                        addMiniMap(width = 150, height = 150)
                })
                
                
                #Rendering the Elevation map that allows users to click on points
                output$ElevationMap=renderPlot({
                    germany_counties <- gisco_get_nuts(
                        year = "2021", #only 2016,2021, 2024 available, data from 2015-2023, most recent
                        epsg = "4326", #gets coordinates
                        resolution = "10",
                        country = "DE", 
                        nuts_level = "3" #regions
                    )
                    
                    #Again nother hard code from the dataset itself, where elevation is a variable that exists in the df.
                    Elevation_m=data23$Elevation
                    
                    #ggplot where we take the country map above which gives us county lines and use it for our overall map
                    #here we plot long and lat, and change the color of the point according to elevation
                    ggplot()+
                        geom_sf(data=germany_counties,color = "black",fill="white")+
                        geom_point(data=data23,size=5,shape = 16, fill = NA,
                                   aes(Longitude,Latitude,color=Elevation_m))+
                        scale_color_viridis_c()+
                        coord_sf()+
                        labs(title = "Elevation in 10 German Cities", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")
                    
                })
                
                    #These value boxes below change their texts and and values according to the plots on the plot above
                    #though currently here these are the boxes you will see when you start up the tab and not click anything
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
                    
                    
                    
                    #Here we will view and code the changes the value boxes will make corresponding to which point we 
                    #click on the graph 
                    observeEvent(input$plotClick,{
                      options(warn = -1)
                      
                        # We assign the clicker to get a value when you click on a point 13 pixeles near the original point on the graph  
                        clicker=nearPoints(data23, input$plotClick, xvar="Longitude",yvar="Latitude",threshold = 13)
                        if (nrow(clicker) == 0){return()}
                        
                        #If you click on the point you will change the value box value and text.
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
                
                    
                    
                   #Creating a table to show the names and descriptions of the variables in the dataset. 
                   #These are taken directly from their original datasource: Kaggle data overview.
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
                
                    
                   #info box created to count NAs in data
                   output$NABox=renderInfoBox({
                      NAVal=sum(is.na(data23))
                      infoBox(NAVal,"NA Values" , 
                              icon=icon("creative-commons-zero",style="opacity:0.5;"),width=12,color="navy")
                    })
                    
                   
                   ##info box created to count factored variables
                    output$factoredVars=renderInfoBox({
                      tempvar=c()
                      for(i in colnames(data23)){
                        if(!is.null(levels(data23[,i]))){
                          tempvar=c(tempvar,i)
                        }}
                      infoBox(length(tempvar),"Factored Variables with Levels" , 
                              icon=icon("plus",style="opacity:0.5;"),width=12,color="navy")
                    })
                
                
     ##Data Exploration       
                
                
                    
                    #We render a plot here modeling the rainfall of Berlin from 2015-2023 as the original display when 
                    #not clicking any of the inputs 
                    output$RainfallCityPlot=renderPlot({
                        pchMonth=substr(format(data23$dates, "%b"),1,2)
                        data23$PchVal=pchMonth[1:length(data23$City)]
                        
                        #Berlin hardcoded.
                        Datasub=data23 %>% 
                            filter(City=="Berlin")
                        
                        monthLetters=factor(Datasub$PchVal)
                        
                        ggplot(Datasub)+
                            aes(x=dates,Rainfall)+
                            geom_line()+
                            geom_text(aes(label = PchVal,color= monthLetters,vjust=-1))+
                            ggtitle(paste0(Datasub$City[1]," Rainfall over ",min(Datasub$Year),"-",max(Datasub$Year)))
                    })
                    
                    
                    
                    #Here we observe the event when we click on the render plot action button
                    #We filter the data based on the dates clicked from the daterange input so we use our yearmon variable we created 
                    #and turn it into date to compare better. 
                    #BUT you need to include the first of the month for the plot to render that specific month
                    observeEvent(input$ClickMe,{
                      req(input$DateRange) #Checking if it was clicked
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
                            
                            #creating plot point labels for months, with 2 letters
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
                    
                    
                    
                    ##Box and Whiskers plot
                    #Changing the plot when you choose a Y variable, and rendering a boxplot
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
                    
                    
                    ##Box and Whiskers plot
                    #Changing the plot when you choose a X variable,and rendering a boxplot
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
                
                
                    
                    ##Correlation matrix plot, creating an upper triangle plot with red/blue circles indicating the 
                    #severity of the correlation between variables
                    output$CorrMat=renderPlot({
                        corData=cor(data23[,-c(1,8,11)])
                        corrplot(corData,method="circle",type="upper",
                                 title="Correlation Matrix",
                                 mar=c(0,0,2,0))
                    })
                    
                    
                    #Info Box that outputs the variables that are correlated
                    #Though the text from the info box only displays the first row of correlated variables
                    #Makes sense when you have one pair since they are repeated, but if there are more it needs to be rewritten
                    output$corrBox=renderInfoBox({
                        corData=cor(data23[,-c(1,8,11)])
                        melty=melt(corData)
                        
                        #threshold made to .6 for high moderate to high correlation
                        absVar=abs(melty$value)>0.6
                        newCorrMat=melty[absVar,]
                        finalCorMat=newCorrMat[newCorrMat$value<1,]
                    
                        infoBox(length(finalCorMat$Var1),subtitle=paste("The number of variables with moderate to high correlation. They are ",
                                      finalCorMat[1,c(1,2)]$Var1," and ",finalCorMat[1,c(1,2)]$Var2 ),
                                 icon=icon("chevron-right",style = "color:white; opacity:0.5;"),color="navy")
                    })
                    
                    
                    
                    ##Histogram changes each time a y variable is selected
                    observeEvent(input$HistVarY,{
                      
                      #sub-setting data to be grouped by the the X variable to get averages across
                      dataTempMat=data23 %>% 
                        group_by_at(input$HistVarX) %>% 
                        #using sym to turn input from user to symbol then unquoting it. 
                        #since the input only returns the name, same for group_by_ above
                        summarise(Avg=mean(!!sym(input$HistVarY)))
                      
                      Histodata=as.data.frame(dataTempMat)
        
                      #plotting the histogram
                      output$Histogram1=renderPlot({
                          X_Var=Histodata[,1]
                          ggplot(Histodata)+
                              aes(X_Var,Avg,fill=X_Var)+
                              geom_bar(stat = "identity")+
                              ggtitle(paste0(input$HistVarX," & ",input$HistVarY ," plot"))
                      })
                    })
                    
                    
                    ##Histogram changes each time a X variable is selected
                    observeEvent(input$HistVarX,{
                      dataTempMat=data23 %>% 
                        group_by_at(input$HistVarX) %>% 
                        summarise(Avg=mean(!!sym(input$HistVarY)))
                      
                      Histodata=as.data.frame(dataTempMat)
                      
                      #plotting the histogram
                      output$Histogram1=renderPlot({
                        X_Var=Histodata[,1]
                        ggplot(Histodata)+
                          aes(X_Var,Avg,fill=X_Var)+
                          geom_bar(stat = "identity")+
                          ggtitle(paste0(input$HistVarX," & ",input$HistVarY ," plot"))
                      })
                    })
                    
                  
                    
                    
                    
    ##Linear Model ALL tabs connected to the same input widgets... 
                    
             ##Observe event (reactive) from linear regression model; tab connects to all tabs
                    
             observeEvent(input$slider,{
               ##Most events have the conditions of whether the predictor is null, if the predictor has 1 var & that one variable is the response
               #and if the training index is 0; 
               
               ##The user has all the freedom to pick and choose variables with the possibility of creating inaccurate pvalues due to unidentifiability and/or multicollinearity
               #Tabs give suggestions for fixing model.
               
               ##Variables created to use throughout the outputs below. Possible to include the linear 
               #information up here too.
                  datatemp=data23[,-c(4,5,11)]
                  pc=input$slider/100
                  set.seed(123)
                  
                  #splitting data with caret
                  indx=createDataPartition(datatemp[[input$response]],times=1,p=pc,list=FALSE)
                  train=datatemp[indx,]
                  test=datatemp[-indx,]
                      
                  
                      ##DATA TAB
                          #Rendering data table of data with conditions; datatable has a horizontal scroll for small windows
                          output$DataChart=renderDT({
                            if (is.null(input$predictor)){
                              datatable(datatemp,options = list(lengthMenu = c(15, 20, 50, 70),scrollX=TRUE))
                            }
                            else{
                              datatable(train[,c(input$response,input$predictor)],options = list(lengthMenu = c(15, 20, 50, 70),scrollX=TRUE))
                            }
                          })
                          
                          
                          #Rendering text for training and testing splits counts the observations in both and returns it as text
                          output$TrainText=renderText({
                            paste("Train:",length(train[,1]),"obs.")
                          })
                          
                          output$TestText=renderText({
                            paste("Test:",length(test[,1]),"obs.")
                          })
                          
                          
                          
                     ##SUMMARY TAB   
                          
                          ##rendering a chart through the stargazer library to make a chart including 
                          #nrows, means, st deviations, mins and maxes for each specific variable
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
                          
                          
                          #Another table but the standard summary statistics for the data
                          output$summa=renderPrint({
                            summary(train[,c(input$response,input$predictor)])
                          })
                          
                          
                          
                     ##MODEL TAB      
                          
                          ##conditions are set first to counter errors from modeling the linear model. as discussed above
                        
                          output$Lmod=renderPrint({
                           if(length(input$predictor)==0){
                             print("Add Predictors")
                           }
                            
                           else if(length(input$predictor) == 1 && input$predictor == input$response &&
                                    nrow(train)==0){
                              print("Add observations & other predictors ")
                            }
                            
                          #modeling the null model if the only predictor is the response itself 
                           else if(length(input$predictor) == 1 && input$predictor == input$response){
                              lmod2=lm(as.formula(paste(input$response, "~1")),data = train)
                              summary(lmod2)
                            }
                            
                           else if (nrow(train)==0){
                              print("Add observations")
                            } 
                            
                          #modeling the linear model through pasting the model together then reading it as a formula in lm
                          else{
                             newPredictors = setdiff(input$predictor, input$response)
                             preds=paste(newPredictors, collapse = " + ")
                             lmodString=as.formula(paste(input$response,'~',preds))
                             lmod1=lm(lmodString,data=train)
                             summary(lmod1)
                           }
            
                          })
                          
                          
                          
                          #Printing the importance of variables, like how random forest has importance as well
                          #With conditions
                          output$varimp=renderPrint({
                            if(length(input$predictor)==0 || 
                               length(input$predictor) == 1 && input$predictor == input$response) {
                               print("Choose predictors to model on the left")
                            }
                            
                            else if(nrow(train)==0){
                              print("Add observations")
                            }
                            
                            else{
                              #Could have been a small global for the observe slider event
                              newPredictors = setdiff(input$predictor, input$response)
                              preds=paste(newPredictors, collapse = " + ")
                              lmodString=as.formula(paste(input$response,'~',preds))
                              lmod1=lm(lmodString,data=train)
                              importance=as.data.frame(varImp(lmod1))
                              importance$names=rownames(importance)
                              #sorting by largest to smallest
                              ordered1=importance[order(importance[,1], decreasing = T),]
                              #don't want rows names like 1,2,3
                              rownames(ordered1)=NULL
                              ordered1
                              
                            }
                          })
                          
                          
                          
                          ##printing frequency of redundant variables through alias as VIF was put in the 
                          #diagnostics page, just to help with variable selection as 
                          #city and long&lat are essentially highly related to each other. 
                          #in geographical data there will be high mulicollinarity 
                          
                          output$alias=renderPrint({
                            if(length(input$predictor)==0 || 
                               length(input$predictor) == 1 && input$predictor == input$response){
                               print("Choose predictors to model on the left")
                            }
                            
                            else if(nrow(train)==0){
                              print("Add observations")
                            }
                            
                            else { #creating the alias frequency chart. with another if else for
                              #catching if there are no redundant variables
                              newPredictors = setdiff(input$predictor, input$response)
                              preds=paste(newPredictors, collapse = " + ")
                              lmodString=as.formula(paste(input$response,'~',preds))
                              lmod1=lm(lmodString,data=train)
                              ali=alias(lmod1)$Complete
                              
                              if (is.null(ali)==TRUE){
                                print("Frequency of Redundant Variables:0")
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
                          
                          
                          
                          #rendering the diagnostic plots, easier without ggplot ... 
                          #normqq, cooks distance, resids vs fitted, etc.
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
                            
                          
                          #printing vif out to check the variance inflation factor along with its tolerance
                          #Like we said before a lot of collinarity 
                          output$Vif=renderPrint({
                            if(length(input$predictor)==0 || 
                               length(input$predictor) == 1 && input$predictor == input$response){
                              print("Choose predictors to model on the left")
                            }
                            
                            else if(nrow(train)==0){
                              print("Add observations")
                            }
                            
                            else if (length(input$predictor)==1){
                              print("Two or more predictors for this VIF")
                            }
                            
                            else {
                              
                              newPredictors = setdiff(input$predictor, input$response)
                              preds=paste(newPredictors, collapse = " + ")
                              lmodString=as.formula(paste(input$response,'~',preds))
                              lmod1=lm(lmodString,data=train)
                              ols_vif_tol(lmod1)}
                            
                          })
                          
                          
                          
                          #creating a table to check training and testing error
                          output$TrainError=renderPrint({
                            if(length(input$predictor)==0 || 
                               length(input$predictor) == 1 && input$predictor == input$response){
                              print("Choose predictors to model on the left")
                            }
                            
                            else if(nrow(train)==0){
                              print("Add observations")
                            }
                            
                            else {
                              #suppressing warnings.. 
                              options(warn = -1)
                              
                              #linear model fitting based on inputs
                              newPredictors = setdiff(input$predictor, input$response)
                              preds=paste(newPredictors, collapse = " + ")
                              lmodString=as.formula(paste(input$response,'~',preds))
                              lmod1=lm(lmodString,data=train)
                              test1=test[, setdiff(names(test), input$response)]
                              
                              #predicting 
                              pred=predict(lmod1,newdata=test1)
                              testEr=mean((pred-test[,input$response])^2)
                              trainpred=predict(lmod1,newdata=train)
                              trainEr=mean((trainpred-test[,input$response]))
                              
                              #creating a df to display values
                              df=data.frame(names=c("Test Error:","Train Error:"),
                                            error=c(testEr,trainEr))
                              colnames(df)=NULL
                              print("Prediction from a rank-deficient fit may be misleading")
                              print(df)
                              
                              }
                            })
                          
                          
                          #rendering predicted plot... with conditions 
                          output$predictPlot=renderPlot({
                            if(length(input$predictor)==0 || 
                               length(input$predictor) == 1 && input$predictor == input$response){
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
                              test1=test[, setdiff(names(test), input$response)]
                              pred=predict(lmod1,newdata=test1)
                              testEr=mean((pred-test[,input$response])^2)
                              plot(test[,input$response],pred,
                                   main = "Best Fit Line",
                                   xlab = "Actual",
                                   ylab = "Predicted")
                            }
                           
                          })
                          
                          
                          
                        })
                      
                        
          
    
}

# Run the application 
shinyApp(ui = ui, server = server)
