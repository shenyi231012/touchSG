library(jsonlite)
library(ggplot2)
library(stringr)
library(shiny)
library(leaflet)
library(XML)
library(plotly)
library(leaflet.extras)
library(RJSONIO)
library(data.table)
raindata<-read.csv("rainfall-monthly-total.csv")
year<-str_sub(raindata$month,1,4)
year<-as.numeric(year)
raindata<-cbind(year=year,raindata)
raindata$month<-str_sub(raindata$month,6,7)


url<-"https://api.data.gov.sg/v1/environment/2-hour-weather-forecast"
data<-jsonlite::fromJSON(url)
forecast<-as.data.frame(data$items$forecasts)
area<-data$area_metadata
Area<-area$label_location
Area$name<-area$name
Area$forecast<-paste0(Area$name,":",forecast$forecast)



ui<-fluidPage(
  titlePanel("来新加坡旅游，你需要知道"),
  tabsetPanel(id="ui",       tabPanel("降雨量",pageWithSidebar(
                              headerPanel(""),
                               sidebarPanel(radioButtons(inputId = "sum",label="（降雨量）总览/按年份",
                                                           choices = c("总览","按年份")),
                                            numericInput(inputId = 'rain_year',label = 'year',value = 2009)),
                               mainPanel (plotOutput("rain"))
                               )),
                             tabPanel("酒店位置",leafletOutput("hotel")),
              
                             tabPanel("景点位置",pageWithSidebar(
                               headerPanel(""),
                               sidebarPanel(selectInput("selectmap","选择你要查看的地图",c("旅游景点","图书馆","博物馆","公园"))),
                               mainPanel(leafletOutput("sight"))
                              )),
                             
                             tabPanel("2小时天气预报",leafletOutput("plotMap")),
                             
                             tabPanel("4天天气预报",pageWithSidebar(
                               headerPanel(""),
                               sidebarPanel(radioButtons(inputId = "Input",label="（4天天气预报）温度/湿度/风速",
                                                         choices = c("Temperature","Humidity","Wind"))),
                               mainPanel(plotlyOutput("plotly"))
                                      )),
                             
                             tabPanel("每年乘飞机到达的游客数目",pageWithSidebar(
                               headerPanel(""),       
                               sidebarPanel(sliderInput(inputId = "year",label="（每年乘飞机到达的游客数目） 年份",value=2000,min=1961,max=2019)),
                                      mainPanel(plotOutput(outputId = "plot"))
                                      )),
                             
                             tabPanel("紫外线/PM2.5",pageWithSidebar(
                               headerPanel(""),       
                               sidebarPanel(selectInput("Type","选择你想看数据",c("Ultra-violet Index","PM2.5")),
                                                   dateInput(inputId = "date", "日期：", value = Sys.Date(),min = "2016-04-12",max = Sys.Date())),
                                      mainPanel(plotOutput("plotIndex"))
                                      ))
                             
  ))


server<-function(input,output)
{
  
  #数据预处理
  raindata<-read.csv("rainfall-monthly-total.csv")
  year<-str_sub(raindata$month,1,4)
  year<-as.numeric(year)
  raindata<-cbind(year=year,raindata)
  raindata$month<-str_sub(raindata$month,6,7)
  
  
  url<-"https://api.data.gov.sg/v1/environment/2-hour-weather-forecast"
  data<-jsonlite::fromJSON(url)
  forecast<-as.data.frame(data$items$forecasts)
  area<-data$area_metadata
  Area<-area$label_location
  Area$name<-area$name
  Area$forecast<-paste0(Area$name,":",forecast$forecast)
  
  #降雨量查询
  output$rain<-renderPlot(
    if(input$sum=="总览")
    {ggplot(raindata,aes(x=month,y=total_rainfall,group=1))+geom_point()+geom_smooth()}
    else 
    {ggplot(raindata[which(raindata$year==input$rain_year),],aes(x=month,y=total_rainfall,group=year))+geom_line()}
  )

  #酒店位置
  output$hotel<-renderLeaflet(
      {a<-leaflet()
    b<-addTiles(a)
    hotel<-jsonlite::fromJSON("hotels-geojson.geojson")
    d<-hotel$features$geometry$coordinates
    location<-unlist(d)
    lng_hotel<-location[seq(1,1272,3)]
    lat_hotel<-location[seq(2,1272,3)]
    new_hotel<-as.data.frame(cbind(lng_hotel,lat_hotel))
    #print(new_hotel)
    addMarkers(b,lat=new_hotel$lat_hotel,lng=new_hotel$lng_hotel)}
  )
  
  #景点位置
  output$sight <- renderLeaflet(
    if(input$selectmap == "旅游景点")
    {
      tour.location <- read.csv("tour.csv")
      m <- leaflet() %>% addTiles()
      m <- addMarkers(m, 
                      lng=tour.location$longitude ,
                      lat=tour.location$latitude, 
                      popup=tour.location$name, 
      )
    }
    else if(input$selectmap == "图书馆")
    {
      library.location <- read.csv("library.csv")
      m <- leaflet() %>% addTiles()
      m <- addMarkers(m, 
                      lng=library.location$longitude ,
                      lat=library.location$latitude, 
                      popup=library.location$name, 
      )
    }
    else if(input$selectmap == "博物馆")
    {
      museum.location <- read.csv("museum.csv")
      m <- leaflet() %>% addTiles()
      m <- addMarkers(m, 
                      lng=museum.location$longitude ,
                      lat=museum.location$latitude, 
                      popup=paste(museum.location$name,museum.location$intro,sep = "<br/>"), 
      )
    }
    else if(input$selectmap == "公园")
    {
      park.location <- read.csv("park.csv")
      m <- leaflet() %>% addTiles()
      m <- addMarkers(m, 
                      lng=park.location$longitude ,
                      lat=park.location$latitude, 
                      popup=park.location$name, 
      )
    }
  )
  
  # 2小时天气预报
  output$plotMap<-renderLeaflet(
    {
      
      leaflet()%>% 
      addTiles()%>%addMarkers(lng=Area$longitude,lat=Area$latitude,popup=Area$forecast)
  })
  
  #4天天气预报
  output$plotly<-renderPlotly(
    if(input$Input=="Temperature"){
      url<-"https://api.data.gov.sg/v1/environment/4-day-weather-forecast"
      data<-jsonlite::fromJSON(url)
      data<-as.data.frame(data)
      forecast<-as.data.frame(data$items.forecasts)
      temperature<-forecast$temperature
      temperature$date<-forecast$date
      temperature$num<-c(1:4)
      tem_low<-temperature[,c("low","date","num")]
      tem_low$class<-"low"
      colnames(tem_low)<-c("temperature","date","day","class")
      tem_high<-temperature[,c("high","date","num")]
      tem_high$class<-"high"
      colnames(tem_high)<-c("temperature","date","day","class")
      tem<-rbind(tem_high,tem_low)
      p<-ggplot(tem,aes(x=day,y=temperature,colour=class))+geom_line()+geom_point()+geom_text(check_overlap = TRUE,label = paste("date:",tem$date),color = "black")
      ggplotly(p)
    }
    else if(input$Input=="Humidity"){
      url<-"https://api.data.gov.sg/v1/environment/4-day-weather-forecast"
      data<-jsonlite::fromJSON(url)
      data<-as.data.frame(data)
      forecast<-as.data.frame(data$items.forecasts)
      humidity<-forecast$relative_humidity
      humidity$date<-forecast$date
      humidity$num<-c(1:4)
      hum_low<-humidity[,c("low","date","num")]
      hum_low$class<-"low"
      colnames(hum_low)<-c("humidity","date","day","class")
      hum_high<-humidity[,c("high","date","num")]
      hum_high$class<-"high"
      colnames(hum_high)<-c("humidity","date","day","class")
      hum<-rbind(hum_high,hum_low)
      p<-ggplot(hum,aes(x=day,y=humidity,colour=class))+geom_line()+geom_point()+geom_text(check_overlap = TRUE,label = paste("date:",hum$date),color = "black")
      ggplotly(p)
    }
    else{
      url<-"https://api.data.gov.sg/v1/environment/4-day-weather-forecast"
      data<-jsonlite::fromJSON(url)
      data<-as.data.frame(data)
      forecast<-as.data.frame(data$items.forecasts)
      wind<-forecast$wind$speed
      wind$date<-forecast$date
      wind$num<-c(1:4)
      wind$direction<-forecast$wind$direction
      wind_high<-wind[,-1]
      wind_high$class<-"high"
      colnames(wind_high)<-c("windspeed","date","day","direction","class")
      wind_low<-wind[,-2]
      wind_low$class<-"low"
      colnames(wind_low)<-c("windspeed","date","day","direction","class")
      wind<-rbind(wind_high,wind_low)
      p<-ggplot(wind,aes(x=day,y=windspeed,colour=class))+geom_line()+geom_point()+geom_text(check_overlap = TRUE,label =paste0("direction:",wind$direction,"\n","date:",wind$date),color = "black")
      ggplotly(p)
    }
  )
  
  
  #年度飞机乘客到达查询
  Data<-reactive({
    data<-read.csv("total-air-passenger-arrivals.csv")
    data$year<-as.integer(substr(data$month,1,4))
    data$month<-substr(data$month,6,7)
    YEAR<-as.integer(input$year)
    Data<-data[data$year==YEAR,]
    Data$year<-as.character(Data$year)
    Data
  })
  output$plot<-renderPlot(
    ggplot(Data(),aes(x=month,y=value))+geom_bar(stat="identity",fill='lightblue')
  )
  
  
  #紫外线，PM2.5
  output$plotIndex <-renderPlot(
    {
      if(input$Type == "Ultra-violet Index"){
        date <- as.character(input$date)
        uv_url = "https://api.data.gov.sg/v1/environment/uv-index?date="
        uv_url <- paste0(uv_url,date)
        uv_index <- fromJSON(uv_url)
        uv<-rbindlist(uv_index$items[[length(uv_index$items)]]$index,fill = T)
        ggplot(uv) + geom_line(aes(x=substring(uv$timestamp,12,16), y = uv$value,group = 1)) + labs(x = "Time",y = "Ultra-violet Index")
      }
      else{
        date <- as.character(input$date)
        pm_url = "https://api.data.gov.sg/v1/environment/pm25?date="
        pm_url <- paste0(pm_url,date)
        pm_index <- fromJSON(pm_url)
        pm<-rbindlist(pm_index$items,fill = T)
        pm.data<-cbind(pm,data.frame(t(rbindlist(list(pm$readings)))))
        ggplot(pm.data,aes(x = substring(pm.data$timestamp,12,16))) + geom_line(aes(y = pm.data$X1,group = 1,colour = "West"))+geom_line(aes(y = pm.data$X2,group = 1,colour = "East"))+geom_line(aes(y = pm.data$X3,group = 1,colour = "Central"))+geom_line(aes(y = pm.data$X4,group = 1,colour = "South"))+geom_line(aes(y = pm.data$X5,group = 1,colour = "North"))+ labs(x = "Time",y = "PM2.5 value, μg/m3")+scale_color_manual("",values = c("West"="red","East"="green","South"="yellow","Central"="black","North"="blue"))
      }
    }
  )
}
  
shinyApp(ui=ui,server = server)
