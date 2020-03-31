#Interactive Covid-19 Case Data Dashboard with R shiny
#Jean-Sebastien Paul
#

#install.packages("shiny")
library(shiny)
#install.packages("rworldmap")
library(rworldmap)
#install.packages("lubridate")
library(lubridate)
#install.packages("utils")
library(utils)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)
#install.packages("ggplot2")
library(ggplot2) 

#get data
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
geographicdata=read.csv(tf)
#user improvement
geographicdata$countriesAndTerritories=gsub("_"," ",geographicdata$countriesAndTerritories)

#JHU data, could be made for bubble maps and greater depth in big countries later
#jhucases=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#jhudeaths=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#jhurec=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

#get latest date
N=nrow(geographicdata)
attach(geographicdata)
big=as.Date(as.Date(paste(year[1],month[1],day[1],sep="-"),"%Y-%m-%d"))
for(k in 2:N){
    j=as.Date(paste(year[k],month[k],day[k],sep="-"),"%Y-%m-%d")
    if(j>big){
        big=j
    }        
}

#determine cumulative deaths and cases
track=countryterritoryCode[N]
dsum=0
csum=0
#exploiting how data is arranged by time
for(k in N:1){
    if(track==countryterritoryCode[k]){
        dsum=dsum+deaths[k]
        csum=csum+cases[k]
    }
    else{
        track=countryterritoryCode[k]
        dsum=deaths[k]
        csum=cases[k]
    }
    geographicdata$deaths2[k]=dsum
    geographicdata$cases2[k]=csum
}
#lets get the recovery data
#big1=as.Date(paste(month(big),day(big),year(big),sep="-"),"%m-%d-%Y")
#check if JHU hasnt posted new data but europa has
#if(http_error(paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",time[t],".csv",sep=""))){
#    big1=big-1
#}
#time=seq(as.Date("1-22-2020","%m-%d-%Y"), big1, by=1)
#for(t in 1:length(time)){
#    link=paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",paste(substr(time[t],6,7),substr(time[t],9,10),substr(time[t],1,4),sep="-"),".csv",sep="")
#    data=read.csv(link)
#}


detach(geographicdata)
#create a new data frame but for entire world!
world = geographicdata[0,]
time=seq(as.Date("2019-12-31","%Y-%m-%d"), big, by=1)
for(t in 1:length(time)){
    i=time[t]
    thatday=subset(geographicdata,day==day(i)&month==month(i)&year==year(i))
    world[t,]=NA
    world$day[t]=day(i)
    world$month[t]=month(i)
    world$year[t]=year(i)
    world$cases[t]=sum(thatday$cases)
    world$cases2[t]=sum(thatday$cases2)
    world$deaths[t]=sum(thatday$deaths)
    world$deaths2[t]=sum(thatday$deaths2)
    world$countriesAndTerritories[t]="Worldwide"
}

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 Worldwide Cases Interactive Dashboard"),

    # Sidebar with a slider input for dates 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "DatesMerge",
                "Date:",
                step = 1,
                min = as.Date("2019-12-31","%Y-%m-%d"),
                max = as.Date(big,"%Y-%m-%d"),
                value=as.Date(big),
                timeFormat="%Y-%m-%d"
            ),
            
            checkboxInput("cum", label = "Cumulative? ", value = TRUE),
            
            selectizeInput(
                "place",
                'Only Interested in a specific country? Pick here!',
                choices = c("World","All Countries  (WARNING SLOW)", geographicdata$countriesAndTerritories)
            )
            
        ),
        

        # Show the plots
        mainPanel(
          plotOutput("Plot1"),
          plotlyOutput("Plot2"),
          plotOutput("Plot3"),
          plotlyOutput("Plot4"),
          br(),
          br(),
          h1("Key Numbers"),
          h2("Total Confirmed Cases:"),
          h2(textOutput("Tcases"),style="color:purple"),
          h2(textOutput("getdate")),
          h2(textOutput("Ncases"),style="color:purple"),
          h2("Total Deaths"),
          h2(textOutput("Tdeaths"),style="color:red"),
          h2("New Deaths:"),
          h2(textOutput("Ndeaths"),style="color:red"),
          h2(textOutput("pop1")),
          h2(textOutput("pop2"),style="color:#005082"),
          br(),
          br(),
          br(),
          br()
            
        ),
        
                

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    Date<-reactive({
        input$DatesMerge
        })
    
    moderndata=reactive({
        subset(geographicdata, day<=day(Date())&month<=month(Date())&year<=year(Date()))
        })
    
    
    today=reactive({
        subset(moderndata(), day==day(Date())&month==month(Date())&year==year(Date()))
    })
    
    output$getdate=renderText({
        paste("New Cases On ",Date(),":",sep="")
    })
    
    output$Plot1 <- renderPlot({
        
        moderndata=moderndata()
        if(!(input$place=="World"|input$place=="All Countries  (WARNING SLOW)")){
            moderndata=subset(moderndata(),countriesAndTerritories==input$place)
        }
        
        mapped_data <- joinCountryData2Map(moderndata, joinCode = "ISO3", nameJoinColumn = "countryterritoryCode")
        
        if(input$cum==T){
            toplot="cases2"
            title="Total Confirmed Covid-19 Cases Worldwide"
        }
        else{
            toplot="cases"
            title="New Confirmed Covid-19 Cases Worldwide"
        }
        
        mapCountryData(
            mapped_data, 
            nameColumnToPlot = toplot, 
            catMethod=c(0,50,150,1000,5000,10000,30000,100000000),
            colourPalette = c("#FFFACD","#FFDAB9","#FFA07A","#FA8072","#DC143C","#B22222","#100c08"),
            oceanCol="lightblue",
            missingCountryCol="white",
            addLegend = F,
            mapTitle=title
            )
        addMapLegendBoxes(
            cutVector = c("Data missing","0-50","50-150","150-1000","1000-5000","5000-10000","10000-30000","30000+"),
            colourVector = c("white","#FFFACD","#FFDAB9","#FFA07A","#FA8072","#DC143C","#B22222","#100c08"),
            x="left",
            title="Legend",
            horiz = F,
            cex=1,
            pt.cex=2
            )
        
    })
    
    output$Plot2 <- renderPlotly({
        
        Sys.setlocale('LC_ALL','C')
        
        moderndata=moderndata()
        
        if(input$place=="World"){
            j="Worldwide"
            moderndata=world
            col="#c11e1e"
        }
        else if(input$place=="All Countries  (WARNING SLOW)"){
            j="For Each Country Over Time"
            col=moderndata$countriesAndTerritories
        }
        else{
            j=paste("in ",input$place," over time", sep="")
            moderndata=subset(moderndata,countriesAndTerritories==input$place)
            col=moderndata$countriesAndTerritories
        }
        
        
        if(input$cum==T){
            p=ggplot(moderndata, aes((as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d")), y = cases2, group=countriesAndTerritories, color = col, text=paste("Country: ", countriesAndTerritories,"\nDate: ", as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d"),"\nNew Cases: ",cases,"\nTotal Cases: ",cases2 ,sep="") ))
            title=paste("Cumulative Confirmed Covid-19 Cases ",j,sep="")
        }
        else{
            p=ggplot(moderndata, aes((as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d")), y = cases, group=countriesAndTerritories, color = col, text=paste("Country: ", countriesAndTerritories,"\nDate: ", as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d"),"\nNew Cases: ",cases,"\nTotal Cases: ",cases2 ,sep="") ))
            title=paste("New Confirmed Covid-19 Cases ",j,sep="")
        }
        
        p=p+geom_line() + geom_point()+theme(legend.position = "none") 
        p=p+xlab("Date") + ylab("Cases") + ggtitle(title)
        p=ggplotly(p,tooltip = "text")
        p
    })
    
    output$Plot3 <- renderPlot({
        
        moderndata=moderndata()
        if(!(input$place=="World"|input$place=="All Countries  (WARNING SLOW)")){
            moderndata=subset(moderndata(),countriesAndTerritories==input$place)
        }
        mapped_data <- joinCountryData2Map(moderndata, joinCode = "ISO3", nameJoinColumn = "countryterritoryCode")
        
        if(input$cum==T){
            toplot="deaths2"
            title="Total Confirmed Covid-19 Deaths Worldwide"
        }
        else{
            toplot="deaths"
            title="New Confirmed Covid-19 Deaths Worldwide"
        }
        
        mapCountryData(
            mapped_data, 
            nameColumnToPlot = toplot, 
            catMethod=c(0,50,150,1000,5000,10000,30000,100000000),
            colourPalette = c("#FFFACD","#FFDAB9","#FFA07A","#FA8072","#DC143C","#B22222","#100c08"),
            oceanCol="lightblue",
            missingCountryCol="white",
            addLegend = F,
            mapTitle=title
        )
        addMapLegendBoxes(
            cutVector = c("Data missing","0-50","50-150","150-1000","1000-5000","5000-10000","10000-30000","30000+"),
            colourVector = c("white","#FFFACD","#FFDAB9","#FFA07A","#FA8072","#DC143C","#B22222","#100c08"),
            x="left",
            title="Legend",
            horiz = F,
            cex=1,
            pt.cex=2
        )
        
    })
    
    output$Plot4 <- renderPlotly({
        
        Sys.setlocale('LC_ALL','C')
        
        moderndata=moderndata()
        
        if(input$place=="World"){
            j="Worldwide"
            moderndata=world
            col="#c11e1e"
        }
        else if(input$place=="All Countries  (WARNING SLOW)"){
            cat(stderr(),"cool")
            j="For Each Country Over Time"
            col=moderndata$countriesAndTerritories
        }
        else{
            j=paste("in ",input$place, sep="")
            moderndata=subset(moderndata(),countriesAndTerritories==input$place)
            col=moderndata$countriesAndTerritories
        }
        
        
        if(input$cum==T){
            p=ggplot(moderndata, aes((as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d")), y = deaths2, group=countriesAndTerritories, color = col, text=paste("Country: ", countriesAndTerritories,"\nDate: ", as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d"),"\nDeaths: ",deaths2,"\nNew Deaths: ", deaths,sep="") ))
            title=paste("Cumulative Confirmed Covid-19 Deaths ",j,sep="")
        }
        else{
            p=ggplot(moderndata, aes((as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d")), y = deaths, group=countriesAndTerritories, color = col, text=paste("Country: ", countriesAndTerritories,"\nDate: ", as.Date(paste(year,month,day,sep='-'),"%Y-%m-%d"),"\nDeaths: ",deaths2,"\nNew Deaths: ", deaths,sep="") ))
            title=paste("New Confirmed Covid-19 Deaths ",j,sep="")
        }
        
        p=p+geom_line() + geom_point()+theme(legend.position = "none") 
        p=p+xlab("Date") + ylab("Deaths") + ggtitle(title)
        p=ggplotly(p,tooltip = "text")
        p
    })
    
    output$Tcases = renderText({
        
        moderndata=today()
        
        if(input$place=="World"|input$place=="All Countries  (WARNING SLOW)"){
            j=sum(moderndata$cases2)
        }
        else{
            moderndata=subset(moderndata,countriesAndTerritories==input$place)
            j=moderndata$cases2
        }
        paste(j,"\n",sep="")
    })
    
    output$Ncases = renderText({
        
        moderndata=today()
        
        if(input$place=="World"|input$place=="All Countries  (WARNING SLOW)"){
            j=sum(moderndata$cases)
        }
        else{
            moderndata=subset(moderndata,countriesAndTerritories==input$place)
            j=moderndata$cases
        }
        paste(j,"\n",sep="")
    })
    
    
    
    output$Tdeaths = renderText({
        
        moderndata=today()
        
        if(input$place=="World"|input$place=="All Countries  (WARNING SLOW)"){
            j=sum(moderndata$deaths2)
        }
        else{
            moderndata=subset(moderndata,countriesAndTerritories==input$place)
            j=moderndata$deaths2
        }
        paste(j,"\n",sep="")
    })
    
    output$Ndeaths = renderText({
        
        moderndata=today()
        
        if(input$place=="World"|input$place=="All Countries  (WARNING SLOW)"){
            j=sum(moderndata$deaths)
        }
        else{
            moderndata=subset(moderndata,countriesAndTerritories==input$place)
            j=moderndata$deaths
        }
        paste(j,"\n",sep="")
    })
    output$pop1 = renderText({
        if(!(input$place=="World"|input$place=="All Countries  (WARNING SLOW)")){
            paste("Population in: (as of 2018)",input$place)
        }
    })
    output$pop2 = renderText({
        if(!(input$place=="World"|input$place=="All Countries  (WARNING SLOW)")){
            data=subset(today(), countriesAndTerritories==input$place)
            if(is.na(data$popData2018)){
                "Data not available"
            }
            else{
                data$popData2018
            }
        }
    })

    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
