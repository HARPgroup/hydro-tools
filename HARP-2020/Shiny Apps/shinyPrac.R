##Shiny practice script

#Load Libraries
library(shiny)
library(ggplot2)
library(dataRetrieval)
library(sqldf)
library(ggnewscale)
library(dplyr)

#Setup
site <- "http://deq2.bse.vt.edu/d.dh" 
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
#source(paste(github_location,"hydro-tools/HARP-2020/Heatmap","unmet_heatmap.R", sep = "/"))

#Fluid Page
ui <- fluidPage(
  titlePanel("Interactive Flow Graph"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Year",
                  label = "Water Year",
                  min = 1970,
                  max = 2019,
                  value = 1995),
      numericInput("runid", "Run ID", value = 18),
      #numericInput("pid", "PID", value = 4964892),
      numericInput("elid", "ELID", value = 299330),
      dateInput("start_date", "Start Date", value = "1985-01-01"),
      dateInput("end_date", "End Date", value = "2014-12-31")
    ),
    mainPanel(
      plotOutput(outputId = "flowplot"),
      plotOutput(outputId = "heatmap")
    )
  )
)

#Server Function
server <- function(input, output) {
  output$flowplot <- renderPlot({
    yr <- input$Year
    dat <- readNWISdv('01646502', '00060', paste(yr-1, "-10-01", sep = ''), paste(yr, "-09-30", sep = ''))
    flowplot <- ggplot(dat, aes(x=dat$Date, y=dat$X_00060_00003))+
                    geom_line()+
                    theme_bw()+
                    labs( title = 'Streamflow of the Potomac River (USGS gage 01646502)', x = paste(yr, " Water Year", sep = ''), y = 'Flow (cfs)') +
                    theme(plot.title = element_text(face = 'bold',hjust = 0.5))
    flowplot
  })
  output$heatmap <- renderPlot({
    elid <- input$elid
    runid <- input$runid
    pid <- input$pid
    start_date <- input$start_date
    end_date <- input$end_date
    dat <- fn_get_runfile(elid, runid, site= omsite,  cached = FALSE) # get data
    # set start and end dates based on inputs
    if (missing(start_date)) {
      syear <- min(dat$year)
      sdate <- as.Date(paste0(syear,"-01-01"))
    } else {
      sdate <- start_date
      syear <- format(as.Date(start_date, format="%Y-%m-%d"),"%Y")
    }
    if (missing(end_date)) {
      eyear <- max(dat$year)
      edate <- as.Date(paste0(eyear,"-12-31"))
    } else {
      edate <- end_date
      eyear <- format(as.Date(end_date, format="%Y-%m-%d"),"%Y")
    }   
    dat <- window(dat, start = sdate, end = edate);
    mode(dat) <- 'numeric'
    
    # change runfile to data frame
    datdf <- as.data.frame(dat)
    
    modat <- sqldf(" select month months, year years, count(*) count from datdf where unmet_demand_mgd > 0
    group by month, year") #Counts sum of unmet_days by month and year
    
    #Join counts with original data frame to get missing month and year combos then selects just count month and year
    modat <- sqldf("SELECT * FROM datdf LEFT JOIN modat ON modat.years = datdf.year AND modat.months = datdf.month group by month, year")
    modat <- sqldf('SELECT month, year, count count_unmet_days FROM modat GROUP BY month, year')
    
    #Replace NA for count with 0s
    modat[is.na(modat)] = 0
    
    ########################################################### Calculating Totals
    # numeric versions of eyear and syear
    num_syear <- as.numeric(syear)  
    num_eyear <- as.numeric(eyear)
    
    # monthly totals via sqldf
    mosum <- sqldf("SELECT  month, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY month")
    mosum$year <- rep(num_eyear+1,12)
    
    #yearly sum
    yesum <-  sqldf("SELECT year, sum(count_unmet_days) count_unmet_days FROM modat GROUP BY year")
    yesum$month <- rep(13,length(yesum$year))
    
    # create monthly averages 
    moavg<-
      mosum %>%
      mutate(avg=count_unmet_days/((num_eyear-num_syear)+1)) %>%
      mutate(year=num_eyear+2)
    
    moavg$avg<-round(moavg$avg, 1)
    
    # create yearly averages
    yeavg <-  
      yesum %>%
      mutate(avg=count_unmet_days/12) %>%
      mutate(month=14)
    
    yeavg$avg<-round(yeavg$avg, 1)
    
    # create x and y axis breaks
    y_breaks <- seq(syear,num_eyear+2,1)
    x_breaks <- seq(1,14,1)
    
    # create x and y labels
    y_labs <- c(seq(syear,eyear,1),'Totals', 'Avg')
    x_labs <- c(month.abb,'Totals','Avg')
    
    
    ############################################################### Plot and Save
    if (sum(mosum$count_unmet_days) == 0) {
      count_grid <- ggplot() +
        geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
        geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
        scale_fill_gradient2(low = "#00cc00", mid= "#00cc00", high = "#00cc00", guide = "colourbar", 
                             name= 'Unmet Days') +
        theme(panel.background = element_rect(fill = "transparent"))+
        theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
        scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
        scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
        theme(axis.ticks= element_blank()) +
        theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
        theme(legend.title.align = 0.5) 
      
      unmet <- count_grid + new_scale_fill() +
        geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
        geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
        geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
        geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
        scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid="#63D1F4",
                             midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
      
      
      unmet_avg <- unmet + new_scale_fill()+
        geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
        geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
        geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
        geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
        scale_fill_gradient2(low = "#FFF8DC", mid = "#FFF8DC", high ="#FFF8DC",
                             name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
    } else{
      count_grid <- ggplot() +
        geom_tile(data=modat, color='black',aes(x = month, y = year, fill = count_unmet_days)) +
        geom_text(aes(label=modat$count_unmet_days, x=modat$month, y= modat$year), size = 3.5, colour = "black") +
        scale_fill_gradient2(low = "#00cc00", high = "red",mid ='yellow',
                             midpoint = 15, guide = "colourbar", 
                             name= 'Unmet Days') +
        theme(panel.background = element_rect(fill = "transparent"))+
        theme() + labs(title = 'Unmet Demand Heatmap', y=NULL, x=NULL) +
        scale_x_continuous(expand=c(0,0), breaks= x_breaks, labels=x_labs, position='top') + 
        scale_y_reverse(expand=c(0,0), breaks=y_breaks, labels= y_labs) +
        theme(axis.ticks= element_blank()) +
        theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5)) +
        theme(legend.title.align = 0.5) 
      
      unmet <- count_grid + new_scale_fill() +
        geom_tile(data = yesum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
        geom_tile(data = mosum, color='black', aes(x = month, y = year, fill = count_unmet_days)) +
        geom_text(data = yesum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
        geom_text(data = mosum, size = 3.5, color='black', aes(x = month, y = year, label = count_unmet_days)) +
        scale_fill_gradient2(low = "#63D1F4", high = "#8A2BE2", mid='#CAB8FF',
                             midpoint = mean(mosum$count_unmet_days), name= 'Total Unmet Days')
      
      
      unmet_avg <- unmet + new_scale_fill()+
        geom_tile(data = yeavg, color='black', aes(x = month, y = year, fill = avg)) +
        geom_tile(data = moavg, color='black', aes(x = month, y = year, fill = avg)) +
        geom_text(data = yeavg, size = 3.5, color='black', aes(x = month, y = year, label = avg)) +
        geom_text(data = moavg, size = 3.5, color='black', aes(x = month, y = year, label = avg))+
        scale_fill_gradient2(low = "#FFF8DC", mid = "#FFDEAD", high ="#DEB887",
                             name= 'Average Unmet Days', midpoint = mean(yeavg$avg))
      
    }
    unmet_avg
  })
}

#Running the app
shinyApp(ui=ui, server=server)

