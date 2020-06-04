#USGS 01646502 POTOMAC RIVER (ADJUSTED) NEAR WASH, DC - '01646502'
#USGS 13010065 SNAKE RIVER AB JACKSON LAKE AT FLAGG RANCH WY - '13010065'

library(dataRetrieval)
library(ggplot2)

site_id <- '01646502'
startDate <- '2018-10-01'
endDate <- '2019- 09-30'
pCode <-'00060'

siteinfo <- readNWISsite(site_id)
rawDailyQ <- readNWISdv(site_id, pCode, startDate, endDate)
area.sq.miles <- siteinfo$drain_area_va
normalizedDailyQ = rawDailyQ$X_00060_00003*(12*31536000)/((area.sq.miles)*5280^2)

plot(rawDailyQ$Date, normalizedDailyQ,
     xlab = 'Date (2001 Water Year)',
     ylab = 'Stream Discharge (in/yr)',
     main = 'USGS 01646502 POTOMAC RIVER (ADJUSTED) NEAR WASH, DC',   
     col = 'darkblue',
     type = 'l',
     cex.axis = 0.75,
     cex.main = 1)

mean(rawDailyQ$X_00060_00003)
max(rawDailyQ$X_00060_00003)
min(rawDailyQ$X_00060_00003)




#Comaprison Plot

#USGS 01646502 POTOMAC RIVER (ADJUSTED) NEAR WASH, DC - '01646502'
# Snake River, WY - '13010065'

site_id1<- '01646502'
site_id2 <- '13010065'
startDate <- '2018-10-01'
endDate <- '2019-09-30'
pCode <-'00060'

siteinfo1 <- readNWISsite(site_id1)
siteinfo2 <- readNWISsite(site_id2)

rawDailyQ1 <- readNWISdv(site_id1, pCode, startDate, endDate)
rawDailyQ2 <- readNWISdv(site_id2, pCode, startDate, endDate)

area.sq.miles1 <- siteinfo1$drain_area_va
area.sq.miles2 <- siteinfo2$drain_area_va

normalizedDailyQ1 = rawDailyQ1$X_00060_00003*(12*31536000/(area.sq.miles1*5280^2))
normalizedDailyQ2 = rawDailyQ2$X_00060_00003*(12*31536000/(area.sq.miles2*5280^2))

plot(rawDailyQ$Date,normalizedDailyQ1, 
     xlab = 'Date (2019 Water Year)',
     ylab = 'Stream Discharge (inches/year)',
     main = 'Comparison of Potomac River, VA & Snake River, WY Discharge (Normalized over Area)',
     col = 'darkblue',
     type = 'l',
     cex.axis = 0.7,
     cex.main = 0.9,)

lines(rawDailyQ$Date,normalizedDailyQ2,type = 'l', col = 'red')

legend('topright', 
       legend = c('USGS 01646502 - Potomac River, VA','USGS 13010065 - Snake River, WY'), 
       col= c('darkblue', 'red'),
       lty = 1:1,
       lwd = 2,
       cex = 0.53)


#ggplot graphs


date<- rawDailyQ$Date
PotomacRiverVA <-normalizedDailyQ1
SnakeRiverWY <-normalizedDailyQ2
nq <- data.frame(PotomacRiverVA,SnakeRiverWY)
Q <- data.frame(date, nq)

library(ggplot2)
ggplot(Q, 
       aes(x=date, y=PotomacRiverVA)) +
       geom_line(color='darkblue')+
       labs(title='USGS 01646502 POTOMAC RIVER (ADJUSTED) NEAR WASH, DC',
       x='Date (2019 Water Year)', 
       y='Discharge (depth/year)' )+
       theme(
        plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))


library("reshape2")
Q <- melt(Q, id.vars="date")


p <- ggplot(data=Q, aes(x= date , y = value, group = variable, colour = variable)) +
        labs(title = "Comparison of Potomac River, VA & Snake River, WY Discharge (Normalized over Area)", x = "Date (2019 Water Year)", y = "Discharge (depth/year)", color = "River Segment\n") +
        scale_color_manual(labels = c("USGS 01646502 - Potomac", "USGS 13010065 - Snake"), values = c("darkblue", "red"))+
        geom_line() 
print(p)

p + theme(
      plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))



#boxplot

Date = rawDailyQ$Date
Year = format(as.Date(Date, format = "%d/%m/%Y"),"%Y")
rawDailyQ <- data.frame(rawDailyQ, Year)


ggplot(rawDailyQ, 
  aes(x=Year, y = X_00060_00003))+
  geom_boxplot(outlier.shape = NA)+
  labs(title= 'USGS 01646502 POTOMAC RIVER (ADJUSTED) NEAR WASH, DC')+
  ylab('Discharge (cfs)')+ 
  xlab('Year')+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face = "bold",  hjust = 0.5))+
  coord_cartesian(ylim=c(0, 7000))

  




#water year, use if else structure (do water year instead of regular year)
#learn more about data retrieval funcitons
#egret


