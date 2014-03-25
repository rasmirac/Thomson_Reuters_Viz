library(zoo)
library(scales)
library(ggplot2)
library(gridExtra)
library(reshape)

#SET WORKING DIRECTORY TO SOURCE CODE FILE LOCATION

# weekly debt search volume
debtData <- read.csv('data/automated_debt.csv')
# daily debt search volume
debtDataDaily <- read.csv('data/automated_daily_debt.csv')
# UK debt search volume
debtDataUK <- read.csv('data/automated_UK_debt.csv')
# India debt search volume
debtDataIndia <- read.csv('data/automated_debt_india.csv')

# calculates relative change given t and a vector
# returns vector
calc_relative_change <- function(t, vector){
  myT <- zoo(vector)
  myDelta <- 0
  for (i in 1:t){
    myLag <- c(rep(NA,  i), lag(myT, -i))
    myDelta <- myDelta + myLag
  }
  relative_change <- vector - myDelta/t
  return(relative_change)
}

calc_stock_returns <- function(t, vector){
  myT <- zoo(vector)
  myDelta <- 0
  myLag <- c(rep(NA, t), lag(myT, -t))
  stock_return <- myT/myLag - 1
  return(stock_return)
}

# calculate relative change for four time periods; three weeks, two weeks, one week, and daily
debtData$relative_changethree <- calc_relative_change(3, debtData$debt.volume)
debtData$relative_changetwo <- calc_relative_change(2, debtData$debt.volume)
debtData$relative_changeone <- calc_relative_change(1, debtData$debt.volume)
debtDataDaily$change <- calc_relative_change(1, debtDataDaily$debt.volume)
debtDataUK$relative_changethree <- calc_relative_change(3, debtDataUK$debt.volume)
debtDataUK$relative_changetwo <- calc_relative_change(2, debtDataUK$debt.volume)
debtDataUK$relative_changeone <- calc_relative_change(1, debtDataUK$debt.volume)
debtDataIndia$relative_changethree <- calc_relative_change(3, debtDataIndia$debt.volume)
debtDataIndia$relative_changetwo <- calc_relative_change(2, debtDataIndia$debt.volume)
debtDataIndia$relative_changeone <- calc_relative_change(1, debtDataIndia$debt.volume)

# create new csv with relative change information for acf, pacf, stl calculations
#write.csv(debtData, 'data/debt_with_relativechange.csv')
#write.csv(debtDataDaily, 'data/debt_with_relativechange_daily.csv')
#write.csv(debtDataUK, 'data/debt_with_relativechange_uk.csv')

# read in DJIA data, convert to numerical
djiaData <- read.csv('data/get_DJI_weekly_data.csv', sep = ',')
djiaDataDaily <- read.csv('data/get_DJI_data_ninety_days.csv', sep = ',')

djiaData$Adj.Close <- as.numeric(gsub(",","", djiaData$Adj.Close))
djiaData$Close <- as.numeric(gsub(",","", djiaData$Close))
djiaDataDaily$Adj.Close <- as.numeric(gsub(",","", djiaDataDaily$Adj.Close))

# returns for DJIA
djiaDataDaily$daily_returns <- calc_stock_returns(1, djiaDataDaily$Adj.Close)
djiaData$weekly_returns <- calc_stock_returns(1, djiaData$Adj.Close)
#write.csv(djiaDataDaily, 'data/get_DJI_data_ninety_days_change.csv')
#write.csv(djiaData, 'data/get_DJI_weekly_data_change.csv')


# function for three week relative change plot for easy replication with new debt data
# change variable must be named 
# beacuse of the nuances we are interested in (different axis labels, etc)
# we don't use this function for every plot
# be sure to label the relative change you want to use as 'relative_change' i.e.
# debtData$relative_change <- calc_relative_change(t, debtData$debt.volume)
# ^^ run above with different t for different time windows for relative change
final_plot <- function(datasetDJIA, datasetDebt, my_title, legend_title){
  final_plot <- ggplot(data = datasetDJIA, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black', size = .9) + 
    labs(title = my_title) + 
    xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
    geom_vline(datasetDebt, mapping = aes(colour= as.numeric(relative_change), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
    scale_colour_gradient2(legend_title, low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), limits = c(-20, 20), space = 'rgb') +   
    geom_line(data = datasetDJIA, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
    theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 
  
  plot(final_plot)
}


final_plot_nolimits <- function(datasetDJIA, datasetDebt, my_title, legend_title){
  final_plot <- ggplot(data = datasetDJIA, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black', size = .9) + 
    labs(title = my_title) + 
    xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
    geom_vline(datasetDebt, mapping = aes(colour= as.numeric(relative_change), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
    scale_colour_gradient2(legend_title, low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), space = 'rgb') +   
    geom_line(data = datasetDJIA, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
    theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 
  
  plot(final_plot)
}

# t = Three Weeks

# for plot function, name time window of change 'relative_change' in debtData
debtData$relative_change <- calc_relative_change(3, debtData$debt.volume)
# final plot, with timeline like in paper
final_plot(djiaData, datasetDebt=debtData, 'Search volume data and stock market moves, t = 3 weeks', 'Relative Search Volume Change over 3 weeks')

#final plot without limits for illustrative purposes
final_plot_nolimits(djiaData, datasetDebt=debtData, 'Search volume data and stock market moves, t = 3 weeks', 'Relative Search Volume Change over 3 weeks')

# t = Two Weeks
# line plot for debt relative change, t= 2 weeks
debt_two <- ggplot(debtData, aes(x=as.Date(start.date, "%Y-%m-%d"), y=as.numeric(relative_changetwo)))+
  geom_line(aes(group = 1))+
  xlab("Time, t [Years]")+
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) + 
  ylab("Relative Change, r(t)") + labs(title ="Search volume data, t = 2 weeks") 
plot(debt_two)

# density for debt t = 2weeks

# for plot function, name time window of change 'relative_change' in debtData
debtData$relative_change <- calc_relative_change(2, debtData$debt.volume)
# final plot, with timeline like in paper
final_plot(djiaData, datasetDebt=debtData, 'Search volume data and stock market moves, t = 2 weeks', 'Relative Search Volume Change over 2 weeks')


# t = One Week
debtData$relative_change <- calc_relative_change(1, debtData$debt.volume)
final_plot(djiaData, datasetDebt=debtData, 'Search volume data and stock market moves, t = 1 week', 'Relative Search Volume Change over 1 week')


## for daily search changes
## couldn't use function because daily debt search data is not in correct format
final_plot_daily <- ggplot(data = djiaDataDaily, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black') + 
  labs(title ="Search volume data and stock market moves, daily change") + 
  xlab("Time, t [Days]") + ylab("Index Value, p(t)") + 
  geom_vline(debtDataDaily, mapping = aes(colour= as.numeric(change), xintercept = as.numeric(as.Date(start.date, "%Y-%m-%d"))), stat = 'vline', size = 5) + 
  scale_colour_gradient2("Relative Search Volume Change over 1 day", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), space = 'rgb') +   
  geom_line(data = djiaDataDaily, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() + coord_cartesian(ylim=c(14000, 16000))

plot(final_plot_daily)

## visually checking for the holiday effect

# for india to see holiday effect

# OPTIONAL UNCOMMENT FOR INDIA GRAPH WITH NO LIMITS

#final_plot_season_india <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black') + 
#  labs(title ="Search volume data in India and stock market moves, t = 1 week") + 
#  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
#  geom_vline(debtDataIndia, mapping = aes(colour= as.numeric(relative_changeone), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
#  scale_colour_gradient2("Relative Search Volume Change over 1 week", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), space = 'rgb') +   
#  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
#  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + scale_x_date(labels = date_format("%b, %Y"), breaks = "6 months") + theme_bw() 

#plot(final_plot_season_india)

final_plot_season_india_nl <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black') + 
  labs(title ="Search volume data in India and stock market moves, t = 1 week") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtDataIndia, mapping = aes(colour= as.numeric(relative_changeone), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
  scale_colour_gradient2("Relative Search Volume Change over 1 week", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), limits = c(-20, 20), space = 'rgb') +   
  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + scale_x_date(labels = date_format("%b, %Y"), breaks = "6 months") + theme_bw() 

plot(final_plot_season_india_nl)


## for the UK
debtDataUK$relative_change <- calc_relative_change(3, debtDataUK$debt.volume)
final_plot_nolimits(djiaData, datasetDebt=debtDataUK, 'Search volume data in the UK and stock market moves, t = 3 weeks', 'Relative Search Volume Change over 3 weeks')

