library(zoo)
library(scales)
library(ggplot2)
library(gridExtra)

# need to use the files from the 'data' folder
## !! SET SESSION WORKING DIRECTORY TO SOURCE FILE LOCATION !! ##
debtData <- read.csv('data/automated_debt.csv')
debtDataDaily <- read.csv('data/automated_daily_debt.csv')

# calculates relative change given t and a vector
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

# calculate relative change for three time periods; three weeks, two weeks, and one week
debtData$relative_changethree <- calc_relative_change(3, debtData$debt.volume)
debtData$relative_changetwo <- calc_relative_change(2, debtData$debt.volume)
debtData$relative_changeone <- calc_relative_change(1, debtData$debt.volume)
# create new csv with relative change information for acf, pacf calculations
#write.csv(debtData, 'data/debt_with_relativechange.csv')
debtDataDaily$change <- calc_relative_change(1, debtDataDaily$debt.volume)
#write.csv(debtData, 'data/debt_with_relativechange_daily.csv')

# read in DJIA data, convert to numerical
djiaData <- read.csv('data/get_DJI_weekly_data.csv', sep = ',')
djiaData$Adj.Close <- as.numeric(gsub(",","", djiaData$Adj.Close))
djiaData$Close <- as.numeric(gsub(",","", djiaData$Close))

djiaDataDaily <- read.csv('data/get_DJI_data_ninety_days.csv', sep = ',')
djiaDataDaily$Adj.Close <- as.numeric(gsub(",","", djiaDataDaily$Adj.Close))


# t = Three Weeks

# final plot
final_plot_three <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black', size = .9) + 
  labs(title ="Search volume data and stock market moves, t = 3") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtData, mapping = aes(colour= as.numeric(relative_changethree), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
  scale_colour_gradient2("Relative Search Volume Change", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), limits = c(-20, 20), space = 'rgb') +   
  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 

plot(final_plot_three)

final_plot_three_nolimits <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black', size = .9) + 
  labs(title ="Search volume data and stock market moves, t = 3") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtData, mapping = aes(colour= as.numeric(relative_changethree), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
  scale_colour_gradient2("Relative Search Volume Change", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), space = 'rgb') +   
  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 

plot(final_plot_three_nolimits)


# t = Two Weeks
final_plot_two <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black', size = .9) + 
  labs(title ="Search volume data and stock market moves, t = 2") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtData, mapping = aes(colour= as.numeric(relative_changetwo), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
  scale_colour_gradient2("Relative Search Volume Change", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), limits = c(-20, 20), space = 'rgb') +   
  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 

plot(final_plot_two)

# t = One Week
final_plot_one <- ggplot(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black') + 
  labs(title ="Search volume data and stock market moves, t = 1") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtData, mapping = aes(colour= as.numeric(relative_changeone), xintercept = as.numeric(as.Date(end.date, "%Y-%m-%d"))), stat = 'vline', size = 1.5) + 
  scale_colour_gradient2("Relative Search Volume Change", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), limits = c(-20, 20), space = 'rgb') +   
  geom_line(data = djiaData, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() 

plot(final_plot_one)

final_plot_daily <- ggplot(data = djiaDataDaily, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close)) + geom_line(data = djiaData, aes(x=as.Date(Date, "%d-%B-%y"), y=Adj.Close), colour = 'black') + 
  labs(title ="Search volume data and stock market moves, daily change") + 
  xlab("Time, t [Years]") + ylab("Index Value, p(t)") + 
  geom_vline(debtDataDaily, mapping = aes(colour= as.numeric(change), xintercept = as.numeric(as.Date(start.date, "%Y-%m-%d"))), stat = 'vline', size = 5) + 
  scale_colour_gradient2("Relative Search Volume Change", low=muted("navyblue", l=50, c=250), na.value = "white", mid = 'snow1', midpoint = 0, high=muted("red3", l=50, c=150), guide = guide_colorbar(title.vjust = .25, title.position = 'right', title.theme = element_text(size=12, angle = 90)), space = 'rgb') +   
  geom_line(data = djiaDataDaily, aes(x=as.Date(Date, "%B %d, %Y"), y=Adj.Close), colour = 'black') +
  theme(axis.text.x = element_text(colour = "black"),  axis.text.y = element_text(colour = "black")) + theme_bw() + coord_cartesian(ylim=c(14000, 16000))

plot(final_plot_daily)