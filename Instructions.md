Instructions 
===
----------

###Required Files and Packages

In order to run final_plot.R, you must download the folder titled *data*. This folder contains .csv files with the Google search volume data for 'debt' and the DJIA data. 

The script requires the packages *zoo*, *scales*, *ggplot2*, and *gridExtra*. Further, in order for the code to run properly, the final_plot.R file and the data folder **must** be in the same directory. 

**Please set the session directory in the R console to the working directory.**

There are optional commands in the code to create new .csv files with the relative change calculations. Uncomment these `write.csv()` calls to save the new data frames to the data folder. 

This code produces 5 plots. 
