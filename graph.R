install.packages("dygraphs")
install.packages("quantmod")
install.packages("forecast")
install.packages("kableExtra")
install.packages("tidyverse") # metapackage of all tidyverse packages\
install.packages("tseries")

install.packages("vars")

setwd("/Users/vickybelario/Desktop/git_repo_lulus/forecast airlane/Forecasting-Airline-Passanger-using-Time-Series")  # Replace with the actual path

# Lets look at the data
data <- read.csv("air traffic.csv")

data <- data[,1:5]
data$Date <- as.Date(with(data, paste(Year,Month,1,sep = "-")),"%Y-%m-%d")
data <- data[,3:5]

# Function to remove commas from data ([DATA FRAME], [COLUMNS SELECTED])
remove_commas <- function(df, columns) {
  df[columns] <- lapply(df[columns], function(x) log(as.numeric(gsub(",", "", x))))
  return(df)
}
                        
data <- remove_commas(data, c("Dom_Pax", "Int_Pax", "Pax"))
tail(data)

# Time series the data
data_ts <- ts(data, start = c(2003, 1), frequency = 12)
tail(data_ts)

# Download Global price of Brent Crude (POILBREUSDM) from FRED
getSymbols("POILBREUSDM", src = "FRED", from = "2003-01-01", to = "2023-09-01")
tail(POILBREUSDM)
                        
# Lets graph the data and take a look at it.
# Graph of flight data
data_ts %>%
  dygraph(main = "Number of Passengers (Monthly)", ylab = "log(Total Number of Passengers)") %>%
  dySeries("Dom_Pax", label = "Domestic", strokeWidth = 2.5, strokeBorderWidth = 1, color = "black") %>%
  dySeries("Int_Pax", label = "International", strokeWidth = 2.5, strokeBorderWidth = 1, color = "red") %>%
  dySeries("Pax", label = "Total", strokeWidth = 2.5, strokeBorderWidth = 1, color = "blue") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2007-12-01", to = "2009-06-01", color = "lightgrey") %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  dyRangeSelector(dateWindow = c("2002-12-01", "2023-10-01")) %>%
  dyEvent("2007-12-01", "Global Financial Crisis", labelLoc = "bottom", strokePattern = "solid") %>%
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>%
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")

# Graph of crude oil prices
POILBREUSDM %>%
  dygraph(main = "Global Price of Brent Crude (Monthly)", ylab = "Price per Barrel") %>%
  dySeries("POILBREUSDM", label = "Price($)", strokeWidth = 2.5, strokeBorderWidth = 1, color = "black") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2007-12-01", to = "2009-06-01", color = "lightgrey") %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  dyRangeSelector(dateWindow = c("2002-12-01", "2023-10-01")) %>%
  dyEvent("2007-12-01", "Global Financial Crisis", labelLoc = "bottom", strokePattern = "solid") %>%
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>%
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")

# Lets combine the data                   
combined <- cbind(data_ts, POILBREUSDM)
colnames(combined) <- c("domestic", "international","total","crude") 
tail(combined)
                        
# Combined time series
combined_ts <- ts(combined, start = c(2003, 1), frequency = 12)

# Break data into testing and training
data_train <- window(combined_ts, end = c(2022, 12))
data_test <- window(combined_ts, start = c(2023, 1))
data_test_dom <- window(combined_ts[,1])
data_test_int <-window(combined_ts[,2])
data_test_tot <-window(combined_ts[,3])
