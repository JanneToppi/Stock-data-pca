#
setwd("C:/Users/35845/Desktop/MSA project/data/Stocks")

# Date range
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2015-12-31")

#aapl stock
aapl_stocks <- read.table("./aapl.us.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)
aapl_stocks$Date <- as.Date(aapl_stocks$Date)

aapl_stocks = aapl_stocks[aapl_stocks$Date >= start_date & aapl_stocks$Date <= end_date, ]

#calcuate log return

aapl_stocks$log_return = c(NA, diff(log(aapl_stocks$Close)))

