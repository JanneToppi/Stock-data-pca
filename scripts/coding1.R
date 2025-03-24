# Install required packages (run only once)
# install.packages("tidyverse")

# Load libraries
library(tidyverse)
library(lubridate)

# Set working directory where your .txt stock files are stored
setwd("C:/Users/35845/Desktop/MSA project/data/Stocks")

# List of stock tickers (lowercase filenames)
tickers <- c("aapl", "msft", "googl", "meta", "nvda",
             "jpm", "bac", "gs", "wfc",
             "jnj", "pfe", "mrk", "lly",
             "xom", "cvx", "bp", "cop")

# Date range
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2015-12-31")

# Function to read and process each stock
process_stock <- function(ticker) {
  file_path <- paste0("./", ticker, ".us.txt")
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  df <- read.table(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]
  
  # Filter date range
  df <- df[df$Date >= start_date & df$Date <= end_date, ]
  
  # Compute log return and range
  df$Return <- c(NA, diff(log(df$Close)))
  df$Range <- df$High - df$Low
  df$Ticker <- toupper(ticker)
  
  # Keep relevant columns
  df <- df[, c("Date", "Ticker", "Return", "Range")]
  return(df)
}

# Process all tickers
stock_list <- lapply(tickers, process_stock)

# Combine into one dataframe
all_stocks <- do.call(rbind, stock_list)



summary(all_stocks)
dim(all_stocks)
# Convert to wide format
returns <- all_stocks %>%
  select(Date, Ticker, Return) %>%
  pivot_wider(names_from = Ticker, values_from = Return, names_prefix = "Return_")

ranges <- all_stocks %>%
  select(Date, Ticker, Range) %>%
  pivot_wider(names_from = Ticker, values_from = Range, names_prefix = "Range_")

# Merge all features
pca_input <- reduce(list(returns, ranges), full_join, by = "Date")
summary(pca_input)
dim(pca_input)

# Drop incomplete rows
pca_input_clean <- pca_input %>% drop_na()
pairs(pca_input_clean)
# Final PCA matrix (excluding Date)
pca_matrix <- pca_input_clean %>% select(-Date)

# Scale data
pca_scaled <- scale(pca_matrix)

# Run PCA
pca_result <- prcomp(pca_scaled, scale. = FALSE)

# Summary of PCA (explained variance)
summary(pca_result)

# Scree plot
plot(pca_result, type = "l", main = "Scree Plot of Principal Components")

# PCA scores (projections onto PCs)
pca_scores <- as.data.frame(pca_result$x)

# Plot PC1 vs PC2
plot(pca_scores$PC1, pca_scores$PC2, 
     main = "PCA: PC1 vs PC2",
     xlab = "Principal Component 1",
     ylab = "Principal Component 2",
     pch = 19, col = "steelblue")

# Inspect loadings (how original variables contribute to PCs)
head(pca_result$rotation)

