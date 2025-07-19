install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
install.packages("xts")
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(xts)

# Download stock data for HIMS from Yahoo Finance (till today)
getSymbols("SY", from = "2020-01-01", to = Sys.Date())
head(SY)

# Plot Adjusted Closing Price Chart
chartSeries(SY, name = "SY Stock Price", theme = chartTheme("white"))

# Calculate Daily Returns from Adjusted Close
daily_returns <- dailyReturn(Ad(SY))

# Convert daily returns (xts) to data frame for plotting
returns_df <- data.frame(
  Date = index(daily_returns),
  Return = coredata(daily_returns)
)
head(returns_df)

# Plot daily returns
ggplot(returns_df, aes(x = Date, y = daily.returns)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "SY Daily Returns", x = "Date", y = "Return") +
  theme_minimal()

# Calculate Key Performance Metrics
mean_return <- mean(returns_df$daily.returns)
volatility <- sd(returns_df$daily.returns)
annualized_return <- (1 + mean_return)^252 - 1   # approx 252 trading days in a year
annualized_volatility <- volatility * sqrt(252)
sharpe_ratio <- annualized_return / annualized_volatility # Assume risk-free rate = 0

cat("Mean Return:", round(mean_return*100, 2), "%\n")
cat("Annualized Return:", round(annualized_return*100, 2), "%\n")
cat("Annualized Volatility:", round(annualized_volatility*100, 2), "%\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 3), "\n")

# Plot Cumulative Returns
returns_df$cumulative <- cumprod(1 + returns_df$daily.returns) - 1

ggplot(returns_df, aes(x = Date, y = cumulative)) +
  geom_line(color = "darkgreen") +
  labs(title = "SY Cumulative Returns", x = "Date", y = "Cumulative Return") +
  theme_minimal()

# Add Technical Indicators: SMA50 and SMA200
SY$SMA50 <- SMA(Cl(SY), n = 50)
SY$SMA200 <- SMA(Cl(SY), n = 200)

# Plot Price Chart with Moving Averages
chartSeries(SY, theme=chartTheme("white"), TA="addSMA(50, col='blue'); addSMA(200, col='red')")

