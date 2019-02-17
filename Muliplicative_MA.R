### Multiplicative Moving Averages 
install.packages("forecast")
install.packages("Ecdat")
library(forecast)
library(Ecdat)
data(AirPassengers)
timeserie_air = AirPassengers
plot(as.ts(timeserie_air))

### Calculate trend using moving average
trend_air = ma(timeserie_air, order = 12, centre = T)
par(mfrow=c(1,1))
plot(as.ts(timeserie_air), col = "red")
lines(trend_air, col = "red")
plot(as.ts(trend_air), col = "red")

### Detrend the time series
detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air), col = "red")

### Average the Sesonality

# Create a Matrix of 12 rows because we have 12 months 
m_air = t(matrix(data = detrend_air, nrow = 12))
# Calculate Means on the column to get aveage seasonalilty per month
seasonal_air = colMeans(m_air, na.rm = T)
# Plot it with 12 repetition i.e ones for each month
plot(as.ts(rep(seasonal_air,12)), col = "red")

### Random Noise
#  The multiplicative formula is “Time series = Seasonal * Trend * Random,
#  which means “Random = Time series / (Trend * Seasonal)”

random_air =timeserie_air / ( trend_air * seasonal_air )
plot(as.ts(random_air), col = "red")


### Reconstruct Whole signal back again 

recomposed_air = seasonal_air * trend_air * random_air
plot(as.ts(recomposed_air), col = "red")


### Implementing all this using Decompose Function
ts_air = ts(timeserie_air, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")

plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)


