data = readxl::read_xlsx("C:/Users/DEVI PRADEEP/Downloads/R/Gold prcie USD.xlsx")
head(data)
str(data)
data$Year = format(data$Date,"%Y")
data

tail(data)
summary(data)
sum(is.na(data))
sum(is.null(data))

#avg gold price using aggregate
yearly_data = aggregate(USD~Year,data = data,mean)
yearly_data

#plot the year wise avg gold price
library(ggplot2)
ggplot(yearly_data,aes(x=Year,y=USD,group = 1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  geom_smooth(method = "lm",se = FALSE,color="black",linetype="dashed",size=1)+
  labs(title = "Year-wise gold price trend (1978-2024)",x="Year",y="Average gold price ($ per gram)")


#plot rolling average
library(zoo)
data$USD_roll = rollmean(data$USD,k=12,fill = NA)
data

ggplot(data,aes(x=Date,y=USD_roll,group = 1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  labs(title = "Rolling average gold price trend (1978-2024)",x="Year",y="Average gold price ($ per gram)")

#rolling avg 100
data$USD_roll_100 = rollmean(data$USD,k=100,fill = NA)
data

ggplot(data,aes(x=Date,y=USD_roll_100,group = 1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  labs(title = "Rolling average gold price trend (1978-2024)",x="Year",y="Average gold price ($ per gram)")

#monthly-analysis
#extract the month from the date.
data$Month<-format(gp$Date,"%B")
data

#Calculate the average gold price for each month.
monthly_data<-aggregate(USD~Month,data = data,FUN=mean)

#ensure the month are ordered correctly.
monthly_data$Month<-factor(monthly_data$Month,levels = month.name)

#plotting the monthly average gold price as a line chart.
ggplot(monthly_data,aes(x=Month,y=USD,group = 1))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  labs(title = "Monthly average gold price trend",x="Month",y="Average gold price ($ per gram)")

#forecast the range
library(forecast)
data_ts= ts(data$USD,start = c(1978,1),frequency = 365)
data_ts
arima_model = auto.arima(data_ts)
forcast_data = forecast(arima_model,h=1825,level = c(90,95))

autoplot(forcast_data)+
  labs(title = "Gold price forecast for the next 5 years",x="Year",y="Price(USD)")

data_ts= ts(data$USD,start = c(1978,1),frequency = 12)
data_ts
arima_model = auto.arima(data_ts)
forcast_data = forecast(arima_model,h=60,level = c(90,95))

autoplot(forcast_data)+
  labs(title = "Gold price forecast for the next 5 years",x="Year",y="Price(USD)")

#forecast the range(2029)
library(forecast)
data_ts= ts(data$USD,start = c(1978,1),frequency = 12) 
data_ts
arima_model = auto.arima(data_ts)
forcast_data = forecast(arima_model,h=60,level = c(90,95))

#retrieve the forecasted values.(gold price for the 5 years )
forcasted_values = forcast_data$mean

#extract the forecasted value for the year 2029(month 49 to 60)
forcasted_values_2029 = forcasted_values[49:60]
forcasted_values_2029
forcasted_values[60]

#forecast the 2027
data_ts= ts(data$USD,start = c(1978,1),frequency = 12) 
data_ts
arima_model = auto.arima(data_ts)
forcast_data = forecast(arima_model,h=36,level = c(90,95))

#retrieve the forecasted values.(gold price for the 3 years )
forcasted_values = forcast_data$mean

#extract the forecasted value for the year 2027(month 24 to 36)
forcasted_values_2027 = forcasted_values[25:36]
forcasted_values_2027
forcasted_values[36]
