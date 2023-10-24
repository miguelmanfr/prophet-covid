
#apt-get install libcurl4-openssl-
#apt-get install g++-8 
#install.packages("curl")
#install.packages("prophet")
#install.packages("jsonlite")

library(prophet)
library(curl)
library(jsonlite)
library(parallel)
library(foreach)
library(doParallel)
library(ggplot2)


states =read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv')

input_file <- 'state-test/state-timesearies.json'

input_file <- 'state-test/state-timesearies - test - RJ.json'

input_file <- 'state-test/state-timeseries-US.json'


inputJson <- fromJSON(txt = input_file)

x = inputJson$'São Paulo/SP'$date[ 1:( length(inputJson$'São Paulo/SP'$date)-7) ]
y = inputJson$'São Paulo/SP'$confirmed[ 1:( length(inputJson$'São Paulo/SP'$date)-7) ]

#x = inputJson$`Rio de Janeiro/RJ`$date[ 11:( length(inputJson$`Rio de Janeiro/RJ`$date)-7) ]
#y = inputJson$`Rio de Janeiro/RJ`$confirmed[ 11:( length(inputJson$`Rio de Janeiro/RJ`$date)-7) ]

x = inputJson$`Rio de Janeiro/RJ`$date[ 11:( length(inputJson$`Rio de Janeiro/RJ`$date)-7) ]
y = inputJson$`Rio de Janeiro/RJ`$confirmed[ 11:( length(inputJson$`Rio de Janeiro/RJ`$date)-7) ]

#
# US
#
x = inputJson$`New York City/New York`$date[ 1:( length(inputJson$`New York City/New York`$date)-7) ]
y = inputJson$`New York City/New York`$confirmed[ 1:( length(inputJson$`New York City/New York`$date)-7) ]



  
json_data_DF = base::data.frame("ds" = x, "y" = y)

tail(json_data_DF)

y_max <- max(json_data_DF$y)
json_data_DF$cap <- y_max * 3

#json_data_DF$cap <- 11159 #RJ Poly

#json_data_DF$cap <- 80000 # 12629 #SP Poly

json_data_DF$floor <- 0

json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 0.1, no = json_data_DF$y)

days = 7
seasonality.period = 9


md_ = prophet::prophet(fit = FALSE, growth = 'logistic'
#md_ = prophet::prophet(fit = FALSE, growth = 'linear'
              ,interval.width = 0.95
              ,changepoint.range = 0.98
              #,changepoint.prior.scale = 0.981558
              ,changepoint.prior.scale = 15
              ,yearly.seasonality = F
              ,weekly.seasonality = F
              ,daily.seasonality = F
)

md_ <- prophet::add_seasonality(md_, name='weekly', 
#md_ <- prophet::add_seasonality(md_, name='monthly', 
                                period = seasonality.period, 
                                mode = 'multiplicative', 
                                fourier.order = 1,
                                #.prior.scale = 0.05
                                )  #  prior.scale = y_max * 550)
#?add_seasonality
md_ <- prophet::fit.prophet(md_, df =  json_data_DF)

future_md_ = prophet::make_future_dataframe(md_, days)
future_md_$cap <- json_data_DF$cap[1]
future_md_$floor <- json_data_DF$floor[1]

pred_md_ <- predict(md_, future_md_)

tail( data.frame("data" = pred_md_$ds, "y" = pred_md_$yhat) )

plot(md_, pred_md_) +
add_changepoints_to_plot(md_)

inputJson$`São Paulo/SP`$confirmed[ (length(inputJson$`São Paulo/SP`$date)-7) : length(inputJson$`São Paulo/SP`$date)  ]

inputJson$`Rio de Janeiro/RJ`$confirmed[ (length(inputJson$`Rio de Janeiro/RJ`$date)-7) : length(inputJson$`Rio de Janeiro/RJ`$date)  ]

as.integer(pred_md_$yhat[ (length(pred_md_$ds)-7) : (length(pred_md_$ds))])

#
#US
#
inputJson$`New York City/New York`$date[ (length(inputJson$`New York City/New York`$date)-7) : length(inputJson$`New York City/New York`$date)  ]
inputJson$`New York City/New York`$confirmed[ (length(inputJson$`New York City/New York`$date)-7) : length(inputJson$`New York City/New York`$date)  ]

as.integer(pred_md_$yhat[ (length(pred_md_$ds)-7) : (length(pred_md_$ds))])
as.integer(pred_md_$yhat_lower[ (length(pred_md_$ds)-7) : (length(pred_md_$ds))])
pred_md_$ds[ (length(pred_md_$ds)-7) : (length(pred_md_$ds))]




#New York
#0,3% ,changepoint.prior.scale = 15,  multiplicative, fourier 1 seasonaluty.period = 9


#São PAulo
#6,3% additive fourier 4 98%
#1,78% multiplicative fourier 3, prior.scale 0.05 - Mas ainda tem barriga de queda sazonal
#0,997% multiplicative fourier 1, prior.scale 0.05, seasonality.period 8 - Minimizada a barriga de queda sazonal

#Rio de Janeiro
#5% multiplicative fourier 1, prior.scale 0.05 - 


x
inputJson$`São Paulo/SP`$date[ (length(inputJson$`São Paulo/SP`$date)-7) : length(inputJson$`São Paulo/SP`$date)  ]
inputJson$`Rio de Janeiro/RJ`$date[ (length(inputJson$`Rio de Janeiro/RJ`$date)-7) : length(inputJson$`Rio de Janeiro/RJ`$date)  ]


pred_md_$ds[(length(pred_md_$ds)-8):(length(pred_md_$ds)-1)]

teste_DF <- data.frame("date" = inputJson$`Rio de Janeiro/RJ`$date, "confirmed" = inputJson$`Rio de Janeiro/RJ`$confirmed)

teste_DF_SP <- data.frame("date" = inputJson$`São Paulo/SP`$date, "confirmed" = inputJson$`São Paulo/SP`$confirmed)

par(mfrow = c(2,2))
plot(teste_DF)
plot(teste_DF_SP)
plot(md_, pred_md_)

####
# Linear Regression para achar o CAP
###

lm_hist = inputJson$`Rio de Janeiro/RJ`$confirmed[ match('2020-03-21', inputJson$`Rio de Janeiro/RJ`$date) :
                                                   match('2020-04-23', inputJson$`Rio de Janeiro/RJ`$date)]

# SP
lm_hist = inputJson$`Rio de Janeiro/RJ`$confirmed[ inputJson$`Rio de Janeiro/RJ`$confirmed > 100 ]


lm_hist = inputJson$`Rio de Janeiro/RJ`$confirmed[  inputJson$`Rio de Janeiro/RJ`$confirmed > 50 ]


lm_data = data.frame("idx" = seq(1, length(lm_hist) ),  "confirmed" = lm_hist )

pred_rl_lin = lm( confirmed ~ idx , data = lm_data, )
pred_rl_poly = lm( confirmed ~ poly(idx, degree = 2, raw = TRUE) , data = lm_data, )

predict.lm(pred_rl_poly, data.frame("idx" = seq(50, 64)))
predict.lm(pred_rl_lin, data.frame("idx" = seq(50, 64)))


summary(pred_rl_poly)
par(mfrow = c(1,1))

plot(lm_data$confirmed)
abline(pred_rl_lin, col="red", lwd=2)
lines( smooth.spline(lm_data$idx, predict(pred_rl_poly)), col="blue", lwd=2)


ggplot(data = lm_data ) +
geom_point( aes(x = lm_data$idx, y = lm_data$confirmed ) ) +
geom_line( aes(x = lm_data$idx, y = pred_rl$model$`I(idx^2)` ) )


plot(x = lm_data$idx, y= lm_data$confirmed)
curve( )

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}



