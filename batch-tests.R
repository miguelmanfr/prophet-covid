#apt-get install libcurl4-openssl-
#apt-get install g++-8 
#install.packages("curl")
#install.packages("prophet")
#install.packages("jsonlite")

library(prophet)
library(curl)
library(jsonlite)
library(ggplot2)

#
# JSON
#

fullCountriesJson <- fromJSON(txt = 'https://pomber.github.io/covid19/timeseries.json')
fullCountriesJson <- fromJSON(txt = 'state-test/timeseries-countries.json')

tail(fullCountriesJson$Brazil)

x = fullCountriesJson$US$date[ 1: ( length(fullCountriesJson$US$date) - 2 ) ]
y = fullCountriesJson$US$deaths [ 1: ( length(fullCountriesJson$US$date) - 2 ) ]
days = 6

json_data_DF = data.frame("ds" = x, "y" = y)

y_max <- max(json_data_DF$y)
json_data_DF$cap <- y_max * 3
json_data_DF$floor <- 0

json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 0.1, no = json_data_DF$y)

tail(json_data_DF)

md_ = prophet(fit = FALSE, growth = 'logistic'
              ,interval.width = 0.95
              ,changepoint.range = 0.98 
              ,changepoint.prior.scale = 0.2588
)

md_ <- add_seasonality(md_, name='daily', period = 4, mode = 'multiplicative', fourier.order = 2)  #  prior.scale = y_max * 550)
#?add_seasonality
md_ <- fit.prophet(md_, df =  json_data_DF)

future_md_ = make_future_dataframe(md_, days)
future_md_$cap <- json_data_DF$cap[1]
future_md_$floor <- json_data_DF$floor[1]

pred_md_ <- predict(md_, future_md_)

tail(pred_md_$yhat)

plot(md_, pred_md_)

x_length = length(x) - 4

#BR
#full_length = length(fullCountriesJson$Brazil$date) +3
#hist = fullCountriesJson$Brazil$deaths[ x_length : full_length ]

full_length = length(fullCountriesJson$US$date) +3
hist = fullCountriesJson$US$deaths[ x_length : full_length ]

view = data.frame(   "data" = pred_md_$ds[ x_length : full_length ]
              , "hist" = hist
              , "pred" = pred_md_$yhat[ x_length : full_length ] 
              , "pred_min" = pred_md_$yhat_lower[ x_length : full_length ] 
              , "pred_max" = pred_md_$yhat_upper[ x_length : full_length ] 
              , "Err %:" = abs(1 - pred_md_$yhat[ x_length : full_length ] 
                              / hist)*100
            )

mean(view$Err..., na.rm = T)

summary(md_)
prophet_plot_components(md_, fcst = pred_md_, uncertainty = TRUE, render_plot = TRUE )
?prophet_plot_components
ggplot2::qplot( md_, pred_md_)
rlang::last_error()


#result_file <- paste(c(path, as.character(Sys.Date()), '-predction-', country_name, '-', file_name, '.csv'), collapse = '')
#result_file_JSON <- paste(c(path, as.character(Sys.Date()), '-predction-', country_name, '-', file_name, '.json'), collapse = '')
#current_result_file_JSON <- paste(c(path, country_name, '-', file_name, '-current.json'), collapse = '')


#
# Export
#
#export = data.frame("ds" = pred_md_$ds, "y" = pred_md_$yhat, "y_lower" = pred_md_$yhat_lower, "y_upper" = pred_md_$yhat_upper)

#write.csv2(x = export[ ( nrow(export)-days ):nrow(export), ], file = result_file) #csv2 ja e padro ; e , pra decimal
#write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = result_file_JSON)
#write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = current_result_file_JSON )

predict_country_accumulated_test <- function(x, y, country_name,  days = 6, path = '/var/covid/', file_name = 'confirmed', seasonality.period = 7, x.period = 0, x.back_offset = 0, seasonality.name = 'weekly') {
    
    json_data_DF = data.frame("ds" = x, "y" = y)
    
    y_max <- max(json_data_DF$y)
    json_data_DF$cap <- y_max * 3
    json_data_DF$floor <- 0
    
    json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 0.1, no = json_data_DF$y)
    
    total_obs <- nrow(json_data_DF)
    
    if(x.back_offset > 0) {
        json_data_DF <- json_data_DF[ 1: (total_obs - x.back_offset), ]
        total_obs <- nrow(json_data_DF)
    }
    
    if(x.period > 0) {
        json_data_DF <- json_data_DF[ ( total_obs - x.period ):total_obs, ]
    }
    
    md_ = prophet(fit = FALSE, growth = 'logistic'
                  ,interval.width = 0.95
                  ,changepoint.range = 0.96 
                  ,changepoint.prior.scale = 0.2588
    )
    
    md_ <- add_seasonality(md_, name= seasonality.name, period = seasonality.period, mode = 'multiplicative', fourier.order = 2)  #  prior.scale = y_max * 550)
    #?add_seasonality
    md_ <- fit.prophet(md_, df =  json_data_DF)
    
    future_md_ = make_future_dataframe(md_, days)
    future_md_$cap <- json_data_DF$cap[1]
    future_md_$floor <- json_data_DF$floor[1]
    
    pred_md_ <- predict(md_, future_md_)
    
    #plot(md_, pred_md_)
    
    
    result_file <- paste(c(path, as.character(Sys.Date()), '-predction-', country_name, '-', file_name, '.csv'), collapse = '')
    result_file_JSON <- paste(c(path, as.character(Sys.Date()), '-predction-', country_name, '-', file_name, '.json'), collapse = '')
    current_result_file_JSON <- paste(c(path, country_name, '-', file_name, '-current.json'), collapse = '')
    
    
    #
    # Export
    #
    export = data.frame("ds" = pred_md_$ds, "y" = pred_md_$yhat, "y_lower" = pred_md_$yhat_lower, "y_upper" = pred_md_$yhat_upper)
    
    write.csv(x = export[ ( nrow(export)-days ):nrow(export), ], file = result_file) #csv2 ja e padro ; e , pra decimal
    write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = result_file_JSON)
    write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = current_result_file_JSON )
    
    return(json_data_DF)
       
}

fancy_scientific <- function(l) {
    # turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e", "%*%10^", l)
    # return this as an expression
    parse(text=l)
}


#
# JSON
#

fullCountriesJson <- fromJSON(txt = 'https://pomber.github.io/covid19/timeseries.json')
CtJson <- fullCountriesJson

plot(fullCountriesJson$Brazil$confirmed)

# BR
predict_country_accumulated_test(x = fullCountriesJson$Brazil$date, 
                            y = fullCountriesJson$Brazil$confirmed
                            , country_name =  'Brazil'
                            , path = ''
                            , file_name = 'confirmed-7days'
                            , x.period = 7)

#BACK OFFSET 
used_history <- predict_country_accumulated_test(x = fullCountriesJson$Brazil$date, 
                                 y = fullCountriesJson$Brazil$confirmed
                                 , country_name =  'Brazil'
                                 , path = ''
                                 , file_name = 'confirmed-fulldays-backOffset-11'
                                 , seasonality.period = 7
                                 , seasonality.name = 'weekly'
                                 , x.back_offset = 12
                                 , days = 8)


used_history <- predict_country_accumulated_test(x = fullCountriesJson$Brazil$date, 
                                                 y = fullCountriesJson$Brazil$confirmed
                                                 , country_name =  'Brazil'
                                                 , path = ''
                                                 , file_name = 'confirmed-7days-backOffset-11'
                                                 , seasonality.period = 7
                                                 , seasonality.name = 'weekly'
                                                 , x.back_offset = 12
                                                 ,x.period = 7
                                                 , days = 7)


used_history <- predict_country_accumulated_test(x = fullCountriesJson$Brazil$date, 
                                                 y = fullCountriesJson$Brazil$confirmed
                                                 , country_name =  'Brazil'
                                                 , path = ''
                                                 , file_name = 'confirmed-7days-backOffset-11-Calibrado'
                                                 , x.period = 7
                                                 , seasonality.period = 1
                                                 , seasonality.name = 'daily'
                                                 , x.back_offset = 12
                                                 , days = 7)



ggplot(data = X2020_04_16_predction_Brazil_confirmed_7days_backOffset_11) + geom_line(aes(x=ds, y = y), col="red")

X2020_04_16_predction_Brazil_confirmed_7days <- read_csv2("2020-04-16-predction-Brazil-confirmed-7days.csv", col_names = TRUE)

#4 DAYS
X2020_04_16_predction_Brazil_confirmed_fulldays$four_days_y <- X2020_04_16_predction_Brazil_confirmed_4days$y
X2020_04_16_predction_Brazil_confirmed_fulldays$four_days_y_lower <- X2020_04_16_predction_Brazil_confirmed_4days$y_lower
X2020_04_16_predction_Brazil_confirmed_fulldays$four_days_y_upper <- X2020_04_16_predction_Brazil_confirmed_4days$y_upper

#7 DAYS
X2020_04_16_predction_Brazil_confirmed_fulldays$seven_days_y <- X2020_04_16_predction_Brazil_confirmed_7days$y
X2020_04_16_predction_Brazil_confirmed_fulldays$seven_days_y_lower <- X2020_04_16_predction_Brazil_confirmed_7days$y_lower
X2020_04_16_predction_Brazil_confirmed_fulldays$seven_days_y_upper <- X2020_04_16_predction_Brazil_confirmed_7days$y_upper

#7 DAYS - OFFSET
#X2020_04_16_predction_Brazil_confirmed_7days_backOffset_11 <- read.csv("2020-04-16-predction-Brazil-confirmed-7days-backOffset-11.csv", header = TRUE)
#X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11 <- read.csv2("2020-04-16-predction-Brazil-confirmed-fulldays-backOffset-11.csv", dec = ',', sep = ";")

X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11$seven_days_y <- X2020_04_16_predction_Brazil_confirmed_7days_backOffset_11$y
X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11$seven_days_y_lower <- X2020_04_16_predction_Brazil_confirmed_7days_backOffset_11$y_lower
X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11$seven_days_y_upper <- X2020_04_16_predction_Brazil_confirmed_7days_backOffset_11$y_upper



sapply(X2020_04_15_predction_Brazil_confirmed_fulldays, class)

plot(X2020_04_16_predction_Brazil_confirmed_fulldays$ds, X2020_04_16_predction_Brazil_confirmed_fulldays$y)

plot(fullCountriesJson$Brazil$ds, X2020_04_16_predction_Brazil_confirmed_fulldays$y)

ggplot(data = X2020_04_16_predction_Brazil_confirmed_fulldays) + 
    geom_line(aes(x=ds, y = y), col="red") + 
    #geom_line(aes(x=ds, y = four_days_y), col="blue") + 
    #geom_line(aes(x=ds, y = four_days_y), col="orange") + 
    geom_line(aes(x=ds, y = seven_days_y), col="darkgreen") + 
    geom_vline(xintercept = X2020_04_16_predction_Brazil_confirmed_fulldays$ds[2] ) +
    scale_y_continuous(labels=fancy_scientific) 


#OFFSET
ggplot(data = X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11) + 
    geom_line(aes(x=ds, y = y), col="red") + 
    geom_line(aes(x=ds, y = seven_days_y), col="darkgreen") + 
    geom_vline(xintercept = X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11$ds[2] )

X2020_04_16_predction_Brazil_confirmed_fulldays_backOffset_11

#BR Deaths
predict_country_accumulated_test(x = fullCountriesJson$Brazil$date, 
                            y = fullCountriesJson$Brazil$deaths
                            , seasonality.period = 4
                            , path = ''
                            , country_name = 'Brazil'
                            , file_name = 'deaths')

write.csv2(fullCountriesJson$Brazil, file = "Brazil-history-16-04.csv")

#fullCountriesJson$Brazil$date[ match('2020-3-29', CtJson$Brazil$date):match('2020-4-4', CtJson$Brazil$date)


#polynomial 
######################
## SIMULAÇÃO 04 queda ok igual Excel com ORDEM 3 do Polinomio
######################
X04_29_03 <- data.frame( "id" = seq(1,7)
                        ,"confirmed" = fullCountriesJson$Brazil$confirmed[ match('2020-3-29', CtJson$Brazil$date):match('2020-4-4', CtJson$Brazil$date)]
                        )

sapply(X04_29_03, class)
X04_29_03$ds <- sapply(X04_29_03$ds, as.character)

mod_lin = lm( confirmed ~ id, data = X04_29_03)
mod_poly = lm( confirmed ~ poly(id, degree = 3, raw = TRUE), data = X04_29_03)
mod = lm( confirmed ~ id + I(id^2), data = X04_29_03)
summary(mod)
summary(mod_lin)
summary(mod_poly)

plot(X04_29_03$id, X04_29_03$confirmed)
abline(mod_lin, col="red", lwd=2)
lines(smooth.spline(X04_29_03$id, predict(mod_poly)), col="blue", lwd=2)

mod$model

new_data <- data.frame('id' = seq(8, 14))

plot(  c(X04_29_03$confirmed , predict(mod_poly, newdata = new_data ) ), ylim = c(1000, 30000) )
lines(smooth.spline(new_data$id, predict(mod_poly, newdata = new_data)), col="blue", lwd=2)
lines(smooth.spline(seq(8,14), X2020_04_17_predction_Brazil_confirmed_fulldays_backOffset_11$y[2:8]), col="red", lwd=3)

lines(smooth.spline(seq(8,14), X2020_04_17_predction_Brazil_confirmed_7days_backOffset_11$y[2:8]), col="pink", lwd=2)

X04_29_03$confirmed
CtJson$Brazil$confirmed[ 75 : 81 ]
CtJson$Brazil$date[ 75 : 81 ]

#7 dias com Calibragem da sazonalidade
lines(smooth.spline(seq(8,14), X2020_04_17_predction_Brazil_confirmed_7days_backOffset_11_Calibrado$y[2:8]), col="orange", lwd=2)

lines(smooth.spline(seq(8,14), CtJson$Brazil$confirmed[ match( '2020-4-5',  CtJson$Brazil$date ) : match( '2020-4-11',  CtJson$Brazil$date ) ]) , col="darkgreen", lwd=2, lty=3 )

( data.frame(
"hist-H" = CtJson$Brazil$confirmed[ match( '2020-4-5',  CtJson$Brazil$date ) : match( '2020-4-12',  CtJson$Brazil$date ) ]
, T50dias = X2020_04_17_predction_Brazil_confirmed_fulldays_backOffset_11$y[ 2:9 ]
, T7dias = X2020_04_17_predction_Brazil_confirmed_7days_backOffset_11_Calibrado$y[ 2:9 ]
, "T7dias-H" = abs( X2020_04_17_predction_Brazil_confirmed_7days_backOffset_11_Calibrado$y[ 2:9 ] - CtJson$Brazil$confirmed[ match( '2020-4-5',  CtJson$Brazil$date ) : match( '2020-4-12',  CtJson$Brazil$date ) ] )
, "T50dias-H" = abs( X2020_04_17_predction_Brazil_confirmed_fulldays_backOffset_11$y[ 2:9 ] - CtJson$Brazil$confirmed[ match( '2020-4-5',  CtJson$Brazil$date ) : match( '2020-4-12',  CtJson$Brazil$date ) ] )
))

(data.frame( CtJson$Brazil$date, CtJson$Brazil$confirmed))



ggplot(X04_29_03) + 
geom_line(aes(x=ds, y=y, group = 1)) +
geom_smooth()




fullCountriesJson$Brazil$date















