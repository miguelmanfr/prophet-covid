#apt-get install libcurl4-openssl-
#apt-get install g++-8 
#install.packages("curl")
#install.packages("prophet")
#install.packages("jsonlite")

library(prophet)
library(curl)
library(jsonlite)

predict_country_accumulated <- function(x, y, country_name,  days = 8, path = '/var/covid/', file_name = 'confirmed', seasonality.period = 7 ) {
  
    json_data_DF = data.frame("ds" = x, "y" = y)
    
    y_max <- max(json_data_DF$y)
    json_data_DF$cap <- y_max * 3
    json_data_DF$floor <- 0
    
    json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 0.1, no = json_data_DF$y)
    
    md_ = prophet(fit = FALSE, growth = 'logistic'
                  ,interval.width = 0.95
                  ,changepoint.range = 0.98 
                  ,changepoint.prior.scale = 0.2588
    )
    
    md_ <- add_seasonality(md_, name='weekly', period = seasonality.period, mode = 'multiplicative', fourier.order = 2)  #  prior.scale = y_max * 550)
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
    
    write.csv2(x = export[ ( nrow(export)-days ):nrow(export), ], file = result_file) #csv2 ja e padro ; e , pra decimal
    write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = result_file_JSON)
    write_json(x = export[ ( nrow(export)-days ):nrow(export), ], path = current_result_file_JSON )
    
}


#
# JSON
#

fullCountriesJson <- fromJSON(txt = 'https://pomber.github.io/covid19/timeseries.json')

# BR
predict_country_accumulated(x = fullCountriesJson$Brazil$date, y = fullCountriesJson$Brazil$confirmed, 'Brazil', file_name = 'confirmed')

#BR Deaths
predict_country_accumulated(x = fullCountriesJson$Brazil$date, y = fullCountriesJson$Brazil$deaths, seasonality.period = 4,  'Brazil', file_name = 'deaths')

#EUA / US
predict_country_accumulated( x = fullCountriesJson$US$date, y = fullCountriesJson$US$confirmed, 'US', file_name = 'confirmed')

#EUA / US Deaths
predict_country_accumulated( x = fullCountriesJson$US$date, y = fullCountriesJson$US$deaths,  seasonality.period = 4, 'US', file_name = 'deaths')







