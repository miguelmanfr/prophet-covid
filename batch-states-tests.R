
#apt-get install libcurl4-openssl-
#apt-get install g++-8 
#install.packages("curl")
#install.packages("prophet")
#install.packages("jsonlite")

options(digits=7) #options(digits=3)
options(scipen=0) #options(scipen=999)


library(prophet)
library(curl)
library(jsonlite)
library(parallel)
library(foreach)
library(doParallel)

predict_run_model <- function(json_data_DF, use.seasonality = FALSE, seasonality.period = 7, predict.days = 7) {
  
    md_ = prophet::prophet(fit = FALSE, 
                           growth = 'logistic'
                           ,interval.width = 0.95
                           ,changepoint.range = 0.98 
                           #,changepoint.prior.scale = 0.2588
                           ,changepoint.prior.scale = 15
                           ,yearly.seasonality = F
                           ,weekly.seasonality = F
                           ,daily.seasonality = F
    )
    
    if(use.seasonality) {
      md_ <- prophet::add_seasonality(md_, 
                                      name='weekly', 
                                      period = seasonality.period, 
                                      mode = 'multiplicative', 
                                      fourier.order = 1,
                                      #.prior.scale = 0.05
      )
    }
    
    md_ <- prophet::fit.prophet(md_, df =  json_data_DF)
    
    future_md_ = prophet::make_future_dataframe(md_, predict.days)
    future_md_$cap <- json_data_DF$cap[1]
    future_md_$floor <- json_data_DF$floor[1]
    
    pred_md_ <- stats::predict(md_, future_md_)
    
    #Avoid Prophet NaN result when serie has no variation
    pred_md_$yhat <- ifelse(is.nan(pred_md_$yhat), yes = 0.1, no = pred_md_$yhat)
    
    
    #
    # Export
    #
    export = base::data.frame("ds" = pred_md_$ds, "y" = pred_md_$yhat, "y_lower" = pred_md_$yhat_lower, "y_upper" = pred_md_$yhat_upper)
    
    return( tail(export, n=predict.days ) )
    
}

predict_accumulated <- function(x, y, predict.days = 8, seasonality.periods = c(7,9), x.back_offset = 0) {
    
    json_data_DF = base::data.frame("ds" = x, "y" = y)
    return_ = base::data.frame()
    
    total_obs <- nrow(json_data_DF)
    json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 1, no = json_data_DF$y)
    
    if(x.back_offset > 0 && total_obs > ( x.back_offset) ) {
      json_data_DF <- json_data_DF[ 1: (total_obs - x.back_offset), ]
      total_obs <- nrow(json_data_DF)
    }
    
    total_obs = ifelse( is.na(total_obs), yes = 0, no = total_obs )
    
     if(total_obs > 7) {
         y_max <- ifelse(max(json_data_DF$y)>0, yes = max(json_data_DF$y), no = 100)
     
         ### CAP FACTOR ###
     
         y_serie_div <- json_data_DF$y[ (length(json_data_DF$y) - predict.days) : length(json_data_DF$y) ]
     
         subttract = c(0, head(y_serie_div, n = (length(y_serie_div)-1) ) )
     
         diff = ( tail( (y_serie_div - subttract), n = (length(y_serie_div) - 1) ) )
     
         diff_max_div_factor = max( abs( tail(( diff / c(1, head( diff, n = (length(diff)-1) ) ) ),
                                         n= length(diff)-1) ) )
         
         #Default for multiply 1.5
         diff_max_div_factor <- ifelse( is.finite(diff_max_div_factor) && diff_max_div_factor > 1 , yes = diff_max_div_factor, no = 1.5 ) 
         
         ### CAP FACTOR ###
         json_data_DF$cap <- (y_max * diff_max_div_factor) #1.383178654 #1.5689
         json_data_DF$floor <- 0.1
     
         result_no_seasonality = predict_run_model(json_data_DF, predict.days =  predict.days)
         rmse_p_no_seasonality = sqrt( mean( ((tail( json_data_DF$y, n=7) - result_no_seasonality$y )^2) / result_no_seasonality$y ) )
         
         #LOOP periods to find best fit for serie
         last_rmse = 999
         rmse_p_with_seasonality = 0

         for(period in seasonality.periods) {
             result_ = predict_run_model(json_data_DF, use.seasonality = TRUE, period, predict.days)
             rmse_p_with_seasonality = sqrt( mean( ( ((tail( json_data_DF$y, n=7) - result_$y)  / result_$y) ^ 2)) )

             if(rmse_p_with_seasonality < last_rmse) {
                 last_rmse = rmse_p_with_seasonality
                 result_with_seasonality = result_
             }
         }

         if(rmse_p_with_seasonality < rmse_p_no_seasonality) {
              return_ = result_with_seasonality
         } else {
              return_ = result_no_seasonality
         }
     
     } else {
       return_ = base::data.frame()
     }
        
    return(return_)
}

registerDoParallel( cores = ( detectCores(logical = T) - 10 ))
#registerDoSEQ() <- para fazer sequencialmente

setwd("~/R/covid")

#args = commandArgs(trailingOnly = TRUE)
#input_file <- args[1]
#use.seasonality.arg <- ifelse(args[2] == "1", TRUE, FALSE)
#input_file <- 'state-test/innerserie.json' 
input_file <- 'state-test/prediction-br.json'
input_file <- 'state-test/csv-city.json'
#input_file <- 'state-test/state-timeseries-US.json'
#Cidades Brasileiras https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv

#input_file_list <- fromJSON('spume.co/covid/get-file-proccess-list')
#input_file_list <- list('state-test/state-timesearies.json', 'state-test/state-timeseries-US.json')

#for ( i in 1:length(input_file_list)) {
    
#input_file <- input_file_list[[i]]

file_name <- strsplit(input_file, "[.]")[[1]][1]
#use.seasonality.arg = T

#
# JSON
#
inputJson <- fromJSON(txt = input_file)
result_file_JSON <- paste(c(file_name, '-prediction-confirmed-', as.character(Sys.Date()), '.json'), collapse = '')
result_file_JSON_current <- paste(c(file_name, '-prediction-confirmed-current', '.json'), collapse = '')


fullCountriesJson <- fromJSON(txt = 'state-test/timeseries-countries.json')
tail(fullCountriesJson$Brazil)
tail(inputJson$São.Paulo.SP)


## SP
##################
export <- data.frame()

# Call
result <- predict_accumulated(x = inputJson$São.Paulo.SP[[1]]$date, #x = inputJson[[i]]$date, 
                              y = inputJson$São.Paulo.SP[[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
                              x.back_offset = 7,
                              predict.days = 7)

result
tail(inputJson$São.Paulo.SP[[1]], n=7)
(data.frame("hist" = tail(inputJson$São.Paulo.SP[[1]]$confirmed, n=7), "pred" = result))

sqrt( mean( (( (tail(inputJson$São.Paulo.SP[[1]]$confirmed, n=7) - result$y ) )^2)/result$y )) #Ainda com 10.9% de erro na media de 7 dias

#R2 = 3.6 usando sazonalidade com 7 dias ! falta testar SP e depois soma total

key_name <- names(inputJson[i][1])
export[1, key_name][[1]] <- list(result)

plot(inputJson$São.Paulo.SP[[1]]$confirmed)

##################

## RJ
##################

# Call
result <- predict_accumulated(x = inputJson$Rio.de.Janeiro.RJ[[1]]$date, #x = inputJson[[i]]$date, 
                              y = inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
                              x.back_offset = 7,
                              predict.days = 7)

result
tail(inputJson$Rio.de.Janeiro.RJ[[1]], n=7)
(data.frame("hist" = tail(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, n=7), "pred" = result))

sqrt( mean( (( (tail(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, n=7) - result$y )/result$y )^2) ) ) #Ainda com 10.9% de erro na media de 7 dias

hist(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed)
plot(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed)

## CE
##################
# Call
result <- predict_accumulated(x = inputJson$Fortaleza.CE[[1]]$date, #x = inputJson[[i]]$date, 
                              y = inputJson$Fortaleza.CE[[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
                              x.back_offset = 7,
                              predict.days = 7,
                              )

result
tail(inputJson$Fortaleza.CE[[1]], n=7)
(data.frame("hist" = tail(inputJson$Fortaleza.CE[[1]]$confirmed, n=7), "pred" = result))

sqrt( mean( (( (tail(inputJson$Fortaleza.CE[[1]]$confirmed, n=7) - result$y ) ^2)/result$y) ) ) #Ainda com 10.9% de erro na media de 7 dias


hist(inputJson$Fortaleza.CE[[1]]$confirmed)
plot(inputJson$Fortaleza.CE[[1]]$confirmed)
plot(result$y, ty)


key_name <- names(inputJson[i][1])
export[1, key_name][[1]] <- list(result)




plot(tail(x=inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, n=7), col = "blue") 
lines(x =  result$y, col="red")

##################

export = data.frame()
export <- foreach(i = 1:length(inputJson), .combine = 'data.frame',   .packages = c('prophet', 'jsonlite', 'curl')) %dopar% {
#export <- foreach(i = 919:920, .combine = 'data.frame',   .packages = c('prophet', 'jsonlite', 'curl')) %dopar% {
  
    fileConn2<-file("results_log4.txt", open = "a")
    writeLines(paste( i, "Iniciando", names(inputJson[i]), max(inputJson[[i]][[1]]$confirmed), sep = ":"), fileConn2)
    
    export <- data.frame()
    
    
    result <- tryCatch({
      # Call
       predict_accumulated(x = inputJson[[i]][[1]]$date, #x = inputJson[[i]]$date, 
                            y = inputJson[[i]][[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
                            #use.seasonality = use.seasonality.arg,
                            x.back_offset = 7,
                            predict.days = 7)

    }, error = function(e) {
        
      
        fileConn4<-file("errors_catched.txt", open = "a")
        writeLines(paste( i, names(inputJson[i]), max(inputJson[[i]][[1]]$confirmed), message(e), sep = ":"), fileConn4)
        close(fileConn4)
        
        return(data.frame()) 
    })
    
    

    key_name <- names(inputJson[i][1])
    export[1, key_name][[1]] <- list(result)
    
    history = tail(inputJson[[i]][[1]]$confirmed, n=7)
    
    if(length(history) == length(result$y) ){
        rsme = mean( sqrt(  (( (history - result$y )/result$y )^2) ) )
        fileConn<-file("results_rmse.txt", open = "a")
        writeLines(paste( i, names(inputJson[i]), rsme, max(inputJson[[i]][[1]]$confirmed), sep = ":"), fileConn)
        close(fileConn)
        
        writeLines(paste( i, "Finalizado", names(inputJson[i]), max(inputJson[[i]][[1]]$confirmed), sep = ":"), fileConn2)
        
        
        
    }
    
    close(fileConn2)
    
    return(export)
    
}

???names(inputJson[2][1])
inputJson[[2]][[1]]

#
# Export
#
write_json(x = export, path = result_file_JSON)
write_json(x = export, path = result_file_JSON_current)

#SUM
sum = rep(0, length(export[[1]][[1]]$ds))

for (date_pred in 1:length(export[[1]][[1]]$ds) ) { 
  
  for (i in 1:ncol(export)) { 
    
    if(length( export[1,i][[1]]$y ) > 0) {
        
        val_ = export[1,i][[1]]$y[ date_pred : date_pred ]
        
        if(!is.nan(val_)) {
            sum[date_pred] = sum[date_pred] + val_
        }
        
    }
  }
}


fullCountriesJson$Brazil$confirmed[ match('2020-5-7', fullCountriesJson$Brazil$date) : match('2020-5-13', fullCountriesJson$Brazil$date)  ]


countryVal <- fullCountriesJson$Brazil$confirmed[ match('2020-5-7', fullCountriesJson$Brazil$date) : match('2020-5-13', fullCountriesJson$Brazil$date) ]

subttract = c(0, head(countryVal, n = (length(countryVal)-1) ) )

diff = ( tail( (countryVal - subttract), n = (length(countryVal) - 1) ) )

diff_max_div_factor = max( tail( ( diff / c(1, head( diff, n = (length(diff)-1) ) ) ),
                                n= length(diff)-1) )

sum


export$São.Paulo.SP[[1]]$ds
fullCountriesJson$Brazil$date[ match('2020-5-7', fullCountriesJson$Brazil$date) : match('2020-5-13', fullCountriesJson$Brazil$date)  ]

sum_hist = rep(0, length(export[[1]][[1]]$ds))
sum_hist_df = data.frame()


for (date_hist in 1:4) { 
  
  current_date = inputJson[1][[1]]$date[  ( match('2020-5-3', inputJson[1][[1]]$date) + date_hist ) : ( match('2020-5-3', inputJson[1][[1]]$date) + date_hist ) ]
  
  for (i in 1:ncol(export)) { 
    
    
    date_idx = match( current_date, inputJson[ i ][[1]]$date)
    
    if(!is.na(date_idx)) {
        val_ = inputJson[ i ][[1]]$confirmed[  date_idx : date_idx ]
        sum_hist[date_hist] = sum_hist[date_hist] + val_
        
        
    }
    
  }
  
  sum_hist_df[date_hist, "date"] = current_date
  sum_hist_df[date_hist, "hist. City Sum"] = sum[date_hist]
  print(current_date)
  print(sum_hist)
}


#match( current_date, inputJson[ 43 ][[1]]$date)


sum_hist_df <- data.frame( sum_hist_df, "Country BR" = fullCountriesJson$Brazil$confirmed[ match('2020-5-4', fullCountriesJson$Brazil$date) : match('2020-5-7', fullCountriesJson$Brazil$date)  ] ) 


fullCountriesJson$Brazil$date[ match('2020-5-4', fullCountriesJson$Brazil$date) : match('2020-5-7', fullCountriesJson$Brazil$date)  ]

inputJson[1][[1]]$date[  ((hist_len - 4) + 2) : ((hist_len - 4) + 2) ]


d = 6
i=1
diff_list = data.frame()

for (i in 1:ncol(export)) {
  
  hist_length = length(inputJson[[i]][[1]]$date) #length(inputJson[[i]]$date)
  
  if (hist_length>d) {
      
      history_values <- inputJson[[i]][[1]]$confirmed[ (hist_length - d): hist_length] #inputJson[[i]]$confirmed[ ( hist_length - d ):hist_length]
      city <- names(inputJson[i])
      
      pred_length = length(export[[i]][[1]]$ds)
      pred_values <- export[[i]][[1]]$y
      
      if(pred_length) {
          diff_list[i, "Err. mean %"] = mean( ((pred_values - history_values)/pred_values)*100 )
          diff_list[i, "Err. max %"] = max(abs ( ((pred_values - history_values)/pred_values)*100 ) )
          diff_list[i, "predict sum"] = sum( pred_values )
          #diff_list[i, "Sum / Max"] = sum( pred_values ) / max(pred_values)
          diff_list[i, "Country Value Repre. %"] = (tail(pred_values, n = 1) / tail(fullCountriesJson$Brazil$confirmed, n=1))*100
          diff_list[i, "city"] = city
      }
  }
}

View(diff_list)

inputJson[[i]][[1]]$date[ (hist_length-d):hist_length]
export[[1]][[1]]$y




inputJson$CASO.SEM.LOCALIZAÇÃO.DEFINIDA.PE[[1]] 
(tail(export$CASO.SEM.LOCALIZAÇÃO.DEFINIDA.PE[[1]]$y, n = 1) / tail(fullCountriesJson$Brazil$confirmed, n=1))*100

tail(inputJson$`CASO SEM LOCALIZAÇÃO DEFINIDA/RJ`)
tail( export$CASO.SEM.LOCALIZAÇÃO.DEFINIDA.RJ )

tail(inputJson$`Taboão da Serra/SP`)
tail( export$Taboão.da.Serra.SP )

tail(inputJson$`Maceió/AL`)
tail( export$Maceió.AL )

tail(fullCountriesJson$Brazil)

summary(df) <- inputJson$`Florianópolis/SC`

summary(fullCountriesJson$Brazil)

par(mfrow = c(2,1))
plot(x= as.Date(inputJson$`Florianópolis/SC`$date), y = inputJson$`Florianópolis/SC`$confirmed )
plot(x= as.Date(inputJson$`São Paulo/SP`$date), y = inputJson$`São Paulo/SP`$confirmed )

plot(x= as.Date(fullCountriesJson$Brazil$date), y = fullCountriesJson$Brazil$confirmed)


sum
plot(export[[1]][[1]]$ds, sum)

inputJson$`Rio de Janeiro/RJ`$confirmed[ (length(inputJson$`Rio de Janeiro/RJ`$date)-7): length(inputJson$`Rio de Janeiro/RJ`$date) ] 
as.integer( export$Rio.de.Janeiro.RJ[[1]]$y )

inputJson$`São Paulo/SP`$confirmed[ (length(inputJson$`São Paulo/SP`$date)-7): length(inputJson$`São Paulo/SP`$date) ]
as.integer( export$São.Paulo.SP[[1]]$y )


json_data_DF = base::data.frame("ds" = inputJson[[i]]$date, "y" = inputJson[[i]]$confirmed)
json_data_DF <- json_data_DF[ 1: (nrow(json_data_DF) - 7), ]



tail(fullCountriesJson$Brazil, n=1)

export TESTAR SOMA AGORA SE FICOU MELHOR DO QUE A USANDO A SERIE DO PAIS TODO

