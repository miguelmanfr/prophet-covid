
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
library(doParallel)sd

predict_accumulated <- function(x, y, predict.days = 8, seasonality.period = 9, use.seasonality = FALSE) {
  
    json_data_DF = base::data.frame("ds" = x, "y" = y)
    
    y_max <- max(json_data_DF$y)
    json_data_DF$cap <- y_max * 10
    json_data_DF$floor <- 0
    
    json_data_DF$y <- ifelse(test = json_data_DF$y==0, yes = 0.1, no = json_data_DF$y)
    
    md_ = prophet::prophet(fit = FALSE, growth = 'logistic'
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
    
    #
    # Export
    #
    export = base::data.frame("ds" = pred_md_$ds, "y" = pred_md_$yhat, "y_lower" = pred_md_$yhat_lower, "y_upper" = pred_md_$yhat_upper)
    
    return(export[ ( nrow(export)-predict.days ):nrow(export), ])
}

registerDoParallel( cores = ( detectCores(logical = T) - 2 ))
#registerDoSEQ() <- para fazer sequencialmente

args = commandArgs(trailingOnly = TRUE)
input_file <- args[1]
use.seasonality.arg <- ifelse(args[2] == "1", TRUE, FALSE)
#input_file <- 'state-test/innerserie.json' 
#input_file <- 'state-test/state-timesearies.json'
#input_file <- 'state-test/state-timeseries-US.json'

#input_file_list <- fromJSON('spume.co/covid/get-file-proccess-list')
#input_file_list <- list('state-test/state-timesearies.json', 'state-test/state-timeseries-US.json')

#for ( i in 1:length(input_file_list)) {
    
#input_file <- input_file_list[[i]]

file_name <- strsplit(input_file, "[.]")[[1]][1]

#
# JSON
#
inputJson <- fromJSON(txt = input_file)
result_file_JSON <- paste(c(file_name, '-prediction-confirmed-', as.character(Sys.Date()), '.json'), collapse = '')
result_file_JSON_current <- paste(c(file_name, '-prediction-confirmed-current', '.json'), collapse = '')


export <- foreach(i = 1:length(inputJson), .combine = 'data.frame',   .packages = c('prophet', 'jsonlite', 'curl')) %dopar% {
    
    export <- data.frame()
    
    # Call
    result <- predict_accumulated(x = inputJson[[i]]$date, y = inputJson[[i]]$confirmed, use.seasonality = use.seasonality.arg)
    key_name <- names(inputJson[i][1])
    export[1, key_name][[1]] <- list(result)
    
    #print(paste(c(key_name, " calculated!"), collapse = ' '))
    #print(paste(c(key_name, " calculated!"), collapse = ' '))
    
    return(export)
}


#
# Export
#
write_json(x = export, path = result_file_JSON)
write_json(x = export, path = result_file_JSON_current)
    
#}




