

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
        
        diff_max_div_factor = max( tail(( diff / c(1, head( diff, n = (length(diff)-1) ) ) ),
                                        n= length(diff)-1) )
        
        diff_max_div_factor <- ifelse( is.finite(diff_max_div_factor) && diff_max_div_factor > 1 , yes = diff_max_div_factor, no = 1.5 )
        
        ### CAP FACTOR ###
        json_data_DF$cap <- y_max * diff_max_div_factor #1.383178654 #1.5689
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

#result <- predict_accumulated(x = inputJson$Barra.Mansa.RJ[[1]]$date, #x = inputJson[[i]]$date, 
#                              y = inputJson$Barra.Mansa.RJ[[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
#                              x.back_offset = 7,
#                              predict.days = 7)


#tail(inputJson$Rio.de.Janeiro.RJ[[1]], n=7)
#(data.frame("hist" = tail(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, n=7), "pred" = result))

#sqrt( mean( (( (tail(inputJson$Rio.de.Janeiro.RJ[[1]]$confirmed, n=7) - result$y )/result$y )^2) ) ) #Ainda com 10.9% de erro na media de 7 dias

# export <- foreach(i = 2:(length(inputJson)-3075), .combine = 'data.frame',   .packages = c('prophet', 'jsonlite', 'curl')) %dopar% {
#     
#     export <- data.frame()
#     
#     # Call
     result <- predict_accumulated(x = inputJson[[i]][[1]]$date, #x = inputJson[[i]]$date, 
                                   y = inputJson[[i]][[1]]$confirmed, #y = inputJson[[i]]$confirmed, 
                                   #use.seasonality = use.seasonality.arg,
                                   x.back_offset = 7,
                                   predict.days = 7)
#     
#     
#     key_name <- names(inputJson[i][1])
#     export[1, key_name][[1]] <- list(result)
#     
#     history = tail(inputJson[[i]][[1]]$confirmed, n=7)
#     
#     if(length(history) == length(result$y) ){
#         rsme = mean( sqrt(  (( (history - result$y )/result$y )^2) ) )
#         fileConn<-file("results_rmse.txt", open = "a")
#         writeLines(paste( i, names(inputJson[i]), rsme, sep = ":"), fileConn)
#         close(fileConn)
#     }
#     
#     return(export)
#     
# }

#registerDoSEQ() <- para fazer sequencialmente
#source("batch-states-tests-DEBUG.R")


