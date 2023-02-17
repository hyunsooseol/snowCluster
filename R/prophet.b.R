
# This file is a generated template, your changes will not be overwritten
#' @import forecast 
#' @import ggplot2
#' @import jmvcore
#' @import prophet 
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom forecast forecast
#' @importFrom prophet prophet
#' @importFrom prophet make_future_dataframe
#' @importFrom prophet prophet_plot_components
#' @export



prophetClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "prophetClass",
    inherit = prophetBase,
    private = list(
      .init = function() {
        
        
        if (is.null(self$options$dep)){
          
          
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            
            <p><b>Instructions</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. Do NOT move the variable into <b>Time box</b> to run ARIMA analysis. 
            <p>2. ARIMA options are classified by two factors; <b>Frequency</b>= the number of observations per unit of time. <b>Prediction</b>= number of periods for forecasting.</p>
            <p>3. The results of ARIMA were implemented with <b>auto.arima() and forecast() function</b> in R.</p>
            <p>4. The variable name to be placed in the 'Time' box must have the name <b>'ds'</b> to run prophet analysis.
            <p>5. The rationale of <b>forecast</b> R package is described in the <a href='https://cran.r-project.org/web/packages/forecast/vignettes/JSS2008.pdf' target = '_blank'>documentation.</a></p>
            <p>6. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
       
      },
      
      ################################################################## 
      .run = function() {
        
        
        if(is.null(self$options$dep))
          return()
        
        dep  <- self$options$dep
        covs <- self$options$covs
       
        # get the data
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        
        # Create a list to store the forecast data frames
        forecast_df_list <- list()
        
        # Define the names of the variables
        #var_names <- c("mdeaths", "fdeaths")
        
        
        # Loop through each variable and create a forecast data frame
        for (i in 1:length(covs)) {

         
          #new_data <- data %>% dplyr::select(ds, all_of(var_names[i])) %>% dplyr::rename(y = all_of(var_names[i]))
          
          new_data <- data %>% dplyr::select(ds, all_of(covs[i])) %>% dplyr::rename(y = all_of(covs[i]))
          
          
          # Fit the model
          model <- prophet::prophet(new_data,
                                    changepoint.prior.scale = 0.05,
                                    daily.seasonality = TRUE,
                                    yearly.seasonality = TRUE,
                                    weekly.seasonality = TRUE)
          
          
          #model <- prophet(new_data, changepoint.prior.scale = 0.05, 
          #                  daily.seasonality=TRUE,
          #                  yearly.seasonality = TRUE, 
          #                  weekly.seasonality = TRUE)
          # 
          # Make predictions for the next 365 days
          future <- prophet::make_future_dataframe(model, periods = 365)
          forecast <- predict(model, future)
          
          # Add the forecast to the list
          forecast_df_list[[i]] <- data.frame(ds = forecast$ds, yhat = forecast$yhat, variable = covs[i])
        }
        
        # Combine the forecast data frames into one
        forecast_combined <- do.call(rbind, forecast_df_list)
        
        #self$results$text$setContent(head(forecast_combined ))
        
        # forecast plot----------
        
        image <- self$results$plot1
        image$setState(forecast_combined)
        
      # Smooth plot--------------
        
        image1 <- self$results$plot2
        image1$setState(forecast_combined)
        
        
        
        
        },
     

.plot1 = function(image,ggtheme, theme,...) {
  

  forecast_combined <- image$state
  
  
  plot1 <- ggplot(forecast_combined, aes(x = ds, y = yhat, color = variable)) +
    geom_line() +
    labs(x = "Date", y = "Forecast", color = "Variable") +
  #  ggtitle("Forecast of Multiple Variables") +
    theme_bw()
  
  plot1+ggtheme
  
  print(plot1)
  TRUE
},
        
.plot2 = function(image1,ggtheme, theme,...) {
  
  
  forecast_combined <- image1$state
  
  
  plot2<- ggplot(forecast_combined, aes(x = ds, y = yhat, color = variable)) +
    geom_smooth() +
    labs(x = "Date", y = "Forecast", color = "Variable") +
   # ggtitle("Forecast of Multiple Variables") +
    theme_bw()
  
  plot2+ggtheme
  
  print(plot2)
  TRUE
}    
        
        
        
)
)
