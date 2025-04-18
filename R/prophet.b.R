
# This file is a generated template, your changes will not be overwritten

#' @importFrom magrittr %>%

prophetClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "prophetClass",
    inherit = prophetBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$dep)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Prophet analysis requires the date column to be in a specific format (%Y-%m-%d). Otherwise, an error occurs.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot2$setSize(width, height)
        }
        
      },
      
      ##################################################################
      .run = function() {
        if (is.null(self$options$dep) || length(self$options$covs) == 0)
          return()
        
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeSIMPLE()
        }
        
        forecast_combined <- private$.allCache
        # forecast plot----------
        image <- self$results$plot1
        image$setState(forecast_combined)
        
        # Smooth plot--------------
        image1 <- self$results$plot2
        image1$setState(forecast_combined)
      },

      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        forecast_combined <- image$state
        
        library(ggplot2)
        plot1 <- ggplot2::ggplot(forecast_combined, aes(x = ds, y = yhat, color = variable)) +
          geom_line() +
          labs(x = "Date", y = "Forecast", color = "Variable") +
          # ggtitle("Forecast of Multiple Variables") +
          theme_bw()
        plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        method <- self$options$method
        forecast_combined <- image1$state
        library(ggplot2)
        plot2 <- ggplot2::ggplot(forecast_combined, aes(x = ds, y = yhat, color = variable)) +
          geom_smooth(method = method) +
          labs(x = "Date", y = "Forecast", color = "Variable") +
          # ggtitle("Forecast of Multiple Variables") +
          theme_bw()
        plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .computeSIMPLE = function() {
         
        dep <- self$options$dep
        covs <- self$options$covs
        data <- self$data
        data <- jmvcore::naOmit(data)
        # Create a list to store the forecast data frames
        forecast_df_list <- list()
        
        # Loop through each variable and create a forecast data frame
        for (i in 1:length(covs)) {
          new_data <- data %>% dplyr::select(ds, all_of(covs[i])) %>% dplyr::rename(y = all_of(covs[i]))
          
          # you can use the following code to convert a column in data
          # with a date format of yy-mm to yy-mm-dd format
          # and perform a Prophet analysis on it:
          
          # # Example in R-----------------------------------
          # # Load the data from a CSV file
          # data <- read.csv("your_data_file.csv")
          #
          # # Check the format of the ds column and process the data accordingly
          # if (nchar(as.character(data$ds[1])) == 7) {
          #   # If the date is in yy-mm format
          #   data$ds <- as.Date(paste0(data$ds, "-01"), format = "%y-%m-%d")
          # } else if (nchar(as.character(data$ds[1])) == 10) {
          #   # If the date is in yy-mm-dd format
          #   data$ds <- as.Date(data$ds)
          # }
          
          # Example converting yy-mm into yy-mm-dd
          # data<- read.csv("beer.csv")
          # head(data)
          # data$Month <- as.Date(paste0(data$Month, "-01"), format = "%Y-%m-%d")
          # head(data)
          #
          
          ### yy-mm-dd-hh is no problem !!!
          
          # Fit the model
          model <- prophet::prophet(
            new_data,
            changepoint.prior.scale = 0.05,
            daily.seasonality = TRUE,
            yearly.seasonality = TRUE,
            weekly.seasonality = TRUE
          )
          
          
          # Make predictions for the next 365 days
          future <- prophet::make_future_dataframe(model,
                                                   periods = self$options$periods,
                                                   freq = self$options$unit)
          
          forecast <- predict(model, future)
          
          # Add the forecast to the list
          forecast_df_list[[i]] <- data.frame(ds = forecast$ds,
                                              yhat = forecast$yhat,
                                              variable = covs[i])
        }
        
        # Combine the forecast data frames into one
        forecast_combined <- data.table::rbindlist(forecast_df_list)
          #do.call(rbind, forecast_df_list)
        
        #self$results$text$setContent(head(forecast_combined ))
        
        return(forecast_combined)
        
      }
    )
  )
