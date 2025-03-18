
# This file is a generated template, your changes will not be overwritten

#' @importFrom magrittr %>%
#' @export



arimaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "arimaClass",
    inherit = arimaBase,
    private = list(
      .htmlwidget = NULL,
      
        .init = function() {
      
          private$.htmlwidget <- HTMLWidget$new()
                
         if (is.null(self$options$dep) | is.null(self$options$dep1)){
                
                    
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # 
            # <p>_____________________________________________________________________________________________</p>
            # <p>1. <b>To run ARIMA,</b> remove the variables from the prophet analysis box.
            # <p>2. In order to perform a prophet analysis, the variables must be named <b>'ds' and 'y'</b> respectively.</p>
            # <p>3. Prophet analysis requires the date column to be in a specific format (%Y-%m-%d). Otherwise, an error occurs</p>
            # <p>4. ARIMA options are classified by two factors; <b>Frequency</b>= the number of observations per unit of time. <b>Prediction</b>= number of periods for forecasting.</p>
            # <p>5. The results of ARIMA were implemented with <b>auto.arima() and forecast() function</b> in R.</p>
            # <p>6. The rationale of <b>forecast</b> R package is described in the <a href='https://cran.r-project.org/web/packages/forecast/vignettes/JSS2008.pdf' target = '_blank'>documentation.</a></p>
            # <p>7. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            # <p>_____________________________________________________________________________________________</p>
            # 
            # </div>
            # </body>
            # </html>"
            # )
            
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li><b>To run ARIMA,</b> remove the variables from the prophet analysis box.</li>',
                '<li>In order to perform a prophet analysis, the variables must be named <b>ds and y</b> respectively.</li>',
                '<li>Prophet analysis requires the date column to be in a specific format (%Y-%m-%d). Otherwise, an error occurs</li>',
                '<li>ARIMA options are classified by two factors; <b>Frequency</b>= the number of observations per unit of time. <b>Prediction</b>= number of periods for forecasting.</li>',
                '<li>The results of ARIMA were implemented with <b>auto.arima() and forecast() function</b> in R.</li>',
                '<li>The rationale of <b>forecast</b> R package is described in the <a href="https://cran.r-project.org/web/packages/forecast/vignettes/JSS2008.pdf" target = "_blank">documentation</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )          
          
            if(self$options$fit)
                self$results$fit$setNote(
                    "Note",
                    "LL=Log Likelihood."
                )
           
            if(isTRUE(self$options$plot)){
              width <- self$options$width7
              height <- self$options$height7
              self$results$plot$setSize(width, height)
            }  
            
            if(isTRUE(self$options$box)){
              width <- self$options$width8
              height <- self$options$height8
              self$results$box$setSize(width, height)
            }  
            
            
            if(isTRUE(self$options$plot1)){
              width <- self$options$width1
              height <- self$options$height1
              self$results$plot1$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot2)){
              width <- self$options$width2
              height <- self$options$height2
              self$results$plot2$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot3)){
              width <- self$options$width3
              height <- self$options$height3
              self$results$plot3$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot4)){
              width <- self$options$width4
              height <- self$options$height4
              self$results$plot4$setSize(width, height)
            }  
            if(isTRUE(self$options$plot5)){
              width <- self$options$width5
              height <- self$options$height5
              self$results$plot5$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot6)){
              width <- self$options$width6
              height <- self$options$height6
              self$results$plot6$setSize(width, height)
            }  
            
         
        },
        
       ################################################################## 
                .run = function() {

         
            dep  <- self$options$dep
            freq <- self$options$freq
            pred <- self$options$pred
          
            dep1  <- self$options$dep1
            time1 <- self$options$time1
          
            # xCol <- jmvcore::toNumeric(self$data[[dep]])
            # yCol <- jmvcore::toNumeric(self$data[[time]])
            # data <- data.frame(x=xCol, y=yCol)
            # data <- jmvcore::naOmit(data)
            # 
            
           
            if(self$options$mode=='simple'){
            
            
               if (is.null(self$options$dep)) 
                 return()      
            
                
            # get the data
            data <- self$data
            data <- jmvcore::naOmit(data)

            #------------------

            tsdata <- stats::ts(data, frequency = freq)
            ddata <- stats::decompose(tsdata, "multiplicative")

            
            #################################################
            
            mymodel <- forecast::auto.arima(tsdata)
            
            #############################################
            
            
            # Decompose plot----------

            image <- self$results$plot
            image$setState(ddata)

            # forecasts from ARIMA-------

            image1 <- self$results$plot1
            image1$setState(tsdata)

            # residual plot----------
            
            res <- mymodel$residuals
            
            image2 <- self$results$plot2
            image2$setState(res)
            
            
            #############################################################
            # Forecast the Values for the Next 10 Years--------
            
            predict <- forecast::forecast(mymodel, level=c(95), h=pred*freq)
            
            ###########################################################
            
            image3 <- self$results$plot3
            image3$setState(predict)
            
            
            # Prediction interval table---------
            
            table <- self$results$point
            
            #  pre <- forecast::forecast(mymodel)
            
            pre<- as.data.frame(predict)
            
            names<- dimnames(pre)[[1]]
            
            for (name in names) {
              
              row <- list()
              
              row[["po"]]   <-  pre[name, 1]
              row[["lower"]] <-  pre[name, 2]
              row[["upper"]] <-  pre[name, 3]
              
              
              table$addRow(rowKey=name, values=row)
              
            }
            
            
            # ARIMA coefficients Table-------
            
            fun <- function(model, dig){
              
              if (length(model$coef) > 0) {
                cat("\nCoefficients:\n")
                coef <- round(model$coef, digits=dig)
                
                if (NROW(model$var.coef)) {
                  se <- rep.int(0, length(coef))
                  se[model$mask] <- round(sqrt(diag(model$var.coef)), digits=dig)
                  coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
                  coef <- rbind(coef, se=se)
                }
                
                
                mch <- match("intercept", colnames(coef))
                if (is.null(model$xreg) & !is.na(mch)) {
                  colnames(coef)[mch] <- "mean"
                }
                print.default(coef, print.gap = 2)
              }
            }
            
            res<- fun(mymodel,4)
            res <- t(res)
            colnames(res)[1]<-'Coefficients'
            res <- as.data.frame(res)
            
            # populating coef. table----------
            
            table <- self$results$coef
            
            names <- dimnames(res)[[1]]
            
            
            for (name in names) {
              
              row <- list()
              
              row[["co"]] <- res[name, 1]
              row[["se"]] <- res[name, 2]
              
              table$addRow(rowKey=name, values=row)
              
            }
            
            # fit table------
            
            log<- mymodel$loglik
            aic<- mymodel$aic
            aicc<- mymodel$aicc
            bic<- mymodel$bic
            
            
            mo<- data.frame(LL=mymodel$loglik,AIC=mymodel$aic,
                            AICc=mymodel$aicc,BIC=mymodel$bic)
            
            mo<- t(mo)
            names<- dimnames(mo)[[1]]
            
            # populating fit table------
            
            table <- self$results$fit
            
            for (name in names) {
              
              row <- list()
              
              row[['value']] <- mo[name,1]
              
              table$addRow(rowKey=name, values=row)
              
            }
            
            } 
            
####################################################################              
            
             if(self$options$mode=='complex'){
              
              dep1  <- self$options$dep1
              time1 <- self$options$time1
              
               if (is.null(self$options$dep1) | is.null(self$options$time1)) 
                 return()     
              
              # prophet analysis example in R----------
              
              # library(prophet)
              # df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv')
              # m <- prophet(df)
              # future <- make_future_dataframe(m, periods = 365)
              # forecast <- predict(m, future)
              # plot(m, forecast)
              # prophet_plot_components(m, forecast)
              #------------------------------------------------ 
              
              
              # get the data
              data <- self$data
              data <- jmvcore::naOmit(data)

              
              # Prophet Analysis -----------
              m <- prophet::prophet(data,changepoint.prior.scale = 0.05,
                                    daily.seasonality=TRUE,
                                    yearly.seasonality = TRUE, 
                                    weekly.seasonality = TRUE)
    
              
              # Basic predictions ------------------------------------
              future <- prophet::make_future_dataframe(m, 
                                                       periods = self$options$periods,
                                                       freq = self$options$unit)
                                                       
              
              #############   A L E R T  ##############
              forecast <- predict(m, future)
              #########################################
              # self$results$text$setContent(forecast)
              
              
              state <- list(m, forecast)
              
              image4 <- self$results$plot4
              image4$setState(state)
              
              # components plot-----------
              
              image5 <- self$results$plot5
              image5$setState(state)
              
              # components plot-----------
              
              image6 <- self$results$plot6
              image6$setState(state)
             
            } 
         
           
            
            
            },
            
            .plot = function(image,...) {
                
              if (is.null(image$state))
                return(FALSE)
              
                ddata <- image$state
                
                plot <- plot(ddata)
                
               
                print(plot)
                TRUE
            },
        
        .box = function(image,ggtheme, theme,...) {

          # if (is.null(image$state))
          #   return(FALSE)
          
            dep  <- self$options$dep
            freq<- self$options$freq
            
            # get the data
            
            data <- self$data
            data <- jmvcore::naOmit(data)
            
            
            tsdata <- stats::ts(data, frequency = freq) 
            
           plot<- boxplot(tsdata~stats::cycle(tsdata))
            
            
           
            print(plot)
            TRUE
        },
        
        .plot1 = function(image1,...) {
          
          if (is.null(image1$state))
            return(FALSE)
          
            
            tsdata <- image1$state
            
             plot<- tsdata %>%
                auto.arima() %>%
                forecast(h=20) %>%
                autoplot() 
             
             
             print(plot)
             TRUE
        },
        
        .plot2 = function(image2,...) {
            
          if (is.null(image2$state))
            return(FALSE)
          
            
           res <- image2$state
            
            plot <- plot(res)
            
            
            print(plot)
            TRUE
        },
        
        .plot3 = function(image3,...) {
            
          if (is.null(image3$state))
            return(FALSE)
          
            
            
            predict <- image3$state
            
            plot <- plot(predict)
            
            
            print(plot)
            TRUE
        },
        
        .plot4 = function(image4,...) {
          
          if (is.null(image4$state))
            return(FALSE)
          
          
          m<- image4$state[[1]]
          forecast <- image4$state[[2]]
          plot4<- plot(m, forecast)
          print(plot4)
          TRUE
        },
        
       .plot5 = function(image5, ...) {
         
         if (is.null(image5$state))
           return(FALSE)
         
         
         m <- image5$state[[1]]
         forecast <- image5$state[[2]]
         
         plot5 <- prophet::prophet_plot_components(m, forecast, 
                                                   plot_cap = FALSE, 
                                                   uncertainty = TRUE)
         
        # print(plot5) # Otherwise, Only first plot is appeared.
         TRUE
       },
       
       # .plot6 = function(image6, ...) {
       #   if(is.null(self$options$time))
       #     return()
       #   
       #   m <- image6$state[[1]]
       #   forecast <- image6$state[[2]]
       #   
       #   actual_df <- data.frame(ds = m$history$ds, y = m$history$y)
       #   expected_df <- data.frame(ds = forecast$ds, y = forecast$yhat)
       #   
       #   plot6 <- ggplot() +
       #     geom_line(data = actual_df, aes(x = ds, y = y, color = "Actual")) +
       #     geom_line(data = expected_df, aes(x = ds, y = y, color = "Expected")) +
       #     labs(title = "Actual vs Expected Values", x = "Date", y = "Value", color = " ") +
       #     theme_bw()
       #   
       #   print(plot6)
       #   TRUE
       # }
       
       # smooth line------------------------
       
       .plot6 = function(image6, ...) {
        
         if (is.null(image6$state))
           return(FALSE)
         
         
         m <- image6$state[[1]]
         forecast <- image6$state[[2]]
         
         actual_df <- data.frame(ds = m$history$ds, y = m$history$y)
         expected_df <- data.frame(ds = forecast$ds, yhat = forecast$yhat)
         
         plot6 <- ggplot() +
           geom_smooth(data = actual_df, aes(x = ds, y = y, color = "Actual"), se = FALSE) +
           geom_smooth(data = expected_df, aes(x = ds, y = yhat, color = "Expected"), se = FALSE) +
           labs(title = "Actual vs Expected Values", x = "Date", y = "Value", color = " ") +
           theme_bw()
         
         print(plot6)
         TRUE
       }
       
       
       
       
     )
    )

