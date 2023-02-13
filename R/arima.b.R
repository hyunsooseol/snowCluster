
# This file is a generated template, your changes will not be overwritten

#' @importFrom stats decompose
#' @importFrom stats ts
#' @importFrom stats cycle
#' @import forecast 
#' @import ggplot2
#' @import jmvcore
#' @import prophet 
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom forecast auto.arima
#' @importFrom forecast forecast
#' @importFrom prophet prophet
#' @importFrom prophet make_future_dataframe
#' @importFrom lubridate force_tz
#' @export



arimaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "arimaClass",
    inherit = arimaBase,
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
            <p>1. Do NOT move the variables into <b>Time box.</b> Otherwise, the error messages will be appeared. 
            <p>2. ARIMA options are classified by two factors; <b>Frequency</b>= the number of observations per unit of time. <b>Prediction</b>= number of periods for forecasting.</p>
            <p>3. The results of ARIMA were implemented with <b>auto.arima() and forecast() function</b> in R.</p>
            <p>4. The rationale of <b>forecast</b> R package is described in the <a href='https://cran.r-project.org/web/packages/forecast/vignettes/JSS2008.pdf' target = '_blank'>documentation.</a></p>
            <p>5. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
            if(self$options$fit)
                self$results$fit$setNote(
                    "Note",
                    "LL=Log Likelihood."
                )
            
        },
        
       ################################################################## 
                .run = function() {

            
            if(is.null(self$options$dep))
              return()
            
            dep  <- self$options$dep
            time <- self$options$time
            freq <- self$options$freq
            pred <- self$options$pred
            
            if(is.null(self$options$time)){
            
            
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

            # forcasts from ARIMA-------

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
            
            } else{
           
              # get the data
              
              data <- self$data
              data <- jmvcore::naOmit(data)
              
              
              # Prophet Analysis -----------
              res <- prophet::prophet(data)
              # Basic predictions ------------------------------------
              future <- prophet::make_future_dataframe(res, 
                                                       periods = 365)
              #############   A L E R T  ##############
              # function returns error in Windows 10-11
              # No errors in Ubuntu 22.04.1
              fore <- predict(res, future)
              #########################################
              self$results$text$setContent(fore)
              
              
              state <- list(res, fore)
              image4 <- self$results$plot4
              image4$setState(state)
              
          
           # prophet analysis example in R----------
           
           # library(prophet)
           # df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv')
           # m <- prophet(df)
           # future <- make_future_dataframe(m, periods = 365)
           # forecast <- predict(m, future)
           # plot(m, forecast)
           # prophet_plot_components(m, forecast)
           #------------------------------------------------ 
           
          
           # time <- self$options$time
           # 
           # data[[time]] <- as.POSIXct(data[[time]], tz = "UTC")
           # 
           # res <- prophet::prophet(data)
           # 
           # # Basic predictions---------
           # 
           # future <- prophet::make_future_dataframe(res, periods = 365)
           # forecast <- predict(res, future)
           # 
           
           # time <- self$options$time
           # 
           # # Convert the time column to a character type
           # data[[time]] <- as.character(data[[time]])
           # 
           # # # Convert the time column to a POSIXct type with the UTC timezone and the specified date format
           # # data[[time]] <- as.POSIXct(data[[time]], format = "%Y-%m-%d", tz = "UTC")
           # # 
           # # # Change the timezone of the time column to UTC
           # # data[[time]] <- lubridate::force_tz(data[[time]], "UTC")
           # # 
           # 
           # data[[time]] <- as.POSIXct(data[[time]], tz = "UTC")
           # data[[time]] <- lubridate::force_tz(data[[time]], "UTC")
           # 
           # 
         
              
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
          
          if(!is.null(self$options$time))
          return()
            
            tsdata <- image1$state
            
             plot<- tsdata %>%
                auto.arima() %>%
                forecast(h=20) %>%
                autoplot() 
             
             
             print(plot)
             TRUE
        },
        
        .plot2 = function(image2,...) {
            
          if(!is.null(self$options$time))
            return()
            
           res <- image2$state
            
            plot <- plot(res)
            
            
            print(plot)
            TRUE
        },
        
        .plot3 = function(image3,...) {
            
          if(!is.null(self$options$time))
            return()
            
            
            predict <- image3$state
            
            plot <- plot(predict)
            
            
            print(plot)
            TRUE
        },
        
        .plot4 = function(image4,...) {
          
          if(is.null(self$options$time))
            return()
          
          res<- image4$state[[1]]
          fore <- image4$state[[2]]
          plot4<- plot(res, fore)
          print(plot4)
          TRUE
        }
        
        
     )
    )

