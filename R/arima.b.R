
# This file is a generated template, your changes will not be overwritten

#' @importFrom stats decompose
#' @importFrom stats ts
#' @importFrom stats cycle
#' @import forecast 
#' @import ggplot2
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom forecast auto.arima
#' @importFrom forecast forecast
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
            
            <p><b>ARIMA Model to univariate time series</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. The analysis options are classified by two factors.
            <p><b>Frequency</b>= the number of observations per unit of time. <b>Predict</b>= Number of periods for forecasting. 
            <p>2.The results were implemented with <b>auto.arima() and forecast() function</b> in R.</p>
            <p>3. The example of time series forecasting is described in the <a href='https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r' target = '_blank'>page.</a></p>
            <p>4.Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
        
        
                .run = function() {

            
            if (!is.null(self$options$dep)) {
            
            dep  <- self$options$dep
            time <- self$options$time
            covs <- self$options$covs
            freq <- self$options$freq
            pred <- self$options$pred
            
            # get the data
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
            tsdata <- stats::ts(data, frequency = freq) 
            
            ddata <- stats::decompose(tsdata, "multiplicative")
            
            # Decompose plot----------
            
            image <- self$results$plot
            image$setState(ddata)
            
            # forcasts from ARIMA-------
            
            image1 <- self$results$plot1
            image1$setState(tsdata)
            
            # residual plot----------
            
            mymodel <- forecast::auto.arima(tsdata)
            
            res <- mymodel$residuals
            
            image2 <- self$results$plot2
            image2$setState(res)
            
            
            # Forecast the Values for the Next 10 Years--------
            
            predict <- forecast::forecast(mymodel, level=c(95), h=pred*freq)
            
            
            image3 <- self$results$plot3
            image3$setState(predict)
            
            
            # Prediction interval table---------
            
            table <- self$results$point
            
           pre <- forecast::forecast(mymodel)
           pre<- as.data.frame(pre)
            
            names<- dimnames(pre)[[1]]
            
            for (name in names) {
                
                row <- list()
                
                row[["po"]]   <-  pre[name, 1]
                row[["lower"]] <-  pre[name, 4]
                row[["upper"]] <-  pre[name, 5]
                
                
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
                    
                    ## Se non ci sono variabili di regressione
                    ## sostituire intercetta con media
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
            
            
            },
            
            .plot = function(image,...) {
                
                if (is.null(self$options$dep))
                    return()
                
                
                ddata <- image$state
                
                plot <- plot(ddata)
                
               
                print(plot)
                TRUE
            },
        
        .box = function(image,ggtheme, theme,...) {

            if (is.null(self$options$dep))
                return()


            dep  <- self$options$dep
            time <- self$options$time
            covs <- self$options$covs
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
            
            if (is.null(self$options$dep))
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
            
            if (is.null(self$options$dep))
                return()
            
            
           res <- image2$state
            
            plot <- plot(res)
            
            
            print(plot)
            TRUE
        },
        
        .plot3 = function(image3,...) {
            
            if (is.null(self$options$dep))
                return()
            
            
            predict <- image3$state
            
            plot <- plot(predict)
            
            
            print(plot)
            TRUE
        }
        
        
        
     )
    )

