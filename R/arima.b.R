
# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

arimaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "arimaClass",
    inherit = arimaBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      # ---------------------------
      # (NEW) Residual diagnostics helper
      # ---------------------------
      .resid_diag_table = function(model, y_ts) {
        res <- stats::na.omit(stats::residuals(model))
        n   <- length(res)
        f   <- stats::frequency(y_ts)
        lag <- if (self$options$lbLag > 0) self$options$lbLag else max(10, 2 * f)
        lb  <- stats::Box.test(res, lag = lag, type = "Ljung-Box")
        
        shap <- if (n <= 5000) stats::shapiro.test(res) else NULL
        
        out <- data.frame(
          Metric    = c(sprintf("Ljung-Box (lag=%d)", lag),
                        if (!is.null(shap)) "Shapiro-Wilk normality" else NULL,
                        "Residual mean", "Residual sd"),
          Statistic = c(unname(lb$statistic),
                        if (!is.null(shap)) unname(shap$statistic) else NULL,
                        mean(res), stats::sd(res)),
          df        = c(unname(lb$parameter),
                        if (!is.null(shap)) length(res) else NA,
                        NA, NA),
          p_value   = c(lb$p.value,
                        if (!is.null(shap)) shap$p.value else NA,
                        NA, NA),
          check.names = FALSE
        )
        rownames(out) <- NULL
        out
      },
      # ---------------------------
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$dep) |
            is.null(self$options$dep1)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
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
        ))
        
        # if (self$options$fit)
        #   self$results$fit$setNote("Note", "LL=Log Likelihood.")
        
        if (isTRUE(self$options$plot)) {
          width <- self$options$width7
          height <- self$options$height7
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$box)) {
          width <- self$options$width8
          height <- self$options$height8
          self$results$box$setSize(width, height)
        }
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot4)) {
          width <- self$options$width4
          height <- self$options$height4
          self$results$plot4$setSize(width, height)
        }
        if (isTRUE(self$options$plot5)) {
          width <- self$options$width5
          height <- self$options$height5
          self$results$plot5$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot6)) {
          width <- self$options$width6
          height <- self$options$height6
          self$results$plot6$setSize(width, height)
        }
        
        # NEW: sizes for ACF/PACF
        if (isTRUE(self$options$plot9)) {
          width <- self$options$width9
          height <- self$options$height9
          self$results$plot9$setSize(width, height)
        }
        if (isTRUE(self$options$plot10)) {
          width <- self$options$width10
          height <- self$options$height10
          self$results$plot10$setSize(width, height)
        }
        
      },
      
      #---
      .run = function() {
        
        dep  <- self$options$dep
        freq <- self$options$freq
        pred <- self$options$pred
        
        dep1  <- self$options$dep1
        time1 <- self$options$time1
        
        if (self$options$mode == 'simple') {
          
          if (is.null(self$options$dep)) return()
          
          if (is.null(private$.allCache)) {
            private$.allCache <- private$.computeSIMPLE()
          }
          
          sim <- private$.allCache
          
          # Decompose plot----------
          image <- self$results$plot
          image$setState(sim$ddata)
          
          # forecasts from ARIMA-------
          image1 <- self$results$plot1
          image1$setState(sim$tsdata)
          
          # residual plot----------
          res <- sim$mymodel$residuals
          image2 <- self$results$plot2
          image2$setState(res)
          
          image3 <- self$results$plot3
          image3$setState(sim$predict)
          
          # populating coef. table----------
          if (isTRUE(self$options$coef)) {
            table <- self$results$coef
            
            res <- private$.fun(model=sim$mymodel, dig=4)
            res <- t(res)
            colnames(res)[1] <- 'Coefficients'
            res <- as.data.frame(res)
            
            lapply(rownames(res), function(name) {
              row <- list(co = res[name, 1], se = res[name, 2])
              table$addRow(rowKey = name, values = row)
            })
          }
          
          # fit table------
          if (isTRUE(self$options$fit)) {
            table <- self$results$fit
            
            mo <- t(
              data.frame(
                LL = sim$mymodel$loglik,
                AIC = sim$mymodel$aic,
                AICc = sim$mymodel$aicc,
                BIC = sim$mymodel$bic
              )
            )
            
            lapply(rownames(mo), function(name) {
              row <- list(value = mo[name, 1])
              table$addRow(rowKey = name, values = row)
            })
            # # >>> NEW (1/2): 모델 식 노트 한 줄
            # ord <- forecast::arimaorder(sim$mymodel); self$results$fit$setNote("Model", sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]", ord[1], ord[2], ord[3], ord[4], ord[5], ord[6], self$options$freq))
            # >>> NEW (1/2): 모델 식 노트 (NA를 0으로 보정)
            ord  <- tryCatch(forecast::arimaorder(sim$mymodel),
                             error = function(e) stats::arimaorder(sim$mymodel))
            vals <- c(p=0L, d=0L, q=0L, P=0L, D=0L, Q=0L)   # 기본 0
            vals[names(ord)] <- as.integer(ord)             # 존재하는 항만 덮어씀
            self$results$fit$setNote(
              "Model",
              sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
                      vals["p"], vals["d"], vals["q"],
                      vals["P"], vals["D"], vals["Q"],
                      self$options$freq)
            )
        }
          
          # Prediction interval table---------
          if (isTRUE(self$options$point)) {
            table <- self$results$point
            pre <- as.data.frame(sim$predict)
            lapply(rownames(pre), function(name) {
              row <- list(po = pre[name, 1],
                          lower = pre[name, 2],
                          upper = pre[name, 3])
              table$addRow(rowKey = name, values = row)
            })
          }
          
          # -----------------------------------------
          # Residual Diagnostics Table (no plots)
          # -----------------------------------------
          if (isTRUE(self$options$resid)) {
            tbl <- self$results$resid$diagTable
            if (!is.null(tbl)) {
              diagdf <- private$.resid_diag_table(sim$mymodel, sim$tsdata)
              for (i in seq_len(4)) {
                if (i <= nrow(diagdf)) {
                  vals <- list(
                    Metric    = diagdf$Metric[i],
                    Statistic = diagdf$Statistic[i],
                    df        = diagdf$df[i],
                    p_value   = diagdf$p_value[i]
                  )
                } else {
                  vals <- list(
                    Metric    = "",
                    Statistic = NA,
                    df        = NA,
                    p_value   = NA
                  )
                }
                tbl$setRow(rowNo = i, values = vals)
              }
              tbl$setNote("Guide", "If p < 0.05, reconsider the model; significant autocorrelation or non-normal residuals suggests mis-specification.")
            }
          }
          
          # -----------------------------------------
          # NEW: ACF/PACF plot states (plot9 / plot10)
          # -----------------------------------------
          if (isTRUE(self$options$plot9)) {
            self$results$plot9$setState(stats::na.omit(stats::residuals(sim$mymodel)))
          }
          if (isTRUE(self$options$plot10)) {
            self$results$plot10$setState(stats::na.omit(stats::residuals(sim$mymodel)))
          }
          
          # -----------------------------------------
          # NEW: Accuracy tables (showAcc)  -- FIXED
          # -----------------------------------------
          if (isTRUE(self$options$showAcc)) {
            # in-sample
            acc <- tryCatch(
              forecast::accuracy(sim$predict, sim$tsdata),
              error = function(e) forecast::accuracy(sim$predict)
            )
            rn <- rownames(acc)
            useRow <- if ("Test set" %in% rn) "Test set"
            else if ("Training set" %in% rn) "Training set"
            else rn[1]
            getm <- function(m) if (m %in% colnames(acc)) as.numeric(acc[useRow, m]) else NA_real_
            rmse <- getm("RMSE"); mae <- getm("MAE"); mape <- getm("MAPE")
            
            tr <- self$results$accTrain
            if (!is.null(tr)) {
              ok <- TRUE
              tryCatch({
                tr$setRow(1, list(Metric="RMSE", Value=round(rmse,4)))
                tr$setRow(2, list(Metric="MAE",  Value=round(mae,4)))
                tr$setRow(3, list(Metric="MAPE", Value=round(mape,4)))
              }, error=function(e) ok <<- FALSE)
              if (!ok) {
                tr$addRow(rowKey="RMSE", values=list(Metric="RMSE", Value=round(rmse,4)))
                tr$addRow(rowKey="MAE",  values=list(Metric="MAE",  Value=round(mae,4)))
                tr$addRow(rowKey="MAPE", values=list(Metric="MAPE", Value=round(mape,4)))
              }
            }
            
            # tsCV (h=1)  --- return a *forecast object*, not $mean
            e1 <- tryCatch(
              forecast::tsCV(
                sim$tsdata,
                forecastfunction = function(x, h) {
                  forecast::forecast(forecast::auto.arima(x), h = h)  # no $mean here
                },
                h = 1
              ),
              error = function(e) rep(NA_real_, length(sim$tsdata))
            )
            rmse1 <- sqrt(mean(e1^2, na.rm = TRUE))
            mae1  <- mean(abs(e1), na.rm = TRUE)
            
            cv <- self$results$accCV
            if (!is.null(cv)) {
              ok2 <- TRUE
              tryCatch({
                cv$setRow(1, list(Metric="RMSE (h=1)", Value=round(rmse1,4)))
                cv$setRow(2, list(Metric="MAE (h=1)",  Value=round(mae1,4)))
              }, error=function(e) ok2 <<- FALSE)
              if (!ok2) {
                cv$addRow(rowKey="RMSE1", values=list(Metric="RMSE (h=1)", Value=round(rmse1,4)))
                cv$addRow(rowKey="MAE1",  values=list(Metric="MAE (h=1)",  Value=round(mae1,4)))
              }
              # >>> NEW (2/2): tsCV 안내 노트 한 줄
              cv$setNote("Info", "tsCV(h=1) can be slow for long series.")
            }
          }
          # -----------------------------------------
          
        }          
        
        if (self$options$mode == 'complex') {
          dep1  <- self$options$dep1
          time1 <- self$options$time1
          
          if (is.null(self$options$dep1) | is.null(self$options$time1))
            return()
          
          if (is.null(private$.allCache)) {
            private$.allCache <- private$.computeCOM()
          }
          
          com <- private$.allCache
          
          state <- list(com$m, com$forecast)
          image4 <- self$results$plot4
          image4$setState(state)
          
          image5 <- self$results$plot5
          image5$setState(state)
          
          image6 <- self$results$plot6
          image6$setState(state)
        }
        
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        ddata <- image$state
        
        plot <- plot(ddata)
        print(plot)
        TRUE
      },
      
      .box = function(image, ggtheme, theme, ...) {
        dep  <- self$options$dep
        freq <- self$options$freq
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        tsdata <- stats::ts(data, frequency = freq)
        plot <- boxplot(tsdata ~ stats::cycle(tsdata))
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        tsdata <- image1$state
        
        plot <- tsdata %>%
          forecast::auto.arima() %>%
          forecast::forecast(h = 20) %>%
          ggplot2::autoplot()
        print(plot)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        res <- image2$state
        plot <- plot(res)
        print(plot)
        TRUE
      },
      
      .plot3 = function(image3, ...) {
        if (is.null(image3$state))
          return(FALSE)
        predict <- image3$state
        plot <- plot(predict)
        print(plot)
        TRUE
      },
      
      .plot4 = function(image4, ...) {
        if (is.null(image4$state))
          return(FALSE)
        m <- image4$state[[1]]
        forecast <- image4$state[[2]]
        plot4 <- plot(m, forecast)
        print(plot4)
        TRUE
      },
      
      .plot5 = function(image5, ...) {
        if (is.null(image5$state))
          return(FALSE)
        m <- image5$state[[1]]
        forecast <- image5$state[[2]]
        plot5 <- prophet::prophet_plot_components(m, forecast, plot_cap = FALSE, uncertainty = TRUE)
        TRUE
      },
      
      .plot6 = function(image6, ...) {
        if (is.null(image6$state))
          return(FALSE)
        m <- image6$state[[1]]
        forecast <- image6$state[[2]]
        
        actual_df <- data.frame(ds = m$history$ds, y = m$history$y)
        expected_df <- data.frame(ds = forecast$ds, yhat = forecast$yhat)
        
        plot6 <- ggplot() +
          geom_smooth(data = actual_df,
                      aes(x = ds, y = y, color = "Actual"),
                      se = FALSE) +
          geom_smooth(data = expected_df,
                      aes(x = ds, y = yhat, color = "Expected"),
                      se = FALSE) +
          labs(
            title = "Actual vs Expected Values",
            x = "Date",
            y = "Value",
            color = " "
          ) +
          theme_bw()
        
        print(plot6)
        TRUE
      },
      
      # ---------------------------
      # NEW: ACF / PACF renderers
      # ---------------------------
      .plot9 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        res <- image$state
        stats::acf(res, main = "Residuals ACF")
        TRUE
      },
      
      .plot10 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        res <- image$state
        stats::pacf(res, main = "Residuals PACF")
        TRUE
      },
      # ---------------------------
      
      .computeSIMPLE = function() {
        
        dep  <- self$options$dep
        freq <- self$options$freq
        pred <- self$options$pred
        dep1  <- self$options$dep1
        time1 <- self$options$time1
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        #------------------
        tsdata <- stats::ts(data, frequency = freq)
        if (self$options$clean == 'TRUE') {
          tsdata <- forecast::tsclean(tsdata)
        }
        
        ddata <- stats::decompose(tsdata, "multiplicative")
        
        mymodel <- forecast::auto.arima(tsdata,
                                        approximation = TRUE)
        
        # Forecast the Values for the Next 10 Years--------
        predict <- forecast::forecast(mymodel,
                                      level = self$options$level,
                                      h = pred * freq)
        
        sim <- list(tsdata = tsdata,
                    ddata = ddata,
                    mymodel = mymodel,
                    predict = predict)
        return(sim)
        
      },
      
      .fun = function(model, dig) {
        if (length(model$coef) > 0) {
          cat("\nCoefficients:\n")
          coef <- round(model$coef, digits = dig)
          
          if (NROW(model$var.coef)) {
            se <- rep.int(0, length(coef))
            se[model$mask] <- round(sqrt(diag(model$var.coef)), digits = dig)
            coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
            coef <- rbind(coef, se = se)
          }
          mch <- match("intercept", colnames(coef))
          if (is.null(model$xreg) & !is.na(mch)) {
            colnames(coef)[mch] <- "mean"
          }
          print.default(coef, print.gap = 2)
        }
      },
      
      .computeCOM = function() {
        data <- self$data
        data <- jmvcore::naOmit(data)
        # Prophet Analysis -----------
        m <- prophet::prophet(
          data,
          changepoint.prior.scale = 0.05,
          daily.seasonality = TRUE,
          yearly.seasonality = TRUE,
          weekly.seasonality = TRUE
        )
        # Basic predictions ------------------------------------
        future <- prophet::make_future_dataframe(m,
                                                 periods = self$options$periods,
                                                 freq = self$options$unit)
        #############   A L E R T  ##############
        forecast <- predict(m, future)
        #########################################
        
        com <- list(m = m,
                    forecast = forecast)
        return(com)
      }
    )
  )
