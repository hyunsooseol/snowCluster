# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

# -------------------------------
# Lightweight Progress Bar (HTML)
# -------------------------------
progressBarH <- function(progress = 0, total = 100, message = '') {
  percentage <- round(progress / total * 100)
  width <- 400 * percentage / 100
  paste0(
    '<div style="text-align:center; padding:16px 0;">',
    '<div style="width:400px; height:20px; border:1px solid #ddd;',
    ' background:#f8f9fa; margin:0 auto; border-radius:6px;">',
    '<div style="width:', width, 'px; height:18px; background:#999;',
    ' border-radius:5px; transition:width 0.2s ease;"></div></div>',
    '<div style="margin-top:6px; font-size:12px; color:#666;">',
    message, ' (', percentage, '%)</div></div>'
  )
}

arimaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "arimaClass",
    inherit = arimaBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      # ---------------------------
      # Residual diagnostics helper
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
        
        if (isTRUE(self$options$plot))  { self$results$plot$setSize(self$options$width7,  self$options$height7) }
        if (isTRUE(self$options$box))   { self$results$box$setSize(self$options$width8,  self$options$height8) }
        if (isTRUE(self$options$plot1)) { self$results$plot1$setSize(self$options$width1, self$options$height1) }
        if (isTRUE(self$options$plot2)) { self$results$plot2$setSize(self$options$width2, self$options$height2) }
        if (isTRUE(self$options$plot3)) { self$results$plot3$setSize(self$options$width3, self$options$height3) }
        if (isTRUE(self$options$plot4)) { self$results$plot4$setSize(self$options$width4, self$options$height4) }
        if (isTRUE(self$options$plot5)) { self$results$plot5$setSize(self$options$width5, self$options$height5) }
        if (isTRUE(self$options$plot6)) { self$results$plot6$setSize(self$options$width6, self$options$height6) }
        if (isTRUE(self$options$plot9)) { self$results$plot9$setSize(self$options$width9, self$options$height9) }
        if (isTRUE(self$options$plot10)){ self$results$plot10$setSize(self$options$width10,self$options$height10) }
      },
      
      #---
      .run = function() {
        
        # -------- SIMPLE (ARIMA) --------
        if (self$options$mode == 'simple') {
          if (is.null(self$options$dep)) return()
          
          # progress bar (simple 전용)
          self$results$progressBarHTML$setVisible(TRUE)
          self$results$progressBarHTML$setContent(progressBarH(5, 100, 'Preparing data...'))
          on.exit(self$results$progressBarHTML$setVisible(FALSE), add = TRUE)
          
          if (is.null(private$.allCache)) {
            private$.allCache <- private$.computeSIMPLE()
          }
          sim <- private$.allCache
          
          # plots state
          self$results$plot$setState(sim$ddata)
          self$results$plot1$setState(sim$tsdata)
          self$results$plot2$setState(sim$mymodel$residuals)
          self$results$plot3$setState(sim$predict)
          
          # coefficients table
          if (isTRUE(self$options$coef)) {
            table <- self$results$coef
            res <- private$.fun(model = sim$mymodel, dig = 4)
            res <- t(res)
            colnames(res)[1] <- 'Coefficients'
            res <- as.data.frame(res)
            lapply(rownames(res), function(name) {
              row <- list(co = res[name, 1], se = res[name, 2])
              table$addRow(rowKey = name, values = row)
            })
          }
          
          # fit table
          if (isTRUE(self$options$fit)) {
            table <- self$results$fit
            mo <- t(data.frame(
              LL   = sim$mymodel$loglik,
              AIC  = sim$mymodel$aic,
              AICc = sim$mymodel$aicc,
              BIC  = sim$mymodel$bic
            ))
            lapply(rownames(mo), function(name) {
              table$addRow(rowKey = name, values = list(value = mo[name, 1]))
            })
            ord  <- tryCatch(forecast::arimaorder(sim$mymodel),
                             error = function(e) stats::arimaorder(sim$mymodel))
            vals <- c(p=0L,d=0L,q=0L,P=0L,D=0L,Q=0L); vals[names(ord)] <- as.integer(ord)
            self$results$fit$setNote("Model",
                                     sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
                                             vals["p"], vals["d"], vals["q"],
                                             vals["P"], vals["D"], vals["Q"],
                                             self$options$freq))
          }
          
          # prediction interval table
          if (isTRUE(self$options$point)) {
            table <- self$results$point
            pre <- as.data.frame(sim$predict)
            lapply(rownames(pre), function(name) {
              table$addRow(rowKey = name, values = list(
                po = pre[name, 1], lower = pre[name, 2], upper = pre[name, 3]
              ))
            })
          }
          
          # Residual Diagnostics (동적 행, 초기 숨김)
          if (isTRUE(self$options$resid)) {
            tbl <- self$results$resid$diagTable
            if (!is.null(tbl)) {
              tbl$setVisible(FALSE)
              try(tbl$clear(), silent = TRUE)
              try({ for (rk in c("row1","row2","row3","row4")) tbl$deleteRow(rk) }, silent = TRUE)
              
              diagdf <- private$.resid_diag_table(sim$mymodel, sim$tsdata)
              if (!is.null(diagdf) && nrow(diagdf) > 0) {
                for (i in seq_len(nrow(diagdf))) {
                  tbl$addRow(rowKey = paste0("row", i), values = list(
                    Metric    = diagdf$Metric[i],
                    Statistic = diagdf$Statistic[i],
                    df        = diagdf$df[i],
                    p_value   = diagdf$p_value[i]
                  ))
                }
                tbl$setNote("Guide", "If p < 0.05, reconsider the model; significant autocorrelation or non-normal residuals suggests mis-specification.")
                tbl$setVisible(TRUE)
              }
            }
          }
          
          # ACF/PACF states
          if (isTRUE(self$options$plot9))  self$results$plot9$setState(stats::na.omit(stats::residuals(sim$mymodel)))
          if (isTRUE(self$options$plot10)) self$results$plot10$setState(stats::na.omit(stats::residuals(sim$mymodel)))
          
          # Accuracy tables
          if (isTRUE(self$options$showAcc)) {
            acc <- tryCatch(forecast::accuracy(sim$predict, sim$tsdata),
                            error = function(e) forecast::accuracy(sim$predict))
            rn <- rownames(acc)
            useRow <- if ("Test set" %in% rn) "Test set" else if ("Training set" %in% rn) "Training set" else rn[1]
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
            e1 <- tryCatch(
              forecast::tsCV(sim$tsdata, forecastfunction = function(x, h) {
                forecast::forecast(forecast::auto.arima(x), h = h)
              }, h = 1),
              error = function(e) rep(NA_real_, length(sim$tsdata))
            )
            rmse1 <- sqrt(mean(e1^2, na.rm = TRUE)); mae1 <- mean(abs(e1), na.rm = TRUE)
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
              cv$setNote("Info", "tsCV(h=1) can be slow for long series.")
            }
          }
          
          # done
          self$results$progressBarHTML$setContent(progressBarH(100, 100, 'Done'))
        }
        
        # -------- COMPLEX (Prophet) --------
        if (self$options$mode == 'complex') {
          if (is.null(self$options$dep1) | is.null(self$options$time1)) return()
          
          # progress bar (complex 전용)
          self$results$progressBarHTML$setVisible(TRUE)
          self$results$progressBarHTML$setContent(progressBarH(5, 100, 'Preparing data for Prophet...'))
          on.exit(self$results$progressBarHTML$setVisible(FALSE), add = TRUE)
          
          if (is.null(private$.allCache)) {
            private$.allCache <- private$.computeCOM()
          }
          com <- private$.allCache
          
          state <- list(com$m, com$forecast)
          self$results$plot4$setState(state)
          self$results$plot5$setState(state)
          self$results$plot6$setState(state)
          
          # done
          self$results$progressBarHTML$setContent(progressBarH(100, 100, 'Done'))
        }
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state)) return(FALSE)
        plot(image$state); TRUE
      },
      
      .box = function(image, ggtheme, theme, ...) {
        data <- jmvcore::naOmit(self$data)
        tsdata <- stats::ts(data, frequency = self$options$freq)
        boxplot(tsdata ~ stats::cycle(tsdata)); TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state)) return(FALSE)
        tsdata <- image1$state
        plot <- tsdata %>%
          forecast::auto.arima() %>%
          forecast::forecast(h = 20) %>%
          ggplot2::autoplot()
        print(plot)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state)) return(FALSE)
        print(plot(image2$state)); TRUE
      },
      
      .plot3 = function(image3, ...) {
        if (is.null(image3$state)) return(FALSE)
        print(plot(image3$state)); TRUE
      },
      
      .plot4 = function(image4, ...) {
        if (is.null(image4$state)) return(FALSE)
        m <- image4$state[[1]]; forecast <- image4$state[[2]]
        print(plot(m, forecast)); TRUE
      },
      
      .plot5 = function(image5, ...) {
        if (is.null(image5$state)) return(FALSE)
        m <- image5$state[[1]]; forecast <- image5$state[[2]]
        prophet::prophet_plot_components(m, forecast, plot_cap = FALSE, uncertainty = TRUE)
        TRUE
      },
      
      .plot6 = function(image6, ...) {
        if (is.null(image6$state)) return(FALSE)
        m <- image6$state[[1]]; forecast <- image6$state[[2]]
        actual_df   <- data.frame(ds = m$history$ds, y = m$history$y)
        expected_df <- data.frame(ds = forecast$ds, yhat = forecast$yhat)
        plot6 <- ggplot() +
          geom_smooth(data = actual_df,   aes(x = ds, y = y,    color = "Actual"),   se = FALSE) +
          geom_smooth(data = expected_df, aes(x = ds, y = yhat, color = "Expected"), se = FALSE) +
          labs(title = "Actual vs Expected Values", x = "Date", y = "Value", color = " ") +
          theme_bw()
        print(plot6)
        TRUE
      },
      
      # ---------------------------
      # ACF / PACF renderers
      # ---------------------------
      .plot9 = function(image, ...) {
        if (is.null(image$state)) return(FALSE)
        stats::acf(image$state, main = "Residuals ACF"); TRUE
      },
      .plot10 = function(image, ...) {
        if (is.null(image$state)) return(FALSE)
        stats::pacf(image$state, main = "Residuals PACF"); TRUE
      },
      # ---------------------------
      
      .computeSIMPLE = function() {
        dep  <- self$options$dep
        freq <- self$options$freq
        pred <- self$options$pred
        
        data <- jmvcore::naOmit(self$data)
        
        # progress updates at heavy steps
        self$results$progressBarHTML$setContent(progressBarH(10, 100, 'Cleaning & structuring series...'))
        private$.checkpoint()
        
        tsdata <- stats::ts(data, frequency = freq)
        if (self$options$clean == 'TRUE')
          tsdata <- forecast::tsclean(tsdata)
        
        ddata <- stats::decompose(tsdata, "multiplicative")
        
        self$results$progressBarHTML$setContent(progressBarH(40, 100, 'Selecting ARIMA via auto.arima()...'))
        private$.checkpoint()
        mymodel <- forecast::auto.arima(tsdata, approximation = TRUE)
        
        self$results$progressBarHTML$setContent(progressBarH(75, 100, 'Forecasting...'))
        private$.checkpoint()
        predict <- forecast::forecast(mymodel, level = self$options$level, h = pred * freq)
        
        self$results$progressBarHTML$setContent(progressBarH(95, 100, 'Rendering results...'))
        private$.checkpoint()
        
        list(tsdata = tsdata, ddata = ddata, mymodel = mymodel, predict = predict)
      },
      
      .fun = function(model, dig) {
        # return matrix [coef; se]
        if (is.null(model$coef) || length(model$coef) == 0)
          return(matrix(numeric(0), nrow = 0, ncol = 0))
        
        cf <- round(model$coef, digits = dig)
        if (!is.null(model$var.coef) && NROW(model$var.coef)) {
          se <- rep(0, length(cf))
          if (!is.null(model$mask)) {
            se[model$mask] <- round(sqrt(diag(model$var.coef)), digits = dig)
          } else {
            se[] <- round(sqrt(diag(model$var.coef)), digits = dig)
          }
          out <- rbind(coef = cf, se = se)
        } else {
          out <- matrix(cf, nrow = 1, dimnames = list("coef", names(cf)))
        }
        mch <- match("intercept", colnames(out))
        if (is.null(model$xreg) && !is.na(mch)) colnames(out)[mch] <- "mean"
        out
      },
      
      .computeCOM = function() {
        data <- jmvcore::naOmit(self$data)
        
        # Prophet progress (few key steps only)
        self$results$progressBarHTML$setContent(progressBarH(10, 100, 'Cleaning & setting up Prophet...'))
        private$.checkpoint()
        
        self$results$progressBarHTML$setContent(progressBarH(40, 100, 'Fitting Prophet model...'))
        private$.checkpoint()
        m <- prophet::prophet(
          data,
          changepoint.prior.scale = 0.05,
          daily.seasonality = TRUE,
          yearly.seasonality = TRUE,
          weekly.seasonality = TRUE
        )
        
        self$results$progressBarHTML$setContent(progressBarH(75, 100, 'Forecasting with Prophet...'))
        private$.checkpoint()
        future <- prophet::make_future_dataframe(m,
                                                 periods = self$options$periods,
                                                 freq = self$options$unit)
        forecast <- predict(m, future)
        
        self$results$progressBarHTML$setContent(progressBarH(95, 100, 'Rendering Prophet results...'))
        private$.checkpoint()
        
        list(m = m, forecast = forecast)
      }
    )
  )
