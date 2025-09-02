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
        
        if (isTRUE(self$options$plotAcc)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plotAcc$setSize(width, height)
        }
        
      },
      
      #############################################################
      .run = function() {
        if (is.null(self$options$dep) || length(self$options$covs) == 0)
          return()
        
        # 캐시 또는 계산
        private$.allCache <- private$.computeSIMPLE()
        res <- private$.allCache
        
        # 플롯 상태 전달
        self$results$plot1$setState(res$forecast)
        self$results$plot2$setState(res$forecast)
        
        # === Accuracy 테이블 채우기 (변수별 + 평균행 두 개) ===
        tbl <- self$results$accuracy
        acc_df <- res$accuracy
        
        if (!is.null(acc_df) && nrow(acc_df) > 0) {
          for (i in seq_len(nrow(acc_df))) {
            rk <- paste0("row:", make.names(acc_df$variable[i]))
            tbl$addRow(
              rowKey = rk,
              values = list(
                variable = acc_df$variable[i],
                MAE      = acc_df$MAE[i],
                RMSE     = acc_df$RMSE[i],
                MAPE     = acc_df$MAPE[i]
              )
            )
          }
        }
        
        # --- (선택) Seasonality 요약 노트 ---
        .getOpt <- function(name, default) {
          tryCatch({
            v <- self$options[[name]]
            if (is.null(v)) default else v
          }, error = function(e) default)
        }
        seas_txt <- switch(.getOpt("seasonality", "none"),
                           "weekly" = "Weekly",
                           "yearly" = "Yearly",
                           "weekly_yearly" = "Weekly + Yearly",
                           "None")
        self$results$accuracy$setNote("seasonality", paste("Seasonality:", seas_txt))
        
        # ★ 정확도 비교 플롯(MAPE barplot)용 상태 전달
        self$results$plotAcc$setState(acc_df)
      },
      
      #############################################################
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        df <- image$state
        if (nrow(df) == 0) return(FALSE)
        
        library(ggplot2)
        
        p <- ggplot(df, aes(x = ds, y = yhat, color = variable, fill = variable)) +
          geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.15, colour = NA) +
          geom_line(size = 0.7) +
          labs(x = "Date", y = "Forecast", color = "Variable", fill = "Variable") +
          theme_bw()
        
        p + ggtheme
        print(p)
        TRUE
      },
      
      #############################################################
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        df <- image$state
        if (nrow(df) == 0) return(FALSE)
        
        method <- self$options$method
        library(ggplot2)
        
        p <- ggplot(df, aes(x = ds, y = yhat, color = variable, fill = variable)) +
          geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.12, colour = NA) +
          geom_smooth(method = method, se = FALSE, size = 0.9) +
          labs(x = "Date", y = "Forecast", color = "Variable", fill = "Variable") +
          theme_bw()
        
        p + ggtheme
        print(p)
        TRUE
      },
      
      #############################################################
      .plotAcc = function(image, ggtheme, theme, ...) {
        acc <- image$state
        if (is.null(acc) || nrow(acc) == 0)
          return(FALSE)
        
        # (no reg) / (+reg) 태그 분리 및 MAPE만 사용
        acc$base  <- sub(" \\(.*\\)$", "", acc$variable)
        acc$model <- ifelse(grepl("\\(\\+reg\\)", acc$variable), "+reg",
                            ifelse(grepl("\\(no reg\\)", acc$variable), "no reg", "other"))
        
        plot_df <- acc[, c("base","model","MAPE")]
        plot_df <- plot_df[plot_df$model %in% c("no reg","+reg"), , drop = FALSE]
        if (nrow(plot_df) == 0) return(FALSE)
        
        # Average는 항상 마지막에 보이도록 정렬
        is_avg <- grepl("^Average", plot_df$base)
        plot_df$base <- factor(plot_df$base,
                               levels = c(sort(unique(plot_df$base[!is_avg])),
                                          sort(unique(plot_df$base[is_avg]))))
        
        library(ggplot2)
        p <- ggplot(plot_df, aes(x = base, y = MAPE, fill = model)) +
          geom_col(position = position_dodge(width = 0.6), width = 0.55) +
          labs(x = NULL, y = "MAPE (%)", fill = "Model") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
        
        p + ggtheme
        p <- p + coord_flip()
        print(p)
        TRUE
      },
      
      #############################################################
      .computeSIMPLE = function() {
        
        # ---- 안전 접근 유틸 (옵션 미정의 시 기본값) ----
        .getOpt <- function(name, default) {
          tryCatch({
            v <- self$options[[name]]
            if (is.null(v)) default else v
          }, error = function(e) default)
        }
        
        covs        <- .getOpt("covs", character(0))
        unit        <- .getOpt("unit", "day")
        periods     <- .getOpt("periods", 365)
        
        growth      <- .getOpt("growth", "linear")
        cp_scale    <- .getOpt("cp_scale", 0.05)
        n_chgpts    <- .getOpt("n_chgpts", 25)
        cp_range    <- .getOpt("cp_range", 0.8)
        
        # --- Seasonality: 단일 콤보박스 매핑 ---
        seas <- .getOpt("seasonality", "none")
        wk <- FALSE; yr <- FALSE
        if (identical(seas, "weekly"))         wk <- TRUE
        if (identical(seas, "yearly"))         yr <- TRUE
        if (identical(seas, "weekly_yearly")) { wk <- TRUE; yr <- TRUE }
        
        interval_w  <- .getOpt("interval_width", 0.80)
        mcmc_samps  <- .getOpt("mcmc_samples", 0)
        
        regs            <- .getOpt("regressors", character(0))
        reg_ps          <- .getOpt("reg_prior_scale", 10)
        reg_future_fill <- .getOpt("reg_future_fill", "last")
        
        data <- jmvcore::naOmit(self$data)
        
        # ds 타입을 unit에 맞춰 강제
        if (unit == "hour") {
          if (!inherits(data$ds, "POSIXct"))
            suppressWarnings({ data$ds <- as.POSIXct(data$ds) })
        } else {
          if (!inherits(data$ds, "Date"))
            suppressWarnings({ data$ds <- as.Date(data$ds) })
        }
        
        if (length(covs) == 0)
          return(list(forecast = data.frame(), accuracy = data.frame()))
        
        forecast_df_list <- list()  # 플롯용(최종 모델만)
        acc_list <- list()          # 표용(두 모델 모두)
        
        # 공통 러너: use_regs=FALSE/TRUE 로 한 번씩 실행
        run_one <- function(varname, use_regs) {
          regs_now <- if (isTRUE(use_regs)) intersect(names(data), regs) else character(0)
          
          cols_keep <- c("ds", varname, regs_now)
          new_data <- data[, cols_keep, drop = FALSE]
          names(new_data)[names(new_data) == varname] <- "y"
          new_data$y <- suppressWarnings(as.numeric(new_data$y))
          if (length(regs_now) > 0) {
            for (r in regs_now) new_data[[r]] <- suppressWarnings(as.numeric(new_data[[r]]))
          }
          
          m <- prophet::prophet(
            growth                  = growth,
            changepoint.prior.scale = cp_scale,
            n.changepoints          = n_chgpts,
            changepoint.range       = cp_range,
            yearly.seasonality      = yr,       # <- seasonality 매핑
            weekly.seasonality      = wk,       # <- seasonality 매핑
            daily.seasonality       = FALSE,    # 단순화를 위해 항상 끔
            seasonality.mode        = "additive",
            interval.width          = interval_w,
            mcmc.samples            = mcmc_samps
          )
          
          if (length(regs_now) > 0) {
            for (r in regs_now) {
              m <- prophet::add_regressor(m, r, prior.scale = reg_ps, standardize = 'auto')
            }
          }
          
          if (identical(growth, "logistic")) {
            cap_val   <- .getOpt("cap",   1.1 * max(new_data$y, na.rm = TRUE))
            floor_val <- .getOpt("floor", 0)
            new_data$cap   <- cap_val
            new_data$floor <- floor_val
          }
          
          m <- prophet::fit.prophet(m, new_data)
          
          future <- prophet::make_future_dataframe(
            m, periods = periods, freq = unit, include_history = TRUE
          )
          
          if (identical(growth, "logistic")) {
            future$cap   <- new_data$cap[1]
            future$floor <- new_data$floor[1]
          }
          
          # 회귀자 미래값 채우기
          if (length(regs_now) > 0) {
            for (r in regs_now) {
              if (!(r %in% names(future))) future[[r]] <- NA_real_
              idx_hist <- match(new_data$ds, future$ds, nomatch = 0)
              future[idx_hist, r] <- new_data[[r]]
              idx_future <- which(is.na(future[[r]]))
              if (length(idx_future) > 0) {
                fill_val <- if (identical(reg_future_fill, "zero")) 0 else {
                  last <- utils::tail(new_data[[r]][!is.na(new_data[[r]])], 1)
                  if (length(last) == 0 || is.na(last)) 0 else last
                }
                future[idx_future, r] <- fill_val
              }
            }
          }
          
          forecast <- predict(m, future)
          
          # 훈련구간 성능
          hist_pred <- forecast[, c("ds","yhat")]
          if (!inherits(new_data$ds, class(hist_pred$ds))) {
            if (inherits(hist_pred$ds, "Date")) new_data$ds <- as.Date(new_data$ds) else new_data$ds <- as.POSIXct(new_data$ds)
          }
          merged <- dplyr::left_join(new_data[, c("ds","y"), drop = FALSE], hist_pred, by = "ds")
          comp   <- merged[stats::complete.cases(merged$y, merged$yhat), , drop = FALSE]
          
          if (nrow(comp) == 0) {
            mae <- rmse <- mape <- NA_real_
          } else {
            err  <- comp$y - comp$yhat
            mae  <- mean(abs(err), na.rm = TRUE)
            rmse <- sqrt(mean(err^2, na.rm = TRUE))
            denom <- ifelse(comp$y == 0, NA, comp$y)
            mape <- mean(abs(err / denom), na.rm = TRUE) * 100
          }
          
          list(
            forecast = data.frame(
              ds         = forecast$ds,
              yhat       = forecast$yhat,
              yhat_lower = forecast$yhat_lower,
              yhat_upper = forecast$yhat_upper,
              variable   = varname
            ),
            acc = data.frame(
              variable = paste0(varname, if (isTRUE(use_regs) && length(regs) > 0) " (+reg)" else " (no reg)"),
              MAE = mae, RMSE = rmse, MAPE = mape,
              stringsAsFactors = FALSE
            )
          )
        }
        
        # 변수별로 두 모델 수행
        for (v in covs) {
          # 1) 베이스라인
          res0 <- run_one(v, use_regs = FALSE)
          acc_list[[length(acc_list)+1]] <- res0$acc
          
          # 2) 회귀자 포함 (회귀자가 실제로 선택되어 있을 때만)
          if (length(regs) > 0) {
            res1 <- run_one(v, use_regs = TRUE)
            acc_list[[length(acc_list)+1]] <- res1$acc
            
            # 플롯은 "회귀자 포함" 모델 기준
            forecast_df_list[[length(forecast_df_list)+1]] <- res1$forecast
          } else {
            # 회귀자가 없으면 베이스라인 예측으로 그림
            forecast_df_list[[length(forecast_df_list)+1]] <- res0$forecast
          }
        }
        
        forecast_combined <- data.table::rbindlist(forecast_df_list)
        acc_df <- data.table::rbindlist(acc_list)
        
        # --- 평균 행 추가: Average (no reg), Average (+reg) ---
        if (nrow(acc_df) > 0) {
          acc_no  <- acc_df[grepl("\\(no reg\\)", acc_df$variable), c("MAE","RMSE","MAPE"), drop = FALSE]
          if (nrow(acc_no) > 0) {
            avg_no <- data.frame(
              variable = "Average (no reg)",
              MAE  = mean(acc_no$MAE,  na.rm = TRUE),
              RMSE = mean(acc_no$RMSE, na.rm = TRUE),
              MAPE = mean(acc_no$MAPE, na.rm = TRUE),
              stringsAsFactors = FALSE
            )
            acc_df <- rbind(acc_df, avg_no)
          }
          
          acc_reg <- acc_df[grepl("\\(\\+reg\\)", acc_df$variable), c("MAE","RMSE","MAPE"), drop = FALSE]
          if (nrow(acc_reg) > 0) {
            avg_reg <- data.frame(
              variable = "Average (+reg)",
              MAE  = mean(acc_reg$MAE,  na.rm = TRUE),
              RMSE = mean(acc_reg$RMSE, na.rm = TRUE),
              MAPE = mean(acc_reg$MAPE, na.rm = TRUE),
              stringsAsFactors = FALSE
            )
            acc_df <- rbind(acc_df, avg_reg)
          }
        }
        
        list(forecast = forecast_combined, accuracy = acc_df)
      }
    )
  )
