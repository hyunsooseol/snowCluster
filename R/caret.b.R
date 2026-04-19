# This file is a generated template, your changes will not be overwritten
#' @export

caretClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "caretClass",
    inherit = caretBase,
    private = list(
      .allCache = NULL,
      .allCacheKey = NULL,
      .compCache = NULL,
      .compCacheKey = NULL,
      .htmlwidget = NULL,
      .evalCache = NULL,
      .evalCacheKey = NULL,
      .evalFitCache = NULL,
      .evalFitCacheKey = NULL,
      .evalTestCache = NULL,
      .evalTestCacheKey = NULL,      
      
      #------------------------------------
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$dep) |
            is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Machine learning based on the <a href="https://topepo.github.io/caret/" target="_blank">caret R package</a>.</li>',
            '<li>The target variable must be categorical (non-numeric).</li>',
            '<li>Both numeric and categorical predictors can be used.</li>',
            '<li>Some ROC/calibration-related plots may not be available for certain models. If a plot is not displayed, try another model or uncheck that plot option.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))

      },
      
      .getEvalComp = function() {
        all <- private$.allCache
        
        if (is.null(all) || is.null(all$fit) || is.null(all$comp))
          return(NULL)
        
        if (!inherits(all$fit, "train") || !inherits(all$comp, "train"))
          return(NULL)
        
        if (is.null(self$options$cm1) || self$options$cm1 == "")
          return(NULL)
        
        key <- paste(
          self$options$method,
          self$options$cm1,
          class(all$fit)[1],
          class(all$comp)[1],
          sep = " | "
        )
        
        if (is.null(private$.evalCacheKey) || private$.evalCacheKey != key) {
          private$.evalCache <- MLeval::evalm(
            list(all$fit, all$comp),
            gnames = c(self$options$method, self$options$cm1)
          )
          private$.evalCacheKey <- key
        }
        
        private$.evalCache
      },
      
      .getEvalFit = function() {
        all <- private$.allCache
        
        if (is.null(all) || is.null(all$fit))
          return(NULL)
        
        if (!inherits(all$fit, "train"))
          return(NULL)
        
        key <- paste(self$options$method, class(all$fit)[1], sep = " | ")
        
        if (is.null(private$.evalFitCacheKey) || private$.evalFitCacheKey != key) {
          private$.evalFitCache <- tryCatch(
            MLeval::evalm(all$fit),
            error = function(e) NULL
          )
          private$.evalFitCacheKey <- key
        }
        
        private$.evalFitCache
      },
      
      .getEvalTest = function(roct) {
        if (is.null(roct))
          return(NULL)
        
        key <- paste(
          nrow(roct),
          paste(names(roct), collapse = ","),
          self$options$method,
          sep = " | "
        )
        
        if (is.null(private$.evalTestCacheKey) || private$.evalTestCacheKey != key) {
          private$.evalTestCache <- tryCatch(
            MLeval::evalm(roct),
            error = function(e) NULL
          )
          private$.evalTestCacheKey <- key
        }
        
        private$.evalTestCache
      },      
      
#---------------------------------------------
      .run = function() {
        
        if (!isTRUE(self$options$run))
          return()
        

        if (is.null(self$options$dep) ||
            length(self$options$covs) < 2)
          return()
        trans <- self$options$trans
        mecon <- self$options$mecon
        repeats <- self$options$repeats
        number <- self$options$number
        tune <- self$options$tune
        per <- self$options$per
        method <- self$options$method
        cm1 <- self$options$cm1
        #ml <- self$options$ml
        me <- self$options$me
        rep <- self$options$rep
        num <- self$options$num
        tune1 <- self$options$tune1
        
        data <- self$data
        dep <- self$options$dep
        covs <- self$options$covs
        facs <- self$options$facs
        
        mainKey <- paste(
          dep,
          paste(covs, collapse = ","),
          paste(facs, collapse = ","),
          per,
          trans,
          mecon,
          method,
          cm1,
          self$options$plot,
          self$options$plot4,
          number,
          repeats,
          tune,
          sep = " | "
        )
        
        
        if (is.null(private$.allCacheKey) || private$.allCacheKey != mainKey) {
          private$.allCache <- NULL
          private$.allCacheKey <- mainKey
        }
        
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeFIT()
        }
        all <- private$.allCache
        
        compKey <- paste(
          dep,
          paste(covs, collapse = ","),
          paste(facs, collapse = ","),
          per,
          paste(strsplit(self$options$ml, ",")[[1]], collapse = ","),
          me,
          num,
          rep,
          self$options$tune1,
          sep = " | "
        )
        
        if (is.null(private$.compCacheKey) || private$.compCacheKey != compKey) {
          private$.compCache <- NULL
          private$.compCacheKey <- compKey
        }
        
        # Model information-----------
        self$results$text$setContent(all$fit)
        
        # Compare models--------------
        if (isTRUE(self$options$accu) || isTRUE(self$options$kapp) || isTRUE(self$options$plot7)) {
          
          if (is.null(private$.compCache)) {
            ctrl.comp <- caret::trainControl(
              method = me,
              number = num,
              repeats = rep,
              p = per,
              classProbs = TRUE,
              savePredictions = TRUE
            )
            
            algorithmList <- strsplit(self$options$ml, ',')[[1]]
            
            models <- caretEnsemble::caretList(
              all$formula,
              data = all$train,
              trControl = ctrl.comp,
              methodList = algorithmList,
              tuneLength = tune1
            )
            
            results <- caret::resamples(models)
            
            private$.compCache <- list(
              results = results,
              summary = summary(results)
            )
          }
          
          results <- private$.compCache$results
          res <- private$.compCache$summary
          
          # Accuracy Table---------
          if (isTRUE(self$options$accu)) {
            table <- self$results$mf$accu
            accu <- as.data.frame(res$statistics$Accuracy)
            
            lapply(rownames(accu), function(name) {
              row <- list(
                min = accu[name, 1],
                q1  = accu[name, 2],
                med = accu[name, 3],
                me  = accu[name, 4],
                q3  = accu[name, 5],
                max = accu[name, 6],
                na  = accu[name, 7]
              )
              table$addRow(rowKey = name, values = row)
            })
          }
          
          # kappa Table---------
          if (isTRUE(self$options$kapp)) {
            table <- self$results$mf$kapp
            kapp <- as.data.frame(res$statistics$Kappa)
            
            lapply(rownames(kapp), function(name) {
              row <- list(
                min = kapp[name, 1],
                q1  = kapp[name, 2],
                med = kapp[name, 3],
                me  = kapp[name, 4],
                q3  = kapp[name, 5],
                max = kapp[name, 6],
                na  = kapp[name, 7]
              )
              table$addRow(rowKey = name, values = row)
            })
          }
          
          # box plots for model comparison----
          if (isTRUE(self$options$plot7)) {
            image7 <- self$results$plot7
            image7$setState(results)
          }
        }
        
        
        # Variable importance plot----------
        if (isTRUE(self$options$plot1)) {
          vi <- caret::varImp(all$fit)
          image1 <- self$results$plot1
          image1$setState(vi)
        }
        
        #TRAINING SET#############################
        
        # Save: Prediction with train model -----------
        if (isTRUE(self$options$pred)) {
          covs <- self$options$covs
          facs <- self$options$facs
          new_data <- jmvcore::select(self$data, c(covs, facs))
          new_data <- jmvcore::naOmit(new_data)
          
          for (cov in covs)
            new_data[[cov]] <- jmvcore::toNumeric(new_data[[cov]])
          
          for (fac in facs)
            new_data[[fac]] <- as.factor(new_data[[fac]])
          
          # same preprocessing as training
          if (!is.null(all$preProcValues))
            new_data <- predict(all$preProcValues, new_data)
          
          # same dummy coding as training
          if (!is.null(all$dummies_model))
            new_data <- data.frame(predict(all$dummies_model, newdata = new_data))
          
          pred <- predict(all$fit, new_data)
          
          self$results$pred$setValues(pred)
          self$results$pred$setRowNums(rownames(new_data))
        }
        
        # Predict with train set-----------------
        pred.tr <- predict(all$fit, all$train)
        
        # # Confusion matrix(train set)---------------------------
        # # Use positive parameter only if it's specified and valid
        # if (!is.null(self$options$positive1) && 
        #     self$options$positive1 != "" && 
        #     self$options$positive1 %in% levels(all$train[[dep]])) {
        #   eval.tr <- caret::confusionMatrix(pred.tr, 
        #                                     all$train[[dep]],
        #                                     positive = self$options$positive1)
        # } else {
        #   eval.tr <- caret::confusionMatrix(pred.tr, all$train[[dep]])
        # }
        # 
        # if (isTRUE(self$options$tra)) {
        #   table <- self$results$tra
        #   tab.tr <- eval.tr$table
        #   res1.tr <- as.matrix(tab.tr)
        #   names <- dimnames(res1.tr)[[1]]
        #   for (name in names) {
        #     table$addColumn(name = paste0(name),
        #                     type = 'Integer',
        #                     superTitle = 'Predicted')
        #   }
        #   for (name in names) {
        #     row <- list()
        #     for (j in seq_along(names)) {
        #       row[[names[j]]] <- res1.tr[name, j]
        #     }
        #     table$addRow(rowKey = name, values = row)
        #   }
        # }
        # Confusion matrix (train set) ---------------------------
        
        # 1. 실제/예측값 factor 변환 및 levels 통일
        actual <- as.factor(all$train[[dep]])
        predicted <- as.factor(pred.tr)
        common_levels <- union(levels(actual), levels(predicted))
        actual <- factor(actual, levels = common_levels)
        predicted <- factor(predicted, levels = common_levels)
        
        # 2. positive 값이 실제 존재하는지 확인
        positive1 <- self$options$positive1
        use_positive <- !is.null(positive1) && positive1 != "" && positive1 %in% common_levels
        
        # 3. confusionMatrix 실행 (positive 옵션 자동 적용)
        if (use_positive) {
          eval.tr <- caret::confusionMatrix(predicted, actual, positive = positive1)
        } else {
          eval.tr <- caret::confusionMatrix(predicted, actual)
        }
        

        # 결과 테이블 생성 (행=Prediction, 열=Reference(Actual))
        if (isTRUE(self$options$tra)) {
          table <- self$results$tra
          tab.tr <- eval.tr$table
          res1.tr <- as.matrix(tab.tr)
          pred_names <- dimnames(res1.tr)[[1]]     # Prediction(행)
          actual_names <- dimnames(res1.tr)[[2]]   # Reference(열)
          
          # 열 생성: Reference(실제값) 명확히 표기
          for (name in actual_names) {
            table$addColumn(
              name = paste0(name),
              type = 'Integer',
              superTitle = 'Reference'
            )
          }
          # 행 추가: Prediction(예측값) 명확히 표기
          for (i in seq_along(pred_names)) {
            row <- list()
            for (j in seq_along(actual_names)) {
              row[[actual_names[j]]] <- res1.tr[pred_names[i], actual_names[j]]
            }
            table$addRow(rowKey = pred_names[i], values = row)
          }
        }        
        # Overall statistics with training set-----------
        if (isTRUE(self$options$over1)) {
          table <- self$results$over1
          row <- list(
            accu = eval.tr[["overall"]][1],
            lower = eval.tr[["overall"]][3],
            upper = eval.tr[["overall"]][4],
            kappa = eval.tr[["overall"]][2]
          )
          table$setRow(rowNo = 1, values = row)
        }
        
        # Statistics by class WITH TRAINing set-----------
        if (isTRUE(self$options$cla1)) {
          table <- self$results$cla1
          
          cla1 <- eval.tr[["byClass"]]
          if (is.vector(cla1)) {
            cla1 <- as.data.frame(t(cla1))
          } else {
            cla1 <- as.data.frame(cla1)
          }
          
          names <- dimnames(cla1)[[1]]
          dims <- dimnames(cla1)[[2]]
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- cla1[name, j]
            }
            table$addRow(rowKey = "Training", values = row)
          }
        }
        
        # Predict with test set-----------------
        pred <- predict(all$fit, all$test)
        
        # ROC curve with test set------
        if (self$options$plot3 == TRUE) {
          pro <- predict(all$fit, all$test, type = 'prob') 
          roct <- data.frame(pro, 
                             all$test[[dep]], 
                             Group = self$options$method)
          
          image3 <- self$results$plot3
          image3$setState(roct)
        }
        
        # Confusion matrix (test set) ---------------------------
        # Predict with test set-----------------
        pred <- predict(all$fit, all$test)
        
        # 1. 실제/예측값 factor 변환 및 levels 통일 (train과 동일)
        actual.test <- as.factor(all$test[[dep]])
        predicted.test <- as.factor(pred)
        common_levels.test <- union(levels(actual.test), levels(predicted.test))
        actual.test <- factor(actual.test, levels = common_levels.test)
        predicted.test <- factor(predicted.test, levels = common_levels.test)
        
        # 2. positive 값이 실제 존재하는지 확인 (train과 동일)
        positive2 <- self$options$positive
        use_positive2 <- !is.null(positive2) && positive2 != "" && positive2 %in% common_levels.test
        
        # 3. confusionMatrix 실행 (positive 옵션 자동 적용)
        if (use_positive2) {
          eval <- caret::confusionMatrix(predicted.test, actual.test, positive = positive2)
        } else {
          eval <- caret::confusionMatrix(predicted.test, actual.test)
        }
        
        # 4. caret 구조(행=Prediction, 열=Reference)로 표 생성
        if (isTRUE(self$options$tes)) {
          table <- self$results$tes
          tab <- eval$table
          res1 <- as.matrix(tab)
          pred_names <- dimnames(res1)[[1]]    # Prediction (행)
          actual_names <- dimnames(res1)[[2]]  # Reference (열)
          
          # 열 생성: Reference(실제값) 명확히 표기
          for (name in actual_names) {
            table$addColumn(
              name = paste0(name),
              type = 'Integer',
              superTitle = 'Reference'
            )
          }
          # 행 추가: Prediction(예측값) 명확히 표기
          for (i in seq_along(pred_names)) {
            row <- list()
            for (j in seq_along(actual_names)) {
              row[[actual_names[j]]] <- res1[pred_names[i], actual_names[j]]
            }
            table$addRow(rowKey = pred_names[i], values = row)
          }
        }
        
        # Overall statistics with test data-----------
        if (isTRUE(self$options$over)) {
          table <- self$results$over
          
          row <- list(
            accu = eval[["overall"]][1],
            lower = eval[["overall"]][3],
            upper = eval[["overall"]][4],
            kappa = eval[["overall"]][2]
          )
          
          table$setRow(rowNo = 1, values = row)
        }
        
        # Statistics by class-----------
        if (isTRUE(self$options$cla)) {
          table <- self$results$cla
          
          cla <- eval[["byClass"]]
          if (is.vector(cla)) {
            cla <- as.data.frame(t(cla))
          } else {
            cla <- as.data.frame(cla)
          }
          
          names <- dimnames(cla)[[1]]
          dims <- dimnames(cla)[[2]]
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- cla[name, j]
            }
            table$addRow(rowKey = "Test", values = row)
          }
        } 
        
        # Feature plot-----------
        if (isTRUE(self$options$plot5) || isTRUE(self$options$plot6)) {
          data <- self$data
          dep <- self$options$dep
          covs <- self$options$covs
          
          for (cov in covs)
            data[[cov]] <- jmvcore::toNumeric(data[[cov]])
          
          data[[dep]] <- as.factor(data[[dep]])
          data <- na.omit(data)
          
          image5 <- self$results$plot5
          image5$setState(data)
          
          image6 <- self$results$plot6
          image6$setState(data)
        }
      },
      
      #Plot functions---
      
      .plot5 = function(image5, ...) {
        if (is.null(image5$state))
          return(FALSE)
        
        data <- image5$state
        covs <- self$options$covs
        dep <- self$options$dep
        
        plot5 <- caret::featurePlot(
          x = data[, covs],
          y = data[[dep]],
          plot = "box",
          strip = lattice::strip.custom(par.strip.text = list(cex = .7)),
          scales = list(
            x = list(relation = "free"),
            y = list(relation = "free")
          )
        )
        print(plot5)
        TRUE
      },
      
      .plot6 = function(image6, ...) {
        if (is.null(image6$state))
          return(FALSE)
        
        data <- image6$state
        covs <- self$options$covs
        dep <- self$options$dep
        
        plot6 <- caret::featurePlot(
          x = data[, covs],
          y = data[[dep]],
          plot = "density",
          strip = lattice::strip.custom(par.strip.text = list(cex = .7)),
          scales = list(
            x = list(relation = "free"),
            y = list(relation = "free")
          )
        )
        print(plot6)
        TRUE
      },
      
      .plot = function(image, ...) {
        if (!self$options$plot)
          return(FALSE)
        
        res <- private$.getEvalComp()
        if (is.null(res) || is.null(res$roc))
          return(FALSE)
        
        plot <- res$roc
        print(plot)
        TRUE
      },
      
      .plot4 = function(image4, ...) {
        if (!self$options$plot4)
          return(FALSE)
        
        res <- private$.getEvalComp()
        if (is.null(res) || is.null(res$cc))
          return(FALSE)
        
        plot4 <- res$cc
        print(plot4)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (!self$options$plot2)
          return(FALSE)
        
        all <- private$.allCache
        
        if (is.null(all) || is.null(all$fit))
          return(FALSE)
        
        if (!inherits(all$fit, "train"))
          return(FALSE)
        
        plot2 <- ggplot2::ggplot(all$fit)
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        vi <- image1$state
        plot1 <- plot(vi)
        print(plot1)
        TRUE
      },
      
      .plot3 = function(image3, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        roct <- image3$state
        res <- private$.getEvalTest(roct)
        if (is.null(res) || is.null(res$roc))
          return(FALSE)
        
        plot3 <- res$roc
        print(plot3)
        TRUE
      },
      
      .plot7 = function(image7, ...) {
        if (is.null(image7$state))
          return(FALSE)
        
        res <- image7$state
        scales <- list(x = list(relation = "free"),
                       y = list(relation = "free"))
        
        plot7 <- lattice::bwplot(res, scales = scales)
        print(plot7)
        TRUE
      },
      
      .plot8 = function(image8, ...) {
        if (!self$options$plot8)
          return(FALSE)
        
        res1 <- private$.getEvalFit()
        if (is.null(res1) || is.null(res1$roc))
          return(FALSE)
        
        plot8 <- res1$roc
        print(plot8)
        TRUE
      },
      
      .computeFIT = function() {
        trans <- self$options$trans
        mecon <- self$options$mecon
        repeats <- self$options$repeats
        number <- self$options$number
        tune <- self$options$tune
        per <- self$options$per
        method <- self$options$method
        cm1 <- self$options$cm1
        
        data <- self$data
        dep <- self$options$dep
        covs <- self$options$covs
        facs <- self$options$facs
        
        # data cleaning---------------
        for (fac in facs)
          data[[fac]] <- as.factor(data[[fac]])
        
        for (cov in covs)
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        
        data[[dep]] <- as.factor(data[[dep]])
        data <- na.omit(data)
        
        formula <- as.formula(paste0(self$options$dep, " ~ ."))
        
        # Create Train/test dataset using caret package-----------------
        set.seed(1234)
        split1 <- caret::createDataPartition(data[[dep]], p = per, list = F)
        train1 <- data[split1, ]
        test1 <- data[-split1, ]
        
        # Transformed dataset
        preProcValues <- caret::preProcess(train1, method = trans)
        self$results$text1$setContent(preProcValues)
        
        train <- predict(preProcValues, train1)
        test  <- predict(preProcValues, test1)
        
        # Dummy coding for factors vars
        dummies_model <- NULL
        if (!is.null(facs) && length(facs) > 0) {
          dummy_formula <- stats::as.formula("~ .")
          
          x_train <- train[, c(covs, facs), drop = FALSE]
          x_test  <- test[, c(covs, facs), drop = FALSE]
          
          dummies_model <- caret::dummyVars(dummy_formula, data = x_train, fullRank = TRUE)
          
          train_x <- predict(dummies_model, newdata = x_train)
          test_x  <- predict(dummies_model, newdata = x_test)
          
          train <- data.frame(train_x)
          test  <- data.frame(test_x)
          
          train[[dep]] <- train1[[dep]]
          test[[dep]]  <- test1[[dep]]
        }
        
        
        
        # trainControl function-----------
        ctrl <- caret::trainControl(
          method = mecon,
          number = number,
          repeats = repeats,
          p = per,
          classProbs = T,
          savePredictions = T
        )
        
        # Training dataset---------------
        fit <- caret::train(
          formula,
          data = train,
          method = method,
          tuneLength = tune,
          trControl = ctrl
        )
        
        # Compare ROC/calibration model: only when needed
        need_comp <- isTRUE(self$options$plot) || isTRUE(self$options$plot4)
        
        comp <- NULL
        if (need_comp && !is.null(cm1) && nzchar(cm1)) {
          comp <- caret::train(
            formula,
            data = train,
            method = cm1,
            tuneLength = tune,
            trControl = ctrl
          )
        }
        
        retlist <- list(
          formula = formula,
          train = train,
          test = test,
          fit = fit,
          comp = comp,
          preProcValues = preProcValues,
          dummies_model = if (!is.null(facs) && length(facs) > 0) dummies_model else NULL
        )
        
        return(retlist)
      }      
)
)






# # iris example in R-----------
# library(caret)
# data(iris)
#
# #Split into train and test dataset
# trainIndex <- createDataPartition(iris$Species, p = .8,
#                                   list = FALSE,
#                                   times = 1)
# train <- iris[ trainIndex,]
# test  <- iris[-trainIndex,]
#
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   repeats = 5)
#
# dt.fit <- train(Species ~ ., data = train,
#                 method = "rpart",
#                 trControl = fitControl,
#                 preProcess=c("center", "scale"))
#
# predictions <- predict(dt.fit, test)
# predictions
#
# eval<- confusionMatrix(predictions, test$Species)
#


# # data cleaning---------------
#
# for(fac in facs)
#   data[[fac]]<-as.factor(data[[fac]])
#
# for(cov in covs)
#   data[[cov]] <- jmvcore::toNumeric(data[[cov]])
#
# # data[[dep]] <- jmvcore::toNumeric(data[[dep]])
#
# # When caretList() runs a tree-based model
# # (here rpart, but also applies to random forests),
# # it converts the factor levels into variables which are used to split the tree.
# # For these variables, names starting with a number are not allowed nor that they contain spaces.
# # So for each of these variables, you can convert the level names to valid labels with the following code.
#
# # The values for the response variable cannot be a number !
#
# data[[dep]] <- as.factor(data[[dep]])
#
# data <- na.omit(data)
#
#
# # To speed up the function------
#
#  formula <- as.formula(paste0(self$options$dep, " ~ ."))
#
#
# # Create Train/test dataset using caret package-----------------
#
#   set.seed(1234)
#   split1<- caret::createDataPartition(data[[dep]], p=per,list = F)
#   train1 <-data[split1,]
#   test1 <- data[-split1,]
#
# # Transformed dataset-----------------
# # Create the bagImpute model on the training data
# # for missing values with continuous variables..
#
#   preProcValues <- caret::preProcess(train1,
#                                      method = trans)
#
#   self$results$text1$setContent(preProcValues)
#
#   train <- predict(preProcValues, train1)
#   test <- predict(preProcValues, test1)
#
#
# # Dummy coding for factors vars.-------------------
#
#   if(isTRUE(self$options$facs==TRUE)){
#   #if(isTRUE(condition)==TRUE) {do something}
#
#   #if ( !is.null(self$options$facs) && self$options$facs==TRUE) {
#
#
#  # To speed up the function------
#
#     formula <- as.formula(paste0(self$options$dep, " ~ ."))
#
#   # One-Hot Encoding
#   # Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
#   dummies_model <- caret::dummyVars(formula,
#                                     data=train1)
#
#   # Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
#   trainData_mat <- predict(dummies_model, newdata = test1)
#
#   # Convert to dataframe
#   train <- data.frame(trainData_mat)
#
#   }
#
#   # trainControl function-----------
#
#    ctrl <- caret::trainControl(method = mecon,
#                                        number =number ,
#                                        repeats = repeats,
#                                        p=per,
#                                        classProbs=T,
#                                        savePredictions = T)
#
#
# # Training dataset---------------
#
#   fit <- caret::train(formula,
#                       data=train,
#                       method = method,
#                       tuneLength = tune,
#                       trControl =  ctrl)


#  # Compare ROC curves------------------
# comp <- caret::train(formula,
#                      data=train,
#                      method = cm1,
#                      tuneLength = tune,
#                      trControl =  ctrl)
#

# Comparing ROC curves with training set-----------------

# if(self$options$plot==TRUE){
#
#  image <- self$results$plot
#  state <- list(fit,comp)
#  image$setState(state)
#
# }
# Calibration curve---------
#
# if(self$options$plot4==TRUE){
#  image4 <- self$results$plot4
#  state <- list(all$fit,all$comp)
#  image4$setState(state)
# }

# Model selection plot2----------
# if(self$options$plot2==TRUE){
#  image2 <- self$results$plot2
#  image2$setState(all$fit)
# }