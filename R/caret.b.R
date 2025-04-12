# This file is a generated template, your changes will not be overwritten
#' @export

caretClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "caretClass",
    inherit = caretBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
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
            '<li>Machine learning based on  <a href="https://topepo.github.io/caret/" target = "_blank">caret R package</a>.</li>',
            '<li>The values for the target variable cannot be a number.</li>',
            '<li>If you use the <b>lda</b> function, uncheck the <b>ROC plot</b> in Test set and the <b>Model selection plot</b> in Plots.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
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
        
        if (isTRUE(self$options$plot7)) {
          width <- self$options$width7
          height <- self$options$height7
          self$results$plot7$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot8)) {
          width <- self$options$width8
          height <- self$options$height8
          self$results$plot8$setSize(width, height)
        }
        
      },
      
      #---------------------------------------------
      
      .run = function() {
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
        ml <- self$options$ml
        me <- self$options$me
        rep <- self$options$rep
        num <- self$options$num
        
        data <- self$data
        dep <- self$options$dep
        covs <- self$options$covs
        facs <- self$options$facs
        #all <- private$.computeFIT()
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeFIT()
        }
        
        all <- private$.allCache
        
        # Model information-----------
        self$results$text$setContent(all$fit)
        # Compare models--------------
        # https://www.machinelearningplus.com/machine-learning/caret-package/
        # Stacking Algorithms - Run multiple algos in one call.
        
        ctrl.comp <- caret::trainControl (
          method = me,
          number = num,
          repeats = rep,
          p = per,
          classProbs = T,
          savePredictions = T
        )
        
        ml <- self$options$ml
        ml <- strsplit(self$options$ml, ',')[[1]]
        algorithmList <- ml
        
        models <- caretEnsemble::caretList(
          all$formula,
          data = all$train,
          trControl = ctrl.comp,
          methodList = algorithmList
        )
        results <- caret::resamples(models)
        res <- summary(results)
        
        # Accuracy Table---------
        if (self$options$accu == TRUE) {
          table <- self$results$mf$accu
          accu <- as.data.frame(res$statistics$Accuracy)
          
          lapply(rownames(accu), function(name) {
            row <- list(
              min = accu[name, 1],
              q1 = accu[name, 2],
              med = accu[name, 3],
              me = accu[name, 4],
              q3 = accu[name, 5],
              max = accu[name, 6],
              na = accu[name, 7]
            )
            table$addRow(rowKey = name, values = row)
          })
        }
        
        # kappa Table---------
        
        if (self$options$kapp == TRUE) {
          table <- self$results$mf$kapp
          kapp <- as.data.frame(res$statistics$Kappa)
          lapply(rownames(kapp), function(name) {
            row <- list(
              min = kapp[name, 1],
              q1 = kapp[name, 2],
              med = kapp[name, 3],
              me = kapp[name, 4],
              q3 = kapp[name, 5],
              max = kapp[name, 6],
              na = kapp[name, 7]
            )
            table$addRow(rowKey = name, values = row)
          })
        }
        
        # # ROC with training set-----------
        # if(self$options$plot8==TRUE){
        # image8 <- self$results$plot8
        # image8$setState(fit)
        # }
        
        # box plots for model comparison----
        
        if (self$options$plot7 == TRUE) {
          image7 <- self$results$plot7
          image7$setState(results)
          
        }
        # Variable importance plot----------
        
        if (self$options$plot1 == TRUE) {
          vi <- caret::varImp(all$fit)
          
          image1 <- self$results$plot1
          image1$setState(vi)
        }
        
        #TRAINING SET#############################
        
        # Save: Prediction with train model -----------
        
        if (self$options$pred == TRUE) {
          # Example in R-------
          
          # # View the predictions
          # print(predictions)
          # ######################################################
          # # Load the necessary libraries
          # library(caret)
          # # Load the iris dataset
          # data(iris)
          # # Train the model using the caret package
          # fitControl <- trainControl(method = "cv", number = 5)
          # model <- train(Species ~ ., data = iris, method = "rpart", trControl = fitControl)
          #
          # # Load the new data
          # new <- data.frame(Sepal.Length = c(6, 5.5, 5,2),
          #                   Sepal.Width = c(3, 2.5, 2,1.2),
          #                   Petal.Length = c(4, 3.5, 3,2.2),
          #                   Petal.Width = c(1, 3.5, 0,1))
          #
          # # Use the selected model to make predictions on the new data
          # pred <- predict(model, newdata = new)
          
          # # trainControl function-----------
          #
          # ctrl <- caret::trainControl(method = mecon,
          #                                   number =number ,
          #                                   repeats = repeats,
          #                                   classProbs=T,
          #                                   savePredictions = T)
          #
          # # Training dataset---------------
          #
          # fit <- caret::train(formula,
          #                     data=data,
          #                     method = method,
          #                     tuneLength = tune,
          #                     trControl =  ctrl)
          
          all <- private$.computeFIT()
          
          covs <- self$options$covs
          facs <- self$options$facs
          # new data-----------
          #dataset to predict dep. with train model------------
          new_data <- jmvcore::select(self$data, c(covs, facs))
          new_data <- jmvcore::naOmit(new_data)
          
          pred <- predict(all$fit, new_data)
          
          self$results$pred$setValues(pred)
          self$results$pred$setRowNums(rownames(new_data))
          
        }
        
        # Predict with train set-----------------
        
        pred.tr <- predict(all$fit, all$train)
        
        # Confusion matrix(train set)---------------------------
        
        eval.tr <- caret::confusionMatrix(pred.tr, all$train[[dep]])
        
        if (isTRUE(self$options$tra)) {
          table <- self$results$tra
          tab.tr <- eval.tr$table
          res1.tr <- as.matrix(tab.tr)
          names <- dimnames(res1.tr)[[1]]
          for (name in names) {
            table$addColumn(name = paste0(name),
                            type = 'Integer',
                            superTitle = 'Predicted')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(names)) {
              row[[names[j]]] <- res1.tr[name, j]
            }
            table$addRow(rowKey = name, values = row)
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
          cla1 <- t(cla1)
          cla1 <- as.data.frame(cla1)
          
          names <- dimnames(cla1)[[1]]
          dims <- dimnames(cla1)[[2]]
          covs <- self$options$covs
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- cla1[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        #TEST SET---
        
        # Predict with test set-----------------
        
        pred <- predict(all$fit, all$test)
        
        # ROC curve with test set------
        # Binary dependent required!
        if (self$options$plot3 == TRUE) {
                  pro <- predict(all$fit, all$test, type = 'prob') 
                  roct<-  data.frame(pro, 
                                   all$test[[dep]], 
                                   Group = self$options$method)
         #self$results$text2$setContent(roct)
                  
          image3 <- self$results$plot3
          image3$setState(roct)
        }
        
        # Confusion matrix(test set)---------------------------
        
        eval <- caret::confusionMatrix(pred, all$test[[dep]])
        
        if (isTRUE(self$options$tes)) {
          table <- self$results$tes
          tab <- eval$table
          res1 <- as.matrix(tab)
          names <- dimnames(res1)[[1]]
          
          for (name in names) {
            table$addColumn(name = paste0(name),
                            type = 'Integer',
                            superTitle = 'Predicted')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(names)) {
              row[[names[j]]] <- res1[name, j]
            }
            table$addRow(rowKey = name, values = row)
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
          cla <- t(cla)
          cla <- as.data.frame(cla)
          
          names <- dimnames(cla)[[1]]
          dims <- dimnames(cla)[[2]]
          covs <- self$options$covs
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- cla[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # Feature plot-----------
        
        if (self$options$plot5 == TRUE ||
            self$options$plot6 == TRUE) {
          data <- self$data
          dep <- self$options$dep
          covs <- self$options$covs
          
          # data cleaning---------------
          
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
      
      #Plot---
      
      .plot5 = function(image5, ...) {
        if (is.null(image5$state))
          return(FALSE)
        
        data <- image5$state
        covs <- self$options$covs
        dep <- self$options$dep
        
        # caret::featurePlot(x = iris[,1:4],
        #                    y = iris$Species,
        #                    plot = "box")
        
        
        plot5 <- caret::featurePlot(
          x = data[, covs],
          y = data[[dep]],
          plot = "box",
          strip = lattice::strip.custom(par.strip.text =
                                          list(cex = .7)),
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
          strip = lattice::strip.custom(par.strip.text =
                                          list(cex = .7)),
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
        
        all <- private$.computeFIT()
        res <- MLeval::evalm(list(all$fit, all$comp),
                             gnames = c(self$options$method, self$options$cm1))
        
        plot <- res$roc
        print(plot)
        TRUE
      },
      
      .plot4 = function(image4, ...) {
        if (!self$options$plot4)
          return(FALSE)
        
        all <- private$.computeFIT()
        
        res <- MLeval::evalm(list(all$fit, all$comp),
                             gnames = c(self$options$method, self$options$cm1))
        
        # Calibration curve
        plot4 <- res$cc
        print(plot4)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (!self$options$plot2)
          return(FALSE)
        
        all <- private$.computeFIT()
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
        # ROC with test set-----
        # Binary only !
        
        if (is.null(image3$state))
          return(FALSE)

        roct <- image3$state
        res <- MLeval::evalm(roct)
        plot3 <- res$roc
        print(plot3)
        TRUE
      },
      
      .plot7 = function(image7, ...) {
        # ROC with test set-----
        
        if (is.null(image7$state))
          return(FALSE)
        
        res <- image7$state
        
        # Box plots to compare models
        scales <- list(x = list(relation = "free"),
                       y = list(relation = "free"))
        
        plot7 <- lattice::bwplot(res, scales = scales)
        print(plot7)
        TRUE
      },
      
      .plot8 = function(image8, ...) {
        if (!self$options$plot8)
          return(FALSE)
        # ROC with traing set-----
        all <- private$.computeFIT()
        res1 <- MLeval::evalm(all$fit)
        # get ROC---
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
        ml <- self$options$ml
        me <- self$options$me
        rep <- self$options$rep
        num <- self$options$num
        
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
        
        # To speed up the function------
        
        formula <- as.formula(paste0(self$options$dep, " ~ ."))
        
        # Create Train/test dataset using caret package-----------------
        
        set.seed(1234)
        split1 <- caret::createDataPartition(data[[dep]], p = per, list = F)
        train1 <- data[split1, ]
        test1 <- data[-split1, ]
        
        # Transformed dataset-----------------
        # Create the bagImpute model on the training data
        # for missing values with continuous variables..
        
        preProcValues <- caret::preProcess(train1, method = trans)
        
        self$results$text1$setContent(preProcValues)
        
        train <- predict(preProcValues, train1)
        test <- predict(preProcValues, test1)
        
        # Dummy coding for factors vars.-------------------
        
        if (isTRUE(self$options$facs == TRUE)) {
          # To speed up the function------
          
          formula <- as.formula(paste0(self$options$dep, " ~ ."))
          
          # One-Hot Encoding
          # Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
          dummies_model <- caret::dummyVars(formula, data = train1)
          
          # Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
          trainData_mat <- predict(dummies_model, newdata = test1)
          
          # Convert to dataframe
          train <- data.frame(trainData_mat)
          
        }
        
        # trainControl function-----------
        
        ctrl <- caret::trainControl(
          method = mecon,
          number = number ,
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
          trControl =  ctrl
        )
        
        # Compare ROC curves------------------
        comp <- caret::train(
          formula,
          data = train,
          method = cm1,
          tuneLength = tune,
          trControl =  ctrl
        )
        
        retlist <- list(
          formula = formula,
          train = train,
          test = test,
          fit = fit,
          comp = comp
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