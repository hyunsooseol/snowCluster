
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom caret preProcess
#' @importFrom caret confusionMatrix
#' @importFrom caret trainControl
#' @importFrom caret varImp
#' @importFrom MLeval evalm
#' @importFrom caret preProcess
#' @importFrom caTools sample.split
#' @importFrom lattice strip.custom
#' @importFrom caret dummyVars
#' @importFrom caretEnsemble caretList
#' @importFrom caret resamples
#' @importFrom lattice bwplot
#' @import caret
#' @import xgboost
#' @import rpart.plot
#' @import neuralnet
#' @import pls
#' @import party
#' @import elasticnet
#' @import nnet
#' @import gbm
#' @import mda
#' @import kernlab
#' @import earth
#' @import ggplot2
#' @import jmvcore
#' @import RANN
#' @import klaR
#' @import randomForest
#' @import plyr
#' @import mboost
#' @import glmnet
#' @import C50
#' @export


caretClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "caretClass",
    inherit = caretBase,
    private = list(

      #------------------------------------
      
      .init = function() {
        if (is.null(self$options$dep) | is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p> 1. Machine learning based on <b>caret</b> R package.</p>
            <p> 2. The values for the target variable cannot be a number. </p> 
            <p> 3. The rationale of caret R package is described in the <a href='https://topepo.github.io/caret/' target = '_blank'>page</a>.</p>
            <p> 4. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
      },
      
      #---------------------------------------------
      
              .run = function() {

                
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
                
          
                if (is.null(self$options$dep) || length(self$options$covs) == 0)
                  return()
                
               
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
                
                for(fac in facs)
                  data[[fac]]<-as.factor(data[[fac]])
                
                for(cov in covs)
                  data[[cov]] <- jmvcore::toNumeric(data[[cov]])
                
                # data[[dep]] <- jmvcore::toNumeric(data[[dep]])
                 
                # When caretList() runs a tree-based model 
                # (here rpart, but also applies to random forests), 
                # it converts the factor levels into variables which are used to split the tree. 
                # For these variables, names starting with a number are not allowed nor that they contain spaces. 
                # So for each of these variables, you can convert the level names to valid labels with the following code.
                
                # The values for the response variable cannot be a number !
                
                data[[dep]] <- as.factor(data[[dep]])
                
                data <- na.omit(data)
                
                
                # To speed up the function------
                
                 formula <- as.formula(paste0(self$options$dep, " ~ ."))
                
                
                # Create Train/test dataset using caret package-----------------
                
                  set.seed(1234)
                
                  split1<- caret::createDataPartition(data[[dep]], p=per,list = F)
                  train1 <-data[split1,]
                  test1 <- data[-split1,]

                # Transformed dataset-----------------
                # Create the bagImpute model on the training data 
                # for missing values with continuous variables..
                  
                  preProcValues <- caret::preProcess(train1,
                                                     method = "bagImpute")
                  
                  self$results$text1$setContent(preProcValues)
                  
                  train <- predict(preProcValues, train1)
                  test <- predict(preProcValues, test1)
                    
                
                # Dummy coding for factors vars.-------------------
                
                  if(isTRUE(self$options$facs==TRUE)){
                  #if(isTRUE(condition)==TRUE) {do something}
                    
                  #if ( !is.null(self$options$facs) && self$options$facs==TRUE) {
                    
                   
                 # To speed up the function------
                    
                    formula <- as.formula(paste0(self$options$dep, " ~ ."))
                    
                  # One-Hot Encoding
                  # Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
                  dummies_model <- caret::dummyVars(formula, 
                                                    data=train1)
                  
                  # Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
                  trainData_mat <- predict(dummies_model, newdata = test1)
                  
                  # Convert to dataframe
                  train <- data.frame(trainData_mat)
                 
                  }
                  
                  # trainControl function-----------
                
                   ctrl <- caret::trainControl(method = mecon, 
                                                       number =number , 
                                                       repeats = repeats,
                                                       p=per,
                                                       classProbs=T,
                                                       savePredictions = T)
                                         
                  
                # Training dataset---------------
               
                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      tuneLength = tune,
                                      trControl =  ctrl)
                  
                              
                  
                # Model information-----------
                
                 self$results$text$setContent(fit)
                
                 
                # Compare models--------------
               # https://www.machinelearningplus.com/machine-learning/caret-package/
               # Stacking Algorithms - Run multiple algos in one call.

                    ctrl.comp <- caret::trainControl (method = me,
                                                   number =num,
                                                   repeats = rep,
                                                   p=per,
                                                   classProbs=T,
                                                   savePredictions = T)

                    ml <- self$options$ml 
                    ml <- strsplit(self$options$ml, ',')[[1]]
                    algorithmList <- ml
                    
                    set.seed(1234)
                    
                    models <- caretEnsemble::caretList(formula,
                                                       data=train,
                                                       trControl= ctrl.comp, 
                                                       methodList=algorithmList) 
                    results <- caret::resamples(models)
                    res<- summary(results)
                    
                    #self$results$text2$setContent(res)
                    
                    # Accuracy Table---------
                    
                    if(self$options$accu==TRUE){
                    
                      table <- self$results$accu
                      
                      accu<- data.frame(res$statistics$Accuracy)
                      
                      names <- dimnames(accu)[[1]]
                      
                      for (name in names) {
                        
                        row <- list()
                        
                        row[["min"]] <- accu[name, 1]
                        row[["q1"]] <- accu[name, 2]
                        row[["med"]] <- accu[name, 3]
                        row[["me"]] <- accu[name, 4]
                        row[["q3"]] <- accu[name, 5]
                        row[["max"]] <- accu[name, 6]
                        row[["na"]] <- accu[name, 7]
                        
                        table$addRow(rowKey=name, values=row)
                        
                      }
                      
                    }
                    
                    
                    # kappa Table---------
                    
                    if(self$options$kapp==TRUE){
                      
                      table <- self$results$kapp
                      
                      kapp<- data.frame(res$statistics$Kappa)
                      
                      names <- dimnames(kapp)[[1]]
                      
                      for (name in names) {
                        
                        row <- list()
                        
                        row[["min"]] <- kapp[name, 1]
                        row[["q1"]] <- kapp[name, 2]
                        row[["med"]] <- kapp[name, 3]
                        row[["me"]] <- kapp[name, 4]
                        row[["q3"]] <- kapp[name, 5]
                        row[["max"]] <- kapp[name, 6]
                        row[["na"]] <- kapp[name, 7]
                        
                        table$addRow(rowKey=name, values=row)
                        
                      }
                    }
                      
                    if(self$options$plot7==TRUE){
                    # box plots for model comparison---- 
                    
                    image7 <- self$results$plot7
                    image7$setState(results)
                    
                    }
                
                  # Compare ROC curves------------------ 
                 comp <- caret::train(formula,
                                      data=train,
                                      method = cm1,
                                      tuneLength = tune,
                                      trControl =  ctrl)
                 
                
                # Comparing ROC curves with training set-----------------
                
                if(self$options$plot==TRUE){ 
                  
                 image <- self$results$plot   
                 state <- list(fit,comp)
                 image$setState(state)
                   
                }
                 # Calibration curve---------
                 
                if(self$options$plot4==TRUE){ 
                 image4 <- self$results$plot4   
                 state <- list(fit,comp)
                 image4$setState(state)
                }
                 
                 # Model selection plot----------
                if(self$options$plot2==TRUE){ 
                 image2 <- self$results$plot2
                 image2$setState(fit)
                }
             
                # Variable importance plot----------
               
                if(self$options$plot1==TRUE){  
                
                vi<- caret::varImp(fit)
                
                image1 <- self$results$plot1
                image1$setState(vi)
                }
                
                #########     TRAINING SET    #############################
                
                # Prediction with train model -----------
                  
                  if(self$options$pred==TRUE){
                    
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
                    
                    # trainControl function-----------
                    
                    ctrl <- caret::trainControl(method = mecon, 
                                                      number =number , 
                                                      repeats = repeats,
                                                      classProbs=T,
                                                      savePredictions = T)
                    
                    # Training dataset---------------
                    
                    fit <- caret::train(formula,
                                        data=data,
                                        method = method,
                                        tuneLength = tune,
                                        trControl =  ctrl)
                    
                    # new data-----------
                    #dataset to predict dep. with train model------------
                    new_data <- jmvcore::select(self$data, c(covs, facs))
                    new_data <- jmvcore::naOmit(new_data)
                    #self$results$text$setContent(new_data)
                    
                    pred <- predict(fit, new_data)
                    
                    
                    self$results$pred$setValues(pred)
                    self$results$pred$setRowNums(rownames(new_data))
                    
                    
                  }
                  
                 
                  # Predict with train set-----------------
                
                pred.tr<-predict(fit, train)
                
               
                # Confusion matrix(train set)---------------------------
                
                eval.tr<- caret::confusionMatrix(pred.tr, train[[dep]]) 
                
                #---------------------------
                
                tab.tr<- eval.tr$table
                
                res1.tr<- as.matrix(tab.tr)
                
                names<- dimnames(res1.tr)[[1]]
                
                table <- self$results$tra
                
                for (name in names) {
                  
                  table$addColumn(name = paste0(name),
                                  type = 'Integer',
                                  superTitle = 'Predicted')
                }
                
                for (name in names) {
                  
                  row <- list()
                  
                  for(j in seq_along(names)){
                    
                    row[[names[j]]] <- res1.tr[name,j]
                    
                  }
                  
                  table$addRow(rowKey=name, values=row)
                  
                }
                
                # Overall statistics with training set-----------
                
                table <- self$results$over1
                
                acc<- eval.tr[["overall"]][1]
                acclow <- eval.tr[["overall"]][3]
                acchigh <- eval.tr[["overall"]][4]
                kappa <- eval.tr[["overall"]][2]
                
                row <- list()
                
                row[['accu']] <- acc
                row[['lower']] <- acclow
                row[['upper']] <- acchigh
                row[['kappa']] <- kappa
                
                table$setRow(rowNo = 1, values = row)
                
                # Statistics by class WITH TRAINing set-----------

                table <- self$results$cla1

                cla1<- eval.tr[["byClass"]]
                cla1<- t(cla1)
                cla1 <- as.data.frame(cla1)

                names<- dimnames(cla1)[[1]]
                dims <- dimnames(cla1)[[2]]
                covs <- self$options$covs

                for (dim in dims) {

                  table$addColumn(name = paste0(dim),
                                  type = 'number')
                }


                for (name in names) {

                  row <- list()


                  for(j in seq_along(dims)){

                    row[[dims[j]]] <- cla1[name,j]

                  }

                  table$addRow(rowKey=name, values=row)


                }


                ############ TEST SET  ####################################
                
                # Predict with test set-----------------
               
                pred<-predict(fit, test)
                
                # ROC curve with test set------
                
                if(self$options$plot3==TRUE){  
                  
                pred1<-predict(fit, test, type='prob')
               
                pred1<- data.frame(pred1, test[[dep]], 
                           Group = self$options$method)
                
                image3 <- self$results$plot3
                image3$setState(pred1)
                }
                
                # Confusion matrix(test set)---------------------------
                
                eval<- caret::confusionMatrix(pred, test[[dep]]) 
                
                #---------------------------
                
                tab<- eval$table
                
                res1<- as.matrix(tab)
                
                names<- dimnames(res1)[[1]]
                
                table <- self$results$tes
                
                for (name in names) {
                  
                  table$addColumn(name = paste0(name),
                                  type = 'Integer',
                                  superTitle = 'Predicted')
                }
                
                for (name in names) {
                  
                  row <- list()
                  
                  for(j in seq_along(names)){
                    
                    row[[names[j]]] <- res1[name,j]
                    
                  }
                  
                  table$addRow(rowKey=name, values=row)
                  
                }
                
                # Overall statistics with test data-----------
                
                table <- self$results$over
                
                acc<- eval[["overall"]][1]
                acclow <- eval[["overall"]][3]
                acchigh <- eval[["overall"]][4]
                kappa <- eval[["overall"]][2]
                
                row <- list()
                
                row[['accu']] <- acc
                row[['lower']] <- acclow
                row[['upper']] <- acchigh
                row[['kappa']] <- kappa
                
                table$setRow(rowNo = 1, values = row)
                
                # Statistics by class-----------
                
                table <- self$results$cla
                
                cla<- eval[["byClass"]]
                cla<- t(cla)
                cla <- as.data.frame(cla)
                
                names<- dimnames(cla)[[1]]
                dims <- dimnames(cla)[[2]]
                covs <- self$options$covs 
                
                for (dim in dims) {
                  
                  table$addColumn(name = paste0(dim),
                                  type = 'number')
                }
                
                
                for (name in names) {
                  
                  row <- list()
                  
                  
                  for(j in seq_along(dims)){
                    
                    row[[dims[j]]] <- cla[name,j]
                    
                  }
                  
                  table$addRow(rowKey=name, values=row)
                  
                  
                }
                
                
                # Feature plot-----------  
                
                if(self$options$plot5==TRUE || self$options$plot6==TRUE ){  
                  
                  data <- self$data
                  dep <- self$options$dep
                  covs <- self$options$covs
                  
                  # data cleaning--------------- 
                
                  for(cov in covs)
                    data[[cov]] <- jmvcore::toNumeric(data[[cov]])
                  
                    data[[dep]] <- as.factor(data[[dep]])
                  
                  data <- na.omit(data)
                  
                  #self$results$text1$setContent(head(data))  
                  
                  image5 <- self$results$plot5 
                  image5$setState(data)
                  
                  image6 <- self$results$plot6 
                  image6$setState(data)
                 
                }
                
                
                },
          
      ##########################################################

      .plot5 = function(image5,...) {
        
       
        if (is.null(image5$state))
          return(FALSE)
        
        data<- image5$state
        covs <- self$options$covs
        dep <- self$options$dep
        
        # caret::featurePlot(x = iris[,1:4],
        #                    y = iris$Species,
        #                    plot = "box")
        
         
        plot5<- caret::featurePlot(x=data[,covs], 
                                   y=data[[dep]], 
                                   plot="box",
                                   strip=lattice::strip.custom(par.strip.text=list(cex=.7)),
                                   scales = list(x = list(relation="free"), 
                                                 y = list(relation="free")))
    
        
        print(plot5)
        TRUE
      }, 
      
      .plot6 = function(image6,...) {
        
        
        if (is.null(image6$state))
          return(FALSE)
        
        data<- image6$state
        covs <- self$options$covs
        dep <- self$options$dep
        
       
        plot6<- caret::featurePlot(x=data[,covs], 
                                   y=data[[dep]], 
                                   plot="density",
                                   strip=lattice::strip.custom(par.strip.text=list(cex=.7)),
                                   scales = list(x = list(relation="free"), 
                                                 y = list(relation="free")))
        
        
        print(plot6)
        TRUE
      }, 
      
      
      .plot = function(image,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
          state<- image$state
         
           fit <- state[[1]]
           comp <- state[[2]]
           
                  
        res<- MLeval::evalm(list(fit,comp),
                            gnames=c(self$options$method,self$options$cm1))
       
        plot <- res$roc
        
        print(plot)
        TRUE
      },
      
      
      .plot2 = function(image2,ggtheme, theme,...) {
        
        if (is.null(image2$state))
          return(FALSE)
        
        fit <- image2$state
        
        plot2 <- ggplot2::ggplot(fit)
        
        plot2 <- plot2+ggtheme
        
        print(plot2)
        TRUE
        
      },
                
  
      .plot1 = function(image1,...) {
        
        if (is.null(image1$state))
          return(FALSE)
        
        vi <- image1$state
        
        plot1<- plot(vi)
        
        print(plot1)
        TRUE
      },
      
      
      .plot3 = function(image3,...) {
        
        # ROC with test set-----
        
        if (is.null(image3$state))
          return(FALSE)
        
        pred1<- image3$state
        
        res <- MLeval::evalm(pred1)
        
        plot3 <- res$roc
        
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image4,...) {
        
        if (is.null(image4$state))
          return(FALSE)
        
        state<- image4$state
        
        fit <- state[[1]]
        comp <- state[[2]]
        
        
        res<- MLeval::evalm(list(fit,comp),
                            gnames=c(self$options$method,self$options$cm1))
        
        # Calibration curve
        plot4 <- res$cc
        
        print(plot4)
        TRUE
      },
      
      .plot7 = function(image7,...) {
        
        # ROC with test set-----
        
        if (is.null(image7$state))
          return(FALSE)
        
        res<- image7$state
        
        # Box plots to compare models
        scales <- list(x=list(relation="free"), y=list(relation="free"))
        
        plot7<- lattice::bwplot(res, scales=scales)
       
        print(plot7)
        TRUE
      }
      
       )
)
