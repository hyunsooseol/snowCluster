
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom caret preProcess
#' @importFrom caret confusionMatrix
#' @importFrom caret trainControl
#' @importFrom caret varImp
#' @importFrom MLeval evalm
#' @import caret
#' @import xgboost
#' @import rpart.plot
#' @import ranger
#' @import neuralnet
#' @import pls
#' @import party
#' @import elasticnet
#' @import nnet
#' @import gbm
#' @import mda
#' @import earth
#' @import ggplot2
#' @import jmvcore
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
            <p> 2. The rationale of caret R package is described in the <a href='https://topepo.github.io/caret/' target = '_blank'>page</a>.</p>
            <p> 3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
      },
      
      #---------------------------------------------
      
              .run = function() {

          
                if (is.null(self$options$dep) || length(self$options$covs) == 0)
                  return()
                
                dep <- self$options$dep
                covs <- self$options$covs
                
                mecon <- self$options$mecon
                repeats <- self$options$repeats
                number <- self$options$number
                tune <- self$options$tune
                per <- self$options$per
                method <- self$options$method
                cm1 <- self$options$cm1
                
                data <- self$data
                data <- jmvcore::naOmit(data)
                
               
                #Create Train dataset-----------------
                
                set.seed(1234)
                
                split1<- caret::createDataPartition(data[[self$options$dep]], p=per,list = F)
                train <-data[split1,]
                test <- data[-split1,] 
                
                formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
                formula <- as.formula(formula)
                
               
                # trainControl-----------
                
                fitControl <- caret::trainControl(method = mecon, 
                                                  number =number , 
                                                  repeats = repeats,
                                                  p=per,
                                                  classProbs=T,
                                                  savePredictions = T)
               
                # Training dataset---------------
               
                if(self$options$scale=='stand'){

                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      preProcess = c("center", "scale"),
                                      tuneLength = tune,
                                      trControl = fitControl)
                  

                } 
                
                if(self$options$scale=='normal'){

                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      preProcess = "range",
                                      tuneLength = tune,
                                      trControl = fitControl)

                }

                
                if(self$options$scale=='pca'){
                  
                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      preProcess = "pca",
                                      tuneLength = tune,
                                      trControl = fitControl)
                  
                }
                
                comp <- caret::train(formula,
                                     data=train,
                                     method = cm1,
                                     preProcess = c("center", "scale"),
                                     tuneLength = tune,
                                     trControl = fitControl)
                
                # Model information-----------
                
                 self$results$text$setContent(fit)
                
                
                
                # Comparing ROC curves with training set-----------------
                
                 image <- self$results$plot   
                 state <- list(fit,comp)
                 image$setState(state)
                   
              
                 # Calibration curve---------
                 
                 image4 <- self$results$plot4   
                 state <- list(fit,comp)
                 image4$setState(state)
                 
                 
                 # Model selection plot----------
                
                 image2 <- self$results$plot2
                 image2$setState(fit)
                
             
                # Variable importance plot----------
               
                if(self$options$plot1==TRUE){  
                
                vi<- caret::varImp(fit)
                
                image1 <- self$results$plot1
                image1$setState(vi)
                }
                
                
                #########     TRAINING SET    #############################
                
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
                
                pred1<-predict(fit, test, type='prob')
               
                pred1<- data.frame(pred1, test[[dep]], 
                           Group = self$options$method)
                
                image3 <- self$results$plot3
                image3$setState(pred1)
              
                
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
                
                
                },
          
      ##########################################################
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
      }
      
     
       )
)
