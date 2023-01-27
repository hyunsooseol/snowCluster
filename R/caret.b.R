
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom caret preProcess
#' @importFrom caret confusionMatrix
#' @importFrom caret trainControl
#' @importFrom caret varImp
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
                per <- self$options$per
                method <- self$options$method
                
                
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
                                                  repeats = repeats)
              
                
                # Train dataset---------------
               
                if(self$options$scale=='stand'){

                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      preProcess = c("center", "scale"),
                                      tuneLength = 10,
                                      trControl = fitControl)
                  

                } else{

                  fit <- caret::train(formula,
                                      data=train,
                                      method = method,
                                      preProcess = "range",
                                      tuneLength = 10,
                                      trControl = fitControl)

                }

                self$results$text$setContent(fit)
                
                
                ####### Plot##############################
                
                # Model selection plot----------
                
                if(isTRUE(self$options$plot2)){
                
                  image2 <- self$results$plot2
                  
                  image2$setState(fit)
                
                }
                # rpart plot----------
                
                if(self$options$method=='rpart'){
                
                  rp<- fit$finalModel  
                
                image <- self$results$plot
                image$setState(rp)
               
                }
              
                # Variable importance plot----------
                
                if(isTRUE(self$options$plot1)){
                
                vi<- caret::varImp(fit)
                
                image1 <- self$results$plot1
                image1$setState(vi)
                }
                
                
                #######################################
                
                
                
                # Predict-----------------
                
                pred<-predict(fit, test)
                
              #  self$results$text$setContent(pred)
                
                #----------------------------
                eval<- caret::confusionMatrix(pred, test[[dep]]) 
                
                #---------------------------
                
                tab<- eval$table
                
                res1<- as.matrix(tab)
                
                names<- dimnames(res1)[[1]]
                
                table <- self$results$tab
                
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
                
                # Overall statistics-----------
                
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
      .plot2 = function(image2,ggtheme, theme,...) {
        
        if (is.null(image2$state))
          return(FALSE)
        
        fit <- image2$state
        
        plot2 <- ggplot2::ggplot(fit)
        
        print(plot2)
        TRUE
        
      },
                
      .plot = function(image,...) {
                  
          if (is.null(image$state))
              return(FALSE)
      
              rp <- image$state
                  
            plot<- rpart.plot::rpart.plot(rp)
            
              print(plot)
              TRUE
                },
      
                  
      .plot1 = function(image1,...) {
        
        if (is.null(image1$state))
          return(FALSE)
        
        vi <- image1$state
        
        plot1<- plot(vi)
        
        print(plot1)
        TRUE
      }
      
   
   )
)
