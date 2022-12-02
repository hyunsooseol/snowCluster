
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom rpart rpart
#' @importFrom rpart.plot rpart.plot
#' @importFrom rpart rpart.control
#' @importFrom caret confusionMatrix
#' @import ggplot2
#' @import jmvcore
#' @export

treeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(
 
      
      
      .run = function() {

          if (is.null(self$options$dep) || length(self$options$covs) == 0)
            return()
          
          # Example---------
          
          # data(iris)
          # 
          # split1<- caret::createDataPartition(iris$Species, p=0.7,list = F)
          # 
          # split1_train <-iris[split1,]
          # split1_test <- iris[-split1,]
          # 
          # model <- rpart::rpart(Species~., data=split1_train,
          #                       control = rpart.control(minsplit=2))
          # 
          # rpart.plot::rpart.plot(model, tweak = 1.1)
          
          
        dep <- self$options$dep
        covs <- self$options$covs
        tw <- self$options$tw
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        set.seed(1234)
        
        split1<- caret::createDataPartition(data[[self$options$dep]], p=0.7,list = F)
        
        train <-data[split1,]
        test <- data[-split1,] 
          
        formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
        formula <- as.formula(formula)
        
        ### Analysis-----------
        
        model.train <- rpart::rpart(formula, data=train,
                              control = rpart::rpart.control(minsplit=2))
        
        #self$results$text$setContent(model.train)
        
        
        # Tree plot----------
        
        image <- self$results$plot
        image$setState(model.train)
        
        # Prediction and Confusion Matrix-----
        
        pred<- stats::predict(model.train,test,type='class')
        
        actual <- test[[dep]]
        
        #######################
        eval<- caret::confusionMatrix(actual, pred, mod='everything')
        ######################
        
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
        
        
        
      },
      
      
        .plot = function(image,...) {
         
          tw <- self$options$tw
          
          model.train <- image$state
          
          plot <- rpart.plot::rpart.plot(model.train, tweak = tw)
          
          print(plot)
          TRUE
        }
        
        
        )
)
