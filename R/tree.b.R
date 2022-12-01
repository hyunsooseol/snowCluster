
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom rpart rpart
#' @importFrom rpart.plot rpart.plot
#' @importFrom rpart rpart.control
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
