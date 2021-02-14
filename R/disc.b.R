
# This file is a generated template, your changes will not be overwritten

#' @importFrom MASS lda
#' @importFrom jmvcore constructFormula
#' @import MASS
#' @import ggplot2
#' @import jmvcore
#' @export


discClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "discClass",
    inherit = discBase,
    private = list(
    
        
        #------------------------------------
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$covs)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            
            <p> The rationale of Multiple Factor Analysis is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
        },
        
        #---------------------------------------------
        
        .run = function() {
            
            dep <- self$options$dep
            
            covs <- self$options$covs
            
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
        
            # dividing two datasets------------------------
            
            training_sample <- sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.6,0.4))
            
            
            train <- data[training_sample, ]
            
            test <- data[!training_sample, ]
            
            
            # Prior probabilities of groups--------------
            #     setosa versicolor  virginica 
            # 0.3536585  0.3414634  0.3048780 
            
            
            formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
            formula <- as.formula(formula)
            
            lda.train <- MASS::lda(formula, data=train)
            
        
            prior <- lda.train$prior
            
            self$results$text$setContent(prior)
            
            
        })
)
