
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
            if (is.null(self$options$dep) | is.null(self$options$covs)) {
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
            
            # for (i in seq_along(covs))
            #     data[[i]] <- jmvcore::toNumeric(data[[i]])
            # 
            
        
            # dividing two datasets------------------------
            
            training_sample <- sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.6,0.4))
            
            
            train <- data[training_sample, ]
            
            test <- data[!training_sample, ]
            
            
            formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
            formula <- as.formula(formula)
            
            # lda analysis-------
            
            lda.train <- MASS::lda(formula, data=train)
            
            # creating table-----
            
            value<- lda.train$prior
            
            prior<- as.data.frame(value)
            
            names<- dimnames(prior)[[1]]
           
            # Prior probabilities of groups table----
            
            table <- self$results$prior
            
            for (name in names) {
                
                row <- list()
                
                row[['value']] <- prior[name,1]
                
                table$addRow(rowKey=name, values=row)
              
                            }
            
            # Group means---------------
            
            gm <- lda.train$means
            
            covs <- self$options$covs 
              
            ncovs <- length(covs)
            
            names<- dimnames(gm)[[1]]
              
            table <- self$results$gm
            
            for (i in seq_along(covs)) {
              
                  cov <- covs[[i]]

                table$addColumn(name = paste0(cov),
                               type = 'number',
                               format = 'zto')
                             
                }
            
            for (name in names) {
                
                row <- list()
                
                
                for(j in seq_along(covs)){

              row[[paste0("cov", j)]] <- gm[name ,j ]

                }
                
                table$addRow(rowKey=name, values=row)
             
                  
            }
           
            
            
            
            
        })
)
