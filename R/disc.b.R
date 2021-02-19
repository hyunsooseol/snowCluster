
# This file is a generated template, your changes will not be overwritten

#' @importFrom MASS lda
#' @importFrom jmvcore constructFormula
#' @importFrom MASS lda
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
            
            <p> The rationale of Discriminant Analysis is described in the <a href='https://rpubs.com/Nolan/298913#:~:text=Discriminant%20Analysis%20(DA)%20is%20a,measurable%20features%20of%20those%20objects.&text=Quadratic%20Discriminant%20Analysis%20(QDA)%20which,equal%20covariance%20across%20the%20classes.' target = '_blank'>page.</a></p>
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
                    
                    cov <- covs[[j]]
                    
                    row[[cov]] <- gm[name, j]

                }
                
                table$addRow(rowKey=name, values=row)
             
                  
            }
           
           # Coefficients of linear discriminants-------
            
            coef<- lda.train$scaling
            
            coef <- as.data.frame(coef)
            
            names <-  dimnames(coef)[[1]]
            dims <- dimnames(coef)[[2]]
            
            table <- self$results$coef
            
            
            for (dim in dims) {
                
                table$addColumn(name = paste0(dim),
                                type = 'number')
            }
            
            
            for (name in names) {
                
                row <- list()
                
                for(j in seq_along(dims)){
                    
                    row[[dims[j]]] <- coef[name,j]
                    
                }
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            
            # Accuracy with training data-----------
            
            # lda.train <- predict(lda.iris)
            # 
            # train$lda <- lda.train$class
            # table(train$lda,train$Species)
            
            
             dep <- self$options$dep
            
            pred = predict(lda.train)
            
            res<- table(train[[dep]],pred$class)
            
            res<- as.matrix(res)
           
            names<- dimnames(res)[[1]]
            
            table <- self$results$tra
            
            for (name in names) {

                table$addColumn(name = paste0(name),
                                type = 'Integer')
                                   }

            for (name in names) {

                 row <- list()

                for(j in seq_along(names)){
                
                    row[[names[j]]] <- res[name,j]
                
                                    }

                table$addRow(rowKey=name, values=row)

            }

          # # Accuracy with test data-----------

            # lda.test <- predict(lda.iris,test)
            # test$lda <- lda.test$class
            # table(test$lda,test$Species)
            # 
            
            dep <- self$options$dep
            
            lda.test = predict(lda.train,test)
            
            test$lda <- lda.test$class
            
            res1<- table(test$lda,test[[dep]])
            
            res1<- as.matrix(res1)
            
            names<- dimnames(res1)[[1]]
            
            table <- self$results$tes
            
            for (name in names) {
                
                table$addColumn(name = paste0(name),
                                type = 'Integer')
            }
            
            for (name in names) {
                
                row <- list()
                
                for(j in seq_along(names)){
                    
                    row[[names[j]]] <- res1[name,j]
                    
                }
                
                table$addRow(rowKey=name, values=row)
                
            }
        
            #  Group plot----------
            
            state <- list(lda.train, train)
           
            image <- self$results$plot
            
            image$setState(state)  
            #---------------------
           
            image1 <- self$results$plot1
            image1$setState(lda.train)
            
           
            },
            
            .plot = function(image,...) {
                
                plot <- self$options$plot
               
                if (!plot)
                    return()
                
                dep<- self$options$dep
                
                lda.train <- image$state[[1]]
                train <- image$state[[2]]
                
                plot <- plot(lda.train, col = as.integer(train[[dep]]))
              
                print(plot)
                TRUE
            },
        
        .plot1 = function(image1,...) {
            
            plot1 <- self$options$plot1
            
            if (!plot1)
                return()
            
            lda.train <- image1$state
            
            plot1 <- plot(lda.train, dimen=1, type="b")
            
            print(plot1)
            TRUE
        }
        
       
         ))

