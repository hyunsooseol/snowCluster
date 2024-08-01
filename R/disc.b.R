
# This file is a generated template, your changes will not be overwritten

#' @importFrom MASS lda
#' @importFrom jmvcore constructFormula
#' @importFrom MASS lda
#' @importFrom caret createDataPartition
#' @importFrom stringr str_interp
#' @import dplyr
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
            <p>____________________________________________________________________________________</p>
            <p> 1. If you set <b>Split set</b> to less than 1, uncheck the LD plot. Otherwise, you will get an error.</p>
            <p> 2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
          
            if(isTRUE(self$options$plot)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot1)){
              width <- self$options$width1
              height <- self$options$height1
              self$results$plot1$setSize(width, height)
            }  

        },
        
        #---------------------------------------------
        
        .run = function() {
            
            if (is.null(self$options$dep) || length(self$options$covs) < 2)
                return()

            dep <- self$options$dep
            covs <- self$options$covs
            per <- self$options$per
            
            data <- self$data
            data <- jmvcore::naOmit(data)
            
            for(cov in covs)
              data[[cov]] <- jmvcore::toNumeric(data[[cov]])
            
            data[[dep]] <- as.factor(data[[dep]])
            
            # dividing two datasets------------------------
            
            set.seed(1234) # Set seed for reproducibility
            
            # training_sample <- sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.7,0.3))
            # train <- data[training_sample, ]
            # test <- data[!training_sample, ]
            
            split1<- caret::createDataPartition(data[[self$options$dep]], p=per,list = F)
            train <-data[split1,]
            test <- data[-split1,] 

            formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
            formula <- as.formula(formula)
            
            ####LDA ANALYSIS##############################################
            
            lda.train <- MASS::lda(formula, data=train)
            
           ###################################################
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
           
            if(isTRUE(self$options$tra)){
            
            
            pred = predict(lda.train)
            
            res<- table(train[[dep]],pred$class)
            res<- as.matrix(res)
            
            #self$results$text$setContent(res)
            
            
            names<- dimnames(res)[[1]]
            table <- self$results$tra
            
            for (name in names) {

                table$addColumn(name = paste0(name),
                                type = 'Integer',
                                superTitle = 'Predicted')
                                   }

            for (name in names) {

                 row <- list()

                for(j in seq_along(names)){
                
                    row[[names[j]]] <- res[name,j]
                
                                    }

                table$addRow(rowKey=name, values=row)

            }
            }

          # # Accuracy with test data-----------

            # lda.test <- predict(lda.iris,test)
            # test$lda <- lda.test$class
            # table(test$lda,test$Species)
            # 
   
        if(isTRUE(self$options$tes)){

        
            lda.test = predict(lda.train,test)
            test$lda <- lda.test$class
            
            res1<- table(test$lda,test[[dep]])
            res1<- as.matrix(res1)
            
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
}


## Proportion of trace------------------
            
            if(isTRUE(self$options$prop)){
              

              if(length(levels(data[[dep]]))<=2){

                err_string <- stringr::str_interp(
                  "Dependent levels should be at least 3."
                )
                stop(err_string)

              }

              if(length(levels(data[[dep]]))>2){
                
                # proportion of trace----------
                
                prop.lda = lda.train$svd^2/sum(lda.train$svd^2)
                
                ###################
                
                table <- self$results$prop
                
                ld1 <- prop.lda[[1]]
                ld2 <- prop.lda[[2]]
                
                row <- list()
                
                row[['LD1']] <- ld1
                row[['LD2']] <- ld2
                
                table$setRow(rowNo = 1, values = row)
            }   
            }    
          
#LD plot---
            
    if(isTRUE(self$options$plot)){
            
              if(length(levels(data[[dep]]))<=2){
                
                err_string <- stringr::str_interp(
                  "Dependent levels should be at least 3."
                )
                stop(err_string)
                
              } 
              
              if(length(levels(data[[dep]]))>2){
 
                df <- cbind(train, predict(lda.train)$x)
                Groups <- data[[dep]]
                df<- cbind(df, Groups)
                df$Groups <- as.factor(df$Groups)
                
                df <- dplyr::select(df, LD1,LD2, Groups)
                df <- data.frame(df)
                
                # Calculate group centroids
                
                lda_scores_grouped <- dplyr::group_by(df, Groups)
                centroids_summarized <- dplyr::summarize(lda_scores_grouped,
                                                         LD1 = mean(LD1),
                                                         LD2 = mean(LD2))
                
                cent <- data.frame(centroids_summarized)
                #self$results$text$setContent(cent) 
                
                state <- list(df, cent)
                image <- self$results$plot
                image$setState(state)  
                
                # group centroids table---
                
                if(isTRUE(self$options$gc)){
                  
                  table <- self$results$gc                 
                  cent <- as.data.frame(cent)
                  names<- dimnames(cent)[[1]]
                  
                  for (name in names) {
                    
                    row <- list()
                    
                    row[["name"]] <- as.character(cent[name, 1])
                    row[["ld1"]] <-  cent[name, 2]
                    row[["ld2"]] <-  cent[name, 3]
                    
                    
                    table$addRow(rowKey=name, values=row)
                    
                  }
                  
                  }
                }
            
            }
            
            # Histogram---------------------
            if(length(self$options$covs) >2){
              
            image1 <- self$results$plot1
            image1$setState(lda.train)
            }
     
            },
            
  .plot = function(image,ggtheme, theme,...) {
              if (is.null(image$state))
                return(FALSE)
              
              df <- image$state[[1]]
              cent <- image$state[[2]]

          plot<- ggplot(df, ggplot2::aes(x = LD1, y = LD2, color = Groups)) +
                geom_point(alpha = 0.6) +
                geom_point(data = cent, ggplot2::aes(x = LD1, 
                                                     y = LD2, 
                                                     color = Groups),
                           size = 7, 
                           shape = 17,
                           alpha=1) +
                labs(title = "",
                     x = "LD1",
                     y = "LD2")      
              
              plot <- plot+ggtheme
              print(plot)
              TRUE
            },

        .plot1 = function(image1,...) {
            
        
           if (is.null(image1$state))
             return(FALSE)
          
            
            lda.train <- image1$state
            
            plot1 <- plot(lda.train, dimen=1, type="both")
            
            print(plot1)
            TRUE
        }
 
         ))

