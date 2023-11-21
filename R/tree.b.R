
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom caret preProcess
#' @importFrom party ctree
#' @importFrom caret confusionMatrix
#' @import ggplot2
#' @import jmvcore
#' @import rpart
#' @import rpart.plot
#' @export

treeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "treeClass",
    inherit = treeBase,
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
            <p> 1. The values for the target variable cannot be a number. </p> 
            <p> 2. Continuous variables were standardized using <b> caret::prePrecess()</b>. </p>
            <p> 3. Plots are drawn with training data. </p>
            <p> 4. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
         
        
        # Using party package--------
        # data(iris)
        # irisct <- party::ctree(Species ~ .,data = iris)
        # plot(irisct)
        # pred <- predict(irisct)
        # actual <- iris$Species
        # table(predict(irisct), iris$Species)
        # eval<- caret::confusionMatrix(pred,actual)
        # 
         
          
        dep <- self$options$dep
        covs <- self$options$covs
        facs <- self$options$facs
        
        per <- self$options$per
        
        data <- self$data
       
        
        # data cleaning--------------- 
        
        for(fac in facs)
          data[[fac]]<-as.factor(data[[fac]])
        
        for(cov in covs)
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        
        data[[dep]] <- as.factor(data[[dep]])
        
        data <- jmvcore::naOmit(data) 
        
        
        ###########################################################
        
        #set.seed(1234)
        
        
        # To speed up the function------
        
        formula <- as.formula(paste0(self$options$dep, " ~ ."))
        
        
        # Create Train/test data set using caret package-----------------
        
        #set.seed(1234)
        
        split1<- caret::createDataPartition(data[[dep]], p=per,list = F)
        train1 <-data[split1,]
        test1 <- data[-split1,]
        
        # Transformed dataset-----------------
        
        preProcValues <- caret::preProcess(train1, 
                                           method = c("center", "scale"))
        
        
        #--------------------------------------------
        train <- predict(preProcValues, train1)
        test <- predict(preProcValues, test1)
        #-------------------------------------------
       
        # rpart plot------------------
        
        # library(rpart)
        # library(rpart.plot)
        # fit <- rpart(survived~., data = data_train, method = 'class')
        # rpart.plot(fit)
        
        rp <-  rpart::rpart(formula, data=train,
                            method='class')
        
        image1 <- self$results$plot1
        image1$setState(rp)
        
        
        ### Analysis using party package-----------
        
        model.train <- party::ctree(formula, data=train)
        
        # prediction on train Data itself
        
        train.pred <- predict(model.train,train)
        
       
        #----------------------------
        eval1<- caret::confusionMatrix(train.pred, train[[dep]]) 
        
        #---------------------------
        
        tab1<- eval1$table
        
        res2<- as.matrix(tab1)
        
        names<- dimnames(res2)[[1]]
        
        table <- self$results$tab1
        
        for (name in names) {
          
          table$addColumn(name = paste0(name),
                          type = 'Integer',
                          superTitle = 'Predicted')
        }
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(names)){
            
            row[[names[j]]] <- res2[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        # Overall statistics-----------
        
      
        table <- self$results$over1
        
        acc<- eval1[["overall"]][1]
        acclow <- eval1[["overall"]][3]
        acchigh <- eval1[["overall"]][4]
        kappa <- eval1[["overall"]][2]
        
        row <- list()
        
        row[['accu']] <- acc
        row[['lower']] <- acclow
        row[['upper']] <- acchigh
        row[['kappa']] <- kappa
        
        table$setRow(rowNo = 1, values = row)
        
        
        ############## Test set##############################################        
       
        if(self$options$per<1){
         
          if(self$options$per==1) return()
          
          pred<-predict(model.train, test)
        
       #----------------------------
        eval<- caret::confusionMatrix(pred, test[[dep]]) 
        
        #confusion matrix---------------------------
        if(isTRUE(self$options$tab1)){
        
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
        
        }
        # Overall statistics-----------
        
        if(isTRUE(self$options$over)){
        
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
        
        }
      
       
        # Statistics by class----------- 
        if(isTRUE(self$options$cla)){
       
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
        
        }
        
        # Tree plot----------
        
        image <- self$results$plot
        image$setState(model.train)
        
        }
         
      },
      
      
        .plot = function(image,...) {
         
          if (is.null(image$state))
            return(FALSE)
          
         model.train <- image$state
          
          plot <- plot(model.train)
          
          print(plot)
          TRUE
        },
      
      .plot1 = function(image1,...) {
        
        if (is.null(image1$state))
          return(FALSE)
        
      
        rpar <- image1$state
        
        plot1 <- rpart.plot::rpart.plot(rpar)
        
        print(plot)
        TRUE
      }
        
        
        )
)
