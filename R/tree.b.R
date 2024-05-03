
# This file is a generated template, your changes will not be overwritten
#' @importFrom caret createDataPartition
#' @importFrom jmvcore constructFormula
#' @importFrom party ctree
#' @importFrom caret confusionMatrix
#' @importFrom  rpart rpart
#' @importFrom rpart.plot rpart.plot
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
            <p> 2. Plots are drawn with Entire dataset. </p>
            <p> 3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
        
        
        #Analysis---------------------------------
        #set.seed(1234)
       
        # To speed up the function------
        formula <- as.formula(paste0(self$options$dep, " ~ ."))
        
        # Example(iris data)----------------
        
        # data(iris)
        # model <- party::ctree(Species ~ .,data = iris)
        # plot(model)
 
        # pred <- predict(model)
        # actual <- iris$Species
        
        # table(predict(model), iris$Species)
        # eval<- caret::confusionMatrix(pred,actual)
        
####################################################################        
       # Analysis using party package-----------
        model <- party::ctree(formula, data=data)
        
        #self$results$text$setContent(model)
        
        # Tree plot----------
        image <- self$results$plot
        image$setState(model)
        
        # rpart plot-------------
        # rp <-  rpart::rpart(formula, data=data,
        #                     method='class')
        # 
        # image1 <- self$results$plot1
        # image1$setState(rp)
        

        # predict model----------
         pred <- predict(model)

#Overall data---------------------------------------------
eval<- caret::confusionMatrix(pred, data[[dep]])
#---------------------------------------------

if(isTRUE(self$options$over)){
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
}

if(isTRUE(self$options$tab)){                
        # Confusion matrix-----------------------------
          table <- self$results$tab      
          tab<- eval$table
        
        res2<- as.matrix(tab)
        names<- dimnames(res2)[[1]]
        
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
}

 ############## Test set(Split set<1)########################
 
if(self$options$per<1){

   per <- self$options$per

   # Split set-----------
   split1<- caret::createDataPartition(data[[dep]], p=per,list = F)
   train <-data[split1,]
   test <- data[-split1,]

#Train---------------------------        
   pred<-predict(model, train)
   eval1<- caret::confusionMatrix(pred, train[[dep]])
#---------------------------------------------------   

if(isTRUE(self$options$over1)){      
 # Overall statistics with train data-----------
   
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
}   

if(isTRUE(self$options$tab1)){   
   # Confusion matrix with train data-----------------------------
   table <- self$results$tab1
   
   tab1<- eval1$table
   
   res2<- as.matrix(tab1)
   names<- dimnames(res2)[[1]]
   
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
}   

 #Test-------------------------------------------  
pred2<-predict(model, test) 
eval2<- caret::confusionMatrix(pred2, test[[dep]])
#-------------------------------------------------

if(isTRUE(self$options$over2)){    
# Overall statistics with test set-----------
    table <- self$results$over2

    acc<- eval2[["overall"]][1]
    acclow <- eval2[["overall"]][3]
    acchigh <- eval2[["overall"]][4]
    kappa <- eval2[["overall"]][2]

    row <- list()

    row[['accu']] <- acc
    row[['lower']] <- acclow
    row[['upper']] <- acchigh
    row[['kappa']] <- kappa

    table$setRow(rowNo = 1, values = row)
}

# confusion matrix with test---------------------------
if(isTRUE(self$options$tab2)){
    table <- self$results$tab2
    tab2<- eval2$table
    res1<- as.matrix(tab2)
    names<- dimnames(res1)[[1]]

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

# Statistics by class with test-----------
    
if(isTRUE(self$options$cla)){
     table <- self$results$cla

     cla<- eval2[["byClass"]]
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

  } 
      },
      
      
        .plot = function(image,...) {

          if (is.null(image$state))
            return(FALSE)

         model <- image$state

          plot <- plot(model)

          print(plot)
          TRUE
        }

      # .plot1 = function(image1,...) {
      # 
      #   if (is.null(image1$state))
      #     return(FALSE)
      # 
      # 
      #   rpar <- image1$state
      # 
      #   plot1 <- rpart.plot::rpart.plot(rpar)
      # 
      #   print(plot)
      #   TRUE
      # }

        
        )
)
