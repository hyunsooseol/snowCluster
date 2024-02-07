
# This file is a generated template, your changes will not be overwritten
#' @import jmvcore
#' @import ggplot2
#' @importFrom multipleROC multipleROC
#' @importFrom multipleROC plot_ROC
#' @export

rocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rocClass",
    inherit = rocBase,
    private = list(
 
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
            <p> Perform ROC curve based on <a href='https://github.com/cardiomoon/multipleROC' target = '_blank'>multipleROC<a> R package.</p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if(isTRUE(self$options$plot1)){
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot2)){
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }  
  
        if(isTRUE(self$options$plot3)){
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }  
        
        
        },
  
  
   .run = function() {

     if (is.null(self$data) | is.null(self$options$dep) | is.null(self$options$covs))
       return()    
    
     # Example--------
    # multipleROC::multipleROC(am~wt,data=mtcars)
    
    dep <- self$options$dep
    covs <- self$options$covs
    data <- self$data
    data <- na.omit(data)
    data <- as.data.frame(data)  
          
    #Formula(male~height+weight)------
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    formula <- as.formula(paste(paste(dep, paste0(covs, collapse ="+"), sep="~")))
           
    if(isTRUE(self$options$plot1)){
   
    image <- self$results$plot1
    image$setState(formula)
    }   
   
    if(isTRUE(self$options$plot2)){
          
            #MUltiple ROC curves----------
           
           # a=multipleROC::multipleROC(formula1,data=data,plot=FALSE)
           # b=multipleROC::multipleROC(formula1,data=data,plot=FALSE)
          # c=multipleROC(form=male~weight,data=radial,plot=FALSE)
          # plot_ROC(list(a,b,c),show.eta=FALSE,show.sens=FALSE)
         
    roc <- list()

    # Loop through each element in covs
     for (i in seq_along(covs)) {
        # Compute ROC curve for the current covariate and store it in the list
         roc[[i]] <- multipleROC::multipleROC(as.formula(paste(paste(dep, paste0(covs[[i]]), sep="~"))), 
                                              data = data, plot = FALSE)
       }
          
       #self$results$text$setContent(roc)
         
          image <- self$results$plot2
          image$setState(roc)
          }
    
    if(isTRUE(self$options$plot3)){
     
      roc <- list()
      
      # Loop through each element in covs
      for (i in seq_along(covs)) {
        # Compute ROC curve for the current covariate and store it in the list
        roc[[i]] <- multipleROC::multipleROC(as.formula(paste(paste(dep, paste0(covs[[i]]), sep="~"))), 
                                             data = data, plot = FALSE)
      }
     
      image <- self$results$plot3
      image$setState(roc)
    }
    
          },
        
  .plot1 = function(image, ...){
          
            if (is.null(image$state))
              return(FALSE)
          
            formula <- image$state
            
            data <- self$data
            data <- na.omit(data)
            data <- as.data.frame(data)  
            
            plot1 <-  multipleROC::multipleROC(formula, data=data)
          
            print(plot1)
            TRUE
              
          },
          
  .plot2 = function(image,ggtheme,theme, ...){
    
    if (is.null(image$state))
      return(FALSE)
    
    roc <- image$state
   
     plot2 <-multipleROC::plot_ROC(roc, show.eta = FALSE, 
                                    show.sens = FALSE)
   
     plot2 <- plot2+ggtheme
       
    print(plot2)
    TRUE
    
  },
  
  .plot3 = function(image,ggtheme, theme, ...){
    
    if (is.null(image$state))
      return(FALSE)
    
    roc <- image$state
    
    plot3 <-multipleROC::plot_ROC(roc, facet=TRUE) 
                                   
    
    plot3 <- plot3+ggtheme
    
    print(plot3)
    TRUE
    
  }
  
  
     )
)
