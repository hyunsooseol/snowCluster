
# This file is a generated template, your changes will not be overwritten

#' @importFrom factoextra fviz_pca_ind
#' @importFrom FactoMineR PCA
#' @importFrom factoextra fviz_pca_biplot
#' @import ggplot2
#' @export


groupClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "groupClass",
    inherit = groupBase,
    private = list(

      .init = function() {
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        
      },
      
  
      .run = function() {

          
          if (is.null(self$options$facs) || length(self$options$vars) < 2)
            return()
          
         
                
                # read the option values into shorter variable names
                
                vars  <- self$options$vars
                facs <- self$options$facs
              
                # get the data
                
                data <- self$data
                
               
                # convert to appropriate data types
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                
                #  data[[vars]] <- jmvcore::toNumeric(data[[vars]])
                
                for (fac in facs)
                    data[[fac]] <- as.factor(data[[fac]])
                
                # data is now all of the appropriate type we can begin!
                
                data <- na.omit(data)
               
                data <- jmvcore::select(data, self$options$vars)
                 
                # principal component analysis---------
                
                pca <- FactoMineR::PCA(data,  graph = FALSE)
                
                
                # group plot----------
                
                image <- self$results$plot
                image$setState(pca)
                   
              # biplot------------------------
                
                image1 <- self$results$plot1
                image1$setState(pca)
            
            
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
          if (is.null(image$state))
            return(FALSE)
            
            pca <- image$state
            
            plot <- factoextra::fviz_pca_ind(pca,
                                              
                                  label = "none", # hide individual labels
                                  habillage = self$data[[self$options$facs]],  # color by groups for example, iris$Species,
                                  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                  addEllipses = TRUE # Concentration ellipses
            )
            
            plot <- plot+ggtheme
            print(plot)
            TRUE
        },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
            
            
            pca <- image1$state
            
            plot1 <- factoextra::fviz_pca_biplot(pca, 
                                                col.ind =  self$data[[self$options$facs]], palette = "jco", 
                                                addEllipses = TRUE, label = "var",
                                                col.var = "black", repel = TRUE,
                                                legend.title = self$options$facs) 
            
            plot1 <- plot1+ggtheme
            print(plot1)
            TRUE
        }
        
    )
)

            
            
            
