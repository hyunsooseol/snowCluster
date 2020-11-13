
# This file is a generated template, your changes will not be overwritten

#' @importFrom factoextra fviz_pca_ind
#' @importFrom FactoMineR PCA
#' @import ggplot2
#' @export


groupClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "groupClass",
    inherit = groupBase,
    private = list(
        .run = function() {

            if (!is.null(self$options$vars)) {
                
                
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
                
                
                # Variable contributions plot----------
                
                image <- self$results$plot
                image$setState(pca)
                   
            
            }
            
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$facs))
                return()
            
            
            pca <- image$state
            
            plot <- factoextra::fviz_pca_ind(pca,
                                              
                                  label = "none", # hide individual labels
                                  habillage = self$data[[self$options$facs]],  # color by groups for example, iris$Species,
                                  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                  addEllipses = TRUE # Concentration ellipses
            )
            
            print(plot)
            TRUE
        }
        
    )
)

            
            
            
