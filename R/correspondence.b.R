
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR CA
#' @importFrom factoextra fviz_ca_row
#' @importFrom factoextra fviz_pca_ind
#' @importFrom factoextra fviz_pca_biplot
#' @import ggplot2
#' @export


correspondenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correspondenceClass",
    inherit = correspondenceBase,
    private = list(
        .run = function() {

            if (!is.null(self$options$vars)) {
                
                vars <- self$options$vars
                
                data <- self$data
                
                data <- jmvcore::naOmit(data)
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])

                
                # Handling id----------
                
                if ( ! is.null(self$options$labels)) {
                    rownames(data) <- data[[self$options$labels]]
                    data[[self$options$labels]] <- NULL
                }
                
                
                # Correspondence analysis---------
                
                ca <- FactoMineR::CA(data, graph = FALSE)
                
                
                #  Raw points plot----------
                
                image1 <- self$results$plot1
                image1$setState(ca)
            
                # Column points plot-------


                image2 <- self$results$plot2
                image2$setState(ca)


                # Biplot--------

                image3 <- self$results$plot3
                image3$setState(ca)

                
                }
            },
       
        .plot1 = function(image1, ggtheme, theme, ...) {
            
            if (length(self$options$vars) < 2)
                return()
            
            ca <- image1$state
            
            plot1 <- factoextra::fviz_ca_row(ca, repel = TRUE)
            
            print(plot1)
            TRUE
        }
        
    ))
            
            
