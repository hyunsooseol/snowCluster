
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR CA
#' @importFrom factoextra fviz_ca_row
#' @importFrom factoextra fviz_ca_col
#' @importFrom factoextra fviz_ca_biplot
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
                
                
                # Handling id----------
                
                if ( ! is.null(self$options$labels)) {
                    rownames(data) <- data[[self$options$labels]]
                    data[[self$options$labels]] <- NULL
                }
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                # Correspondence analysis---------
                
                res.ca <- FactoMineR::CA(data, graph = FALSE)
                
                
                #  Raw points plot----------
                
                image1 <- self$results$plot1
                image1$setState(res.ca)
            
                # Column points plot-------


                image2 <- self$results$plot2
                image2$setState(res.ca)


                # Biplot--------

                image3 <- self$results$plot3
                image3$setState(res.ca)

                
                }
            },
       
        .plot1 = function(image1, ggtheme, theme, ...) {
            
              if (length(self$options$vars) <= 2)
                 
                #  return()
                  
           stop("Please use  greater than two variables.")
            
            res.ca <- image1$state
            
            plot1 <- factoextra::fviz_ca_row(res.ca, col.row = "cos2",
                                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                             repel = TRUE)
            
            print(plot1)
            TRUE
        },
        
        .plot2 = function(image2, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res.ca <- image2$state
            
            plot2 <- factoextra::fviz_ca_col(res.ca, col.col = "cos2", 
                                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                             repel = TRUE)
            
            print(plot2)
            TRUE
        },
        
        .plot3 = function(image3, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res.ca <- image3$state
            
            plot3 <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE)
            
            print(plot3)
            TRUE
        }
        
       
    ))
            
            
