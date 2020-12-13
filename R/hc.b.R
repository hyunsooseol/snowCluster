
# This file is a generated template, your changes will not be overwritten
#' @importFrom factoextra hcut
#' @importFrom factoextra fviz_dend
#' @import ggplot2
#' @export


hcClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "hcClass",
    inherit = hcBase,
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
                
            
            ### Hierarchical Clustering--------- 
            
            # Compute hierarchical clustering and cut into 4 clusters
            
            hc <- factoextra::hcut(data, k = self$options$k, stand = TRUE)
            
            image <- self$results$plot
            
            # vars <- length(self$options$vars)
            case <- nrow(data)
            
            width <- 200 + case * 10
            
            image$setSize(width, 500)
            
            
            image$setState(hc)
            
            }
            
        },
        
        # Hierarchical clustering---------------
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) < 2)
                return()
            
            
            hc <- image$state
            
            
            plot <- factoextra:: fviz_dend(hc, rect = TRUE, cex = 0.8,
                                           color_labels_by_k = TRUE)
            
            print(plot)
            TRUE
            
        }
        
        
        )
)
