
# This file is a generated template, your changes will not be overwritten

#' @importFrom stats dist
#' @importFrom stats cmdscale
#' @import magrittr
#' @import ggpubr 
#' @import ggplot2 
#' @export


mdsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mdsClass",
    inherit = mdsBase,
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
                
                
                # MDS analysis---------
                
                
                d <- stats:: dist(data)
                mds<- stats::cmdscale(d)
                colnames(mds) <- c("Dim.1", "Dim.2")
                mds<- as.data.frame(mds)
                name <- rownames(mds)
                
                state <- list(mds, name)
                
                #  MDS plot----------
                
                image <- self$results$plot
                image$setState(state)
                
            }
        },
        
        .plot = function(image,ggtheme, theme, ...) {
            
            if (length(self$options$vars) < 2)
                return()
            
            mds <- image$state[[1]]
            name <- image$state[[2]]
            
            plot <- ggpubr::ggscatter(mds,x = "Dim.1", y = "Dim.2", 
                                      label = name,
                                      size = 1,
                                      repel = TRUE)
            
            plot <- plot+ggtheme
            print(plot)
            TRUE
        }
        
        )
)
