
# This file is a generated template, your changes will not be overwritten
#' @importFrom factoextra hcut
#' @importFrom factoextra fviz_dend
#' @import ggplot2
#' @export


hcClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "hcClass",
    inherit = hcBase,
    private = list(
 
        #------------------------------------
        
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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>1. Do not move any variable into <b> Labels </b> box to get cluster number.</p>
            <p>2. The result of <b> Hierarchical cluster number </b> will be displayed in the datasheet.</p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },
        
        #---------------------------------------------
        
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
            
            
                #### Cluter number for the output variable--------------------
                
                cluster <- hc$cluster
              
                self$results$clust$setRowNums(rownames(data))
                
                self$results$clust$setValues(cluster)
                
                
                
                ##### plot-------------------
                
           image <- self$results$plot
            
            vars <- length(self$options$vars)
            case <- nrow(data)

            height <- 300 + case * 10

            image$setSize(500, height)
            
            
            image$setState(hc)
            
            }
            
        },
        
        # Hierarchical clustering---------------
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) < 2)
                return()
            
            type <- self$options$type
            
            hc <- image$state
            
            
            plot <- factoextra:: fviz_dend(hc, 
                                           # rect = TRUE,
                                           repel = TRUE,
                                           lwd = 1,
                                           type = type,
                                           horiz= TRUE,
                                           cex=0.9,
                                           color_labels_by_k = TRUE)
            
            plot <- plot+ggtheme
            
            print(plot)
            TRUE
            
        }
        
        
        )
)
