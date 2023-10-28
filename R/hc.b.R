
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
            <p>____________________________________________________________________________________</p>
            <p>1. Do not move any variable into <b> Labels </b> box to get cluster number.</p>
            <p>2. Cluster numbers will be displayed in the datasheet. </p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
            
            hc <- factoextra::hcut(data, 
                                   k = self$options$k, 
                                   stand= self$options$stand,
                                   hc_metric = self$options$metric,
                                   hc_method = self$options$method
                                  )
            
            
                #### Cluter number for the output variable--------------------
                
                cluster <- hc$cluster
              
                
                #self$results$text$setContent(cluster)
                # self$results$clust$setValues(cluster)
                # self$results$clust$setRowNums(rownames(data))
                
                if (self$options$clust&& self$results$clust$isNotFilled()){
                  
                  
                  self$results$clust$setRowNums(rownames(data))
                  self$results$clust$setValues(cluster)
                  
                }
                
                
                ##### plot-------------------
                
           image <- self$results$plot
            
            # vars <- length(self$options$vars)
            # case <- nrow(data)
            # 
            # height <- 300 + case * 10
            # image$setSize(500, height)
            
            
            image$setState(hc)
            
            }
            
        },
        
        # Hierarchical clustering---------------
        
        .plot = function(image, ggtheme, theme, ...) {
            
          if (is.null(image$state))
            return(FALSE)
            
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
