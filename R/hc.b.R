
# This file is a generated template, your changes will not be overwritten
#' @importFrom factoextra hcut
#' @importFrom factoextra fviz_dend
#' @importFrom stringr str_interp
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
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
             
                #  Assuming your data frame is named 'data'
                # row.names(data) <- data$City
                # data <- data[, -1]
                
                
                if ( ! is.null(self$options$labels)) {
                    
                    rownames(data) <- data[[self$options$labels]]
                    data[[self$options$labels]] <- NULL
                
                    }
              
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])


            ### Hierarchical Clustering--------- 
            hc <- try(factoextra::hcut(data, 
                                   k = self$options$k, 
                                   stand= self$options$stand,
                                   hc_metric = self$options$metric,
                                   hc_method = self$options$method
                                  ))
            
            
                #### Cluster number for the output variable--------------------
              
                 # if(jmvcore::isError(hc)){
                 #    err_string <- stringr::str_interp(
                 #      "Please remove the variable from the Label box to get cluster numbers in datasheet."
                 #    )
                 #    stop(err_string)
                 #    
                 #  } 
                 #  
                 #  if (! jmvcore::isError(hc) ){
                  
                
                if ( ! is.null(self$options$labels)) {
                
                      cluster <- as.data.frame(hc$cluster)
                     #self$results$text$setContent(cluster)
                    
                      for (i in 1:length(self$options$labels)) {
                        scores <- as.numeric(cluster[, i])
                        self$results$clust$setValues(index=i, scores)
                      }
                   } else{
                  
                  cluster <- hc$cluster
                  
                   self$results$clust$setValues(cluster)
                   self$results$clust$setRowNums(rownames(data))
                }
                
            
            ##### plot-------------------
                
           image <- self$results$plot
           image$setState(hc) 
           
            }
         
           # Clustering dendrogram with p-values-------------
           # https://github.com/shimo-lab/pvclust
           
           # Example           
           # library(pvclust)
           # data(lung)
           # res <- pvclust::pvclust(lung, 
           #                         method.dist="cor", 
           #                         method.hclust="average", 
           #                         nboot=1000, 
           #                         parallel=TRUE)
           
           
           
           
              
        },
        
        # Hierarchical clustering plot---------------
        
        .plot = function(image, ggtheme, theme, ...) {
            
          if (is.null(image$state))
            return(FALSE)
            
            type <- self$options$type
            hc <- image$state
            
            if(self$options$horiz == TRUE){
            plot <- factoextra:: fviz_dend(hc, 
                                           rect = TRUE,
                                           repel = TRUE,
                                           lwd = 1,
                                           type = type,
                                           horiz= TRUE,
                                           cex=0.9,
                                           color_labels_by_k = TRUE)
            
            } else{
              
              plot <- factoextra:: fviz_dend(hc, 
                                             rect = TRUE,
                                             repel = TRUE,
                                             lwd = 1,
                                             type = type,
                                             cex=0.9,
                                             color_labels_by_k = TRUE)
              
            }
            
            plot <- plot+ggtheme
            
            print(plot)
            TRUE
            
        }
        
        )
)
