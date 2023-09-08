
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
        
        #------------------------------------
        
        .init = function() {
            if (is.null(self$options$vars) | is.null(self$options$labels)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <h2><b>Instructions</b></h2>
            <p>____________________________________________________________________________________</p>
            <p>1. The rationale of Classical Multidimensional Scaling is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/' target = '_blank'>page.</a></p>
            <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
                
                k <- self$options$k
                
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
                
                
                # kmeans clustering--------
                
                # clust <- kmeans(mds, 3)$cluster %>%
                #     as.factor()
                # mds <- mds %>%
                #     mutate(groups = clust)
                
                clust <- stats::kmeans(mds, k)$cluster
                clust <- as.factor(clust)
                
                mds1 <- mutate(mds,Groups=clust)
                
                name1 <- rownames(data)
                
                state <- list(mds1, name1)
                
                #  MDS plot----------
                
                image1 <- self$results$plot1
                image1$setState(state)
                
                
            }
        },
        
        .plot = function(image,ggtheme, theme, ...) {
            
          if (is.null(image$state))
            return(FALSE)
            
            mds <- image$state[[1]]
            name <- image$state[[2]]
            
            plot <- ggpubr::ggscatter(mds,x = "Dim.1", y = "Dim.2", 
                                      label = name,
                                      size = 1,
                                      repel = TRUE)
            
            plot <- plot+ggtheme
            print(plot)
            TRUE
        },
        
        .plot1 = function(image1,ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
            
            mds1 <- image1$state[[1]]
            name1 <- image1$state[[2]]
            
            plot1 <- ggpubr::ggscatter(mds1,
                                      x = "Dim.1", y = "Dim.2", 
                                      label = name1,
                                      color = "Groups",
                                      palette = "jco",
                                      size = 1, 
                                      ellipse = TRUE,
                                      ellipse.type = "convex",
                                      repel = TRUE)
        
            plot1 <- plot1+ggtheme
            print(plot1)
            TRUE
        }                         
                                      
        )
)
