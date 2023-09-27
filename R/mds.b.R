
# This file is a generated template, your changes will not be overwritten

#' @importFrom stats dist
#' @importFrom stats cmdscale
#' @import magrittr
#' @import ggpubr 
#' @import ggplot2 
#' @import scatterplot3d scatterplot3d
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
                
                x <- self$options$xlab
                y <- self$options$ylab
                z <- self$options$zlab
                
                data <- self$data
                
                data <- jmvcore::naOmit(data)
                
                
                # Handling id----------
                
                if ( ! is.null(self$options$labels)) {
                    rownames(data) <- data[[self$options$labels]]
                    data[[self$options$labels]] <- NULL
                }
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                
    if(self$options$mode == "simple"){
                
                
                # MDS analysis---------
                
                d <- stats:: dist(data)
                mds<- stats::cmdscale(d)
                #---------------------------
                
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
                
                #  kmeans plot----------
                
                image1 <- self$results$plot1
                image1$setState(state)
               
    } else{
                  # # Perform MDS analysis
                  # mds_iris <- stats::cmdscale(dist(iris[,1:4]),
                  #                           k = 3)
                  # 
                  # # Plot the results
                  # library(scatterplot3d)
                  # 
                  # colors <- c("red", "blue", "pink")
                  # colors <- colors[as.numeric(iris$Species)]
                  # scatterplot3d(mds_iris[,1:3], pch = 16,
                  #               xlab = "Sepal Length",
                  #               ylab = "Sepal Width",
                  #               zlab = "Petal Length",
                  #               color=colors)
                  
                  d <- stats:: dist(data)
                  three<- stats::cmdscale(dist(d), k=3)
                    
                  image <- self$results$plot2
                  image$setState(three)               
                  
                }
                
                 
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
        },
        
        .plot2 = function(image,...) {
          
          if (is.null(image$state))
            return(FALSE)
         
          three <- image$state
        
          x<-self$options$xlab
          y <- self$options$ylab
          z <- self$options$zlab
         
          plot2<- scatterplot3d::scatterplot3d(three,
                                               xlab =x,
                                               ylab=y,
                                               zlab=z,
                                              highlight.3d=TRUE,
                                               grid=TRUE,
                                               pch = 19
                                              )
           
          
          print(plot2)
          TRUE
          
        }
                                     
        )
)
