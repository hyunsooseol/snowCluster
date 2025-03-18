
# This file is a generated template, your changes will not be overwritten


#' @export


mdsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mdsClass",
    inherit = mdsBase,
    private = list(
      .htmlwidget = NULL,
        #------------------------------------
        
        .init = function() {
            
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$options$vars) | is.null(self$options$labels)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # <p>____________________________________________________________________________________</p>
            # <p>1. The rationale of Classical Multidimensional Scaling is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/' target = '_blank'>page.</a></p>
            # <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            # <p>____________________________________________________________________________________</p>
            # 
            # </div>
            # </body>
            # </html>"
            # )
       
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The rationale of Classical Multidimensional Scaling is described in the <a href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/" target = "_blank">page</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )          
          
          
            if(self$options$mode == "simple"){  
            if(isTRUE(self$options$plot)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
            }
            
              if(isTRUE(self$options$plot1)){
                width <- self$options$width1
                height <- self$options$height1
                self$results$plot1$setSize(width, height)
              }
              
            }
             
            if(self$options$mode == "complex"){  
              if(isTRUE(self$options$plot2)){
                width <- self$options$width2
                height <- self$options$height2
                self$results$plot2$setSize(width, height)
              }    
            }
        },
        
        #---------------------------------------------
        
        .run = function() {

          if (is.null(self$options$vars)) return()
          
          #res <- private$.dataClear()         

          vars <- self$options$vars
          labels <- self$options$labels
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
 
          # if(length(labels) > 0) {
          #   ime <- as.character(self$data[,which(names(self$data) == labels)])
          #   pod0 <- self$data[,which(names(self$data) != labels)]
          #   
          #   data <- na.omit(pod0)
          #   rownames(data) <- ime  
          # }
          # 
          #   self$results$text$setContent(data)
          
         
          # MDS analysis---------
          d <- stats:: dist(data)
          mds<- stats::cmdscale(d)
          # kmeans clustering--------
          # clust <- kmeans(mds, 3)$cluster %>%
          #     as.factor()
          # mds <- mds %>%
          #     mutate(groups = clust)
          
          model <- stats::kmeans(mds, k)
          mc <- model$cluster
          
    if(self$options$mode == "simple"){
                
         if(self$options$plot){
            
                colnames(mds) <- c("Dim.1", "Dim.2")
                mds<- as.data.frame(mds)
                name <- rownames(mds)
                state <- list(mds, name)
                #  MDS plot----------
                image <- self$results$plot
                image$setState(state)
          }
                
         if(self$options$plot1){    
             
                clust <- as.factor(mc)
                mds1 <- mutate(mds, Clusters=clust)
                name1 <- rownames(data)
                state <- list(mds1, name1)
                #  kmeans plot----------
                image1 <- self$results$plot1
                image1$setState(state)
               
     } 
    
         if(isTRUE(self$options$clust)){
  
           self$results$text$setContent(mc)
            
            clust1 <- as.data.frame(mc)
            clust2 <- clust1$mc
            
            self$results$clust$setValues(clust2)
            self$results$clust$setRowNums(data$clust2)         
}      
      
    }
          
    if(self$options$mode == "complex"){            
       
      
      if (is.null(self$options$xlab)) return()         
                
      d <- stats:: dist(data)
      three<- stats::cmdscale(dist(d), k=3)
                
        image2 <- self$results$plot2
        image2$setState(three)               
              
             
                }
          
        },
        
    # .dataClear = function() {
    #   
    #   vars <- self$options$vars
    #   labels <- self$options$labels
    #   k <- self$options$k
    #   
    #   data <- self$data
    #   data <- jmvcore::naOmit(data)
    #   
    #   # Handling id----------
    #   
    #   if ( ! is.null(self$options$labels)) {
    #     rownames(data) <- data[[self$options$labels]]
    #     data[[self$options$labels]] <- NULL
    #   }
    #   
    #   for (i in seq_along(vars))
    #     data[[i]] <- jmvcore::toNumeric(data[[i]])
    #   
    #   # MDS analysis---------
    #   d <- stats:: dist(data)
    #   mds<- stats::cmdscale(d)
    #   #---------------------------
    #   # kmeans clustering--------
    #   # clust <- kmeans(mds, 3)$cluster %>%
    #   #     as.factor()
    #   # mds <- mds %>%
    #   #     mutate(groups = clust)
    #   
    #   km <- stats::kmeans(mds, k)
    #   kmc <- km$cluster
    # 
    #   name1 <- rownames(data)
    #   
    #   retlist <- list(d=d, mds=mds, kmc=kmc, name1=name1)
    #   return(retlist)
    #   
    # },
    # 

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
                                      color = "Clusters",
                                      palette = "jco",
                                      size = 1, 
                                      ellipse = TRUE,
                                      ellipse.type = "convex",
                                      repel = TRUE)
        
            plot1 <- plot1+ggtheme
            print(plot1)
            TRUE
        },
        
        .plot2 = function(image2,...) {

          if (is.null(image2$state))
            return(FALSE)

          three <- image2$state

          x<-  self$options$xlab
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
