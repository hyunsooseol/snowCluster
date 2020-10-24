
# This file is a generated template, your changes will not be overwritten

#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @import ggplot2
#' @export


kmeansClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "kmeansClass",
        inherit = kmeansBase,
        private = list(
            .init = function() {
                
                ##initialize the centroids of cluster table-------------
                
                tab2 <- self$results$centroids
                vars <- self$options$vars
                vars <- factor(vars, levels = vars)
                nVars <- length(vars)
                k <- self$options$k
                
                tab2$addColumn(name = "cluster",
                               title = "Cluster No",
                               type = 'Integer')
                
                for (i in seq_along(vars)) {
                    var <- vars[[i]]
                    
                    tab2$addColumn(name = paste0(var),
                                   type = 'number',
                                   format = 'zto')
                    
                }
                
                
                values <- list(cluster = 1)
                
                for (i in seq_len(nVars)) {
                    values[[paste0(vars[[i]])]]  <- '\u2014'
                }
                
                ##add dummy values to table:
                
                for (j in 1:k)
                {
                    values[["cluster"]] <- j
                    tab2$setRow(rowNo = j, values)
                }
                
                ss <- self$results$ss
                for (j in seq_len(k))
                
                ss$addRow(rowKey=j, values=list(source=paste('Cluster', j)))
                
                ss$addRow(rowKey='between', values=list(source='Between clusters'))
                
                ss$addFormat(rowKey='between', col=1, jmvcore::Cell.BEGIN_END_GROUP)
                
                ss$addRow(rowKey='total', values=list(source='Total'))
                
                ss$addFormat(rowKey='total', col=1, jmvcore::Cell.BEGIN_END_GROUP)
                
                
               
            },
            
            
            ########################################################
            
            .run = function() {
               
                
                if (!is.null(self$options$vars))
                {
                    dat2 <- jmvcore::select(self$data, self$options$vars)
                    
                    
                    #standardize variables-------------
                    
                    if (self$options$stand)
                    {
                        for (var in 1:ncol(dat2))
                        {
                            tmp <- dat2[, var]
                            dat2[, var] <-
                                (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
                            
                        }
                    }
                    
                    
                    if (dim(dat2)[2] > 0)
                    {
                        model <- stats::kmeans(
                            dat2,
                            centers = self$options$k,
                            nstart = self$options$nstart,
                            algorithm = self$options$algo
                        )
                        
                        
                        cluster <- model$cluster
                        SSW <- model$withinss
                        SSB <- model$betweenss
                        SST <- model$totss
                        
                        
                        tab <- self$results$clustering
                        tab$deleteRows()
                        
                        
                        
                        clusters <- table(model$cluster)
                        rowno <- 1
                        for (i in 1:dim(clusters))
                        {
                            tab$addRow(
                                rowKey = paste("Cluster", i),
                                values = list(cluster = i, count = clusters[i])
                            )
                        }
                        
                        
                        ##The centroids of clusters table------------
                        
                        tab2 <- self$results$centroids
                        vars <- self$options$vars
                        vars <- factor(vars, levels = vars)
                        
                        nVars <- length(vars)
                        k <- self$options$k
                        
                        for (i in 1:k)
                        {
                            values <- unlist(list(cluster = i, model$centers[i, ]))
                            tab2$setRow(rowNo = i, values)
                            
                        }
                        
                        
                        self$results$text$setContent(cluster)
                        
                        ### Sum of squares Table----------
                        
                        ss <- self$results$ss
                        
                        ss$setRow(rowKey='between', values=list(value=SSB))
                        
                        ss$setRow(rowKey='total', values=list(value=SST))
                        
                        for (i in seq_len(k))
                            ss$setRow(rowKey=i, values=list(value=SSW[i]))
                        
                        
                        # plot data function---------
                        
                        plotData <- data.frame(
                            cluster = as.factor(rep(1:k, nVars)),
                            var = rep(vars, each = k),
                            centers = as.vector(model$centers)
                        )
                        
                        image <- self$results$plot
                        image$setState(plotData)
                        
                    } else{
                        image <- self$results$plot
                        image$setState(NULL)
                        text <- "No results"
                        
                        
                    }
                    
                    
                    
                } else {
                
                    image <- self$results$plot
                    image$setState(NULL)
                    
                }
                
        ##### Prepare Data For Plot1(optimal number of clusters) -------
              
                
                 data <- jmvcore::select(self$data, self$options$vars)
                  
                 plotData1 <- data
                  
                  # Data for plot ----
                  
                  image1 <- self$results$plot1
                  image1$setState(plotData1)
            
        ###### Prepare data for plot2(cluster plot)-----------
                  
                  data <- jmvcore::select(self$data, self$options$vars)
                  
                  if (dim(data)[2] > 0){
                   km.res <- stats::kmeans(
                          data,
                          centers = self$options$k,
                          nstart = self$options$nstart,
                          algorithm = self$options$algo
                      )
                  
                   image2 <- self$results$plot2
                   
                   image2$setState(km.res)
                   
                    }
                  
            },
            
            
            ###### Plot of means across groups--------------------
            
            .plot = function(image, ggtheme, theme, ...) {
                
                #Errors ----
                
                if (is.null(self$options$vars))
                    return()
                
                
                plotData <- image$state
                if (!is.null(plotData))
                {
                    plot <-
                        ggplot(plotData,
                               aes(
                                   x = var,
                                   y = centers,
                                   group = cluster,
                                   colour = cluster
                               )) +
                        geom_path(size = 1.2) + 
                        geom_point(size = 4) + 
                        xlab("Variable") + 
                        ylab("Mean value")+
                        ggtheme
                    
                    print(plot)
                    TRUE
                }
            },
                
              # Optimal number of clusters------
                
                .plot1 = function(image1,ggtheme, theme, ...) {
                    
                    
                    if (is.null(self$options$vars))
                        return()
                
                    # read data ----
                    
                    plotData1 <- image1$state
                
                plot1<- factoextra::fviz_nbclust(plotData1,stats::kmeans, method = "gap_stat")
                print(plot1)
                TRUE
                
                },
            
            
            # cluster plot------
            
            .plot2 = function(image2,ggtheme, theme, ...) {
                
                
                if (is.null(self$options$vars))
                    return()
                
                # read data ----
                
                data <- jmvcore::select(self$data, self$options$vars)
                
                km.res <- image2$state
                
                plot2<- factoextra::fviz_cluster(km.res, data = data,
                                     ellipse.type = "convex",
                                     palette = "jco",
                                     ggtheme = theme_minimal())
                print(plot2)
                TRUE
            
            }
        )
   
    )
