
# This file is a generated template, your changes will not be overwritten

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
                
            },
            
            
            ########################################################
            
            .run = function() {
                # print('.runnig"')
                # text <- "started"
                
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
                        self$results$text1$setContent(SSW)
                        self$results$text2$setContent(SSB)
                        self$results$text3$setContent(SST)
                        
                        
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
                 #   text <- cat(text, "Error in clustering analysis")
                    image <- self$results$plot
                    image$setState(NULL)
                    
                }
                
                
            },
            
            
            ### Plot of means across groups
            
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
                }
                TRUE
                
            }
            
        )
        
        
    )
