
# PCA & GROUP PLOT

pcaClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "pcaClass",
    inherit = pcaBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$mode == "simple") {
          if (isTRUE(self$options$plot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$plot$setSize(width, height)
          }
          
          if (isTRUE(self$options$plot1)) {
            width <- self$options$width1
            height <- self$options$height1
            self$results$plot1$setSize(width, height)
          }
          
          if (isTRUE(self$options$plot2)) {
            width <- self$options$width2
            height <- self$options$height2
            self$results$plot2$setSize(width, height)
          }
          
        }
        if (self$options$mode == "complex") {
          if (isTRUE(self$options$plot3)) {
            width <- self$options$width3
            height <- self$options$height3
            self$results$plot3$setSize(width, height)
          }
          
          if (isTRUE(self$options$plot4)) {
            width <- self$options$width4
            height <- self$options$height4
            self$results$plot4$setSize(width, height)
          }
        }
      },
      
      .run = function() {
        if (self$options$mode == 'simple') {
          if (length(self$options$vars) < 3) return()
            
            vars <- self$options$vars
            data <- self$data
            data <- jmvcore::naOmit(data)
            # Handling id----------
            if (!is.null(self$options$labels)) {
              rownames(data) <- data[[self$options$labels]]
              data[[self$options$labels]] <- NULL
            }
            for (i in seq_along(vars))
              data[[i]] <- jmvcore::toNumeric(data[[i]])
            # principal component analysis---------
            pca <- FactoMineR::PCA(data, graph = FALSE)
            
            #---
            if (isTRUE(self$options$eigen)) {
              table <- self$results$eigen
              
              eigen <- as.vector(pca$eig[, 1])
              
              lapply(seq_along(eigen), function(i) {
                table$addRow(rowKey = i,
                             values = list(comp = as.character(i)))
              })
              
              eigenTotal <- sum(abs(eigen))
              varProp <- (abs(eigen) / eigenTotal) * 100
              varCum <- cumsum(varProp)
              
              lapply(seq_along(eigen), function(i) {
                row <- list(eigen = eigen[i],
                            varProp = varProp[i],
                            varCum = varCum[i])
                table$setRow(rowNo = i, values = row)
              })
            }
            
            # Variable contributions plot----------
            image <- self$results$plot
            image$setState(pca)
            
            # Individual plot-------
            image1 <- self$results$plot1
            image1$setState(pca)
            # Biplot--------
            image2 <- self$results$plot2
            image2$setState(pca)
        }
        
        if (self$options$mode == 'complex') {
          if (is.null(self$options$facs) || length(self$options$vars1) < 2)
            return()
          # read the option values into shorter variable names
          vars1  <- self$options$vars1
          facs <- self$options$facs
          data <- self$data
          # convert to appropriate data types
          
          for (i in seq_along(vars1))
            data[[i]] <- jmvcore::toNumeric(data[[i]])
          
          for (fac in facs)
            data[[fac]] <- as.factor(data[[fac]])
          
          # data is now all of the appropriate type we can begin!
          data <- na.omit(data)
          data <- jmvcore::select(data, self$options$vars1)
          
          # principal component analysis---------
          pca <- FactoMineR::PCA(data, graph = FALSE)
          # group plot----------
          image3 <- self$results$plot3
          image3$setState(pca)
          
          # biplot------------------------
          image4 <- self$results$plot4
          image4$setState(pca)
        }
      },
      
      # Control variable colors using their contributions----------
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        pca <- image$state
        plot <- factoextra::fviz_pca_var(pca, #col.var="contrib",
                                         #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE) # Avoid text overlapping
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        pca <- image1$state
        
        plot1 <- factoextra::fviz_pca_ind(pca, #col.ind = "cos2",
                                          #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                          repel = TRUE) # Avoid text overlapping (slow if many points))
        
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        pca <- image2$state
        plot2 <- factoextra::fviz_pca_biplot(pca, repel = TRUE)
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        pca <- image3$state
        plot3 <- factoextra::fviz_pca_ind(
          pca,
          label = "none",
          # hide individual labels
          habillage = self$data[[self$options$facs]],
          # color by groups for example, iris$Species,
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          addEllipses = TRUE # Concentration ellipses
        )
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        pca <- image4$state
        
        plot4 <- factoextra::fviz_pca_biplot(
          pca,
          col.ind =  self$data[[self$options$facs]],
          palette = "jco",
          addEllipses = TRUE,
          label = "var",
          col.var = "black",
          repel = TRUE,
          legend.title = self$options$facs
        )
        plot4 <- plot4 + ggtheme
        print(plot4)
        TRUE
      }
    )
  )
