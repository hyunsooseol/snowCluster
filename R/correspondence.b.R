# This file is a generated template, your changes will not be overwritten

correspondenceClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "correspondenceClass",
    inherit = correspondenceBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      #------------------------------------
      
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
        

      },
      
      .run = function() {
        if (length(self$options$vars) < 3) return()
        
        vars <- self$options$vars
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        
        res.ca <- private$.allCache
        #data <- res.ca$data
        #res.ca <- res.ca$res
        
        #---
        if (isTRUE(self$options$chi)) {
          chi <- stats::chisq.test(res.ca$data)
          table <- self$results$chi
          row <- list(
            statistic = chi$statistic,
            df = chi$parameter,
            p = chi$p.value
          )
          
          table$setRow(rowNo = 1, values = row)
        }
        #---
        if (isTRUE(self$options$eigen)) {
          nd <- self$options$nd
          table <- self$results$eigen
          eigen <- as.vector(res.ca$res$eig[1:nd, 1])
          eigenTotal <- sum(abs(eigen))
          varProp <- (abs(eigen) / eigenTotal) * 100
          varCum <- cumsum(varProp)
          lapply(seq_along(eigen), function(i) {
            table$addRow(rowKey = i,
                         values = list(comp = as.character(i)))
          })
          lapply(seq_along(eigen), function(i) {
            row <- list(eigen = eigen[i],
                        varProp = varProp[i],
                        varCum = varCum[i])
            table$setRow(rowNo = i, values = row)
          })
        }
        #---
        if (isTRUE(self$options$col)) {
        nd <- self$options$nd
        type <- self$options$type 
        
        data_map <- list(
          "coordinates" = res.ca$res$col$coord,
          "cos2"        = res.ca$res$col$cos2,
          "contribution"= res.ca$res$col$contrib  
        )          
        
        cc <- data_map[[type]]
        
        #----------------
          table <- self$results$col
          
          for (i in 1:nd)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Dimension'
            )
          for (i in seq_along(self$options$vars)) {
            row <- list()
            for (j in 1:nd) {
              row[[paste0("pc", j)]] <- cc[i, j]
            }
            table$setRow(rowNo = i, values = row)
          }
        }
        
        #row---
        if (isTRUE(self$options$row)) {
          nd <- self$options$nd
          type1 <- self$options$type1 
          
          data_map <- list(
            "coordinates" = res.ca$res$row$coord,
            "cos2"        = res.ca$res$row$cos2,
            "contribution"= res.ca$res$row$contrib  
          )          
          
          rr <- data_map[[type1]]

          table <- self$results$row
          
          for (i in 1:nd)
            
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Dimension'
            )
          
          for (i in 1:nrow(res.ca$data)) {
            row <- list()
            for (j in 1:nd) {
              row[[paste0("pc", j)]] <- rr[i, j]
            }
            table$addRow(rowKey = i, values = row)
          }
        }
        
        if (isTRUE(self$options$plot1)) {
          #  Raw points plot----------
          image1 <- self$results$plot1
          image1$setState(res.ca$res)
        }
        
        if (isTRUE(self$options$plot2)) {
          # Column points plot-------
          image2 <- self$results$plot2
          image2$setState(res.ca$res)
        }
        
        # Biplot--------
        if (isTRUE(self$options$plot3)) {
          image3 <- self$results$plot3
          image3$setState(res.ca$res)
        }
        # Scree plot--------
        if (isTRUE(self$options$plot4)) {
          image4 <- self$results$plot4
          image4$setState(res.ca$res)
        }
      },
      
      # Plot==================================================
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        res.ca <- image1$state
        
        plot1 <- factoextra::fviz_ca_row(res.ca, # col.row = rowvar,
                                         # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE)
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        res.ca <- image2$state
        plot2 <- factoextra::fviz_ca_col(res.ca, repel = TRUE)
        
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        res.ca <- image3$state
        plot3 <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE)
        
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        
        res.ca <- image4$state
        
        plot4 <- factoextra::fviz_screeplot(res.ca, addlabels = TRUE)
        plot4 <- plot4 + ggtheme
        print(plot4)
        TRUE
        
      },
      
      .computeRES = function() {
        
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
        
        nd <- self$options$nd
        
        ##### Correspondence analysis---------
        
        res <- FactoMineR::CA(data, ncp = nd, graph = FALSE)
        
        res.ca <- list(data = data, res = res)
        return(res.ca)
      }
    )
  )
