

famdClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "famdClass",
    inherit = famdBase,
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
            '<li>The rationale of Factor Analysis of mixed data is described in the <a href="https://rpkgs.datanovia.com/factoextra/reference/fviz_famd.html" target = "_blank">page</a>.</li>',
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
        
        res <- private$.allCache
        
        #-----------
        if (isTRUE(self$options$eigen)) {
          table <- self$results$eigen
          eigen <- as.vector(res$eig[, 1])
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
        
        #------------
        if (isTRUE(self$options$ind)) {
          type1 <- self$options$type1
          data_map <- list(
            "coordinates" = res$ind$coord,
            "cos2"        = res$ind$cos2,
            "contribution" = res$ind$contrib
          )
          
          ind <- data_map[[type1]]
          names <- dimnames(ind)[[1]]
          table <- self$results$ind
          for (i in 1:5)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Dimension'
            )
          for (name in names) {
            row <- list()
            for (j in seq_along(1:5)) {
              row[[paste0("pc", j)]] <- ind[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
          #---
          if (isTRUE(self$options$vari)) {
            type2 <- self$options$type2
            data_map <- list(
              "coordinates" = res$var$coord,
              "cos2"        = res$var$cos2,
              "contribution" = res$var$contrib
            )
            vari <- data_map[[type2]]
            names <- dimnames(vari)[[1]]
            table <- self$results$vari
            
            for (i in 1:5)
              
              table$addColumn(
                name = paste0("pc", i),
                title = as.character(i),
                type = 'number',
                superTitle = 'Dimension'
              )
            for (name in names) {
              row <- list()
              
              for (j in seq_along(1:5)) {
                row[[paste0("pc", j)]] <- vari[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
          #---
          if (isTRUE(self$options$quan)) {
            type3 <- self$options$type3
            
            data_map <- list(
              "coordinates" = res$quanti.var$coord,
              "cos2"        = res$quanti.var$cos2,
              "contribution" = res$quanti.var$contrib
            )
            
            quan <- data_map[[type3]]
            names <- dimnames(quan)[[1]]
            table <- self$results$quan
            
            for (i in 1:5)
              table$addColumn(
                name = paste0("pc", i),
                title = as.character(i),
                type = 'number',
                superTitle = 'Dimension'
              )
            for (name in names) {
              row <- list()
              for (j in seq_along(1:5)) {
                row[[paste0("pc", j)]] <- quan[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
          #---
          if (isTRUE(self$options$qual)) {
            type4 <- self$options$type4
            
            data_map <- list(
              "coordinates" = res$quali.var$coord,
              "cos2"        = res$quali.var$cos2,
              "contribution" = res$quali.var$contrib
            )
            
            qual <- data_map[[type4]]
            names <- dimnames(qual)[[1]]
            
            table <- self$results$qual
            for (i in 1:5)
              table$addColumn(
                name = paste0("pc", i),
                title = as.character(i),
                type = 'number',
                superTitle = 'Dimension'
              )
            for (name in names) {
              row <- list()
              for (j in seq_along(1:5)) {
                row[[paste0("pc", j)]] <- qual[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
          
          # plot--------
          if (isTRUE(self$options$plot)) {
            image <- self$results$plot
            image$setState(res)
          }
          if (isTRUE(self$options$plot1)) {
            image1 <- self$results$plot1
            image1$setState(res)
          }
          if (isTRUE(self$options$plot2)) {
            image2 <- self$results$plot2
            image2$setState(res)
          }
          if (isTRUE(self$options$plot3)) {
            image3 <- self$results$plot3
            image3$setState(res)
          }
          if (isTRUE(self$options$plot4)) {
            image4 <- self$results$plot4
            image4$setState(res)
          }
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state
        # Eigenvalues/variances of dimensions
        plot <- factoextra::fviz_screeplot(res)
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        res <- image1$state
        plot1 <- factoextra::fviz_famd_var(res)
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        res <- image2$state
        # Quantitative variables
        
        plot2 <- factoextra::fviz_famd_var(res,
                                           "quanti.var",
                                           col.var = "black",
                                           repel = TRUE)
        
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        res <- image3$state
        
        # # Qualitative variables
        plot3 <- factoextra::fviz_famd_var(res, "quali.var", col.var = "black")
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        
        res <- image4$state
        
        #  Graph of individuals---------
        
        plot4 <- factoextra::fviz_famd_ind(res,
                                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                           repel = TRUE)
        
        plot4 <- plot4 + ggtheme
        print(plot4)
        TRUE
        
      },
      
      .computeRES = function() {
        if (length(self$options$vars) < 3)
          return()
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        vars <- self$options$vars
        
        # Handling id----------
        
        if (!is.null(self$options$labels)) {
          rownames(data) <- data[[self$options$labels]]
          data[[self$options$labels]] <- NULL
        }
        
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]])
        
        
        # FA analysis of mixed data ##########################
        
        res <- FactoMineR::FAMD(data, graph = FALSE)
        
        ##############################################
        
        #res <- list(data = data, res = res)
        return(res)
        
      }
    )
  )