
# Clustering dendrogram

hcClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "hcClass",
    inherit = hcBase,
    private = list(
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
        
        if (self$options$mode == "simple") {
          if (isTRUE(self$options$plot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$plot$setSize(width, height)
          }
        }
        
        if (self$options$mode == "complex") {
          if (isTRUE(self$options$plot1)) {
            width <- self$options$width1
            height <- self$options$height1
            self$results$plot1$setSize(width, height)
          }
        }
      },
      
      #---------------------------------------------
      
      .run = function() {
        
        if (self$options$mode == "simple") {
          
          if(length(self$options$vars) < 3) return() 
          
          vars <- self$options$vars
          data <- self$data
          
          # 1. 결측 제거 인덱스 저장
          complete_idx <- which(stats::complete.cases(data[, vars, drop=FALSE]))
          data_noNA <- data[complete_idx, , drop=FALSE]
          
          # Handling id----------
          if (!is.null(self$options$labels)) {
            rownames(data_noNA) <- data_noNA[[self$options$labels]]
            data_noNA[[self$options$labels]] <- NULL
          }
          for (i in seq_along(vars))
            data_noNA[[i]] <- jmvcore::toNumeric(data_noNA[[i]])
          
          ### Hierarchical Clustering---------
          hc <- try(factoextra::hcut(
            data_noNA,
            k = self$options$k,
            stand = self$options$stand,
            hc_metric = self$options$metric,
            hc_method = self$options$method
          ))
          #### Cluster number for the output variable--------------------
          if (!jmvcore::isError(hc)) {
            cluster <- hc$cluster
            # 2. 원본 행 수만큼 NA로 초기화 후, 결측 없는 행에만 cluster 결과 입력
            cluster_full <- rep(NA, nrow(self$data))
            cluster_full[complete_idx] <- cluster
            
            self$results$clust$setValues(cluster_full)
            self$results$clust$setRowNums(rownames(self$data))
          }
          ##### plot-------------------
          image <- self$results$plot
          image$setState(hc)
        }
        
        if (self$options$mode == "complex") {
          if (length(self$options$vars1) < 3) return()
          
          vars1 <- self$options$vars1
          data <- self$data
          data <- jmvcore::naOmit(data)
          data <- as.data.frame(data)
          
          nb <- self$options$nb
          method1 <-  self$options$method1
          dm <- self$options$dm
          
          res <- pvclust::pvclust(
            data,
            method.dist = dm,
            method.hclust = method1,
            nboot = nb,
            parallel = FALSE
          )
          image <- self$results$plot1
          image$setState(res)
          
          if (isTRUE(self$options$plot1)) {
            list <- pvclust::pvpick(res)
            self$results$text$setContent(list)
          }
        }
      },
      
      # Hierarchical clustering plot---------------
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        type <- self$options$type
        hc <- image$state
        
        if (self$options$horiz == TRUE) {
          plot <- factoextra::fviz_dend(
            hc,
            rect = TRUE,
            repel = TRUE,
            lwd = 1,
            type = type,
            horiz = TRUE,
            cex = 0.9,
            color_labels_by_k = TRUE
          )
        } else{
          plot <- factoextra::fviz_dend(
            hc,
            rect = TRUE,
            repel = TRUE,
            lwd = 1,
            type = type,
            cex = 0.9,
            color_labels_by_k = TRUE
          )
        }
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      # Dendrogram with p-values-----------------
      
      .plot1 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state
        plot(res)
        ask.bak <- par()$ask
        par(ask = TRUE)
        plot1 <- pvclust::pvrect(res)
        print(plot1)
        TRUE
      }
    )
  )
