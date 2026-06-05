
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
      },
      
      #---------------------------------------------
      
      .updateClusterInfo = function(picked) {
        
        if (is.null(self$results$clusterInfo))
          return()
        
        table <- self$results$clusterInfo
        
        if (is.null(picked) || is.null(picked$clusters) || length(picked$clusters) == 0) {
          table$setVisible(FALSE)
          return()
        }
        
        table$setVisible(TRUE)
        
        clusters <- picked$clusters
        
        for (i in seq_along(clusters)) {
          
          members <- clusters[[i]]
          
          table$addRow(rowKey = as.character(i), values = list(
            cluster = i,
            size = length(members),
            members = paste(members, collapse = ", ")
          ))
        }
      },
      
      #---------------------------------------------
      
      .run = function() {
        
        if (self$options$mode == "simple") {
          
          if (length(self$options$vars) < 3)
            return()
          
          vars <- self$options$vars
          data <- self$data
          
          # 1. 결측 제거 인덱스 저장
          complete_idx <- which(stats::complete.cases(data[, vars, drop = FALSE]))
          data_noNA <- data[complete_idx, , drop = FALSE]
          
          # Handling id----------
          if (!is.null(self$options$labels)) {
            rownames(data_noNA) <- data_noNA[[self$options$labels]]
          }
          
          # 선택한 변수만 분석에 사용
          data_hc <- data_noNA[, vars, drop = FALSE]
          
          for (v in vars)
            data_hc[[v]] <- jmvcore::toNumeric(data_hc[[v]])
          
          ### Hierarchical Clustering---------
          hc <- try(factoextra::hcut(
            data_hc,
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
          
          if (length(self$options$vars1) < 3)
            return()
          
          vars1 <- self$options$vars1
          data <- self$data[, vars1, drop = FALSE]
          data <- jmvcore::naOmit(data)
          data <- as.data.frame(data)
          
          for (v in vars1)
            data[[v]] <- jmvcore::toNumeric(data[[v]])
          
          nb <- self$options$nb
          method1 <- self$options$method1
          dm <- self$options$dm
          
          # Show progress spinner
          self$results$progressBarHTML$setVisible(TRUE)
          self$results$progressBarHTML$setContent(
            appleSpinnerH('Performing Clustering Dendrogram...')
          )
          private$.checkpoint()
          
          
          
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
            picked <- pvclust::pvpick(res)
            private$.updateClusterInfo(picked)
          } else {
            if (!is.null(self$results$clusterInfo))
              self$results$clusterInfo$setRowCount(0)
          }
          # 100%: complete and hide
          self$results$progressBarHTML$setVisible(FALSE)
          
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
        } else {
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
        pvclust::pvrect(res)
        TRUE
      }
    )
  )

# Progress Bar HTML  (R/progressBarH.R)
appleSpinnerH <- function(message = '') {
  paste0(
    '<div style="text-align:center;padding:24px;">',
    
    '<style>',
    '@keyframes snowsoftAppleDotPulse {',
    '0%, 80%, 100% { transform: scale(0.72); opacity: 0.55; }',
    '40% { transform: scale(1.20); opacity: 1; }',
    '}',
    '</style>',
    
    '<div style="margin-bottom:10px;">',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#007AFF;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#34C759;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.15s;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#FF9500;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.30s;',
    'vertical-align:middle;',
    '"></span>',
    
    '</div>',
    
    '<div style="font-size:12px;color:#666;">',
    message,
    '</div>',
    
    '</div>'
  )
}