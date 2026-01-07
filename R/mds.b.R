
mdsClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "mdsClass",
    inherit = mdsBase,
    private = list(
      .htmlwidget = NULL,
      #------------------------------------
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$vars) |
            is.null(self$options$labels)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The rationale of Classical Multidimensional Scaling is described in the <a href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/" target = "_blank">page</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
       
      },
      
      #---------------------------------------------
      .run = function() {
        if (is.null(self$options$vars))
          return()
        
        vars <- self$options$vars
        labels <- self$options$labels
        k <- self$options$k
        data <- self$data
        
        # 1. 결측 없는 행 인덱스 추출
        complete_idx <- which(stats::complete.cases(data[, vars, drop=FALSE]))
        data_noNA <- data[complete_idx, , drop=FALSE]
        
        # Handling id----------
        if (!is.null(self$options$labels)) {
          rownames(data_noNA) <- data_noNA[[self$options$labels]]
          data_noNA[[self$options$labels]] <- NULL
        }
        for (i in seq_along(vars))
          data_noNA[[i]] <- jmvcore::toNumeric(data_noNA[[i]])
        
        # MDS analysis---------
        d <- stats::dist(data_noNA)
        mds <- stats::cmdscale(d)
        
        # kmeans clustering--------
        model <- stats::kmeans(mds, k)
        mc <- model$cluster
        
        # 결측 포함한 전체 행 수만큼 NA로 초기화, 결측 없는 행에만 cluster 번호 입력
        cluster_full <- rep(NA, nrow(data))
        cluster_full[complete_idx] <- mc
        
        # rowname/label 복원
        orig_rownames <- rownames(data)
        mds_full <- matrix(NA, nrow = nrow(data), ncol = ncol(mds))
        mds_full[complete_idx, ] <- mds
        colnames(mds_full) <- c("Dim.1", "Dim.2")
        
        # plot용 라벨 (결측 포함 전체 행)
        if (!is.null(orig_rownames)) {
          rownames(mds_full) <- orig_rownames
        }
        
        if (self$options$mode == "simple") {
          if (isTRUE(self$options$plot)) {
            mds_df <- as.data.frame(mds_full)
            name <- rownames(mds_df)
            state <- list(mds_df, name)
            image <- self$results$plot
            image$setState(state)
          }
          
          if (isTRUE(self$options$plot1)) {
            clust <- as.factor(cluster_full)
            mds1 <- as.data.frame(mds_full)
            mds1$Clusters <- clust
            name1 <- rownames(data)
            state <- list(mds1, name1)
            image1 <- self$results$plot1
            image1$setState(state)
          }
          
          if (isTRUE(self$options$clust)) {
            self$results$clust$setValues(cluster_full)
            self$results$clust$setRowNums(rownames(data))
          }
        }
        
        if (self$options$mode == "complex") {
          if (is.null(self$options$xlab)) return()
          
          d <- stats::dist(data_noNA)
          three <- stats::cmdscale(dist(d), k = 3)
          image2 <- self$results$plot2
          image2$setState(three)
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        mds <- image$state[[1]]
        name <- image$state[[2]]
        plot <- ggpubr::ggscatter(
          mds,
          x = "Dim.1",
          y = "Dim.2",
          label = name,
          size = 1,
          repel = TRUE
        )
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        mds1 <- image1$state[[1]]
        name1 <- image1$state[[2]]
        plot1 <- ggpubr::ggscatter(
          mds1,
          x = "Dim.1",
          y = "Dim.2",
          label = name1,
          color = "Clusters",
          palette = "jco",
          size = 1,
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE
        )
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        three <- image2$state
        x <-  self$options$xlab
        y <- self$options$ylab
        z <- self$options$zlab
        plot2 <- scatterplot3d::scatterplot3d(
          three,
          xlab = x,
          ylab = y,
          zlab = z,
          highlight.3d = TRUE,
          grid = TRUE,
          pch = 19
        )
        print(plot2)
        TRUE
      }
    )
  )
