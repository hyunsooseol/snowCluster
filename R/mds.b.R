
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
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>The rationale of Classical Multidimensional Scaling is described in the <a href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/" target="_blank">page</a>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
      },
      
      #---------------------------------------------
      
      .run = function() {
        if (is.null(self$options$vars))
          return()
        
        vars <- self$options$vars
        labels <- self$options$labels
        k <- self$options$k
        data <- self$data
        
        isSimple <- identical(self$options$mode, "simple")
        isComplex <- identical(self$options$mode, "complex")
        
        # 3D Plot은 3D Plot 탭에서만 표시
        self$results$plot2$setVisible(isComplex)
        
        # 분석 변수 기준으로 결측 없는 행 인덱스 추출
        complete_idx <- which(
          stats::complete.cases(data[, vars, drop = FALSE])
        )
        
        if (length(complete_idx) < 2)
          return()
        
        # MDS 분석에는 선택된 변수만 사용
        data_noNA <- data[complete_idx, vars, drop = FALSE]
        
        # Handling labels
        if (!is.null(labels)) {
          rownames(data_noNA) <- as.character(
            data[complete_idx, labels]
          )
        }
        
        # 선택된 분석 변수만 numeric으로 변환
        for (v in vars) {
          data_noNA[[v]] <- jmvcore::toNumeric(data_noNA[[v]])
        }
        
        # MDS analysis---------
        d <- stats::dist(
          data_noNA,
          method = self$options$metric
        )
        
        mds <- stats::cmdscale(d)
        
        # stress 계산
        stress_value <- NULL
        
        if (isTRUE(self$options$stress)) {
          d_hat <- stats::dist(mds)
          d_vec <- as.vector(d)
          d_hat_vec <- as.vector(d_hat)
          
          stress_value <- sqrt(
            sum((d_vec - d_hat_vec)^2) /
              sum(d_vec^2)
          )
        }
        
        # Stress note 출력
        if (isSimple &&
            isTRUE(self$options$stress) &&
            !is.null(stress_value)) {
          
          stress_text <- paste0(
            "Stress = ",
            format(round(stress_value, 3), nsmall = 3),
            "\n\nNote.\n",
            "Stress < .05 : excellent fit\n",
            "Stress .05–.10 : good fit\n",
            "Stress .10–.20 : fair fit\n",
            "Stress > .20 : poor fit"
          )
          
          try({
            self$results$stressNote$setContent(stress_text)
            self$results$stressNote$setVisible(TRUE)
          }, silent = TRUE)
          
        } else {
          
          try({
            self$results$stressNote$setVisible(FALSE)
          }, silent = TRUE)
        }
        
        # Shepard diagram state 저장
        if (isSimple && isTRUE(self$options$plot3)) {
          d_hat <- stats::dist(mds)
          
          shepard_df <- data.frame(
            Observed = as.vector(d),
            Fitted = as.vector(d_hat)
          )
          
          image3 <- self$results$plot3
          image3$setState(shepard_df)
        }
        
        # Simple MDS 탭
        if (isSimple) {
          
          # kmeans clustering--------
          model <- stats::kmeans(mds, k)
          mc <- model$cluster
          
          # 결측 포함 전체 행 수만큼 NA로 초기화
          cluster_full <- rep(NA, nrow(data))
          cluster_full[complete_idx] <- mc
          
          # 결측 포함 전체 행에 MDS 좌표 복원
          mds_full <- matrix(
            NA,
            nrow = nrow(data),
            ncol = ncol(mds)
          )
          
          mds_full[complete_idx, ] <- mds
          colnames(mds_full) <- c("Dim.1", "Dim.2")
          
          # plot용 labels
          if (!is.null(labels)) {
            
            plot_labels <- as.character(data[[labels]])
            
            rownames(mds_full) <- plot_labels
            
          } else {
            
            plot_labels <- rownames(data)
            
            if (!is.null(plot_labels))
              rownames(mds_full) <- plot_labels
          }
          
          if (isTRUE(self$options$plot)) {
            mds_df <- as.data.frame(mds_full)
            
            name <- rownames(mds_df)
            
            state <- list(
              mds_df,
              name
            )
            
            image <- self$results$plot
            image$setState(state)
          }
          
          if (isTRUE(self$options$plot1)) {
            clust <- as.factor(cluster_full)
            
            mds1 <- as.data.frame(mds_full)
            mds1$Clusters <- clust
            
            name1 <- rownames(mds1)
            
            state <- list(
              mds1,
              name1
            )
            
            image1 <- self$results$plot1
            image1$setState(state)
          }
          
          if (isTRUE(self$options$clust)) {
            self$results$clust$setValues(cluster_full)
            self$results$clust$setRowNums(rownames(data))
          }
        }
        
        # 3D Plot 탭
        if (isComplex) {
          if (is.null(self$options$xlab))
            return()
          
          three <- stats::cmdscale(
            d,
            k = 3
          )
          
          image2 <- self$results$plot2
          image2$setState(three)
        }
      },
      
      #---------------------------------------------
      
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
      
      #---------------------------------------------
      
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
      
      #---------------------------------------------
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        three <- image2$state
        
        x <- self$options$xlab
        y <- self$options$ylab
        z <- self$options$zlab
        
        scatterplot3d::scatterplot3d(
          three,
          xlab = x,
          ylab = y,
          zlab = z,
          highlight.3d = TRUE,
          grid = TRUE,
          pch = 19
        )
        
        TRUE
      },
      
      #---------------------------------------------
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        shepard_df <- image3$state
        
        plot3 <- ggplot2::ggplot(
          shepard_df,
          ggplot2::aes(
            x = Observed,
            y = Fitted
          )
        ) +
          ggplot2::geom_point(
            size = 1,
            alpha = 0.7
          ) +
          ggplot2::geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed"
          ) +
          ggplot2::labs(
            x = "Observed distances",
            y = "Fitted distances"
          )
        
        plot3 <- plot3 + ggtheme
        
        print(plot3)
        
        TRUE
      }
    )
  )