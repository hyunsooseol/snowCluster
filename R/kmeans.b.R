kmeansClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "kmeansClass",
    inherit = kmeansBase,
    private = list(
      .allCache = list(
        model = NULL,
        plotData = NULL,
        clusterData = NULL,
        gowerData = NULL,
        silhouetteData = NULL,
        elbowData = NULL,
        cachedOptions = NULL
      ),
      .htmlwidget = NULL,
      
      .optionsChanged = function() {
        if (is.null(private$.allCache$cachedOptions)) return(TRUE)
        currentOptions <- list(
          vars = self$options$vars,
          k = self$options$k,
          stand = self$options$stand,
          nstart = self$options$nstart,
          algo = self$options$algo,
          factors = self$options$factors,
          k1 = self$options$k1
        )
        return(!identical(currentOptions, private$.allCache$cachedOptions))
      },
      
      .saveOptions = function() {
        private$.allCache$cachedOptions <- list(
          vars = self$options$vars,
          k = self$options$k,
          stand = self$options$stand,
          nstart = self$options$nstart,
          algo = self$options$algo,
          factors = self$options$factors,
          k1 = self$options$k1
        )
      },
      
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
            '<li>If the variables consist solely of <b>continuous variables</b>, apply K-means clustering analysis.</li>',
            '<li>If the variables are <b>categorical or mixed-type variables</b>, apply Gower distance analysis.</li>',
            '<li><b>Variable clustering(PCA) plot</b> is always computed on standardized (z-scored) variables; the “Standardize variables” option does not affect this plot.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
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
        if (isTRUE(self$options$plot5)) {
          width <- self$options$width5
          height <- self$options$height5
          self$results$plot5$setSize(width, height)
        }
        if (self$options$oc)
          self$results$oc$setNote(
            "Note",
            "The highest silhouette score can generally be interpreted as the optimal number of cluster."
          )
        tab2 <- self$results$centroids
        vars <- self$options$vars
        vars <- factor(vars, levels = vars)
        nVars <- length(vars)
        k <- self$options$k
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
        for (j in 1:k) {
          values[["cluster"]] <- j
          tab2$setRow(rowNo = j, values)
        }
        ss <- self$results$ss
        for (j in seq_len(k))
          ss$addRow(rowKey = j, values = list(source = paste('Cluster', j)))
        ss$addRow(rowKey = 'between',
                  values = list(source = 'Between clusters'))
        ss$addFormat(rowKey = 'between',
                     col = 1,
                     jmvcore::Cell.BEGIN_END_GROUP)
        ss$addRow(rowKey = 'total', values = list(source = 'Total'))
        ss$addFormat(rowKey = 'total',
                     col = 1,
                     jmvcore::Cell.BEGIN_END_GROUP)
      },
      
      .run = function() {
        if (length(self$options$vars) < 3) return()
        optionsChanged <- private$.optionsChanged()
        
        # === 1. 결측치 없는 행 인덱스 저장 (반드시 원본 데이터에서!)
        n_row <- nrow(self$data)
        not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
        
        if (optionsChanged || is.null(private$.allCache$model)) {
          set.seed(1234)
          k <- self$options$k
          vars <- self$options$vars
          facs <- self$options$factors
          data_nomiss <- self$data[not_na_idx, , drop=FALSE]
          for (i in seq_along(vars))
            data_nomiss[[i]] <- jmvcore::toNumeric(data_nomiss[[i]])
          dat2 <- jmvcore::select(data_nomiss, self$options$vars)
          if (self$options$stand) {
            for (var in 1:ncol(dat2)) {
              tmp <- dat2[, var]
              dat2[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
            }
          }
          if (dim(dat2)[2] > 0) {
            model <- stats::kmeans(
              dat2,
              centers = self$options$k,
              nstart = self$options$nstart,
              algorithm = self$options$algo
            )
            private$.allCache$model <- model
            private$.saveOptions()
          }
        } else {
          model <- private$.allCache$model
        }
        
        if (!is.null(model)) {
          # === 2. cluster 번호를 전체 row와 매칭 (NA는 NA로)
          cluster <- model$cluster
          cluster_vec <- rep(NA, n_row)
          cluster_vec[not_na_idx] <- as.numeric(cluster)
          
          # === 3. 클러스터 요약 표 생성
          tab <- self$results$clustering
          tab$deleteRows()
          clusters <- table(cluster_vec)
          for (i in seq_along(clusters)) {
            if (!is.na(names(clusters)[i])) {
              tab$addRow(
                rowKey = paste("Cluster", names(clusters)[i]),
                values = list(cluster = as.integer(names(clusters)[i]), count = clusters[i])
              )
            }
          }
          
          # === 4. clust(군집 번호) 결과 할당
          self$results$clust$setRowNums(rownames(self$data))
          self$results$clust$setValues(cluster_vec)
          
          # 이하 기존 코드 동일
          SSW <- model$withinss
          SSB <- model$betweenss
          SST <- model$totss
          tab2 <- self$results$centroids
          vars <- self$options$vars
          vars <- factor(vars, levels = vars)
          nVars <- length(vars)
          k <- self$options$k
          for (i in 1:k) {
            values <- unlist(list(cluster = i, model$centers[i, ]))
            tab2$setRow(rowNo = i, values)
          }
          ss <- self$results$ss
          ss$setRow(rowKey = 'between', values = list(value = SSB))
          ss$setRow(rowKey = 'total', values = list(value = SST))
          for (i in seq_len(k))
            ss$setRow(rowKey = i, values = list(value = SSW[i]))
          if (is.null(private$.allCache$plotData) || optionsChanged) {
            plotData <- data.frame(
              cluster = as.factor(rep(1:k, nVars)),
              var = rep(vars, each = k),
              centers = as.vector(model$centers)
            )
            private$.allCache$plotData <- plotData
          } else {
            plotData <- private$.allCache$plotData
          }
          image <- self$results$plot
          image$setState(plotData)
        }
        
        ##### Plot1(optimal number of clusters)
        if (isTRUE(self$options$plot1)) {
          plotData1 <- self$data
          image1 <- self$results$plot1
          image1$setState(plotData1)
        }
        ##### Plot2(cluster plot)
        if (isTRUE(self$options$plot2) && !is.null(model)) {
          image2 <- self$results$plot2
          image2$setState(model)
        }
        ##### Plot3
        if (isTRUE(self$options$plot3) && !is.null(model)) {
          if (is.null(private$.allCache$clusterData) || optionsChanged) {
            data <- jmvcore::select(self$data, self$options$vars)
            data <- jmvcore::naOmit(data)
            for (i in seq_along(self$options$vars))
              data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            # 표준화 옵션 적용
            if (self$options$stand) {
              for (var in 1:ncol(data)) {
                tmp <- data[, var]
                data[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
              }
            }
            
            res.pca <- FactoMineR::PCA(data, graph = FALSE)
            var <- factoextra::get_pca_var(res.pca)
            set.seed(1234)
            res.km <- stats::kmeans(
              var$coord,
              centers = self$options$k,
              nstart = self$options$nstart,
              algorithm = self$options$algo
            )
            grp <- as.factor(res.km$cluster)
            state <- list(res.pca, grp)
            private$.allCache$clusterData <- state
          }
          image3 <- self$results$plot3
          image3$setState(private$.allCache$clusterData)
        }
        # Scree plot (plot5) - 표준화 옵션 반영
        if (isTRUE(self$options$plot5)) {
          if (is.null(private$.allCache$elbowData) || optionsChanged) {
            data <- jmvcore::select(self$data, self$options$vars)
            data <- jmvcore::naOmit(data)
            for (i in seq_along(self$options$vars))
              data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            # 표준화 옵션 적용
            if (self$options$stand) {
              for (var in 1:ncol(data)) {
                tmp <- data[, var]
                data[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
              }
            }
            
            inertia <- numeric(self$options$max)
            for (k in 1:self$options$max) {
              set.seed(1234)
              km.res <- stats::kmeans(
                data,
                centers = k,
                nstart = self$options$nstart,
                algorithm = self$options$algo
              )
              inertia[k] <- km.res$tot.withinss
            }
            elbow_data <- data.frame(K = 1:self$options$max, Inertia = inertia)
            private$.allCache$elbowData <- elbow_data
          }
          image5 <- self$results$plot5
          image5$setState(private$.allCache$elbowData)
        }
        #---------------------------------------
        if (length(self$options$factors) >= 1) {
          if (is.null(private$.allCache$gowerData) || optionsChanged) {
            k1 <- self$options$k1
            vars <- self$options$vars
            facs <- self$options$factors
            data <- self$data
            
            # continuous vars 처리
            if (length(vars) > 0) {
              for (i in seq_along(vars))
                data[[vars[i]]] <- jmvcore::toNumeric(data[[vars[i]]])
              
              # 표준화 옵션 적용 (연속변수에만)
              if (self$options$stand && length(vars) > 0) {
                for (var in vars) {
                  tmp <- data[[var]]
                  data[[var]] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
                }
              }
            }
            
            # factor vars---
            if (length(facs) > 0) {
              for (fac in facs)
                data[[fac]] <- as.factor(data[[fac]])
            }
            # combine dataset---
            selected_vars <- c(vars, facs)
            dat <- jmvcore::select(data, selected_vars)
            # Gower 거리 계산 및 캐싱
            if (isTRUE(self$options$oc)) {
              set.seed(1234)
              oc <- clustMixType::validation_kproto(data = dat, type = 'gower')
              private$.allCache$gowerData <- list(indices = oc$indices)
            }
            if (isTRUE(self$options$kp)) {
              set.seed(1234)
              proto <- clustMixType::kproto(dat, k = k1, type = 'gower')
              if (is.null(private$.allCache$gowerData)) {
                private$.allCache$gowerData <- list()
              }
              private$.allCache$gowerData$proto <- proto
              if (isTRUE(self$options$plot4)) {
                gn <- proto$cluster
                gower_dist <- stats::as.dist(proto$dists)
                sil <- cluster::silhouette(gn, gower_dist)
                sil_data <- data.frame(cluster = factor(sil[, 1]),
                                       silhouette_width = sil[, 3])
                agg_sil <- stats::aggregate(silhouette_width ~ cluster, data = sil_data, mean)
                agg_sil$cluster_num <- as.integer(as.character(agg_sil$cluster))
                agg_sil <- agg_sil[order(agg_sil$cluster_num), ]
                private$.allCache$silhouetteData <- agg_sil
              }
            }
          }
          if (isTRUE(self$options$oc) && !is.null(private$.allCache$gowerData$indices)) {
            table <- self$results$oc
            oc <- data.frame(private$.allCache$gowerData$indices)
            names <- dimnames(oc)[[1]]
            for (name in names) {
              row <- list()
              row[['value']] <- oc[name, 1]
              table$addRow(rowKey = name, values = row)
            }
          }
          if (isTRUE(self$options$kp) && !is.null(private$.allCache$gowerData$proto)) {
            proto <- private$.allCache$gowerData$proto
            table <- self$results$kp
            mat <- data.frame(proto$dists)
            colnames(mat) <- paste0("Cluster", seq_along(colnames(mat)))
            names <- dimnames(mat)[[1]]
            dims <- colnames(mat)
            for (dim in dims) {
              table$addColumn(name = paste0(dim),
                              type = 'text',
                              combineBelow = TRUE)
            }
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- mat[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
            # cluster number: clust1
            gn <- proto$cluster
            # 결측치 매칭 (clust1용)
            n_row <- nrow(self$data)
            not_na_idx <- which(stats::complete.cases(self$data[, c(vars, facs), drop=FALSE]))
            gn_vec <- rep(NA, n_row)
            gn_vec[not_na_idx] <- as.numeric(gn)
            self$results$clust1$setRowNums(rownames(self$data))
            self$results$clust1$setValues(gn_vec)
          }
          if (isTRUE(self$options$plot4) && !is.null(private$.allCache$silhouetteData)) {
            image4 <- self$results$plot4
            image4$setState(private$.allCache$silhouetteData)
          }
        }
      },
      
      # Plot of means across groups---
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        plotData <- image$state
        
        if (!is.null(plotData)) {
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
            xlab("") +
            ylab("Mean value") +
            ggtheme
          
          if (self$options$angle > 0) {
            plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
          }
          print(plot)
          TRUE
        }
      },
      
      # Optimal number of clusters - 표준화 옵션 반영
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        vars <- self$options$vars
        data <- image1$state
        
        # 1. naOmit 적용
        data <- jmvcore::naOmit(data)
        if (nrow(data) < 2 || ncol(data) < 1) {
          stop("There is no variable left after removing missing values.")
        }
        
        # 2. 변수들을 선택하고 numeric 변환
        dat <- jmvcore::select(data, vars)
        for (i in seq_along(vars))
          dat[[i]] <- jmvcore::toNumeric(dat[[i]])
        
        # 3. 표준화 옵션 적용 (.run() 함수와 동일한 로직)
        if (self$options$stand) {
          for (var in 1:ncol(dat)) {
            tmp <- dat[, var]
            dat[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
          }
        }
        
        # 4. gap_stat 분석 (표준화된 데이터 또는 원본 데이터 사용)
        plot1 <- factoextra::fviz_nbclust(dat, stats::kmeans, method = "gap_stat")
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot5 = function(image5,...) {
        if (is.null(image5$state))
          return(FALSE)      
        
        elbow_plot <- image5$state
        
        plot5 <- plot(elbow_plot$K, elbow_plot$Inertia,
                      type = "b", 
                      pch = 19,   
                      col = "blue", 
                      xlab = "Number of clusters",
                      ylab = "Inertia")
        print(plot5)
        TRUE
      },
      
      # cluster plot - 표준화 옵션 반영
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        # 변수 선택 및 numeric 변환
        dat <- jmvcore::select(data, vars)
        for (i in seq_along(vars))
          dat[[i]] <- jmvcore::toNumeric(dat[[i]])
        
        # 표준화 옵션 적용
        if (self$options$stand) {
          for (var in 1:ncol(dat)) {
            tmp <- dat[, var]
            dat[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
          }
        }
        
        km.res <- image2$state
        plot2 <-
          factoextra::fviz_cluster(
            km.res,
            data = dat, # 표준화 옵션에 따른 데이터 사용
            ellipse.type = "convex",
            palette = "jco",
            ggtheme = theme_minimal()
          )
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      # Color variables by groups----------------
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        res.pca <- image3$state[[1]]
        grp     <- image3$state[[2]]
        plot3 <-
          factoextra::fviz_pca_var(res.pca, col.var = grp,
                                   legend.title = "Cluster")
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        
        agg_sil <- image4$state
        
        plot4 <- ggplot2::ggplot(agg_sil, ggplot2::aes(x = cluster_num, y = silhouette_width)) +
          ggplot2::geom_point(color = "purple", size = 3) +
          ggplot2::geom_line(color = "purple") +
          ggplot2::labs(x = "Number of Clusters", y = "Average Silhouette Width", title = "Average Silhouette Width by Cluster")
        
        plot4 <- plot4 + ggtheme
        print(plot4)
        TRUE
      }
    )
  )