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
        cachedOptions = NULL,
        standardizedData = NULL  # Cache standardized data
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
      
      # Helper function to standardize data
      .standardizeData = function(data) {
        if (!self$options$stand) return(data)
        
        standardized_data <- data
        for (var in 1:ncol(data)) {
          tmp <- data[, var]
          standardized_data[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
        }
        return(standardized_data)
      },
      
      # Helper function to prepare continuous data
      .prepareContinuousData = function(data, vars, remove_na = TRUE) {
        if (remove_na) {
          data <- jmvcore::naOmit(data)
        }
        
        # Select variables and convert to numeric
        dat <- jmvcore::select(data, vars)
        for (i in seq_along(vars)) {
          dat[[i]] <- jmvcore::toNumeric(dat[[i]])
        }
        
        # Apply standardization if needed
        dat <- private$.standardizeData(dat)
        return(dat)
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
            '<li><b>Variable clustering(PCA) plot</b> is always computed on standardized (z-scored) variables; the "Standardize variables" option does not affect this plot.</li>',
            '<li><b>Variable clustering(PCA) plot</b> is not available when only two variables are used.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        # Set plot sizes
        plot_configs <- list(
          list(option = "plot", width = "width", height = "height", result = "plot"),
          list(option = "plot1", width = "width1", height = "height1", result = "plot1"),
          list(option = "plot2", width = "width2", height = "height2", result = "plot2"),
          list(option = "plot3", width = "width3", height = "height3", result = "plot3"),
          list(option = "plot4", width = "width4", height = "height4", result = "plot4"),
          list(option = "plot5", width = "width5", height = "height5", result = "plot5")
        )
        
        for (config in plot_configs) {
          if (isTRUE(self$options[[config$option]])) {
            width <- self$options[[config$width]]
            height <- self$options[[config$height]]
            self$results[[config$result]]$setSize(width, height)
          }
        }
        
        if (self$options$oc)
          self$results$oc$setNote(
            "Note",
            "The highest silhouette score can generally be interpreted as the optimal number of cluster."
          )
        
        # Initialize centroids table
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
        
        # Initialize sum of squares table
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
        if (length(self$options$vars) < 2) return()
        optionsChanged <- private$.optionsChanged()
        
        # Store indices of complete cases from original data
        n_row <- nrow(self$data)
        not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
        
        # K-means clustering for continuous variables
        if (optionsChanged || is.null(private$.allCache$model)) {
          set.seed(1234)
          k <- self$options$k
          vars <- self$options$vars
          
          # Prepare data for clustering
          data_nomiss <- self$data[not_na_idx, , drop=FALSE]
          dat2 <- private$.prepareContinuousData(data_nomiss, vars, remove_na = FALSE)
          
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
          # Match cluster numbers to all rows (NA for missing values)
          cluster <- model$cluster
          cluster_vec <- rep(NA, n_row)
          cluster_vec[not_na_idx] <- as.numeric(cluster)
          
          # Create cluster summary table
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
          
          # Assign cluster numbers to results
          self$results$clust$setRowNums(rownames(self$data))
          self$results$clust$setValues(cluster_vec)
          
          # Update centroids and sum of squares tables
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
          
          # Prepare plot data
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
        
        # Plot1: Optimal number of clusters
        if (isTRUE(self$options$plot1)) {
          plotData1 <- self$data
          image1 <- self$results$plot1
          image1$setState(plotData1)
        }
        
        # Plot2: Cluster plot
        if (isTRUE(self$options$plot2) && !is.null(model)) {
          image2 <- self$results$plot2
          image2$setState(model)
        }
        
        # Plot3: Variable clustering (PCA) plot
        if (isTRUE(self$options$plot3) && !is.null(model)) {
          if (is.null(private$.allCache$clusterData) || optionsChanged) {
            # Always use standardized data for PCA plot (as per instructions)
            data <- jmvcore::select(self$data, self$options$vars)
            data <- jmvcore::naOmit(data)
            for (i in seq_along(self$options$vars))
              data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            # Force standardization for PCA plot
            for (var in 1:ncol(data)) {
              tmp <- data[, var]
              data[, var] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
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
        
        # Plot5: Scree plot (Elbow method)
        if (isTRUE(self$options$plot5)) {
          if (is.null(private$.allCache$elbowData) || optionsChanged) {
            data <- private$.prepareContinuousData(self$data, self$options$vars)
            
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
        
        # Gower distance analysis for mixed-type variables
        if (length(self$options$factors) >= 1) {
          if (is.null(private$.allCache$gowerData) || optionsChanged) {
            k1 <- self$options$k1
            vars <- self$options$vars
            facs <- self$options$factors
            data <- self$data
            
            # Process continuous variables
            if (length(vars) > 0) {
              for (i in seq_along(vars))
                data[[vars[i]]] <- jmvcore::toNumeric(data[[vars[i]]])
              
              # Apply standardization option (only for continuous variables)
              if (self$options$stand && length(vars) > 0) {
                for (var in vars) {
                  tmp <- data[[var]]
                  data[[var]] <- (tmp - mean(tmp, na.rm = TRUE)) / sd(tmp, na.rm = TRUE)
                }
              }
            }
            
            # Process categorical variables
            if (length(facs) > 0) {
              for (fac in facs)
                data[[fac]] <- as.factor(data[[fac]])
            }
            
            # Combine dataset
            selected_vars <- c(vars, facs)
            dat <- jmvcore::select(data, selected_vars)
            
            # Calculate Gower distance and cache
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
          
          # Update optimal clusters table
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
          
          # Update kproto table
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
            
            # Assign cluster numbers with missing value handling
            gn <- proto$cluster
            n_row <- nrow(self$data)
            not_na_idx <- which(stats::complete.cases(self$data[, c(vars, facs), drop=FALSE]))
            gn_vec <- rep(NA, n_row)
            gn_vec[not_na_idx] <- as.numeric(gn)
            self$results$clust1$setRowNums(rownames(self$data))
            self$results$clust1$setValues(gn_vec)
          }
          
          # Plot4: Silhouette plot
          if (isTRUE(self$options$plot4) && !is.null(private$.allCache$silhouetteData)) {
            image4 <- self$results$plot4
            image4$setState(private$.allCache$silhouetteData)
          }
        }
      },
      
      # Plot of means across groups
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
      
      # Optimal number of clusters plot
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        vars <- self$options$vars
        data <- image1$state
        
        # Prepare data using helper function
        dat <- private$.prepareContinuousData(data, vars)
        
        if (nrow(dat) < 2 || ncol(dat) < 1) {
          stop("There is no variable left after removing missing values.")
        }
        
        # Gap statistic analysis
        plot1 <- factoextra::fviz_nbclust(dat, stats::kmeans, method = "gap_stat")
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      # Elbow method plot
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
      
      # Cluster plot
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        vars <- self$options$vars
        # Prepare data using helper function
        dat <- private$.prepareContinuousData(self$data, vars)
        
        km.res <- image2$state
        plot2 <-
          factoextra::fviz_cluster(
            km.res,
            data = dat,
            ellipse.type = "convex",
            palette = "jco",
            ggtheme = theme_minimal()
          )
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      # Color variables by groups (PCA plot)
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
      
      # Silhouette plot
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