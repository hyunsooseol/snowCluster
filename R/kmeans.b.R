
kmeansClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "kmeansClass",
    inherit = kmeansBase,
    private = list(
      .allCache = NULL,
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
            '<li>If the variables consist solely of continuous variables, apply K-means clustering analysis.</li>',
            '<li>If the variables are categorical or mixed-type variables, apply Gower distance analysis.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        # min or max option is possible in .a yaml.
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
        ##initialize the centroids of cluster table-------------
        
        tab2 <- self$results$centroids
        vars <- self$options$vars
        vars <- factor(vars, levels = vars)
        nVars <- length(vars)
        k <- self$options$k
        
        # tab2$addColumn(name = "cluster",
        #                title = "Cluster No",
        #                type = 'integer')
        
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
        
        ##add dummy values to table----------
        
        for (j in 1:k)
        {
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
      
      #---
      .run = function() {
        
        if (length(self$options$vars) < 3) return()
        
        # Solved Problem that does not change plot using set.seed()
        set.seed(1234)
        
        k <- self$options$k
        vars <- self$options$vars
        facs <- self$options$factors
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]])
        
        dat2 <- jmvcore::select(data, self$options$vars)
        
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
        
        if (dim(dat2)[2] > 0){
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
          ## The centroids of clusters table------------
          
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
          # resulting clustering vector-----
          # options(max.print=999999)
          #
          # self$results$text$setContent(cluster)
          
          self$results$clust$setValues(cluster)
          self$results$clust$setRowNums(rownames(data))
          
          ### Sum of squares Table----------
          ss <- self$results$ss
          ss$setRow(rowKey = 'between', values = list(value =
                                                        SSB))
          ss$setRow(rowKey = 'total', values = list(value =
                                                      SST))
          for (i in seq_len(k))
            ss$setRow(rowKey = i, values = list(value = SSW[i]))
          
          # plot data function---------
          plotData <- data.frame(
            cluster = as.factor(rep(1:k, nVars)),
            var = rep(vars, each = k),
            centers = as.vector(model$centers)
          )
          image <- self$results$plot
          image$setState(plotData)
        }
        ##### Prepare Data For Plot1(optimal number of clusters) -------
        plotData1 <- data
        
        # Data for plot ----
        if(isTRUE(self$options$plot1)){
        image1 <- self$results$plot1
        image1$setState(plotData1)
        }
        ###### Prepare data for plot2(cluster plot)-----------
        data <-
          jmvcore::select(data, self$options$vars)
        
        #set.seed(1234)
        if (dim(data)[2] > 0) {
          # km.res <- stats::kmeans(
          #   data,
          #   centers = self$options$k,
          #   nstart = self$options$nstart,
          #   algorithm = self$options$algo
          # )
          km.res <- model
          if(isTRUE(self$options$plot2)){
          image2 <- self$results$plot2
          image2$setState(km.res)
          }
          # Create a grouping variable using kmeans-----
          
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
          
          if(isTRUE(self$options$plot3)){
          image3 <- self$results$plot3
          state <- list(res.pca, grp)
          image3$setState(state)
          }
        }
        #Scree plot---        
        if(isTRUE(self$options$plot5)){
          
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
          image5 <- self$results$plot5
          image5$setState(elbow_data)          
        }
        
        #---------------------------------------
        if (length(self$options$factors) >= 1) {
          k1 <- self$options$k1
          vars <- self$options$vars
          facs <- self$options$factors
          data <- self$data
          
          # continuous vars---
          if (length(vars) > 0) {
            for (i in seq_along(vars))
              data[[vars[i]]] <- jmvcore::toNumeric(data[[vars[i]]])
          }
          # factor vars---
          if (length(facs) > 0) {
            for (fac in facs)
              data[[fac]] <- as.factor(data[[fac]])
          }
          # a <- capture.output(summary(data[fac]))
          # self$results$text$setContent(paste(a, collapse = "\n"))
          
          # combine dataset---
          selected_vars <- c(vars, facs)
          dat <- jmvcore::select(data, selected_vars)
          
          if (isTRUE(self$options$oc)) {
            set.seed(1234)
            oc <- clustMixType::validation_kproto(data = dat, type =
                                                    'gower')
            #self$results$text1$setContent(oc$indices)
            table <- self$results$oc
            oc <- data.frame(oc$indices)
            names <- dimnames(oc)[[1]]
            for (name in names) {
              row <- list()
              row[['value']] <- oc[name, 1]
              table$addRow(rowKey = name, values = row)
            }
          }
          
          if (isTRUE(self$options$kp)) {
            set.seed(1234)
            # Gower distance---
            proto <- clustMixType::kproto(dat, k = k1, type = 'gower')
            
            # Matrix with distances---
            #self$results$text1$setContent(proto$dists)
            
            # Table of Gower distance---
            table <- self$results$kp
            mat <- data.frame(proto$dists)
            colnames(mat) <-  paste0("Cluster", seq_along(colnames(mat)))
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
            #cluster number
            gn <- proto$cluster
            self$results$clust1$setValues(gn)
            self$results$clust1$setRowNums(rownames(data))
          }
          
          if (isTRUE(self$options$plot4)) {
            #require(cluster)
            gower_dist <- stats::as.dist(proto$dists)
            sil <- cluster::silhouette(gn, gower_dist)
            sil_data <- data.frame(cluster = factor(sil[, 1]),
                                   silhouette_width = sil[, 3])
            agg_sil <- stats::aggregate(silhouette_width ~ cluster, data = sil_data, mean)
            agg_sil$cluster_num <- as.integer(as.character(agg_sil$cluster))
            agg_sil <- agg_sil[order(agg_sil$cluster_num), ]
            
            image4 <- self$results$plot4
            image4$setState(agg_sil)
          }
        }
      },
      
      # Plot of means across groups---
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
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
      
      # Optimal number of clusters------
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        # read data ----
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]])
        plotData1 <- image1$state
        plot1 <-
          factoextra::fviz_nbclust(plotData1, stats::kmeans, method = "gap_stat")
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
      
      # cluster plot------
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        for (i in seq_along(vars))
          data[[i]] <- jmvcore::toNumeric(data[[i]])
        km.res <- image2$state
        plot2 <-
          factoextra::fviz_cluster(
            km.res,
            data = data,
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
          factoextra::fviz_pca_var(res.pca, col.var = grp, # palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
                                   legend.title = "Cluster")
        plot3 <- plot3 + ggtheme
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        
        agg_sil <- image4$state
        #random_color <- sample(c("red", "green", "blue", "orange", "purple"), 1)
        
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
