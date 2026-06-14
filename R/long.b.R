
# Trajectory k-means

longClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "longClass",
  inherit = longBase,
  private = list(
    .htmlwidget = NULL,
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      
      self$results$instructions$setVisible(visible = TRUE)

      self$results$instructions$setContent(
        private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Select numeric repeated-measure variables in wide format, then click <b>Run</b>.</li>',
            '<li>Each row is treated as one subject trajectory.</li>',
            '<li>Use the elbow plot and silhouette values to help choose the number of clusters.</li>',
            '<li>The saved <b>Cluster</b> variable can be used for follow-up analyses.</li>',
            '<li>This is an exploratory clustering method, not a latent growth mixture model.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul>',
            
            '</div></div>'
          )
        )
      )
      
      
    },
    
    .run = function() {
      
      # ------------------------------------------------------------
      # Run button guard
      # ------------------------------------------------------------
      
      if (is.null(self$options$run) || self$options$run == 0)
        return()
      
      # ------------------------------------------------------------
      # Basic checks
      # ------------------------------------------------------------
      
      vars <- self$options$vars
      
      if (is.null(vars) || length(vars) < 2) {
        private$.setError(
          "Please select at least two repeated-measure variables."
        )
        return()
      }
      
      dat0 <- self$data
      x0 <- dat0[, vars, drop = FALSE]
      
      for (v in vars) {
        if (!is.numeric(x0[[v]])) {
          private$.setError(
            "All repeated-measure variables must be numeric."
          )
          return()
        }
      }
      
      # ------------------------------------------------------------
      # Missing value handling
      # ------------------------------------------------------------
      
      x_original <- x0
      rowIndex <- seq_len(nrow(x0))
      
      if (self$options$missing == "listwise") {
        
        keep <- stats::complete.cases(x0)
        
        x_original <- x0[keep, , drop = FALSE]
        rowIndex <- rowIndex[keep]
        
      } else if (self$options$missing == "mean") {
        
        # Exclude rows with all repeated-measure values missing
        keep <- rowSums(!is.na(x0)) > 0
        
        x_original <- x0[keep, , drop = FALSE]
        rowIndex <- rowIndex[keep]
        
        for (j in seq_along(vars)) {
          m <- mean(x_original[[j]], na.rm = TRUE)
          
          if (is.nan(m)) {
            private$.setError(
              paste0(
                "Variable '", vars[j],
                "' contains only missing values."
              )
            )
            return()
          }
          
          x_original[[j]][is.na(x_original[[j]])] <- m
        }
      }
      
      
      n <- nrow(x_original)
      p <- ncol(x_original)
      k <- self$options$nclust
      
      if (n == 0) {
        private$.setError(
          "No valid cases are available after missing value handling."
        )
        return()
      }
      
      if (n < k) {
        private$.setError(
          "The number of valid cases must be greater than or equal to the number of clusters."
        )
        return()
      }
      
      if (k < 2) {
        private$.setError(
          "The number of clusters must be at least 2."
        )
        return()
      }
      
      x_cluster <- as.data.frame(x_original)
      
      # ------------------------------------------------------------
      # Standardization
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$standardize)) {
        
        sds <- vapply(x_cluster, stats::sd, numeric(1), na.rm = TRUE)
        
        if (any(is.na(sds) | sds == 0)) {
          bad <- vars[which(is.na(sds) | sds == 0)]
          
          private$.setError(
            paste0(
              "The following variable(s) have zero or undefined standard deviation: ",
              paste(bad, collapse = ", "),
              ". Please remove them or turn off standardization."
            )
          )
          return()
        }
        
        x_cluster <- as.data.frame(scale(x_cluster))
      }
      
      xmat <- as.matrix(x_cluster)
      
      # ------------------------------------------------------------
      # K-means
      # ------------------------------------------------------------
      
      set.seed(self$options$seed)
      
      km <- tryCatch(
        stats::kmeans(
          x = xmat,
          centers = k,
          iter.max = self$options$itermax,
          nstart = self$options$nstart,
          algorithm = self$options$algorithm
        ),
        error = function(e) e
      )
      
      if (inherits(km, "error")) {
        private$.setError(
          paste0("K-means failed: ", km$message)
        )
        return()
      }
      
      cluster <- km$cluster
      clusterFactor <- factor(cluster, levels = seq_len(k))
      sizes <- as.integer(table(clusterFactor))
      
      private$.handleMembership(
        cluster = cluster,
        analysisRows = rowIndex,
        nOriginal = nrow(self$data)
      )
      
      sil <- private$.computeSilhouette(
        xmat = xmat,
        cluster = cluster,
        k = k
      )
      
      if (isTRUE(self$options$showSilPlot)) {
        
        self$results$silplot$setState(
          list(
            values = sil$values,
            cluster = cluster,
            k = k,
            overall = sil$overall,
            message = NULL
          )
        )
      }
      
      # ------------------------------------------------------------
      # Cluster summary
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$showSummary)) {
        
        for (i in seq_len(k)) {
          
          self$results$summary$addRow(
            rowKey = paste0("cluster", i),
            values = list(
              cluster = paste0("Cluster ", i),
              n = sizes[i],
              percent = 100 * sizes[i] / n
            )
          )
        }
      }
      
      # ------------------------------------------------------------
      # Fit indices
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$showFit)) {
        
        fitRows <- list(
          list(index = "Number of cases", value = n),
          list(index = "Number of time points", value = p),
          list(index = "Number of clusters", value = k),
          list(index = "Total sum of squares", value = km$totss),
          list(index = "Within-cluster sum of squares", value = km$tot.withinss),
          list(index = "Between-cluster sum of squares", value = km$betweenss),
          list(index = "Between SS / Total SS", value = km$betweenss / km$totss),
          list(index = "Average silhouette width", value = sil$overall),
          list(index = "Random starts", value = self$options$nstart),
          list(index = "Maximum iterations", value = self$options$itermax)
        )
        
        for (i in seq_along(fitRows)) {
          
          self$results$fit$addRow(
            rowKey = paste0("fit", i),
            values = fitRows[[i]]
          )
        }
      }
      
      # ------------------------------------------------------------
      # Cluster centers
      # ------------------------------------------------------------
      
      centersOriginal <- private$.clusterMeans(
        x = x_original,
        cluster = cluster,
        k = k
      )
      
      centersStandardized <- private$.clusterMeans(
        x = as.data.frame(xmat),
        cluster = cluster,
        k = k
      )
      
      if (identical(self$options$centerScale, "standardized")) {
        centersDisplay <- centersStandardized
      } else {
        centersDisplay <- centersOriginal
      }
      
      
      if (isTRUE(self$options$showCenters)) {
        
        private$.prepareCentersTable(vars)
        
        for (i in seq_len(k)) {
          
          values <- list(
            cluster = paste0("Cluster ", i)
          )
          
          for (j in seq_along(vars)) {
            values[[paste0("time", j)]] <- centersDisplay[i, j]
          }
          
          self$results$centers$addRow(
            rowKey = paste0("cluster", i),
            values = values
          )
        }
      }
      
      # ------------------------------------------------------------
      # Silhouette summary
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$showSilhouette)) {
        
        for (i in seq_len(k)) {
          
          self$results$silhouette$addRow(
            rowKey = paste0("cluster", i),
            values = list(
              cluster = paste0("Cluster ", i),
              n = sizes[i],
              avgSilhouette = sil$byCluster[i]
            )
          )
        }
        
        self$results$silhouette$addRow(
          rowKey = "overall",
          values = list(
            cluster = "Overall",
            n = n,
            avgSilhouette = sil$overall
          )
        )
      }
      
      
      # ------------------------------------------------------------
      # Trajectory plot state: original scale
      # ------------------------------------------------------------
      
      plotState <- list(
        x = as.matrix(x_original),
        vars = vars,
        cluster = cluster,
        centers = centersOriginal,
        k = k,
        plotType = self$options$plotType,
        lineAlpha = self$options$lineAlpha,
        ylab = "Value",
        main = "Trajectory K-Means (Original Scale)",
        message = NULL
      )
      
      self$results$plot$setState(plotState)
      
      # ------------------------------------------------------------
      # Trajectory plot state: z-score scale
      # ------------------------------------------------------------
      
      sds_z <- vapply(x_original, stats::sd, numeric(1), na.rm = TRUE)
      
      if (any(is.na(sds_z) | sds_z == 0)) {
        
        zMessage <- paste0(
          "Z-score trajectory plot cannot be drawn because the following variable(s) ",
          "have zero or undefined standard deviation: ",
          paste(vars[which(is.na(sds_z) | sds_z == 0)], collapse = ", "),
          "."
        )
        
        self$results$plot1$setState(
          list(message = zMessage)
        )
        
      } else {
        
        x_z <- as.data.frame(scale(x_original))
        
        centersZ <- private$.clusterMeans(
          x = x_z,
          cluster = cluster,
          k = k
        )
        
        plotStateZ <- list(
          x = as.matrix(x_z),
          vars = vars,
          cluster = cluster,
          centers = centersZ,
          k = k,
          plotType = self$options$plotType,
          lineAlpha = self$options$lineAlpha,
          ylab = "Z-score",
          main = "Trajectory K-Means (Z-score Scale)",
          message = NULL
        )
        
        self$results$plot1$setState(plotStateZ)
      }
      
      # ------------------------------------------------------------
      # Elbow plot state
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$showElbow)) {
        
        maxK <- self$options$maxK
        maxK <- min(maxK, n - 1)
        
        if (maxK < 2) {
          
          self$results$elbow$setState(
            list(
              message = "Elbow plot requires at least three valid cases."
            )
          )
          
        } else {
          
          elbow <- private$.computeElbow(
            xmat = xmat,
            maxK = maxK,
            nstart = self$options$nstart,
            itermax = self$options$itermax,
            algorithm = self$options$algorithm,
            seed = self$options$seed
          )
          
          self$results$elbow$setState(
            list(
              elbow = elbow,
              message = NULL
            )
          )
        }
      }
    },
    
    # ------------------------------------------------------------
    # Helper: save cluster membership
    # ------------------------------------------------------------
    
    .handleMembership = function(cluster, analysisRows, nOriginal) {
      
      if (!isTRUE(self$options$member))
        return()
      
      if (!self$results$member$isNotFilled())
        return()
      
      mem_vec <- rep(NA_integer_, nOriginal)
      mem_vec[analysisRows] <- cluster
      
      self$results$member$setRowNums(
        rownames(self$data)
      )
      
      self$results$member$setValues(
        mem_vec
      )
    },
    
    # ------------------------------------------------------------
    # Helper: error message
    # ------------------------------------------------------------
    
    .setError = function(message) {
      
      self$results$instructions$setContent(
        paste0(
          "<div style='padding: 10px; color: #b00020;'>",
          "<b>Analysis cannot be performed.</b><br><br>",
          message,
          "</div>"
        )
      )
      
      self$results$plot$setState(
        list(message = message)
      )
      
      self$results$plot1$setState(
        list(message = message)
      )
      
      self$results$elbow$setState(
        list(message = message)
      )
    
      self$results$silplot$setState(
        list(message = message)
      )
      
      },
    
    # ------------------------------------------------------------
    # Helper: cluster means on original scale
    # ------------------------------------------------------------
    
    .clusterMeans = function(x, cluster, k) {
      
      xmat <- as.matrix(x)
      p <- ncol(xmat)
      
      out <- matrix(NA_real_, nrow = k, ncol = p)
      colnames(out) <- colnames(xmat)
      
      for (i in seq_len(k)) {
        idx <- cluster == i
        
        if (any(idx)) {
          out[i, ] <- colMeans(xmat[idx, , drop = FALSE], na.rm = TRUE)
        }
      }
      
      out
    },
    
    # ------------------------------------------------------------
    # Helper: silhouette width
    # ------------------------------------------------------------
    
    .computeSilhouette = function(xmat, cluster, k) {
      
      n <- nrow(xmat)
      
      out <- list(
        values = rep(NA_real_, n),
        byCluster = rep(NA_real_, k),
        overall = NA_real_
      )
      
      if (n <= k || k < 2)
        return(out)
      
      d <- as.matrix(stats::dist(xmat))
      
      sil <- rep(NA_real_, n)
      
      for (i in seq_len(n)) {
        
        ci <- cluster[i]
        same <- which(cluster == ci)
        same <- same[same != i]
        
        if (length(same) == 0) {
          sil[i] <- 0
          next
        }
        
        a <- mean(d[i, same])
        
        b <- Inf
        
        otherClusters <- setdiff(seq_len(k), ci)
        
        for (cj in otherClusters) {
          
          other <- which(cluster == cj)
          
          if (length(other) > 0) {
            b <- min(b, mean(d[i, other]))
          }
        }
        
        if (!is.finite(b)) {
          sil[i] <- NA_real_
        } else {
          sil[i] <- (b - a) / max(a, b)
        }
      }
      
      byCluster <- rep(NA_real_, k)
      
      for (cj in seq_len(k)) {
        byCluster[cj] <- mean(sil[cluster == cj], na.rm = TRUE)
      }
      
      out$values <- sil
      out$byCluster <- byCluster
      out$overall <- mean(sil, na.rm = TRUE)
      
      out
    },
    
    # ------------------------------------------------------------
    # Helper: dynamic center table columns
    # ------------------------------------------------------------
    
    .prepareCentersTable = function(vars) {
      
      for (j in seq_along(vars)) {
        
        try(
          self$results$centers$addColumn(
            name = paste0("time", j),
            title = vars[j],
            type = "number"
          ),
          silent = TRUE
        )
      }
    },
    
    # ------------------------------------------------------------
    # Helper: elbow computation
    # ------------------------------------------------------------
    
    .computeElbow = function(xmat, maxK, nstart, itermax, algorithm, seed) {
      
      ks <- seq.int(2, maxK)
      wss <- rep(NA_real_, length(ks))
      
      for (i in seq_along(ks)) {
        
        set.seed(seed)
        
        fit <- tryCatch(
          stats::kmeans(
            x = xmat,
            centers = ks[i],
            iter.max = itermax,
            nstart = nstart,
            algorithm = algorithm
          ),
          error = function(e) NULL
        )
        
        if (!is.null(fit))
          wss[i] <- fit$tot.withinss
      }
      
      data.frame(
        k = ks,
        wss = wss
      )
    },
    
    # ------------------------------------------------------------
    # Trajectory plot
    # ------------------------------------------------------------
    
    .plot = function(image, ggtheme, theme, ...) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (!is.null(state$message)) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = paste(strwrap(state$message, width = 60), collapse = "\n"),
          cex = 0.9
        )
        return(TRUE)
      }
      
      x <- state$x
      vars <- state$vars
      cluster <- state$cluster
      centers <- state$centers
      k <- state$k
      plotType <- state$plotType
      lineAlpha <- state$lineAlpha
      
      time <- seq_along(vars)
      ylim <- range(x, centers, na.rm = TRUE)
      
      angle <- self$options$angle
      if (is.null(angle))
        angle <- 0
      
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op), add = TRUE)
      
      graphics::par(
        mar = c(6, 5, 4, 8),
        xpd = TRUE
      )
      
      graphics::plot(
        x = time,
        y = centers[1, ],
        type = "n",
        xaxt = "n",
        xlab = "Time point",
        ylab = "Value",
        ylim = ylim,
        main = "Trajectory K-Means"
      )
      
      # x-axis ticks
      graphics::axis(
        side = 1,
        at = time,
        labels = FALSE
      )
      
      # x-axis labels with optional angle
      if (angle > 0) {
        graphics::text(
          x = time,
          y = graphics::par("usr")[3] - 0.06 * diff(graphics::par("usr")[3:4]),
          labels = vars,
          srt = angle,
          adj = 1,
          xpd = TRUE,
          cex = 0.8
        )
      } else {
        graphics::axis(
          side = 1,
          at = time,
          labels = vars,
          las = 1,
          cex.axis = 0.8
        )
      }
      
      graphics::grid()
      
      cols <- grDevices::rainbow(k)
      
      if (plotType == "individual") {
        
        for (i in seq_len(nrow(x))) {
          cl <- cluster[i]
          
          graphics::lines(
            x = time,
            y = x[i, ],
            col = grDevices::adjustcolor(cols[cl], alpha.f = lineAlpha),
            lwd = 1
          )
        }
      }
      
      for (cl in seq_len(k)) {
        graphics::lines(
          x = time,
          y = centers[cl, ],
          col = cols[cl],
          lwd = 3
        )
        
        graphics::points(
          x = time,
          y = centers[cl, ],
          col = cols[cl],
          pch = 19,
          cex = 1.1
        )
      }
      
      graphics::legend(
        x = max(time) + 0.25,
        y = ylim[2],
        legend = paste0("Cluster ", seq_len(k)),
        col = cols,
        lwd = 3,
        pch = 19,
        bty = "n",
        xpd = TRUE
      )
      
      return(TRUE)
    },
    
    
    # ------------------------------------------------------------
    # Trajectory plot: z-score scale
    # ------------------------------------------------------------
    
    .plot1 = function(image, ggtheme, theme, ...) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (!is.null(state$message)) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = paste(strwrap(state$message, width = 60), collapse = "\n"),
          cex = 0.9
        )
        return(TRUE)
      }
      
      x <- state$x
      vars <- state$vars
      cluster <- state$cluster
      centers <- state$centers
      k <- state$k
      plotType <- state$plotType
      lineAlpha <- state$lineAlpha
      
      ylab <- state$ylab
      main <- state$main
      
      if (is.null(ylab))
        ylab <- "Z-score"
      
      if (is.null(main))
        main <- "Trajectory K-Means (Z-score Scale)"
      
      time <- seq_along(vars)
      ylim <- range(x, centers, na.rm = TRUE)
      
      angle <- self$options$angle
      if (is.null(angle))
        angle <- 0
      
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op), add = TRUE)
      
      graphics::par(
        mar = c(6, 5, 4, 8),
        xpd = TRUE
      )
      
      graphics::plot(
        x = time,
        y = centers[1, ],
        type = "n",
        xaxt = "n",
        xlab = "Time point",
        ylab = ylab,
        ylim = ylim,
        main = main
      )
      
      # x-axis ticks
      graphics::axis(
        side = 1,
        at = time,
        labels = FALSE
      )
      
      # x-axis labels with optional angle
      if (angle > 0) {
        graphics::text(
          x = time,
          y = graphics::par("usr")[3] - 0.06 * diff(graphics::par("usr")[3:4]),
          labels = vars,
          srt = angle,
          adj = 1,
          xpd = TRUE,
          cex = 0.8
        )
      } else {
        graphics::axis(
          side = 1,
          at = time,
          labels = vars,
          las = 1,
          cex.axis = 0.8
        )
      }
      
      # Grid and zero reference line should stay inside the plot box
      graphics::par(xpd = FALSE)
      
      graphics::grid()
      
      usr <- graphics::par("usr")
      
      graphics::segments(
        x0 = usr[1],
        y0 = 0,
        x1 = usr[2],
        y1 = 0,
        lty = 2,
        lwd = 1.5,
        col = "gray40"
      )
      
      cols <- grDevices::rainbow(k)
      
      if (plotType == "individual") {
        
        for (i in seq_len(nrow(x))) {
          cl <- cluster[i]
          
          graphics::lines(
            x = time,
            y = x[i, ],
            col = grDevices::adjustcolor(cols[cl], alpha.f = lineAlpha),
            lwd = 1
          )
        }
      }
      
      for (cl in seq_len(k)) {
        
        graphics::lines(
          x = time,
          y = centers[cl, ],
          col = cols[cl],
          lwd = 3
        )
        
        graphics::points(
          x = time,
          y = centers[cl, ],
          col = cols[cl],
          pch = 19,
          cex = 1.1
        )
      }
      
      # Legend can be drawn outside the plot box
      graphics::par(xpd = TRUE)
      
      graphics::legend(
        x = max(time) + 0.25,
        y = ylim[2],
        legend = paste0("Cluster ", seq_len(k)),
        col = cols,
        lwd = 3,
        pch = 19,
        bty = "n",
        xpd = TRUE
      )
      
      return(TRUE)
    },
    
    # ------------------------------------------------------------
    # Elbow plot
    # ------------------------------------------------------------
    
    .elbow = function(image, ggtheme, theme, ...) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (!is.null(state$message)) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = paste(strwrap(state$message, width = 60), collapse = "\n"),
          cex = 0.9
        )
        return(TRUE)
      }
      
      elbow <- state$elbow
      
      if (is.null(elbow) || nrow(elbow) == 0) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = "No elbow plot data are available.",
          cex = 0.9
        )
        return(TRUE)
      }
      
      graphics::plot(
        x = elbow$k,
        y = elbow$wss,
        type = "b",
        pch = 19,
        xlab = "Number of clusters (K)",
        ylab = "Total within-cluster sum of squares",
        main = "Elbow Plot",
        xaxt = "n"
      )
      
      graphics::axis(
        side = 1,
        at = elbow$k
      )
      
      graphics::grid()
      
      return(TRUE)
    },
    
    # ------------------------------------------------------------
    # Silhouette plot
    # ------------------------------------------------------------
    
    .silplot = function(image, ggtheme, theme, ...) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (!is.null(state$message)) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = paste(strwrap(state$message, width = 60), collapse = "\n"),
          cex = 0.9
        )
        return(TRUE)
      }
      
      sil <- state$values
      cluster <- state$cluster
      k <- state$k
      overall <- state$overall
      
      if (is.null(sil) || length(sil) == 0 || all(is.na(sil))) {
        graphics::plot.new()
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = "No silhouette values are available.",
          cex = 0.9
        )
        return(TRUE)
      }
      
      ord <- order(cluster, -sil)
      silOrd <- sil[ord]
      clOrd <- cluster[ord]
      
      cols <- grDevices::rainbow(k)
      barCols <- cols[clOrd]
      
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op), add = TRUE)
      
      graphics::par(
        mar = c(5, 5, 4, 4),
        xpd = FALSE
      )
      
      graphics::barplot(
        height = silOrd,
        horiz = TRUE,
        names.arg = rep("", length(silOrd)),
        col = barCols,
        border = NA,
        xlim = c(-1, 1),
        xlab = "Silhouette width",
        ylab = "Cases grouped by cluster",
        main = "Silhouette Plot"
      )
      
      graphics::abline(
        v = 0,
        lty = 2
      )
      
      if (!is.na(overall)) {
        graphics::abline(
          v = overall,
          lwd = 2
        )
      }
      
      avgLabel <- ifelse(
        is.na(overall),
        "Average = NA",
        paste0("Average = ", sprintf("%.2f", overall))
      )
      
      graphics::legend(
        "topleft",
        inset = 0.02,
        legend = c(
          paste0("Cluster ", seq_len(k)),
          avgLabel
        ),
        col = c(cols, "black"),
        lwd = c(rep(5, k), 2),
        bty = "n",
        cex = 0.85
      )
      
      return(TRUE)
    }
    
    
  )
)
