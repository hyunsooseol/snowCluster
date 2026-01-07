
# This file is a generated template, your changes will not be overwritten

dbscanClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class(
    "dbscanClass",
    inherit = dbscanBase,
    private = list(
      
      .X      = NULL,     # numeric matrix for PCA plotting (when applicable)
      .fit    = NULL,     # dbscan::dbscan() result
      .coords = NULL,     # 2D coordinates for plotting (PCA or MDS)
      .dist   = NULL,     # distance object (for gower/binary; used for MDS & silhouette)
      .kinfo  = NULL,     # list(kdist, k, epsUsed, auto, q) for kNN plot & auto-eps
      .profColsAdded = FALSE,
      .lastProfVars  = NULL,
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
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
      },

      # ---------------------------------------------------------
      # Main entry
      # ---------------------------------------------------------
      .run = function() {
        
        # 0) package check
        if (!requireNamespace("dbscan", quietly = TRUE))
          jmvcore::reject('The "dbscan" package is required but not installed.')
        
        # 1) variables check
        if (is.null(self$options$vars) || length(self$options$vars) < 1) {
          if (!is.null(self$results$text))
            self$results$text$setContent("Select one or more variables to run DBSCAN.")
          return()
        }
        
        # 2) complete-case filtering (keep original row mapping)
        dataAll <- self$data
        sel     <- jmvcore::select(dataAll, self$options$vars)
        keep    <- stats::complete.cases(sel)
        if (!any(keep))
          jmvcore::reject("No rows remaining after removing missing values.")
        dat <- as.data.frame(sel[keep, , drop = FALSE])
        
        # track numeric vars for profile table rebuild
        numVars <- names(Filter(is.numeric, dat))
        if (!identical(private$.lastProfVars, numVars)) {
          private$.profColsAdded <- FALSE
          private$.lastProfVars  <- numVars
        }
        
        # 3) options
        metric <- self$options$dist
        minPts <- max(1L, as.integer(self$options$minPts))
        n <- nrow(dat)
        if (minPts >= n) {
          minPts <- n - 1L
          if (minPts < 1L) jmvcore::reject("minPts must be < number of complete rows.")
        }
        
        # reset caches
        private$.fit  <- NULL
        private$.X    <- NULL
        private$.dist <- NULL
        private$.coords <- NULL
        private$.kinfo  <- NULL
        
        # ---------- Build representation ----------
        if (metric == "gower") {
          # Gower distance for mixed data
          if (!requireNamespace("cluster", quietly = TRUE))
            jmvcore::reject('The "cluster" package is required for Gower distance.')
          d <- cluster::daisy(dat, metric = "gower", stand = isTRUE(self$options$standardize))
          d <- stats::as.dist(d)
          private$.dist <- d
          
        } else if (metric == "binary") {
          # Hamming distance for 0/1 or logical variables
          dat_bin <- dat
          for (j in seq_along(dat_bin)) {
            x <- dat_bin[[j]]
            if (is.logical(x)) {
              dat_bin[[j]] <- as.integer(x)
            } else if (is.numeric(x)) {
              ux <- unique(x[!is.na(x)])
              if (!all(ux %in% c(0, 1)))
                jmvcore::reject(paste0("Variable '", colnames(dat_bin)[j],
                                       "' is not binary (must be 0/1)."))
            } else {
              jmvcore::reject(paste0("Variable '", colnames(dat_bin)[j],
                                     "' is not numeric/logical for binary distance."))
            }
          }
          Xb <- as.matrix(dat_bin)
          private$.dist <- stats::dist(Xb, method = "binary")  # Hamming
          
        } else if (metric %in% c("manhattan", "maximum")) {
          # Numeric-only distances
          if (any(!vapply(dat, is.numeric, logical(1))))
            jmvcore::reject("Selected variables must be numeric for Manhattan/Maximum.")
          X <- as.matrix(dat)
          if (isTRUE(self$options$standardize)) X <- scale(X)
          private$.X    <- X
          private$.dist <- stats::dist(X, method = metric)
          
        } else { # euclidean (fast path)
          if (any(!vapply(dat, is.numeric, logical(1))))
            jmvcore::reject("Selected variables must be numeric for Euclidean distance.")
          X <- as.matrix(dat)
          if (isTRUE(self$options$standardize)) X <- scale(X)
          private$.X <- X
          # we can skip building full dist for kNNdist on X
        }
        
        # ---------- kNN-distance vector ----------
        kdist <- NULL
        if (!is.null(private$.X)) {
          # fast path using data matrix
          kdist <- dbscan::kNNdist(private$.X, k = minPts)
        } else if (!is.null(private$.dist)) {
          # compute k-th positive distance from full matrix (OK for teaching-size n)
          Dm <- as.matrix(private$.dist)
          kdist <- apply(Dm, 1L, function(v) {
            vv <- v[v > 0]
            if (length(vv) < minPts) return(NA_real_)
            sort(vv, partial = minPts)[minPts]
          })
        }
        
        # ---------- choose eps (auto or user) ----------
        epsUsed <- self$options$eps
        autoFlag <- isTRUE(self$options$autoEps) && is.numeric(kdist) && !all(is.na(kdist))
        if (autoFlag) {
          q <- min(max(as.numeric(self$options$epsQuantile), 0.5), 0.999)
          epsUsed <- as.numeric(stats::quantile(kdist, probs = q, na.rm = TRUE))
          private$.kinfo <- list(kdist = kdist, k = minPts, epsUsed = epsUsed, auto = TRUE, q = q)
        } else {
          private$.kinfo <- list(kdist = kdist, k = minPts, epsUsed = epsUsed, auto = FALSE, q = NA_real_)
        }
        
        # ---------- fit DBSCAN with epsUsed ----------
        if (!is.null(private$.dist)) {
          private$.fit <- dbscan::dbscan(private$.dist, eps = epsUsed, minPts = minPts)
        } else if (!is.null(private$.X)) {
          private$.fit <- dbscan::dbscan(private$.X, eps = epsUsed, minPts = minPts)
        } else {
          jmvcore::reject("Internal error: no data or distance for DBSCAN.")
        }
        
        # ---------- membership to datasheet ----------
        cl <- private$.fit$cluster  # 0 = noise
        cm_out <- rep(NA_integer_, nrow(dataAll))
        cm_out[keep] <- cl
        private$.populateOutputs(list(cm = cm_out))
        
        # ---------- Cluster Sizes (include noise = 0) ----------
        if (!is.null(self$results$sizeTable)) {
          tab <- table(cl, useNA = "no")   # includes 0 (noise)
          denom <- sum(tab)                # proportions over ALL points
          if (length(tab) > 0 && denom > 0) {
            st <- self$results$sizeTable
            for (cid_chr in names(tab)) {
              cid <- as.integer(cid_chr)   # 0 for noise
              n_i <- as.integer(tab[[cid_chr]])
              st$addRow(
                rowKey = cid,
                values = list(
                  cluster    = cid,
                  n          = n_i,
                  proportion = n_i / denom
                )
              )
            }
          }
        }
        
        # ---------- Summary ----------
        if (!is.null(self$results$text)) {
          k      <- length(unique(cl[cl > 0]))
          noiseN <- sum(cl == 0)
          sz     <- table(cl[cl > 0])
          szStr  <- if (length(sz) > 0)
            paste(paste0("C", names(sz), " = ", as.integer(sz)), collapse = "\n")
          else "(no clusters)"
          
          autoLine <- if (isTRUE(self$options$autoEps) && !is.null(private$.kinfo) && private$.kinfo$auto)
            paste0(" (auto from kNN q=", private$.kinfo$q, ")")
          else ""
          
          summ <- paste0(
            "DBSCAN summary\n",
            "------------------------------\n",
            "N (complete) = ", sum(keep), ", p = ", ncol(dat), "\n",
            "eps (used) = ", signif(epsUsed, 6), autoLine, ", minPts = ", minPts, "\n",
            "distance = ", metric, ", standardize = ", isTRUE(self$options$standardize), "\n",
            "clusters found = ", k, "\n",
            "noise points = ", noiseN, "\n\n",
            "Cluster sizes:\n", szStr
          )
          self$results$text$setContent(summ)
        }
        
        # ---------- Auto ε details table ----------
        if (isTRUE(self$options$autoEps) && !is.null(self$results$autoTable)) {
          at <- self$results$autoTable
          qv <- if (!is.null(private$.kinfo) && !is.null(private$.kinfo$q)) private$.kinfo$q else NA_real_
          at$addRow(rowKey = 1, values = list(
            eps_used = as.numeric(if (!is.null(private$.kinfo$epsUsed)) private$.kinfo$epsUsed else self$options$eps),
            quantile = as.numeric(qv),
            k        = as.integer(minPts),
            n        = as.integer(sum(keep))
          ))
        }
        
        # ---------- Cluster Profiles ----------
        if (isTRUE(self$options$profileTable) && !is.null(self$results$profileTable)) {
          
          pt <- self$results$profileTable
          statChoice <- self$options$prostat
          statFun <- switch(statChoice,
                            mean   = function(x) base::mean(x, na.rm = TRUE),
                            median = function(x) stats::median(x, na.rm = TRUE),
                            function(x) base::mean(x, na.rm = TRUE))
          dat_num <- dat[, names(Filter(is.numeric, dat)), drop = FALSE]
          
          if (!isTRUE(private$.profColsAdded)) {
            if (ncol(dat_num) > 0)
              for (vn in colnames(dat_num)) pt$addColumn(name = vn, title = vn, type = "number")
            private$.profColsAdded <- TRUE
          }
          
          idx_non <- cl > 0
          if (any(idx_non) && ncol(dat_num) > 0) {
            prof <- stats::aggregate(dat_num[idx_non, , drop = FALSE],
                                     by = list(cluster = cl[idx_non]),
                                     FUN = statFun)
            for (r in seq_len(nrow(prof))) {
              values <- list(cluster = as.integer(prof$cluster[r]))
              for (vn in colnames(dat_num))
                values[[vn]] <- as.numeric(prof[r, vn])
              pt$addRow(rowKey = as.integer(prof$cluster[r]), values = values)
            }
          }
        }
        
        # ---------- Quality indices (CheckBox 'quality' + ComboBox 'qoption') ----------
        if (isTRUE(self$options$quality) && !is.null(self$results$qualityTable)) {
          qopt <- self$options$qoption
          if (!is.null(qopt) && length(qopt) == 1L && qopt %in% c("sil", "dbi", "both")) {
            
            qt <- self$results$qualityTable
            idx_non <- cl > 0
            k_non   <- length(unique(cl[idx_non]))
            n_non   <- sum(idx_non)
            
            # -- Silhouette (mean): needs 'cluster' and a distance
            if (qopt %in% c("sil", "both") && k_non >= 2 && n_non >= 2) {
              if (!requireNamespace("cluster", quietly = TRUE))
                jmvcore::reject('The "cluster" package is required for silhouette.')
              
              # prefer the distance used for clustering; else build on X
              D <- NULL
              if (!is.null(private$.dist)) {
                D <- private$.dist
              } else if (!is.null(private$.X)) {
                meth <- if (metric %in% c("euclidean","manhattan","maximum","binary"))
                  metric else "euclidean"
                D <- stats::dist(private$.X, method = meth)
              }
              if (!is.null(D)) {
                Dm <- as.matrix(D)[idx_non, idx_non, drop = FALSE]
                cl_sub <- cl[idx_non]
                if (nrow(Dm) >= 2 && length(unique(cl_sub)) >= 2) {
                  sil <- try(cluster::silhouette(cl_sub, stats::as.dist(Dm)), silent = TRUE)
                  val <- if (!inherits(sil, "try-error")) base::mean(sil[,3], na.rm = TRUE) else NA_real_
                  qt$addRow(rowKey = "sil", values = list(metric = "Silhouette (mean)", value = val))
                } else {
                  qt$addRow(rowKey = "sil", values = list(metric = "Silhouette (mean)", value = NA_real_))
                }
              } else {
                qt$addRow(rowKey = "sil", values = list(metric = "Silhouette (mean)", value = NA_real_))
              }
            }
            
            # -- Davies–Bouldin (internal): numeric X only
            if (qopt %in% c("dbi", "both") && k_non >= 2 && !is.null(private$.X)) {
              X_sub  <- private$.X[idx_non, , drop = FALSE]
              cl_sub <- as.integer(cl[idx_non])
              dbi_metric <- if (metric %in% c("euclidean","manhattan","maximum"))
                metric else "euclidean"
              dbv <- private$.dbiInternal(X_sub, cl_sub, dbi_metric)
              qt$addRow(rowKey = "dbi", values = list(metric = "Davies–Bouldin", value = dbv))
            }
          }
        }
        
        # ---------- Coordinates for plotting ----------
        if (isTRUE(self$options$plot)) {
          if (!is.null(private$.dist)) {
            # MDS on precomputed distance (gower/binary/manhattan/maximum path if no X)
            coords <- try(stats::cmdscale(private$.dist, k = 2), silent = TRUE)
            if (!inherits(coords, "try-error"))
              private$.coords <- cbind(PC1 = coords[, 1], PC2 = coords[, 2])
          } else if (!is.null(private$.X)) {
            # PCA on numeric matrix
            private$.coords <- private$.makeCoords(private$.X)
          }
        }
      },
      
      # ---------------------------------------------------------
      # Datasheet outputs
      # ---------------------------------------------------------
      .populateOutputs = function(results) {
        cm <- results$cm
        if (isTRUE(self$options$cm) && self$results$cm$isNotFilled()) {
          rn <- rownames(self$data); if (is.null(rn)) rn <- as.character(seq_len(nrow(self$data)))
          self$results$cm$setRowNums(rn)
          self$results$cm$setValues(cm)
        }
      },
      
      # ---------------------------------------------------------
      # PCA coords helper
      # ---------------------------------------------------------
      .makeCoords = function(X) {
        if (ncol(X) >= 2) {
          pc <- stats::prcomp(X, center = FALSE, scale. = FALSE)
          cbind(PC1 = pc$x[, 1], PC2 = pc$x[, 2])
        } else {
          cbind(PC1 = as.numeric(X[, 1]), PC2 = rep(0, nrow(X)))
        }
      },
      
      # ---------------------------------------------------------
      # Internal: vector distance for DBI (euclidean/manhattan/maximum)
      # ---------------------------------------------------------
      .vecDist = function(a, b, metric = "euclidean") {
        if (metric == "manhattan")
          return(sum(abs(a - b)))
        if (metric == "maximum")
          return(max(abs(a - b)))
        # default euclidean
        sqrt(sum((a - b) ^ 2))
      },
      
      # ---------------------------------------------------------
      # Internal: Davies–Bouldin index (no clusterCrit)
      # ---------------------------------------------------------
      .dbiInternal = function(X, labels, metric = "euclidean") {
        labs <- as.integer(labels)
        clus <- sort(unique(labs))
        K <- length(clus)
        if (K < 2L || nrow(X) < 2L)
          return(NA_real_)
        
        # centroids (mean vector)
        cents <- lapply(clus, function(k) colMeans(X[labs == k, , drop = FALSE]))
        
        # intra-cluster scatter S_i: mean distance to centroid
        S <- numeric(K)
        for (i in seq_len(K)) {
          Xi <- X[labs == clus[i], , drop = FALSE]
          ci <- cents[[i]]
          if (nrow(Xi) == 1L) {
            S[i] <- 0
          } else {
            di <- apply(Xi, 1L, function(row) private$.vecDist(row, ci, metric))
            S[i] <- mean(di)
          }
        }
        
        # between-centroid distances
        R <- numeric(K)
        for (i in seq_len(K)) {
          rmax <- -Inf
          for (j in seq_len(K)) {
            if (i == j) next
            Mij <- private$.vecDist(cents[[i]], cents[[j]], metric)
            rij <- if (Mij <= 0) Inf else (S[i] + S[j]) / Mij
            if (rij > rmax) rmax <- rij
          }
          R[i] <- rmax
        }
        mean(R)
      },
      
      # ---------------------------------------------------------
      # Cluster plot renderer
      # ---------------------------------------------------------
      .plot = function(image, ...) {
        if (!isTRUE(self$options$plot))
          return(FALSE)
        if (is.null(private$.fit) || is.null(private$.coords))
          return(FALSE)
        if (!requireNamespace("ggplot2", quietly = TRUE))
          jmvcore::reject('The "ggplot2" package is required but not installed.')
        
        coords <- as.data.frame(private$.coords)
        cl <- private$.fit$cluster
        coords$clusterLabel <- ifelse(cl == 0, "Noise", paste0("C", cl))
        coords$isNoise <- cl == 0
        
        ggplot2::theme_set(ggplot2::theme_minimal())
        p <- ggplot2::ggplot(coords, ggplot2::aes(x = PC1, y = PC2)) +
          ggplot2::geom_point(
            ggplot2::aes(color = clusterLabel, shape = isNoise),
            alpha = 0.9, size = 2.2, stroke = 0.6
          ) +
          ggplot2::scale_shape_manual(values = c(`TRUE` = 4, `FALSE` = 16), guide = "none") +
          ggplot2::labs(x = "PC1", y = "PC2", color = "Cluster")
        
        print(p)
        TRUE
      },
      
      # ---------------------------------------------------------
      # kNN-distance plot renderer
      # ---------------------------------------------------------
      .plotKNN = function(image, ...) {
        if (is.null(private$.kinfo) || is.null(private$.kinfo$kdist))
          return(FALSE)
        if (!requireNamespace("ggplot2", quietly = TRUE))
          jmvcore::reject('The "ggplot2" package is required but not installed.')
        
        kd <- private$.kinfo$kdist
        if (!is.numeric(kd) || length(kd) == 0L || all(is.na(kd)))
          return(FALSE)
        
        valid <- is.finite(kd)
        if (!any(valid))
          return(FALSE)
        
        # sorted kNN distances (ascending)
        kd_sorted <- sort(kd[valid], decreasing = FALSE)
        df <- data.frame(idx = seq_along(kd_sorted), kdist = kd_sorted)
        
        ggplot2::theme_set(ggplot2::theme_minimal())
        p <- ggplot2::ggplot(df, ggplot2::aes(x = idx, y = kdist)) +
          ggplot2::geom_line() +
          ggplot2::labs(
            x = "Points (sorted by kNN distance)",
            y = paste0(private$.kinfo$k, "-NN distance")
          )
        
        if (!is.null(private$.kinfo$epsUsed) &&
            is.finite(private$.kinfo$epsUsed)) {
          p <- p + ggplot2::geom_hline(
            yintercept = private$.kinfo$epsUsed,
            linetype = "dashed"
          )
        }
        
        print(p)
        TRUE
      }
    )
  )
