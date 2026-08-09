

timeclustClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "timeclustClass",
    inherit = timeclustBase,
    private = list(
      .htmlwidget = NULL,
      
      # ------------------------------------------------------------------
      # Helper: Reset cluster table rows
      # ------------------------------------------------------------------
      .resetClusterTable = function(k) {
        
        if (is.null(k) || is.na(k) || k < 1)
          return()
        
        for (i in seq_len(k)) {
          self$results$clusterTable$setRow(
            rowNo = i,
            values = list(
              cluster    = NA,
              n_items    = NA,
              mean_value = NA,
              sd_value   = NA,
              min_value  = NA,
              max_value  = NA
            )
          )
        }
      },
      
      # ------------------------------------------------------------------
      # Helper: Reset silhouette table rows
      # ------------------------------------------------------------------
      .resetSilhouetteTable = function(k) {
        
        if (is.null(k) || is.na(k) || k < 1)
          return()
        
        for (i in seq_len(k)) {
          self$results$silhouetteTable$setRow(
            rowNo = i,
            values = list(
              cluster         = NA,
              n_items         = NA,
              mean_silhouette = NA
            )
          )
        }
      },
      
      # ------------------------------------------------------------------
      # Init
      # ------------------------------------------------------------------
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new()
        
        instruction_html <- private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Time-series clustering using the <b>widyr</b> approach.</li>',
            '<li>Select variables representing <b>time</b>, <b>item</b>, and <b>value</b>.</li>',
            '<li>Each item should have a value for every time point.</li>',
            '<li>The Elbow plot is provided as a reference for selecting the number of clusters.</li>',
            '<li><b>Standardize series</b> applies z-score standardization within each item before clustering. Use this option when the temporal pattern is more important than differences in the absolute level of the series.</li>',
            '<li>When series are standardized, clustering, the Elbow plot, and Silhouette analysis use the standardized values, while the cluster summary and time-series plots remain on the original value scale.</li>',
            '<li><b>Silhouette analysis</b> evaluates cluster cohesion and separation. Values closer to 1 indicate better clustering, values near 0 indicate overlapping clusters, and negative values suggest that an item may fit another cluster better.</li>',
            '<li>Report issues or requests on <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul>',
            '</div>',
            '</div>'
          )
        )
        
        self$results$instructions$setContent(instruction_html)
        
        if (!isTRUE(self$options$summary))
          self$results$clusterTable$setVisible(FALSE)
        
        if (!isTRUE(self$options$silhouette))
          self$results$silhouetteTable$setVisible(FALSE)
      },
      
      # ------------------------------------------------------------------
      # Run
      # ------------------------------------------------------------------
      .run = function() {
        
        if (is.null(self$data) ||
            is.null(self$options$item) ||
            is.null(self$options$feature) ||
            is.null(self$options$value))
          return()
        
        data <- as.data.frame(self$data)
        
        selected_vars <- unique(c(
          self$options$item,
          self$options$feature,
          self$options$value
        ))
        
        # Check missing values only in selected variables
        selected_data <- data[, selected_vars, drop = FALSE]
        
        if (anyNA(selected_data)) {
          
          instruction_html <- private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              'Please remove all missing values in the selected ',
              '<b>time / item / value</b> variables.',
              '</div>',
              '</div>'
            )
          )
          
          self$results$instructions$setContent(instruction_html)
          
          return()
        }
        
        # Compute results without cache
        all <- private$.computeRES()
        
        # ------------------------------------------------------------------
        # Elbow plot
        # ------------------------------------------------------------------
        if (isTRUE(self$options$plot1))
          self$results$plot1$setState(all$elbow_df)
        
        # ------------------------------------------------------------------
        # Save cluster membership
        # ------------------------------------------------------------------
        if (isTRUE(self$options$clust)) {
          
          m <- as.factor(all$df$cluster)
          
          self$results$clust$setValues(m)
          self$results$clust$setRowNums(rownames(self$data))
        }
        
        # ------------------------------------------------------------------
        # Time-series cluster plot
        # ------------------------------------------------------------------
        if (isTRUE(self$options$plot))
          self$results$plot$setState(all$df)
        
        # ------------------------------------------------------------------
        # Cluster mean plot
        # ------------------------------------------------------------------
        if (isTRUE(self$options$plot3))
          self$results$plot3$setState(all$cluster_mean_df)
        
        # ------------------------------------------------------------------
        # Silhouette plot
        # ------------------------------------------------------------------
        if (isTRUE(self$options$silhouettePlot))
          self$results$silhouettePlot$setState(all$silhouette_df)
        
        # ------------------------------------------------------------------
        # Cluster Summary Table
        # ------------------------------------------------------------------
        if (isTRUE(self$options$summary)) {
          
          self$results$clusterTable$setVisible(TRUE)
          
          private$.resetClusterTable(self$options$k)
          
          df_local <- all$df
          
          suppressWarnings({
            
            cluster_summary <- df_local
            
            cluster_summary <- dplyr::mutate(
              cluster_summary,
              cluster_chr = as.character(cluster),
              cluster_num = as.integer(cluster_chr)
            )
            
            cluster_summary <- dplyr::group_by(
              cluster_summary,
              cluster_chr,
              cluster_num
            )
            
            cluster_summary <- dplyr::summarise(
              cluster_summary,
              n_items = dplyr::n_distinct(item),
              mean_value = mean(
                value,
                na.rm = TRUE
              ),
              sd_value = stats::sd(
                value,
                na.rm = TRUE
              ),
              min_value = min(
                value,
                na.rm = TRUE
              ),
              max_value = max(
                value,
                na.rm = TRUE
              ),
              .groups = "drop"
            )
            
            cluster_summary <- dplyr::arrange(
              cluster_summary,
              dplyr::coalesce(
                cluster_num,
                NA_integer_
              ),
              cluster_chr
            )
          })
          
          
          n_rows <- min(
            nrow(cluster_summary),
            self$options$k
          )
          
          if (n_rows > 0) {
            
            for (i in seq_len(n_rows)) {
              
              cl_display <- cluster_summary$cluster_num[i]
              
              if (is.na(cl_display))
                cl_display <- cluster_summary$cluster_chr[i]
              
              self$results$clusterTable$setRow(
                rowNo = i,
                values = list(
                  cluster = cl_display,
                  n_items = cluster_summary$n_items[i],
                  mean_value = cluster_summary$mean_value[i],
                  sd_value = cluster_summary$sd_value[i],
                  min_value = cluster_summary$min_value[i],
                  max_value = cluster_summary$max_value[i]
                )
              )
            }
          }
          
        } else {
          
          self$results$clusterTable$setVisible(FALSE)
          
          private$.resetClusterTable(self$options$k)
        }
        
        # ------------------------------------------------------------------
        # Silhouette Analysis Table
        # ------------------------------------------------------------------
        if (isTRUE(self$options$silhouette)) {
          
          self$results$silhouetteTable$setVisible(TRUE)
          
          private$.resetSilhouetteTable(self$options$k)
          
          silhouette_df <- all$silhouette_df
          
          if (!is.null(silhouette_df) &&
              is.data.frame(silhouette_df) &&
              nrow(silhouette_df) > 0) {
            
            silhouette_summary <- dplyr::group_by(
              silhouette_df,
              cluster
            )
            
            silhouette_summary <- dplyr::summarise(
              silhouette_summary,
              n_items = dplyr::n(),
              mean_silhouette = mean(
                silhouette,
                na.rm = TRUE
              ),
              .groups = "drop"
            )
            
            silhouette_summary <- dplyr::arrange(
              silhouette_summary,
              cluster
            )
            
            n_rows <- min(
              nrow(silhouette_summary),
              self$options$k
            )
            
            if (n_rows > 0) {
              
              for (i in seq_len(n_rows)) {
                
                self$results$silhouetteTable$setRow(
                  rowNo = i,
                  values = list(
                    cluster = silhouette_summary$cluster[i],
                    n_items = silhouette_summary$n_items[i],
                    mean_silhouette = silhouette_summary$mean_silhouette[i]
                  )
                )
              }
            }
          }
          
        } else {
          
          self$results$silhouetteTable$setVisible(FALSE)
          
          }
      },
      
      # ------------------------------------------------------------------
      # Plot 1: Elbow plot
      # ------------------------------------------------------------------
      .plot1 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        elbow_df <- image$state
        
        if (!is.data.frame(elbow_df) || nrow(elbow_df) < 1)
          return(FALSE)
        
        p <- ggplot2::ggplot(
          elbow_df,
          ggplot2::aes(
            x = k,
            y = tot_withinss
          )
        ) +
          ggplot2::geom_line(
            linewidth = 0.8
          ) +
          ggplot2::geom_point(
            size = 2
          ) +
          ggplot2::scale_x_continuous(
            breaks = elbow_df$k
          ) +
          ggplot2::labs(
            title = "",
            x = "Number of Clusters",
            y = "Total Within-Cluster Sum of Squares"
          )
        
        p <- p + ggtheme
        
        print(p)
        
        TRUE
      },
      
      # ------------------------------------------------------------------
      # Plot 2: Time-series clusters
      # ------------------------------------------------------------------
      .plot = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        df <- image$state
        
        if (!is.data.frame(df) || nrow(df) < 1)
          return(FALSE)
        
        # Ensure time is Date
        if (!inherits(df$time, "Date"))
          df$time <- as.Date(df$time)
        
        time_breaks <- sort(unique(df$time))
        
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = time,
            y = value,
            group = item,
            colour = cluster
          )
        ) +
          ggplot2::geom_line(
            show.legend = FALSE
          ) +
          ggplot2::scale_x_date(
            breaks = time_breaks,
            labels = scales::label_date("%Y"),
            expand = ggplot2::expansion(
              mult = c(0.02, 0.02)
            )
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::comma
          ) +
          ggplot2::facet_wrap(
            ggplot2::vars(cluster)
          ) +
          ggthemes::scale_color_solarized() +
          ggplot2::labs(
            title = "",
            x = "Time",
            y = "Value"
          )
        
        p <- p + ggtheme
        
        if (self$options$angle > 0) {
          
          p <- p +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = self$options$angle,
                hjust = 1,
                vjust = 0.5
              ),
              plot.margin = ggplot2::margin(
                t = 5.5,
                r = 5.5,
                b = 15,
                l = 5.5
              )
            )
          
        } else {
          
          p <- p +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = 0,
                hjust = 0.5,
                vjust = 0.5
              )
            )
        }
        
        print(p)
        
        TRUE
      },
      
      
      # ------------------------------------------------------------------
      # Plot 3: Cluster mean time-series
      # ------------------------------------------------------------------
      .plot3 = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        dfm <- image$state
        
        if (!is.data.frame(dfm) || nrow(dfm) < 1)
          return(FALSE)
        
        # Ensure time is Date
        if (!inherits(dfm$time, "Date"))
          dfm$time <- as.Date(dfm$time)
        
        time_breaks <- sort(unique(dfm$time))
        
        p <- ggplot2::ggplot(
          dfm,
          ggplot2::aes(
            x = time,
            y = mean_value,
            colour = cluster,
            group = cluster
          )
        ) +
          ggplot2::geom_line(
            linewidth = 1
          ) +
          ggplot2::scale_x_date(
            breaks = time_breaks,
            labels = scales::label_date("%Y"),
            expand = ggplot2::expansion(
              mult = c(0.02, 0.02)
            )
          ) +
          ggplot2::labs(
            title = "",
            x = "Time",
            y = "Mean",
            colour = "Cluster"
          ) +
          ggthemes::scale_color_solarized()
        
        p <- p + ggtheme
        
        if (self$options$angle > 0) {
          
          p <- p +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = self$options$angle,
                hjust = 1,
                vjust = 0.5
              ),
              plot.margin = ggplot2::margin(
                t = 5.5,
                r = 5.5,
                b = 15,
                l = 5.5
              )
            )
          
        } else {
          
          p <- p +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = 0,
                hjust = 0.5,
                vjust = 0.5
              )
            )
        }
        
        print(p)
        
        TRUE
      },
      
      # ------------------------------------------------------------------
      # Plot 4: Silhouette plot
      # ------------------------------------------------------------------
      .silhouettePlot = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        dfs <- image$state
        
        if (!is.data.frame(dfs) || nrow(dfs) < 1)
          return(FALSE)
        
        dfs <- dplyr::arrange(
          dfs,
          cluster,
          silhouette
        )
        
        dfs$item_order <- factor(
          seq_len(nrow(dfs)),
          levels = rev(seq_len(nrow(dfs)))
        )
        
        mean_silhouette <- mean(
          dfs$silhouette,
          na.rm = TRUE
        )
        
        p <- ggplot2::ggplot(
          dfs,
          ggplot2::aes(
            x = silhouette,
            y = item_order,
            fill = factor(cluster)
          )
        ) +
          ggplot2::geom_col(
            show.legend = FALSE
          ) +
          ggplot2::geom_vline(
            xintercept = mean_silhouette,
            linetype = "dashed",
            linewidth = 0.6
          ) +
          ggplot2::facet_grid(
            rows = ggplot2::vars(cluster),
            scales = "free_y",
            space = "free_y"
          ) +
          ggplot2::labs(
            title = "",
            x = "Silhouette width",
            y = NULL,
            subtitle = paste0(
              "Average silhouette width = ",
              formatC(
                mean_silhouette,
                format = "f",
                digits = 3
              )
            )
          )
        
        # Apply jamovi theme first
        p <- p + ggtheme
        
        # Remove item numbers and y-axis elements after ggtheme
        p <- p +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank()
          )
        
        print(p)
        
        TRUE
      },
      
      # ------------------------------------------------------------------
      # Compute results
      # ------------------------------------------------------------------
      .computeRES = function() {
        
        item <- self$options$item
        feature <- self$options$feature
        value <- self$options$value
        k <- self$options$k
        standardize <- isTRUE(self$options$standardize)
        
        # Use only the selected variables
        data <- as.data.frame(
          self$data[, c(
            item,
            feature,
            value
          ), drop = FALSE]
        )
        
        # Convert non-Date time variables using the existing method
        if (!inherits(data[[feature]], "Date")) {
          
          data[[feature]] <- as.Date(
            paste(
              data[[feature]],
              1,
              1,
              sep = "-"
            ),
            format = "%Y-%m-%d"
          )
        }
        
        d2 <- dplyr::rename(
          data,
          time = !!rlang::sym(feature),
          item = !!rlang::sym(item),
          value = !!rlang::sym(value)
        )
        
        if (!is.numeric(d2$value))
          stop("The value variable must be numeric.")
        
        # ------------------------------------------------------------------
        # Standardize each item series before clustering when requested
        # ------------------------------------------------------------------
        d2_cluster <- d2
        
        if (standardize) {
          
          standardize_series <- function(x) {
            
            s <- stats::sd(x)
            
            if (is.na(s) || s == 0)
              return(rep(0, length(x)))
            
            (x - mean(x)) / s
          }
          
          d2_cluster <- dplyr::group_by(
            d2_cluster,
            item
          )
          
          d2_cluster <- dplyr::mutate(
            d2_cluster,
            value = standardize_series(value)
          )
          
          d2_cluster <- dplyr::ungroup(d2_cluster)
        }
        
        # ------------------------------------------------------------------
        # Create the same item-by-time structure used for clustering
        # ------------------------------------------------------------------
        wide_df <- tidyr::pivot_wider(
          d2_cluster,
          id_cols = item,
          names_from = time,
          values_from = value
        )
        
        wide_matrix <- as.matrix(
          wide_df[
            ,
            setdiff(
              names(wide_df),
              "item"
            ),
            drop = FALSE
          ]
        )
        
        storage.mode(wide_matrix) <- "double"
        
        # Missing item-time combinations create NA values after pivoting
        if (anyNA(wide_matrix)) {
          stop(
            paste(
              "Each item must have a value for every time point.",
              "Please check for missing item-time combinations."
            )
          )
        }
        
        n_items <- nrow(wide_matrix)
        
        if (n_items < 2) {
          stop(
            "At least two items are required for time-series clustering."
          )
        }
        
        # Number of distinct item profiles
        n_unique_profiles <- nrow(
          unique(
            as.data.frame(wide_matrix)
          )
        )
        
        # ------------------------------------------------------------------
        # Elbow values
        # ------------------------------------------------------------------
        max_k <- min(
          10L,
          n_items - 1L,
          n_unique_profiles
        )
        
        if (max_k >= 1L) {
          
          set.seed(1234)
          
          elbow_df <- data.frame(
            k = seq_len(max_k),
            tot_withinss = vapply(
              seq_len(max_k),
              function(k_value) {
                
                km <- stats::kmeans(
                  wide_matrix,
                  centers = k_value,
                  nstart = if (k_value == 1L) 1L else 25L
                )
                
                km$tot.withinss
              },
              numeric(1)
            )
          )
          
        } else {
          
          elbow_df <- NULL
        }
        
        # ------------------------------------------------------------------
        # Main time-series clustering using widyr
        # ------------------------------------------------------------------
        set.seed(1234)
        
        res <- widyr::widely_kmeans(
          tbl = d2_cluster,
          item = item,
          feature = time,
          value = value,
          k = k
        )
        
        df <- dplyr::left_join(
          d2,
          res,
          by = "item"
        )
        
        # ------------------------------------------------------------------
        # Silhouette analysis
        # ------------------------------------------------------------------
        silhouette_df <- NULL
        
        if (isTRUE(self$options$silhouette) ||
            isTRUE(self$options$silhouettePlot)) {
          
          if (k >= n_items) {
            stop(
              "Silhouette analysis requires fewer clusters than items."
            )
          }
          
          cluster_match <- match(
            wide_df$item,
            res$item
          )
          
          cluster_assignment <- res$cluster[cluster_match]
          
          if (anyNA(cluster_assignment)) {
            stop(
              "Unable to match cluster assignments to all items."
            )
          }
          
          cluster_chr <- as.character(cluster_assignment)
          cluster_num <- suppressWarnings(
            as.integer(cluster_chr)
          )
          
          if (anyNA(cluster_num)) {
            cluster_num <- as.integer(
              factor(cluster_chr)
            )
          }
          
          sil <- cluster::silhouette(
            cluster_num,
            stats::dist(wide_matrix)
          )
          
          silhouette_df <- data.frame(
            item = wide_df$item,
            cluster = as.integer(sil[, "cluster"]),
            silhouette = as.numeric(sil[, "sil_width"]),
            stringsAsFactors = FALSE
          )
        }
        
        # ------------------------------------------------------------------
        # Cluster mean time-series
        # ------------------------------------------------------------------
        cluster_mean_df <- stats::aggregate(
          df$value,
          by = list(
            time = df$time,
            cluster = df$cluster
          ),
          FUN = mean,
          na.rm = TRUE
        )
        
        names(cluster_mean_df)[
          names(cluster_mean_df) == "x"
        ] <- "mean_value"
        
        # Update clustering information text
        clustering_info <- paste(
          "The Elbow plot is based on the item-by-time matrix.",
          "Lower within-cluster variation indicates more homogeneous clusters.",
          sep = "\n"
        )
        
        if (standardize) {
          clustering_info <- paste(
            clustering_info,
            "Series were standardized within each item using z-scores before clustering.",
            "Summary statistics and time-series plots are shown on the original value scale.",
            sep = "\n"
          )
        }
        
        self$results$text$setContent(clustering_info)
        
        list(
          res = res,
          df = df,
          elbow_df = elbow_df,
          cluster_mean_df = cluster_mean_df,
          silhouette_df = silhouette_df
        )
      }
    )
  )