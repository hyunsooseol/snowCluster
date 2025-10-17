# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>% 

timeclustClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "timeclustClass",
    inherit = timeclustBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      # ------------------------------------------------------------------
      # Helper: Reset cluster table rows (since jmvcore Table has no clear())
      # ------------------------------------------------------------------
      .resetClusterTable = function(k) {
        for (i in seq_len(k)) {
          self$results$clusterTable$setRow(rowNo = i, values = list(
            cluster    = NA,
            n_items    = NA,
            mean_value = NA,
            sd_value   = NA,
            min_value  = NA,
            max_value  = NA
          ))
        }
      },
      
      # ------------------------------------------------------------------
      # Init
      # ------------------------------------------------------------------
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        # Instructions accordion
        instruction_html <- private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Time-series clustering using the <b>widyr</b> approach.</li>',
            '<li>Columns must be named <b>time</b>, <b>item</b>, <b>value</b> (regular time index for all items).</li>',
            '<li>Report issues or requests on <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        )
        self$results$instructions$setContent(instruction_html)
        
        # Output sizes (plot2 removed)
        if (isTRUE(self$options$plot))   self$results$plot$setSize(self$options$width,  self$options$height)
        if (isTRUE(self$options$plot1))  self$results$plot1$setSize(self$options$width1, self$options$height1)
        if (isTRUE(self$options$plot3))  self$results$plot3$setSize(self$options$width3,  self$options$height3)
        
        # Hide summary table initially
        if (!isTRUE(self$options$summary))
          self$results$clusterTable$setVisible(FALSE)
      },
      
      # ------------------------------------------------------------------
      # Run
      # ------------------------------------------------------------------
      .run = function() {
        if (is.null(self$data) |
            is.null(self$options$item) |
            is.null(self$options$feature) | is.null(self$options$value))
          return()
        
        data <- self$data
        
        # NA check
        if (any(is.na(data))) {
          instruction_html <- private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">Please remove all missing values in <b>time / item / value</b>.</div>',
              '</div>'
            )
          )
          self$results$instructions$setContent(instruction_html)
          return()
        }
        
        # Compute only once
        if (is.null(private$.allCache))
          private$.allCache <- private$.computeRES()
        all <- private$.allCache
        
        # States
        if (isTRUE(self$options$plot1))
          self$results$plot1$setState(all$bic)
        
        if (isTRUE(self$options$clust)) {
          m <- as.factor(all$df$cluster)
          if (self$options$clust && self$results$clust$isNotFilled()) {
            self$results$clust$setValues(m)
            self$results$clust$setRowNums(rownames(self$data))
          }
        }
        
        if (isTRUE(self$options$plot))
          self$results$plot$setState(all$df)
        
        if (isTRUE(self$options$plot3))
          self$results$plot3$setState(all$cluster_mean_df)
        
        # ------------------------------------------------------------------
        # Cluster Summary Table
        # ------------------------------------------------------------------
        if (isTRUE(self$options$summary)) {
          self$results$clusterTable$setVisible(TRUE)
          private$.resetClusterTable(self$options$k)  # blank first
          
          df_local <- all$df
          
          suppressWarnings({
            cluster_summary <- df_local %>%
              dplyr::mutate(
                cluster_chr = as.character(cluster),
                cluster_num = as.integer(cluster_chr)
              ) %>%
              dplyr::group_by(cluster_chr, cluster_num) %>%
              dplyr::summarise(
                n_items    = dplyr::n_distinct(item),
                mean_value = mean(value, na.rm = TRUE),
                sd_value   = sd(value,   na.rm = TRUE),
                min_value  = min(value,  na.rm = TRUE),
                max_value  = max(value,  na.rm = TRUE),
                .groups = "drop"
              ) %>%
              dplyr::arrange(dplyr::coalesce(cluster_num, NA_integer_), cluster_chr)
          })
          
          # fill sequentially
          for (i in seq_len(min(nrow(cluster_summary), self$options$k))) {
            cl_display <- cluster_summary$cluster_num[i]
            if (is.na(cl_display)) cl_display <- cluster_summary$cluster_chr[i]
            
            self$results$clusterTable$setRow(rowNo = i, values = list(
              cluster    = cl_display,
              n_items    = cluster_summary$n_items[i],
              mean_value = cluster_summary$mean_value[i],
              sd_value   = cluster_summary$sd_value[i],
              min_value  = cluster_summary$min_value[i],
              max_value  = cluster_summary$max_value[i]
            ))
          }
        } else {
          self$results$clusterTable$setVisible(FALSE)
          private$.resetClusterTable(self$options$k)
        }
      },
      
      # ------------------------------------------------------------------
      # Plot 1: BIC
      # ------------------------------------------------------------------
      .plot1 = function(image, ...) {
        if (is.null(image$state)) return(FALSE)
        print(plot(image$state)); TRUE
      },
      
      # ------------------------------------------------------------------
      # Plot: Time-series clusters
      # ------------------------------------------------------------------
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        df <- image$state
        library(ggplot2)
        p <- ggplot(df, aes(x = time, y = value, group = item, colour = cluster)) +
          geom_line(show.legend = FALSE) +
          scale_y_continuous(labels = scales::comma) +
          facet_wrap(vars(cluster)) +
          ggthemes::scale_color_solarized()
        p <- p + ggtheme
        print(p); TRUE
      },
      
      # ------------------------------------------------------------------
      # Plot 3: Cluster mean time-series
      # ------------------------------------------------------------------
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        dfm <- image$state
        library(ggplot2)
        p <- ggplot(dfm, aes(time, mean_value, colour = cluster, group = cluster)) +
          geom_line(size = 1) +
          labs(title = "",
               x = "Time", y = "Mean", colour = "Cluster") +
          ggthemes::scale_color_solarized()
        p <- p + ggtheme
        print(p); TRUE
      },
      
      # ------------------------------------------------------------------
      # Compute results
      # ------------------------------------------------------------------
      .computeRES = function() {
        item    <- self$options$item
        feature <- self$options$feature
        value   <- self$options$value
        k       <- self$options$k
        
        data <- jmvcore::naOmit(as.data.frame(self$data))
        
        if (!inherits(data[[feature]], "Date")) {
          data[[feature]] <- as.Date(paste(data[[feature]], 1, 1, sep = "-"),
                                     format = "%Y-%m-%d")
        }
        
        d2 <- dplyr::rename(
          data,
          time  = !!rlang::sym(feature),
          item  = !!rlang::sym(item),
          value = !!rlang::sym(value)
        )
        
        bic <- mclust::mclustBIC(d2$value)
        self$results$text$setContent(bic)
        
        set.seed(1234)
        res <- widyr::widely_kmeans(tbl = d2, item = item, feature = time, value = value, k = k)
        df  <- dplyr::left_join(d2, res, by = "item")
        
        cluster_mean_df <- stats::aggregate(
          df$value, by = list(time = df$time, cluster = df$cluster),
          FUN = mean, na.rm = TRUE
        )
        names(cluster_mean_df)[names(cluster_mean_df) == "x"] <- "mean_value"
        
        list(
          res = res,
          df = df,
          bic = bic,
          cluster_mean_df = cluster_mean_df
        )
      }
    )
  )
