# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>% 

timeclustClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "timeclustClass",
    inherit = timeclustBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        if (is.null(self$data) | is.null(self$options$item)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Perform time series clustering based on <a href="https://www.r-bloggers.com/2024/07/time-series-clustering-in-r/" target = "_blank">Widyr R package</a>.</li>',
            '<li>The variables must be named <b>time, item, and value</b> respectively.</li>',
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
      },
      
      .run = function() {
        if (is.null(self$data) |
            is.null(self$options$item) |
            is.null(self$options$feature) | is.null(self$options$value))
          return()
        
        # 결과 캐시
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        all <- private$.allCache
        
        if (isTRUE(self$options$plot1)) {
          image <- self$results$plot1
          image$setState(all$bic)
        }
        
        # 군집 결과값 (결측 매칭)
        if (isTRUE(self$options$clust)) {
          m <- all$cluster_vec
          if (self$options$clust && self$results$clust$isNotFilled()) {
            self$results$clust$setValues(m)
            self$results$clust$setRowNums(rownames(self$data))
          }
        }
        
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(all$df)
        }
      },
      
      # plot---
      .plot1 = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        bic <- image$state
        plot1 <- plot(bic)
        print(plot1)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        df <- image$state
        library(ggplot2)
        plot <- ggplot(df, aes(
          x = time,
          y = value,
          group = item,
          colour = cluster
        )) +
          geom_line(show.legend = F) +
          scale_y_continuous(labels = scales::comma) +
          facet_wrap(vars(cluster)) +
          ggthemes::scale_color_solarized()
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      .computeRES = function() {
        item <- self$options$item
        feature <- self$options$feature
        value <- self$options$value
        k <- self$options$k
        
        data <- self$data
        na_vars <- c(item, feature, value)
        complete_idx <- which(stats::complete.cases(data[, na_vars, drop=FALSE]))
        data_noNA <- data[complete_idx, , drop=FALSE]
        
        is.Date <- function(x) inherits(x, "Date")
        if (!is.Date(data_noNA[, feature])) {
          data_noNA[, feature] <- as.Date(paste(data_noNA[, feature], 1, 1, sep = "-"), format = "%Y-%m-%d")
        }
        data_tbl <- tibble::as_tibble(data_noNA)
        
        # --- [여기서 k가 distinct item 개수보다 크면 에러!] ---
        n_items <- length(unique(data_noNA[[item]]))
        if (k > n_items) {
          stop(paste0(
            "Number of clusters (k=", k, ") must be less than or equal to number of distinct items (", n_items, ") after removing missing values."
          ))
        }
        # -----------------------------------------------------
        
        # BIC 계산
        bic <- mclust::mclustBIC(data_noNA[[value]])
        self$results$text$setContent(bic)
        
        # kmeans clustering (여기가 핵심)
        set.seed(1234)
        res <- widyr::widely_kmeans(
          tbl = data_tbl,
          item = item,        # 변수명 문자열!
          feature = feature,
          value = value,
          k = k
        )
        
        df <- dplyr::left_join(data_tbl, res)
        cluster_vec <- rep(NA, nrow(data))
        cluster_vec[complete_idx] <- as.character(res$cluster)
        
        retlist = list(
          res = res,
          df = df,
          bic = bic,
          cluster_vec = cluster_vec,
          complete_idx = complete_idx
        )
        return(retlist)
      }
    )
  )
