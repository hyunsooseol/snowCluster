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
        
        # Set default instruction message
        instruction_html <- private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Perform time series clustering based on <a href="https://www.r-bloggers.com/2024/07/time-series-clustering-in-r/" target = "_blank">Widyr R package</a>.</li>',
            '<li>The variables must be named <b>time, item, and value</b> respectively.</li>',
            '<li>All <b>item</b> and <b>time</b> combinations must be present in your data, and <b>time</b> values must be consistent and regular for all items (e.g., every year or every five years). Otherwise, the analysis may fail or results may be incorrect.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        )
        self$results$instructions$setContent(instruction_html)
        
        # Set plot sizes if the options are TRUE
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
        # Check for basic required options
        if (is.null(self$data) |
            is.null(self$options$item) |
            is.null(self$options$feature) | is.null(self$options$value))
          return()
        
        data <- self$data
        
        # Show missing value warning and stop analysis if any missing values exist
        if (any(is.na(data))) {
          instruction_html <- private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Perform time series clustering based on <a href="https://www.r-bloggers.com/2024/07/time-series-clustering-in-r/" target = "_blank">Widyr R package</a>.</li>',
              '<li>The variables must be named <b>time, item, and value</b> respectively.</li>',
              '<li>All <b>item</b> and <b>time</b> combinations must be present in your data, and <b>time</b> values must be consistent and regular for all items (e.g., every year or every five years). Otherwise, the analysis may fail or results may be incorrect.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>',
              # 
              '<div style="margin-top:20px; padding:12px; border:2px solid #f08c7e; background:#ffe8e2; border-radius:10px;">',
              '<b>⚠️ Missing values detected in your data.</b><br>',
              'Please locate the row(s) with missing values in any variable (time, item, or value)—including character (text), numeric, or date types—and enter a valid value.',
              '</div>'
            )
          )
          self$results$instructions$setContent(instruction_html)
          return()
        }
        
        # Run actual clustering only if no missing values are present
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        all <- private$.allCache
        
        # Set plot contents if required
        if (isTRUE(self$options$plot1)) {
          image <- self$results$plot1
          image$setState(all$bic)
        }
        
        if (isTRUE(self$options$clust)) {
          m <- as.factor(all$df$cluster)
          if (self$options$clust && self$results$clust$isNotFilled()) {
            self$results$clust$setValues(m)
            self$results$clust$setRowNums(rownames(data))
          }
        }
        
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(all$df)
        }
      },
      
      .plot1 = function(image, ...) {
        # Plot BIC selection
        if (is.null(image$state))
          return(FALSE)
        bic <- image$state
        plot1 <- plot(bic)
        print(plot1)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        # Plot time series clustering results
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
        # Compute results for time series clustering analysis
        item <- self$options$item
        feature <- self$options$feature
        value <- self$options$value
        k <- self$options$k
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        data <- as.data.frame(data)
        
        # Convert to Date if needed
        is.Date <- function(x) { inherits(x, "Date") }
        if (!is.Date(data[, feature])) {
          data[, feature] <- as.Date(paste(data[, feature], 1, 1, sep = "-"), format = "%Y-%m-%d")
        } else {
          data[, feature] <- data[, feature]
        }
        
        data <- tibble::as_tibble(data)
        
        # Find optimal number of clusters using BIC (univariate)
        bic <- mclust::mclustBIC(data$value)
        self$results$text$setContent(bic)
        
        # Perform k-means clustering using widely_kmeans
        set.seed(1234)
        res <- data %>%
          widyr::widely_kmeans(
            tbl = data,
            item = item,
            feature = time,
            value = value,
            k = k
          )
        df <- dplyr::left_join(data, res)
        retlist = list(res = res, df = df, bic = bic)
        return(retlist)
      }
    )
  )
