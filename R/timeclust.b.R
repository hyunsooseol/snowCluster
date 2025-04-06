# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>% 
timeclustClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "timeclustClass",
    inherit = timeclustBase,
    private = list(
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
        
        
        # item<- self$options$item
        # feature<- self$options$feature
        # value<- self$options$value
        # k <- self$options$k
        #
        # data <- self$data
        # data <- jmvcore::naOmit(data)
        # data <- as.data.frame(data)
        #
        #
        # data[,self$options$feature] <- as.Date(data[,self$options$feature])
        #
        # data <- tibble::as.tibble(data)
        
        #self$results$text$setContent(data)
        
        # km<- function (tbl, item, feature, value, k, fill = 0, ...)
        # {
        #   item_str <- as.character(substitute(item))
        #   feature_str <- as.character(substitute(feature))
        #   value_str <- as.character(substitute(value))
        #   form <- stats::as.formula(paste(item_str, "~", feature_str))
        #   m <- tbl %>% reshape2::acast(form, value.var = value_str,
        #                                fill = fill)
        #
        #   clustered <- stats::kmeans(m, k, ...)
        #   i <- match(rownames(m), as.character(tbl[[item_str]]))
        #   tibble::tibble(`:=`(!!sym(item_str), tbl[[item_str]][i]),
        #                  cluster = factor(clustered$cluster)) %>%
        #                  dplyr::arrange(cluster)
        # }
        
        all <- private$.computeRES()
        
        if (isTRUE(self$options$plot1)) {
          image <- self$results$plot1
          image$setState(all$bic)
        }
        
        # cluster number--
        
        if (isTRUE(self$options$clust)) {
          m <- as.factor(all$df$cluster)
          
          if (self$options$clust
              && self$results$clust$isNotFilled()) {
            self$results$clust$setValues(m)
            self$results$clust$setRowNums(rownames(data))
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
        data <- jmvcore::naOmit(data)
        data <- as.data.frame(data)
        
        is.Date <- function(x) {
          inherits(x, "Date")
        }
        
        # Step 1: transform year into Date---
        if (!is.Date(data[, self$options$feature])) {
          data[, self$options$feature] <- as.Date(paste(data[, self$options$feature], 1, 1, sep = "-"), format = "%Y-%m-%d")
        } else {
          data[, self$options$feature] <- data[, self$options$feature]
        }
        
        data <- tibble::as.tibble(data)
        
        
        # Optimal number of clusters using BIC---
        bic <- mclust::mclustBIC(data$value)
        self$results$text$setContent(bic)
        
        
        # Perform k-means clustering using widely_kmeans
        # The variable names should be item, time, value respectively!!!
        set.seed(1234)
        res <-  data %>%
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