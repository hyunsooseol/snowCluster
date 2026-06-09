
# This file is a generated template, your changes will not be overwritten
discClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "discClass",
    inherit = discBase,
    private = list(
      .htmlwidget = NULL,
      #------------------------------------
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$dep) |
            is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>When <b>Split set</b> is less than 1, the LDA model, discriminant function statistics, group centroids, and structure coefficients are based on the training set, while test accuracy is evaluated on the held-out test set.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
      },
      
      #---------------------------------------------
      
      .run = function() {
        
        if (is.null(self$options$dep) || length(self$options$covs) < 2)
          return()
        
        dep <- self$options$dep
        covs <- self$options$covs
        per <- self$options$per
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        for (cov in covs)
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        
        data[[dep]] <- as.factor(data[[dep]])
        
        res <- private$.computeRES()
        
        # Prior probabilities of groups table----
        
        value <- res$lda.train$prior
        prior <- as.data.frame(value)
        names <- dimnames(prior)[[1]]
        
        table <- self$results$prior
        for (name in names) {
          row <- list()
          row[['value']] <- prior[name, 1]
          table$addRow(rowKey = name, values = row)
        }
        
        # Group means---------------
        gm <- res$lda.train$means
        names <- dimnames(gm)[[1]]
        table <- self$results$gm
        
        for (i in seq_along(covs)) {
          cov <- covs[[i]]
          table$addColumn(name = paste0(cov),
                          type = 'number',
                          format = 'zto')
        }
        
        for (name in names) {
          row <- list()
          for (j in seq_along(covs)) {
            cov <- covs[[j]]
            row[[cov]] <- gm[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
        
        # Coefficients of linear discriminants-------
        
        coef <- res$lda.train$scaling
        coef <- as.data.frame(coef)
        
        names <-  dimnames(coef)[[1]]
        dims <- dimnames(coef)[[2]]
        
        table <- self$results$coef
        
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        for (name in names) {
          row <- list()
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- coef[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
        
        # Normalized loading---
        nl <- sweep(res$lda.train$scaling, 2, sqrt(colSums(res$lda.train$scaling^2)), "/")
        #self$results$text$setContent(nl)
        nl <- as.data.frame(nl)
        
        names <-  dimnames(nl)[[1]]
        dims <- dimnames(nl)[[2]]
        
        table <- self$results$nl
        
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        for (name in names) {
          row <- list()
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- nl[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
        
        # Discriminant scores and group centroids ------------------
        # Group centroids are table results, not plot-only results.
        
        pred_x <- as.data.frame(predict(res$lda.train)$x)
        ld_names <- colnames(pred_x)
        
        df <- cbind(res$train, pred_x)
        df$Groups <- as.factor(res$train[[self$options$dep]])
        
        cent <- stats::aggregate(
          df[, ld_names, drop = FALSE],
          by = list(Groups = df$Groups),
          FUN = mean
        )
        
        # Accuracy with training data-----------
        
        # lda.train <- predict(lda.iris)
        #
        # train$lda <- lda.train$class
        # table(train$lda,train$Species)
        
        if (isTRUE(self$options$tra)) {
          
          pred = predict(res$lda.train)
          #self$results$text$setContent(pred)
          
          res1 <- table(res$train[[self$options$dep]], pred$class)
          res1 <- as.matrix(res1)
          
          
          names <- dimnames(res1)[[1]]
          table <- self$results$tra
          
          for (name in names) {
            table$addColumn(name = paste0(name),
                            type = 'Integer',
                            superTitle = 'Predicted')
          }
          
          for (name in names) {
            row <- list()
            for (j in seq_along(names)) {
              row[[names[j]]] <- res1[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # # Accuracy with test data-----------
        
        # lda.test <- predict(lda.iris,test)
        # test$lda <- lda.test$class
        # table(test$lda,test$Species)
        #
        
        if (isTRUE(self$options$tes)) {
          lda.test = predict(res$lda.train, res$test)
          te <- lda.test$class
          
          #res2 <- table(te, res$test[[self$options$dep]])
          res2 <- table(res$test[[self$options$dep]], te)
          res2 <- as.matrix(res2)
          
          names <- dimnames(res2)[[1]]
          table <- self$results$tes
          
          for (name in names) {
            table$addColumn(name = paste0(name),
                            type = 'Integer',
                            superTitle = 'Predicted')
          }
          
          for (name in names) {
            row <- list()
            for (j in seq_along(names)) {
              row[[names[j]]] <- res2[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # Proportion of trace------------------
        
        # Variance explained ------------------
        # This can be reported even when only LD1 exists.
        
        if (isTRUE(self$options$prop)) {
          
          prop.lda <- res$lda.train$svd ^ 2 / sum(res$lda.train$svd ^ 2)
          
          table <- self$results$prop
          
          row <- list()
          
          if (length(prop.lda) >= 1)
            row[['LD1']] <- prop.lda[[1]]
          
          if (length(prop.lda) >= 2)
            row[['LD2']] <- prop.lda[[2]]
          
          table$setRow(rowNo = 1, values = row)
        }
        
        # Canonical discriminant functions table---
        
        # Canonical discriminant functions table---
        
        if (isTRUE(self$options$can)) {
          
          n <- nrow(res$train)
          g <- nlevels(droplevels(res$train[[dep]]))
          
          # MASS::lda()$svd^2 is not on the same scale as the
          # canonical discriminant eigenvalues reported by SPSS.
          raw_roots <- res$lda.train$svd^2
          
          eigenvalues <- raw_roots * (g - 1) / (n - g)
          
          variance <- eigenvalues / sum(eigenvalues) * 100
          cumulative <- cumsum(variance)
          canonical <- sqrt(eigenvalues / (1 + eigenvalues))
          
          table <- self$results$can
          
          for (i in seq_along(eigenvalues)) {
            fn <- paste0("LD", i)
            
            row <- list()
            row[["function"]] <- fn
            row[["eigen"]] <- as.numeric(eigenvalues[i])
            row[["variance"]] <- as.numeric(variance[i])
            row[["cumulative"]] <- as.numeric(cumulative[i])
            row[["canonical"]] <- as.numeric(canonical[i])
            
            table$addRow(
              rowKey = fn,
              values = row
            )
          }
        }
        
        # Wilks' Lambda tests---
        # Approximate chi-square tests for discriminant functions.
        
        # Wilks' Lambda tests---
        # Approximate chi-square tests for discriminant functions.
        
        # Wilks' Lambda tests---
        # Sequential approximate chi-square tests for discriminant functions.
        
        if (isTRUE(self$options$wilks)) {
          
          n <- nrow(res$train)
          p <- length(covs)
          g <- nlevels(droplevels(res$train[[dep]]))
          
          raw_roots <- res$lda.train$svd^2
          eigenvalues <- raw_roots * (g - 1) / (n - g)
          
          s <- length(eigenvalues)
          table <- self$results$wilks
          
          for (i in seq_len(s)) {
            
            # Wilks' lambda for functions i through s
            lambda <- prod(1 / (1 + eigenvalues[i:s]))
            
            # Sequential-test degrees of freedom
            df_wilks <- (p - i + 1) * (g - i)
            
            # Bartlett chi-square approximation
            chisq <- -(
              n - 1 - (p + g) / 2
            ) * log(lambda)
            
            pval <- stats::pchisq(
              chisq,
              df = df_wilks,
              lower.tail = FALSE
            )
            
            test_label <- if (i == s) {
              paste0(i)
            } else {
              paste0(i, " through ", s)
            }
            
            row <- list(
              test = test_label,
              lambda = as.numeric(lambda),
              chisq = as.numeric(chisq),
              df = as.numeric(df_wilks),
              p = as.numeric(pval)
            )
            
            table$addRow(
              rowKey = paste0("func", i),
              values = row
            )
          }
        }
        
        # Structure coefficients table---
        # Correlations between original variables and discriminant functions.
        
        if (isTRUE(self$options$struct)) {
          
          xvars <- res$train[, covs, drop = FALSE]
          xvars[] <- lapply(xvars, jmvcore::toNumeric)
          
          structure <- stats::cor(
            xvars,
            pred_x,
            use = "pairwise.complete.obs"
          )
          
          structure <- as.data.frame(structure)
          
          table <- self$results$struct
          
          for (ld in colnames(structure)) {
            table$addColumn(name = ld,
                            title = ld,
                            type = 'number',
                            format = 'zto')
          }
          
          for (var in rownames(structure)) {
            row <- list()
            row[["variable"]] <- var
            
            for (ld in colnames(structure)) {
              row[[ld]] <- structure[var, ld]
            }
            
            table$addRow(rowKey = var, values = row)
          }
        }
        
        # Group centroids table---
        # This table is independent of the linear discriminant plot.
        # It can be reported even when only LD1 exists.
        
        if (isTRUE(self$options$gc)) {
          table <- self$results$gc
          
          for (i in seq_len(nrow(cent))) {
            row <- list()
            row[["name"]] <- as.character(cent$Groups[i])
            
            if ("LD1" %in% names(cent))
              row[["ld1"]] <- cent$LD1[i]
            
            if ("LD2" %in% names(cent))
              row[["ld2"]] <- cent$LD2[i]
            
            table$addRow(rowKey = as.character(cent$Groups[i]), values = row)
          }
        }
        
       
       
        # LD plot---
        
        if (isTRUE(self$options$plot)) {
          
          image <- self$results$plot
          
          # The 2D discriminant plot requires at least two discriminant functions.
          # If only LD1 exists, show an informative message instead of an empty plot.
          
          if (length(ld_names) >= 2) {
            
            plot_df <- df[, c("LD1", "LD2", "Groups"), drop = FALSE]
            plot_cent <- cent[, c("Groups", "LD1", "LD2"), drop = FALSE]
            
            state <- list(
              df = plot_df,
              cent = plot_cent,
              message = NULL
            )
            image$setState(state)
            
          } else {
            
            state <- list(
              df = NULL,
              cent = NULL,
              message = "A 2D discriminant plot requires both LD1 and LD2. For two-group LDA, only LD1 is available."
            )
            image$setState(state)
          }
        }
        
        
        if (isTRUE(self$options$scores)) {
          
          needed <- c(dep, covs)
          complete_rows <- complete.cases(self$data[, needed, drop = FALSE])
          
          data_scores <- self$data[complete_rows, , drop = FALSE]
          
          for (cov in covs)
            data_scores[[cov]] <- jmvcore::toNumeric(data_scores[[cov]])
          
          data_scores[[dep]] <- as.factor(data_scores[[dep]])
          
          pred <- as.data.frame(predict(res$lda.train, data_scores)$x)
          score_names <- colnames(pred)
          n_scores <- length(score_names)
          
          keys <- seq_len(n_scores)
          titles <- score_names
          descriptions <- score_names
          measureTypes <- rep("continuous", n_scores)
          
          self$results$scores$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$scores$setRowNums(rownames(self$data))
          
          full_scores <- matrix(NA, nrow = nrow(self$data), ncol = n_scores)
          
          full_scores[complete_rows, ] <- as.matrix(pred)
          
          for (i in seq_len(n_scores)) {
            values <- as.numeric(full_scores[, i])
            self$results$scores$setValues(index = i, values)
          }
        }
        
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        if (!is.null(image$state$message)) {
          plot.new()
          par(mar = c(2, 2, 2, 2))
          
          msg <- strwrap(image$state$message, width = 55)
          
          text(
            x = 0.5,
            y = 0.55,
            labels = paste(msg, collapse = "\n"),
            cex = 0.95,
            adj = c(0.5, 0.5)
          )
          
          return(TRUE)
        }
        
        df <- image$state$df
        cent <- image$state$cent
        
        library(ggplot2)
        plot <- ggplot(df, ggplot2::aes(x = LD1, y = LD2, color = Groups)) +
          geom_point(alpha = 0.6) +
          #include circle---
          stat_ellipse(aes(fill = Groups), geom = "polygon", alpha = 0.2, level = 0.95) +
          geom_point(
            data = cent,
            ggplot2::aes(x = LD1, y = LD2, color = Groups),
            size = 7,
            shape = 17,
            alpha = 1
          ) +
          labs(title = "", x = "LD1", y = "LD2")
        
        plot <- plot + ggtheme
        print(plot)
        TRUE
      },
      
      #Histogram---
      .plot1 = function(image1, ...) {
        if (!self$options$plot1)
          return(FALSE)
        if (length(self$options$covs) <= 2)
          return()
        
        res <- private$.computeRES()
        if (is.null(res))
          return(FALSE)
        
        plot1 <- plot(res$lda.train, dimen = 1, type = "both")
        
        print(plot1)
        TRUE
      },
      
      #data cleaning---
      
      .computeRES = function() {
        dep <- self$options$dep
        covs <- self$options$covs
        per <- self$options$per
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        for (cov in covs)
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        
        data[[dep]] <- as.factor(data[[dep]])
        
        # dividing two datasets------------------------
        
        set.seed(1234) # Set seed for reproducibility
        
        # training_sample <- sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.7,0.3))
        # train <- data[training_sample, ]
        # test <- data[!training_sample, ]
        
        split1 <- caret::createDataPartition(data[[self$options$dep]], p = per, list = F)
        train <- data[split1, ]
        test <- data[-split1, ]
        
        formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
        formula <- as.formula(formula)
        
        ####LDA ANALYSIS##############################################
        
        lda.train <- MASS::lda(formula, data = train)
        
        ###################################################
        
        res <- list(lda.train = lda.train,
                    train = train,
                    test = test)
        return(res)
        
        
      }
    )
  )





#
#  # dividing two datasets------------------------
#
#  set.seed(1234) # Set seed for reproducibility
#
#  # training_sample <- sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.7,0.3))
#  # train <- data[training_sample, ]
#  # test <- data[!training_sample, ]
#
#  split1<- caret::createDataPartition(data[[self$options$dep]], p=per,list = F)
#  train <-data[split1,]
#  test <- data[-split1,]
#
#  formula <- jmvcore::constructFormula(self$options$dep, self$options$covs)
#  formula <- as.formula(formula)
#
#  ####LDA ANALYSIS##############################################
#
#  lda.train <- MASS::lda(formula, data=train)
#
# ###################################################

#res <- private$.computeRES()