

rocClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "rocClass",
    inherit = rocBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        
        if (is.null(self$options$dep) | is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Select the outcome category to be treated as the positive level in ROC analysis.</li>',
            '<li>With one predictor, you can evaluate the optimal cutpoint, a specified cutpoint, and the corresponding classification performance.</li>',
            '<li>With two or more predictors, you can compare AUCs using DeLong tests and obtain predictor-specific optimal cutpoints. Specified cutpoint analysis is not performed because a single cutpoint cannot be applied meaningfully to predictors with different scales.</li>',
            '<li>ROC analysis based on Binomial logistic regression.</li>',
            '<li>Perform ROC curve based on <a href="https://github.com/cardiomoon/multipleROC" target = "_blank">multipleROC R package</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
      },
      
      
      .run = function() {
        if (is.null(self$data) |
            is.null(self$options$dep) | is.null(self$options$covs))
          return()
        
        
        predictorCount <- length(self$options$covs)
        
        # Hide result tables that do not apply to the current number of predictors
        self$results$auc$setVisible(
          visible = isTRUE(self$options$auc) &&
            predictorCount >= 2
        )
        self$results$dif$setVisible(
          visible = isTRUE(self$options$auc) &&
            isTRUE(self$options$dif) &&
            predictorCount >= 2
        )
        self$results$overall$setVisible(
          visible = isTRUE(self$options$auc) &&
            isTRUE(self$options$overall) &&
            predictorCount >= 2
        )
        self$results$specifiedCutpoint$setVisible(
          visible = isTRUE(self$options$specifiedCutpoint) &&
            predictorCount == 1
        )
        
        # Example--------
        # multipleROC::multipleROC(am~wt,data=mtcars)
        # dep <- self$options$dep
        # covs <- self$options$covs
        # data <- self$data
        # data <- na.omit(data)
        # data <- as.data.frame(data)
        # #Formula(male~height+weight)------
        # covs <- vapply(covs, function(x)
        #   jmvcore::composeTerm(x), '')
        # 
        # formula <- as.formula(paste(paste(dep, paste0(covs, collapse = "+"), 
        #                                   sep ="~")))
        # dep <- jmvcore::composeTerm(self$options$dep)
        # covs <- vapply(self$options$covs, jmvcore::composeTerm, character(1))
        # 
        # data <- self$data
        # data <- na.omit(data)
        # data <- as.data.frame(data)
        # 
        # formula <- as.formula(
        #   paste(dep, paste(covs, collapse = " + "), sep = " ~ ")
        # )
        # 
        #         
        # # if(isTRUE(self$options$plot1)){
        # #
        # # image <- self$results$plot1
        # # image$setState(formula)
        # # }
        # p2 <- private$.computeP2()
        # #self$results$text$setContent(p2)
        # p3 <- private$.computeP3()
        
        if (isTRUE(self$options$auc)) {
          if (length(self$options$covs) < 2) {
            NULL
          } else{
            data <- self$data
            dep <- self$options$dep
            covs <- self$options$covs
            
            class <- self$options$dep
            df <- c(class, covs)
            data <- jmvcore::select(self$data, df)
            for (cov in covs)
              data[[cov]] <- jmvcore::toNumeric(data[[cov]])
            data <- jmvcore::naOmit(data)
            #delong test function------------
            deLong.test <- function(x,
                                    labels,
                                    labpos,
                                    ref = NULL,
                                    conf.level = 0.95)
            {
              if (length(labels) != dim(x)[1])
                stop("\n The number of rows in x must match the length of labels\n")
              #id.pos <- labels == labpos
              id.pos <- as.character(labels) == as.character(labpos)
              
              if (sum(id.pos) < 1)
                stop("\n wrong level specified!\n")
              if (dim(x)[2] < 2)
                stop("\n x must contain at least two columns!\n")
              if (dim(x)[1] < 2)
                stop("\n x must contain at least two rows!\n")
              nn <- sum(!id.pos)
              np <- sum(id.pos)
              nauc <- ncol(x)
              
              if (is.null(ref)) {
                L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
                newa <- 0
                for (i in 1:(nauc - 1)) {
                  newl <- nauc - i
                  L[(newa + 1):(newa + newl), i] <- rep(1, newl)
                  L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <- diag(-1, nrow =
                                                                              newl, ncol = newl)
                  newa <- newa + newl
                }
              }
              else {
                # test for superiority of one method against all others)
                if (ref > nauc)
                  stop(
                    paste(
                      "Reference ref must be one of the markers (1...",
                      nauc,
                      " in this case)",
                      sep = ""
                    )
                  )
                L <- matrix(1, ncol = nauc, nrow = nauc - 1)
                L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
              }
              
              markern <- as.matrix(x[!id.pos, ])
              markerp <- as.matrix(x[id.pos, ])
              
              ###
              ### compute wilcox statistic
              ###
              WK.STAT <- function(x, y) {
                r <- rank(c(x, y))
                n.x <- length(x)
                n.y <- length(y)
                STATISTIC <- sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2
                STATISTIC
              }
              
              auc <- vector("numeric", length = nauc)
              for (r in 1:nauc) {
                auc[r] <- WK.STAT(markerp[, r], markern[, r])
              }
              auc <- auc / (nn * np)
              
              ###
              ### if AUCs smaller than 0.5: 1-auc
              ###
              if (any(auc < 0.5)) {
                x[, auc < 0.5] <- -x[, auc < 0.5]
                auc[auc < 0.5] <- 1 - auc[auc < 0.5]
                markern <- as.matrix(x[!id.pos, ])
                markerp <- as.matrix(x[id.pos, ])
              }
              
              V10 <- matrix(0, nrow = np, ncol = nauc)
              V01 <- matrix(0, nrow = nn, ncol = nauc)
              
              tmn <- t(markern)
              tmp <- t(markerp)
              for (i in 1:np) {
                V10[i, ] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
              }
              for (i in 1:nn) {
                V01[i, ] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
              }
              V10 <- V10 / nn
              V01 <- V01 / np
              
              W10 <- cov(V10)
              W01 <- cov(V01)
              
              ###
              ### estimated covariance matrix
              ###
              S <- W10 / np + W01 / nn
              
              ###
              ### compute variances of AUCs and test for AUC > 0.5
              ###
              
              ### Hanley, McNeil (1982)
              q1 <- auc / (2 - auc)
              q2 <- 2 * auc ^ 2 / (1 + auc)
              
              ### Haney, McNeil (1982) / Bamber (1975)
              aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc ^ 2) + (nn - 1) *
                           (q2 - auc ^ 2)) / (np * nn)
              zhalf <- (auc - 0.5) / sqrt(aucvar)
              phalf <- 1 - pnorm(zhalf)
              zdelong <- (auc - 0.5) / sqrt(diag(S))
              pdelong <- 1 - pnorm(zdelong)
              
              
              ### global p-value
              
              aucdiff <- L %*% auc
              z <- t(aucdiff) %*% rms::matinv(L %*% S %*% t(L)) %*% aucdiff
              p <- pchisq(z,
                          df = qr(L %*% S %*% t(L))$rank,
                          lower.tail = FALSE)
              
              if (is.null(ref)) {
                cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
                ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
                ctr <- 1
                rows <- vector("character", length = (nauc * (nauc - 1) / 2))
                pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
                quantil <- qnorm(1 - (1 - conf.level) / 2)
                for (i in 1:(nauc - 1)) {
                  for (j in (i + 1):nauc) {
                    cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])
                    LSL <- t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
                    tmpz <- (aucdiff[ctr]) %*% rms::matinv(LSL) %*% aucdiff[ctr]
                    pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
                    ci[ctr, ] <- c(aucdiff[ctr] - quantil * sqrt(LSL),
                                   aucdiff[ctr] + quantil * sqrt(LSL))
                    rows[ctr] <- paste(i, j, sep = " vs. ")
                    ctr <- ctr + 1
                  }
                }
              } else {
                cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
                ci <- matrix(ncol = 2, nrow = nauc - 1)
                rows <- vector("character", length = nauc - 1)
                pairp <- matrix(nrow = nauc - 1, ncol = 1)
                comp <- (1:nauc)[-ref]
                for (i in 1:(nauc - 1)) {
                  cor.auc[i] <- S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])
                  LSL <- t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
                  tmpz <- aucdiff[i] %*% rms::matinv(LSL) %*% aucdiff[i]
                  pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
                  ci[i, ] <- c(aucdiff[i] - quantil * sqrt(LSL),
                               aucdiff[i] + quantil * sqrt(LSL))
                  rows[i] <- paste(ref, comp[i], sep = " vs. ")
                }
              }
              
              newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
              names(newres) <- c("AUC Difference",
                                 "CI(lower)",
                                 "CI(upper)",
                                 "P.Value",
                                 "Correlation")
              rownames(newres) <- rows
              row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
              colnames(ci) <- c(
                paste0(100 * conf.level, "% CI (lower)"),
                paste0(100 * conf.level, "% CI (upper)")
              )
              names(auc) <- 1:nauc
              auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
              colnames(auc) <- c("AUC",
                                 "SD(Hanley)",
                                 "P(H0: AUC=0.5)",
                                 "SD(DeLong)",
                                 "P(H0: AUC=0.5)")
              
              ERG <- list(
                AUC = auc,
                difference = newres,
                covariance = S,
                global.z = z,
                global.p = p
              )
              class(ERG) <- "DeLong"
              ERG
            }
            
            # delong test ---------------------
            
            # res <- deLong.test(x = data[, -1],
            #                    labels = data[, 1],
            #                    labpos = "1")
            
            # self$results$delong$setVisible(visible = TRUE)
            # self$results$delong$setContent(delongres)
            # labels <- data[[1]]
            # 
            # if (is.factor(labels)) {
            #   labelLevels <- levels(droplevels(labels))
            # } else {
            #   labelLevels <- unique(labels[!is.na(labels)])
            # }
            # 
            # if (length(labelLevels) != 2) {
            #   stop("The dependent variable must have exactly two levels.")
            # }
            # 
            # positiveLevel <- labelLevels[2]
            # 
            # res <- deLong.test(
            #   x = data[, -1, drop = FALSE],
            #   labels = labels,
            #   labpos = positiveLevel
            # )            
            labels <- data[[1]]
            positiveLevel <- self$options$positive
            
            if (is.factor(labels)) {
              labelLevels <- levels(droplevels(labels))
            } else {
              labelLevels <- unique(labels[!is.na(labels)])
            }
            
            if (length(labelLevels) != 2) {
              stop("The dependent variable must have exactly two levels.")
            }
            
            if (is.null(positiveLevel) || length(positiveLevel) == 0) {
              stop("Please specify the positive level.")
            }
            
            if (!as.character(positiveLevel) %in% as.character(labelLevels)) {
              stop("The selected positive level is not a valid level of the dependent variable.")
            }
            
            res <- deLong.test(
              x = data[, -1, drop = FALSE],
              labels = labels,
              labpos = positiveLevel
            )
            #----------------------------------------
            table <- self$results$auc
            res1 <- res$AUC
            names <- dimnames(res$AUC)[[1]]
            
            for (name in names) {
              row <- list()
              row[["auc"]] <- res1[name, 1]
              row[["p"]] <- res1[name, 5]
              
              table$addRow(rowKey = name, values = row)
            }
            
            #--------------------------------------
            if (isTRUE(self$options$dif)) {
              table <- self$results$dif
              res1 <- res$difference
              lapply(rownames(res1), function(name) {
                row <- list(
                  auc = res1[name, 1],
                  lower = res1[name, 2],
                  upper = res1[name, 3],
                  p = res1[name, 4]
                )
                table$addRow(rowKey = name, values = row)
              })
            }
            #---------------------
            if (isTRUE(self$options$overall)) {
              table <- self$results$overall
              row <- list(
                Z = as.vector(res$global.z),
                p = as.vector(res$global.p)
              )
              table$setRow(rowNo = 1, values = row)
            }
          }
        }
        
        if (isTRUE(self$options$optimalCutpoint) ||
            isTRUE(self$options$specifiedCutpoint) ||
            isTRUE(self$options$classificationTable)) {
          private$.runClassificationPerformance()
        }
      },
      
      .runClassificationPerformance = function() {
        depName <- self$options$dep
        covs <- self$options$covs
        positiveLevel <- self$options$positive
        
        if (is.null(depName) || is.null(covs) || length(covs) == 0)
          return()
        
        if (is.null(positiveLevel) || length(positiveLevel) == 0)
          stop("Please specify the positive level.")
        
        depValues <- self$data[[depName]]
        depLevels <- levels(droplevels(as.factor(depValues)))
        
        if (length(depLevels) != 2)
          stop("The dependent variable must have exactly two levels.")
        
        positiveLevel <- as.character(positiveLevel)
        
        if (!positiveLevel %in% as.character(depLevels))
          stop("The selected positive level is not a valid level of the dependent variable.")
        
        specifiedValue <- self$options$cutpointValue
        useSpecifiedCutpoint <-
          isTRUE(self$options$specifiedCutpoint) &&
          length(covs) == 1
        
        for (predictor in covs) {
          values <- jmvcore::toNumeric(self$data[[predictor]])
          outcome <- as.character(depValues)
          
          keep <- !is.na(values) &
            is.finite(values) &
            !is.na(outcome)
          
          values <- values[keep]
          outcome <- outcome[keep]
          positive <- outcome == positiveLevel
          
          if (length(values) == 0)
            next
          
          if (sum(positive) == 0 || sum(!positive) == 0)
            next
          
          optimal <- private$.findOptimalCutpoint(
            values = values,
            positive = positive
          )
          
          if (isTRUE(self$options$optimalCutpoint)) {
            table <- self$results$optimalCutpoint
            row <- list(
              predictor = predictor,
              n = optimal$total,
              auc = optimal$auc,
              cutpoint = optimal$cutpoint,
              direction = optimal$direction,
              sensitivity = optimal$sensitivity,
              specificity = optimal$specificity,
              youden = optimal$youden,
              accuracy = optimal$accuracy,
              ppv = optimal$ppv,
              npv = optimal$npv,
              lrPositive = optimal$lrPositive,
              lrNegative = optimal$lrNegative
            )
            table$addRow(rowKey = predictor, values = row)
          }
          
          specified <- NULL
          
          if (useSpecifiedCutpoint) {
            observedRange <- range(values, na.rm = TRUE)
            
            if (!is.finite(specifiedValue)) {
              stop("Please enter a valid specified cutpoint.")
            }
            
            if (specifiedValue < observedRange[1] ||
                specifiedValue > observedRange[2]) {
              stop(
                paste0(
                  "The specified cutpoint for '",
                  predictor,
                  "' must be within the observed range: ",
                  observedRange[1],
                  " to ",
                  observedRange[2],
                  "."
                )
              )
            }
            
            specified <- private$.classificationMetrics(
              values = values,
              positive = positive,
              cutpoint = specifiedValue,
              direction = optimal$direction
            )
            
            table <- self$results$specifiedCutpoint
            row <- list(
              predictor = predictor,
              n = specified$total,
              cutpoint = specified$cutpoint,
              direction = specified$direction,
              sensitivity = specified$sensitivity,
              specificity = specified$specificity,
              accuracy = specified$accuracy,
              balancedAccuracy = specified$balancedAccuracy,
              ppv = specified$ppv,
              npv = specified$npv,
              lrPositive = specified$lrPositive,
              lrNegative = specified$lrNegative
            )
            table$addRow(rowKey = predictor, values = row)
          }
          
          if (isTRUE(self$options$classificationTable)) {
            if (!is.null(specified)) {
              selected <- specified
              cutpointSource <- "Specified"
            } else {
              selected <- optimal
              cutpointSource <- "Optimal (Youden)"
            }
            
            table <- self$results$classificationTable
            row <- list(
              predictor = predictor,
              cutpointSource = cutpointSource,
              cutpoint = selected$cutpoint,
              direction = selected$direction,
              truePositive = selected$truePositive,
              falseNegative = selected$falseNegative,
              falsePositive = selected$falsePositive,
              trueNegative = selected$trueNegative,
              total = selected$total
            )
            table$addRow(rowKey = predictor, values = row)
          }
        }
      },
      
      .findOptimalCutpoint = function(values, positive) {
        positiveValues <- values[positive]
        negativeValues <- values[!positive]
        
        comparisons <- outer(
          positiveValues,
          negativeValues,
          FUN = "-"
        )
        
        aucIncreasing <- (
          sum(comparisons > 0) +
            0.5 * sum(comparisons == 0)
        ) / length(comparisons)
        
        if (aucIncreasing >= 0.5) {
          direction <- ">="
          auc <- aucIncreasing
        } else {
          direction <- "<="
          auc <- 1 - aucIncreasing
        }
        
        cutpoints <- sort(unique(values))
        
        metrics <- lapply(
          cutpoints,
          function(cutpoint) {
            private$.classificationMetrics(
              values = values,
              positive = positive,
              cutpoint = cutpoint,
              direction = direction
            )
          }
        )
        
        youden <- vapply(metrics, function(x) x$youden, numeric(1))
        best <- which(youden == max(youden, na.rm = TRUE))
        
        if (length(best) > 1) {
          accuracy <- vapply(
            metrics[best],
            function(x) x$accuracy,
            numeric(1)
          )
          best <- best[which.max(accuracy)]
        }
        
        result <- metrics[[best[1]]]
        result$auc <- auc
        result$direction <- direction
        result
      },
      
      .classificationMetrics = function(values,
                                        positive,
                                        cutpoint,
                                        direction) {
        if (identical(direction, ">=")) {
          predictedPositive <- values >= cutpoint
        } else {
          predictedPositive <- values <= cutpoint
        }
        
        truePositive <- sum(predictedPositive & positive)
        falsePositive <- sum(predictedPositive & !positive)
        trueNegative <- sum(!predictedPositive & !positive)
        falseNegative <- sum(!predictedPositive & positive)
        
        positiveN <- truePositive + falseNegative
        negativeN <- trueNegative + falsePositive
        predictedPositiveN <- truePositive + falsePositive
        predictedNegativeN <- trueNegative + falseNegative
        total <- positiveN + negativeN
        
        sensitivity <- if (positiveN > 0)
          truePositive / positiveN
        else
          NA_real_
        
        specificity <- if (negativeN > 0)
          trueNegative / negativeN
        else
          NA_real_
        
        accuracy <- if (total > 0)
          (truePositive + trueNegative) / total
        else
          NA_real_
        
        balancedAccuracy <- mean(
          c(sensitivity, specificity),
          na.rm = TRUE
        )
        
        ppv <- if (predictedPositiveN > 0)
          truePositive / predictedPositiveN
        else
          NA_real_
        
        npv <- if (predictedNegativeN > 0)
          trueNegative / predictedNegativeN
        else
          NA_real_
        
        lrPositive <- if (is.na(sensitivity) || is.na(specificity)) {
          NA_real_
        } else if ((1 - specificity) == 0) {
          if (sensitivity > 0) Inf else NA_real_
        } else {
          sensitivity / (1 - specificity)
        }
        
        lrNegative <- if (is.na(sensitivity) || is.na(specificity)) {
          NA_real_
        } else if (specificity == 0) {
          if ((1 - sensitivity) > 0) Inf else NA_real_
        } else {
          (1 - sensitivity) / specificity
        }
        
        list(
          cutpoint = cutpoint,
          sensitivity = if (is.finite(sensitivity)) sensitivity else NA_real_,
          specificity = if (is.finite(specificity)) specificity else NA_real_,
          youden = if (is.finite(sensitivity + specificity - 1))
            sensitivity + specificity - 1
          else
            NA_real_,
          accuracy = if (is.finite(accuracy)) accuracy else NA_real_,
          balancedAccuracy = if (is.finite(balancedAccuracy))
            balancedAccuracy
          else
            NA_real_,
          ppv = if (is.finite(ppv)) ppv else NA_real_,
          npv = if (is.finite(npv)) npv else NA_real_,
          lrPositive = if (is.finite(lrPositive)) lrPositive else NA_real_,
          lrNegative = if (is.finite(lrNegative)) lrNegative else NA_real_,
          truePositive = truePositive,
          falseNegative = falseNegative,
          falsePositive = falsePositive,
          trueNegative = trueNegative,
          total = total,
          direction = direction
        )
      },
      
      .plot1 = function(image, ...) {
        if (!self$options$plot1)
          return(FALSE)
        
        depName <- self$options$dep
        positiveLevel <- self$options$positive
        
        dep <- jmvcore::composeTerm(depName)
        covs <- vapply(
          self$options$covs,
          jmvcore::composeTerm,
          character(1)
        )
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        # Apply the selected positive level
        depValues <- data[[depName]]
        depLevels <- levels(droplevels(as.factor(depValues)))
        
        if (length(depLevels) != 2) {
          stop("The dependent variable must have exactly two levels.")
        }
        
        if (is.null(positiveLevel) || length(positiveLevel) == 0) {
          stop("Please specify the positive level.")
        }
        
        positiveLevel <- as.character(positiveLevel)
        
        if (!positiveLevel %in% as.character(depLevels)) {
          stop(
            "The selected positive level is not a valid level of the dependent variable."
          )
        }
        
        negativeLevel <- setdiff(
          as.character(depLevels),
          positiveLevel
        )
        
        data[[depName]] <- factor(
          as.character(depValues),
          levels = c(negativeLevel, positiveLevel)
        )
        
        formula <- as.formula(
          paste(
            dep,
            paste(covs, collapse = " + "),
            sep = " ~ "
          )
        )
        
        plot1 <- multipleROC::multipleROC(
          formula,
          data = data
        )
        
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (!self$options$plot2)
          return(FALSE)
        
        p2 <- private$.computeP2()
        
        plot2 <- multipleROC::plot_ROC(p2, show.eta = FALSE, show.sens = FALSE)
        
        #  plot2 <- plot2+ggtheme
        
        print(plot2)
        TRUE
        
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (!self$options$plot3)
          return(FALSE)
        
        p3 <- private$.computeP3()
        plot3 <- multipleROC::plot_ROC(p3, facet = TRUE)
        #  plot3 <- plot3+ggtheme
        print(plot3)
        TRUE
        
      },
      
      #Function---
      
      .computeP2 = function() {
        depName <- self$options$dep
        positiveLevel <- self$options$positive
        
        dep <- jmvcore::composeTerm(depName)
        covs <- vapply(
          self$options$covs,
          jmvcore::composeTerm,
          character(1)
        )
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        # Apply the selected positive level
        depValues <- data[[depName]]
        depLevels <- levels(droplevels(as.factor(depValues)))
        
        if (length(depLevels) != 2) {
          stop("The dependent variable must have exactly two levels.")
        }
        
        if (is.null(positiveLevel) || length(positiveLevel) == 0) {
          stop("Please specify the positive level.")
        }
        
        positiveLevel <- as.character(positiveLevel)
        
        if (!positiveLevel %in% as.character(depLevels)) {
          stop(
            "The selected positive level is not a valid level of the dependent variable."
          )
        }
        
        negativeLevel <- setdiff(
          as.character(depLevels),
          positiveLevel
        )
        
        data[[depName]] <- factor(
          as.character(depValues),
          levels = c(negativeLevel, positiveLevel)
        )
        
        roc <- list()
        
        for (i in seq_along(covs)) {
          formula <- as.formula(
            paste(dep, covs[[i]], sep = " ~ ")
          )
          
          roc[[i]] <- multipleROC::multipleROC(
            formula,
            data = data,
            plot = FALSE
          )
        }
        
        return(roc)
      },
      
      .computeP3 = function() {
        depName <- self$options$dep
        positiveLevel <- self$options$positive
        
        dep <- jmvcore::composeTerm(depName)
        covs <- vapply(
          self$options$covs,
          jmvcore::composeTerm,
          character(1)
        )
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        # Apply the selected positive level
        depValues <- data[[depName]]
        depLevels <- levels(droplevels(as.factor(depValues)))
        
        if (length(depLevels) != 2) {
          stop("The dependent variable must have exactly two levels.")
        }
        
        if (is.null(positiveLevel) || length(positiveLevel) == 0) {
          stop("Please specify the positive level.")
        }
        
        positiveLevel <- as.character(positiveLevel)
        
        if (!positiveLevel %in% as.character(depLevels)) {
          stop(
            "The selected positive level is not a valid level of the dependent variable."
          )
        }
        
        negativeLevel <- setdiff(
          as.character(depLevels),
          positiveLevel
        )
        
        data[[depName]] <- factor(
          as.character(depValues),
          levels = c(negativeLevel, positiveLevel)
        )
        
        roc <- list()
        
        for (i in seq_along(covs)) {
          formula <- as.formula(
            paste(dep, covs[[i]], sep = " ~ ")
          )
          
          roc[[i]] <- multipleROC::multipleROC(
            formula,
            data = data,
            plot = FALSE
          )
        }
        
        return(roc)
      }
    )
  )