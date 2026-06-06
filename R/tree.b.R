# This file is a generated template, your changes will not be overwritten

treeClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(
      .htmlwidget = NULL,
      
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$options$dep) ||
            (length(self$options$covs) == 0 &&
             length(self$options$facs) == 0)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>For classification trees, the target variable is treated as categorical.</li>',
              '<li>The regression tree plot is available only when the target variable is continuous.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
        # if (self$options$cla)
        #   self$results$cla$setNote(
        #     "Note",
        #     "By default, confusion matrix statistics treat the first factor level, based on alphabetical or numeric order, as the positive class."
        #   )
      },
      
      
      .run = function() {
        
        if (is.null(self$options$dep) ||
            (length(self$options$covs) == 0 &&
             length(self$options$facs) == 0))
          return()
        
        resdc <- private$.dataClear()
        
        
        # ---- Train Data ------------------------- #
        
        if (self$options$over1 || self$options$tab1) {
          
          # 예측값 및 실제값 factor 변환 및 levels 통일
          actual.train <- as.factor(
            resdc$train[[self$options$dep]]
          )
          
          predicted.train <- as.factor(
            predict(resdc$mtrain, resdc$train)
          )
          
          common_levels.train <- union(
            levels(actual.train),
            levels(predicted.train)
          )
          
          actual.train <- factor(
            actual.train,
            levels = common_levels.train
          )
          
          predicted.train <- factor(
            predicted.train,
            levels = common_levels.train
          )
          
          
          # Confusion matrix
          eval1 <- caret::confusionMatrix(
            predicted.train,
            actual.train
          )
          
          
          # Overall statistics
          if (isTRUE(self$options$over1)) {
            
            table <- self$results$over1
            
            row <- list(
              accu = eval1[["overall"]][1],
              lower = eval1[["overall"]][3],
              upper = eval1[["overall"]][4],
              kappa = eval1[["overall"]][2]
            )
            
            table$setRow(
              rowNo = 1,
              values = row
            )
          }
          
          
          # Confusion matrix
          # 행 = Prediction, 열 = Reference
          if (self$options$tab1) {
            
            table <- self$results$tab1
            tab1 <- eval1$table
            res2 <- as.matrix(tab1)
            
            pred_names <- dimnames(res2)[[1]]
            actual_names <- dimnames(res2)[[2]]
            
            
            # 열 생성: Reference
            for (name in actual_names) {
              
              table$addColumn(
                name = paste0(name),
                type = 'Integer',
                superTitle = 'Reference'
              )
            }
            
            
            # 행 추가: Prediction
            for (i in seq_along(pred_names)) {
              
              row <- list()
              
              for (j in seq_along(actual_names)) {
                
                row[[actual_names[j]]] <-
                  res2[pred_names[i], actual_names[j]]
              }
              
              table$addRow(
                rowKey = pred_names[i],
                values = row
              )
            }
          }
        }
        
        
        # ---- Test model ------------------------- #
        
        if (self$options$over2 ||
            self$options$tab2 ||
            self$options$cla) {
          
          # 예측값 및 실제값 factor 변환 및 levels 통일
          actual2 <- as.factor(
            resdc$test[[self$options$dep]]
          )
          
          predicted2 <- as.factor(
            predict(resdc$mtrain, resdc$test)
          )
          
          common_levels2 <- union(
            levels(actual2),
            levels(predicted2)
          )
          
          actual2 <- factor(
            actual2,
            levels = common_levels2
          )
          
          predicted2 <- factor(
            predicted2,
            levels = common_levels2
          )
          
          
          # Positive level 확인
          positive2 <- self$options$positive
          
          use_positive2 <-
            length(common_levels2) == 2L &&
            !is.null(positive2) &&
            length(positive2) == 1L &&
            !is.na(positive2) &&
            nzchar(positive2) &&
            positive2 %in% common_levels2
          
          
          # Confusion matrix
          if (use_positive2) {
            
            eval2 <- caret::confusionMatrix(
              predicted2,
              actual2,
              positive = positive2
            )
            
          } else {
            
            eval2 <- caret::confusionMatrix(
              predicted2,
              actual2
            )
          }
          
          
          # Overall statistics
          if (isTRUE(self$options$over2)) {
            
            table <- self$results$over2
            
            row <- list(
              accu = eval2[["overall"]][1],
              lower = eval2[["overall"]][3],
              upper = eval2[["overall"]][4],
              kappa = eval2[["overall"]][2]
            )
            
            table$setRow(
              rowNo = 1,
              values = row
            )
          }
          
          
          # Confusion matrix
          # 행 = Prediction, 열 = Reference
          if (self$options$tab2) {
            
            table <- self$results$tab2
            tab2 <- eval2$table
            res1 <- as.matrix(tab2)
            
            pred_names <- dimnames(res1)[[1]]
            actual_names <- dimnames(res1)[[2]]
            
            
            # 열 생성: Reference
            for (name in actual_names) {
              
              table$addColumn(
                name = paste0(name),
                type = 'Integer',
                superTitle = 'Reference'
              )
            }
            
            
            # 행 추가: Prediction
            for (i in seq_along(pred_names)) {
              
              row <- list()
              
              for (j in seq_along(actual_names)) {
                
                row[[actual_names[j]]] <-
                  res1[pred_names[i], actual_names[j]]
              }
              
              table$addRow(
                rowKey = pred_names[i],
                values = row
              )
            }
          }
          
          
          # Statistics by class
          if (self$options$cla) {
            
            table <- self$results$cla
            cla <- eval2[["byClass"]]
            
            
            # 이진분류
            if (is.vector(cla)) {
              
              cla <- as.data.frame(t(cla))
              
              class_name <- if (use_positive2) {
                positive2
              } else {
                common_levels2[1]
              }
              
              rownames(cla) <- class_name
              
              
              # 다중분류
            } else {
              
              cla <- as.data.frame(cla)
              
              # "Class: A" 형식을 "A"로 정리
              rownames(cla) <- sub(
                "^Class:\\s*",
                "",
                rownames(cla)
              )
            }
            
            class_names <- rownames(cla)
            dims <- colnames(cla)
            
            
            # 통계량 열 생성
            for (dim in dims) {
              
              table$addColumn(
                name = paste0(dim),
                type = 'number'
              )
            }
            
            
            # 클래스별 결과 행 추가
            for (class_name in class_names) {
              
              row <- list()
              
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- cla[class_name, j]
              }
              
              table$addRow(
                rowKey = class_name,
                values = row
              )
            }
          }
        }
      },
      
      
      .plot = function(image, ...) {
        
        if (!self$options$plot)
          return(FALSE)
        
        resdc <- private$.dataClear()
        plot <- plot(resdc$mtrain)
        
        print(plot)
        
        TRUE
      },
      
      
      .plot1 = function(image1, ...) {
        
        if (!self$options$plot1)
          return(FALSE)
        
        resdc <- private$.dataClear()
        plot1 <- rpart.plot::rpart.plot(resdc$rpart)
        
        print(plot1)
        
        TRUE
      },
      
      
      .plot2 = function(image, ...) {
        
        if (!self$options$plot2)
          return(FALSE)
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        for (cov in self$options$covs) {
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        }
        
        for (fac in self$options$facs) {
          data[[fac]] <- as.factor(data[[fac]])
        }
        
        
        # Dependent variable must be continuous
        dep_var <- data[[self$options$dep]]
        
        if (!is.numeric(dep_var)) {
          return(FALSE)
        }
        
        set.seed(1234)
        
        formula <- jmvcore::constructFormula(
          self$options$dep,
          c(self$options$covs, self$options$facs)
        )
        
        formula <- as.formula(formula)
        
        model <- rpart::rpart(
          formula = formula,
          data = data,
          method = 'anova'
        )
        
        plot2 <- rpart.plot::rpart.plot(
          model,
          type = 2,
          extra = 101,
          fallen.leaves = TRUE
        )
        
        print(plot2)
        
        TRUE
      },
      
      
      .dataClear = function() {
        
        data <- self$data
        
        data[[self$options$dep]] <-
          as.factor(data[[self$options$dep]])
        
        for (cov in self$options$covs) {
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        }
        
        for (fac in self$options$facs) {
          data[[fac]] <- as.factor(data[[fac]])
        }
        
        data <- jmvcore::naOmit(data)
        
        
        # 동일한 데이터와 옵션에서 동일한 train/test 분할
        set.seed(1234)
        
        split <- caret::createDataPartition(
          data[[self$options$dep]],
          p = self$options$per,
          list = FALSE
        )
        
        train <- data[split, ]
        test <- data[-split, ]
        
        
        formula <- jmvcore::constructFormula(
          self$options$dep,
          c(self$options$covs, self$options$facs)
        )
        
        formula <- as.formula(formula)
        
        
        # Conditional inference tree
        mtrain <- party::ctree(
          formula = formula,
          data = train
        )
        
        
        # Classification tree
        set.seed(1234)
        
        rpart <- rpart::rpart(
          formula = formula,
          data = train,
          method = 'class'
        )
        
        
        retlist <- list(
          train = train,
          test = test,
          mtrain = mtrain,
          rpart = rpart
        )
        
        return(retlist)
      }
    )
  )


# Example---------

# data(iris)
#
# split1<- caret::createDataPartition(iris$Species, p=0.7,list = F)
#
# split1_train <-iris[split1,]
# split1_test <- iris[-split1,]
#
# model <- rpart::rpart(Species~., data=split1_train,
#                       control = rpart.control(minsplit=2))
#
# rpart.plot::rpart.plot(model, tweak = 1.1)


#Analysis---------------------------------
#set.seed(1234)

# Example(iris data)----------------

# data(iris)
# model <- party::ctree(Species ~ .,data = iris)
# plot(model)

# pred <- predict(model)
# actual <- iris$Species

# table(predict(model), iris$Species)
# eval<- caret::confusionMatrix(pred,actual)
