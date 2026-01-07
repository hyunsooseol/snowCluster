
# This file is a generated template, your changes will not be overwritten

treeClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "treeClass",
    inherit = treeBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      
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
            '<li>The values for the target variable cannot be a number.</li>',
            '<li>Regression tree plot is only run when the dependent variable is a continuous variable.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        # if (self$options$cla)
        #   self$results$cla$setNote("Note", "By default, confusion matrix statistics treat the first factor level, based on alphabetical or numeric order, as the positive class.")
        
      },
      
      .run = function() {
        if (is.null(self$options$dep) || length(self$options$covs) == 0)
          return()
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.dataClear()
        }
        
        resdc <- private$.allCache

        # ---- Train Data ------------------------- #
        if (self$options$over1 || self$options$tab1) {
          # 1. 예측값 및 실제값 factor 변환 및 levels 통일 (test set과 동일하게)
          actual.train <- as.factor(resdc$train[[self$options$dep]])
          predicted.train <- as.factor(predict(resdc$mtrain, resdc$train))
          common_levels.train <- union(levels(actual.train), levels(predicted.train))
          actual.train <- factor(actual.train, levels = common_levels.train)
          predicted.train <- factor(predicted.train, levels = common_levels.train)
          
          # 3. confusionMatrix 실행
          
          eval1 <- caret::confusionMatrix(predicted.train, actual.train)
          
          # Overall statistics
          if (isTRUE(self$options$over1)) {
            table <- self$results$over1
            row <- list(
              accu = eval1[["overall"]][1],
              lower = eval1[["overall"]][3],
              upper = eval1[["overall"]][4],
              kappa = eval1[["overall"]][2]
            )
            table$setRow(rowNo = 1, values = row)
          }
          
          # Confusion matrix (행=Prediction, 열=Reference)
          if (self$options$tab1) {
            table <- self$results$tab1
            tab1  <- eval1$table
            res2 <- as.matrix(tab1)
            pred_names <- dimnames(res2)[[1]]    # Prediction (행)
            actual_names <- dimnames(res2)[[2]]  # Reference (열)
            
            # 열 생성: Reference(실제값) 명확히 표기
            for (name in actual_names) {
              table$addColumn(
                name = paste0(name),
                type = 'Integer',
                superTitle = 'Reference'
              )
            }
            # 행 추가: Prediction(예측값)
            for (i in seq_along(pred_names)) {
              row <- list()
              for (j in seq_along(actual_names)) {
                row[[actual_names[j]]] <- res2[pred_names[i], actual_names[j]]
              }
              table$addRow(rowKey = pred_names[i], values = row)
            }
          }
        }

        # ---- Test model --------------------------------------------- #
        if (self$options$over2 ||
            self$options$tab2 || self$options$cla) {
          
          # 1. 예측값 및 실제값 factor 변환 및 levels 통일
          actual2 <- as.factor(resdc$test[[self$options$dep]])
          predicted2 <- as.factor(predict(resdc$mtrain, resdc$test))
          common_levels2 <- union(levels(actual2), levels(predicted2))
          actual2 <- factor(actual2, levels = common_levels2)
          predicted2 <- factor(predicted2, levels = common_levels2)
          
          # 2. positive 값 존재 확인
          positive2 <- self$options$positive
          use_positive2 <- !is.null(positive2) && positive2 != "" && positive2 %in% common_levels2
          
          # 3. confusionMatrix 실행
          if (use_positive2) {
            eval2 <- caret::confusionMatrix(predicted2, actual2, positive = positive2)
          } else {
            eval2 <- caret::confusionMatrix(predicted2, actual2)
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
            table$setRow(rowNo = 1, values = row)
          }
          
          # Confusion matrix (행=Prediction, 열=Reference)
          if (self$options$tab2) {
            table <- self$results$tab2
            tab2  <- eval2$table
            res1  <- as.matrix(tab2)
            pred_names <- dimnames(res1)[[1]]    # Prediction(행)
            actual_names <- dimnames(res1)[[2]]  # Reference(열)
            
            # 열 생성: Reference(실제값) 명확히 표기
            for (name in actual_names)
              table$addColumn(
                name = paste0(name),
                type = 'Integer',
                superTitle = 'Reference'
              )
            # 행 추가: Prediction(예측값)
            for (i in seq_along(pred_names)) {
              row <- list()
              for (j in seq_along(actual_names))
                row[[actual_names[j]]] <- res1[pred_names[i], actual_names[j]]
              table$addRow(rowKey = pred_names[i], values = row)
            }
          }
          
          # Statistics by class (caret 기준 그대로)
          if (self$options$cla) {
            table <- self$results$cla
            cla <- eval2[["byClass"]]
            if (is.vector(cla)) {
              cla <- as.data.frame(t(cla))
            } else {
              cla <- as.data.frame(cla)
            }
            names <- rownames(cla)
            dims  <- colnames(cla)
            for (dim in dims)
              table$addColumn(name = paste0(dim), type = 'number')
            for (name in names) {
              row <- list()
              for (j in seq_along(dims))
                row[[dims[j]]] <- cla[name, j]
              table$addRow(rowKey = NULL, values = row) # 왼쪽 불필요한 인덱스 숨김
            }
          }
        }
      },
      .plot = function(image, ...) {
        if (!self$options$plot)
          return(FALSE)
        
        resdc <- private$.dataClear()
        plot  <- plot(resdc$mtrain)
        
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
        
        # if dep.var. is continuous---
        dep_var <- data[[self$options$dep]]
        if (!is.numeric(dep_var)) {
          return(FALSE)
        }
        
        set.seed(1234)
        model <- rpart::rpart(formula = as.formula(paste0(
          self$options$dep, " ~ ", paste(c(self$options$covs,self$options$facs), 
                                         collapse = " + "))),
        data = data,
        method = 'anova')
        
        plot2 <- rpart.plot::rpart.plot(
          model,
          type = 2,
          extra = 101,
          fallen.leaves = TRUE
          #main = "Regression Tree"
        )
        print(plot2)
        TRUE
      },
      
      .dataClear = function() {
        
        data <- self$data
        
        data[[self$options$dep]] <- as.factor(data[[self$options$dep]])
        
        for (cov in self$options$covs) {
          data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        }
        
        for (fac in self$options$facs) {
          data[[fac]] <- as.factor(data[[fac]])
          
        }
        data <- jmvcore::naOmit(data)
        
        split <- caret::createDataPartition(data[[self$options$dep]], p =
                                              self$options$per, list = FALSE)
        
        train <- data[split, ]
        test  <- data[-split, ]
        
        # mtrain <- party::ctree(formula=as.formula(paste0(self$options$dep, " ~ .")),
        #                        data=train)
        mtrain <- party::ctree(formula = as.formula(paste0(
          self$options$dep, " ~ ", paste(c(self$options$covs, self$options$facs), collapse = " + ")
        )), data = train)
        
        
        # rpart <- rpart::rpart(formula=as.formula(paste0(self$options$dep, " ~ .")),
        #                       data=train,
        #                       method='class')
        set.seed(1234)
        rpart <- rpart::rpart(formula = as.formula(paste0(
          self$options$dep, " ~ ", paste(c(self$options$covs, self$options$facs), collapse = " + ")
        )),
        data = train,
        method = 'class')

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
