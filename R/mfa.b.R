
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR MFA
#' @importFrom factoextra get_eigenvalue
#' @import FactoMineR
#' @import factoextra
#' @export



mfaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mfaClass",
    inherit = mfaBase,
    private = list(
        .run = function() {

            
            
            
            if(length(self$options$vars>2)){
                
                vars <- self$options$vars
                
                k <- self$options$k #number of groups
                
              
                group <- as.numeric(strsplit(self$options$group, ',')[[1]])
                
              
                type <- self$options$type # n:category, s:unit variable
                
                type1 <- strsplit(self$options$type, ',')[[1]]
                
                
                gn <- self$options$gn # group name
                
                gn1 <- strsplit(self$options$gn, ',')[[1]]
                
                
                data <- self$data
                
                data <- jmvcore::naOmit(data)
                
                
                # Handling id----------
                
                if ( ! is.null(self$options$labels)) {
                    rownames(data) <- data[[self$options$labels]]
                    data[[self$options$labels]] <- NULL
                }
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                
                ##### MFA analysis---------
                
                # res.mfa <- MFA(data, 
                #                group = c(1,3, 4, 3), 
                #                type = c("n","s", "s","s"),
                #                name.group = c("Oak_type","Expert1","Expert2","Expert3"),
                #                num.group.sup =c(1, 4),
                #                graph = FALSE)
                # 
                
                
                mfa <- FactoMineR::MFA(data,
                                          group=group,
                                          type=type1,
                                          name.group = gn1,
                                          num.group.sup= k,
                                          graph = FALSE)  
                
                
             #   self$results$text$setContent(mfa)
                
                eigen <- mfa$eig[,1]
                eigen<- as.vector(eigen)
                
                # eigenvalue table-------------
                
                table <- self$results$eigen
                
                for (i in seq_along(eigen))
                    table$addRow(rowKey=i, values=list(comp = as.character(i)))
                
                # populating eigenvalue table-----
                
                eigenTotal <- sum(abs(eigen))
                varProp <- (abs(eigen) / eigenTotal) * 100
                varCum <- cumsum(varProp)
                
                for (i in seq_along(eigen)) {
                    
                    row <- list()
                    row[["eigen"]] <- eigen[i]
                    row[["varProp"]] <- varProp[i]
                    row[["varCum"]] <- varCum[i]
                    
                    
                    table$setRow(rowNo=i, values=row)
                }
                
            
                # Plot==================================================
                
                #  Groups of variables----------
                
                image <- self$results$plot
                image$setState(mfa)
                
                
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            mfa <- image$state
            
            plot <- factoextra::fviz_mfa_var(mfa, "group")
            
            print(plot)
            TRUE
        
        }
        
        
            
        )
)
