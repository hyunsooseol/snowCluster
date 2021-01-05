
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
        
        #------------------------------------
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            
            <p> The rationale of Multiple Factor Analysis module is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
        },
        
        #---------------------------------------------
        
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
                
                # Quantitative variables colored by groups-------
                
                image <- self$results$plot1
                image$setState(mfa)
                
                # Contributions to dimension 1------------
                
                image <- self$results$plot2
                image$setState(mfa)
                
                # Contributions to dimension 2------------
                
                image <- self$results$plot3
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
        
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            mfa <- image$state
            
            plot1 <- factoextra::fviz_mfa_var(mfa, "quanti.var", palette = "jco", 
                                 col.var.sup = "violet", repel = TRUE)
            
            print(plot1)
            TRUE
            
        },
        
        .plot2 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            mfa <- image$state
            
            plot2 <- factoextra::fviz_contrib(mfa, choice = "quanti.var", axes = 1, top = 20,
                                  palette = "jco")
            print(plot2)
            TRUE
            
        },
        
        .plot3 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            mfa <- image$state
            
            plot3 <- factoextra::fviz_contrib(mfa, choice = "quanti.var", axes = 2, top = 20,
                                              palette = "jco")
            print(plot3)
            TRUE
            
        } 
        
        
           
        )
)
