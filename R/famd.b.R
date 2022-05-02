
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR FAMD
#' @importFrom factoextra fviz_screeplot
#' @importFrom factoextra fviz_famd_var
#' @importFrom factoextra fviz_famd_ind
#' @import factoextra
#' @import FactoMineR
#' @import ggplot2
#' @export


famdClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "famdClass",
    inherit = famdBase,
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
           <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p> 1. Specify 'Model Component'.</p>
            <p> 2. Type of value indicates 's' for the variables are scaled to unit variance,'n' for categorical variables, or 'f' for frequencies.</p>
            <p> 3. Move the variables into 'Variables'box.
            <p> 4. Factor box can be specified for visualizing 'individuals by group' plot. </p>
            <p> 5. The rationale of Multiple Factor Analysis is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/' target = '_blank'>page.</a></p>
            <p> 6. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },
        
        
        
        .run = function() {

            
            if(length(self$options$vars>2)){
            
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
            vars <- self$options$vars
            
            
            # Handling id----------
            
            if ( ! is.null(self$options$labels)) {
                rownames(data) <- data[[self$options$labels]]
                data[[self$options$labels]] <- NULL
            }
            
            for (i in seq_along(vars))
                data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            
            # analysis##########################
            
            res <- FactoMineR::FAMD(data, graph = FALSE)
            
            ##############################################
            
            eigen <- res$eig[,1]
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
            
            
            # contribution of  individuals-----------
            
            
            ind<-res$ind$contrib
            
            names <- dimnames(ind)[[1]]  
            
            table <- self$results$ci
            
            for (i in 1:5)
                
                table$addColumn(
                    name = paste0("pc", i),
                    title = as.character(i),
                    type = 'number',
                    superTitle = 'Dimension'
                )
            
            
            for (name in names) {
                
                row <- list()
                
                
                for (j in seq_along(1:5)) {
                    row[[paste0("pc", j)]] <- ind[name, j]
                }
                
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            # contribution of  variables-----------
            
            
            v<-res$var$contrib
            
            names <- dimnames(v)[[1]]  
            
            table <- self$results$cg
            
            for (i in 1:5)
                
                table$addColumn(
                    name = paste0("pc", i),
                    title = as.character(i),
                    type = 'number',
                    superTitle = 'Dimension'
                )
            
            
            for (name in names) {
                
                row <- list()
                
                
                for (j in seq_along(1:5)) {
                    row[[paste0("pc", j)]] <- v[name, j]
                }
                
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            # contribution of  quanti. variables-----------
            
            
            quanti<-res$quanti.var$contrib
            
            names <- dimnames(quanti)[[1]]  
            
            table <- self$results$quanti
            
            for (i in 1:5)
                
                table$addColumn(
                    name = paste0("pc", i),
                    title = as.character(i),
                    type = 'number',
                    superTitle = 'Dimension'
                )
            
            
            for (name in names) {
                
                row <- list()
                
                
                for (j in seq_along(1:5)) {
                    row[[paste0("pc", j)]] <- quanti[name, j]
                }
                
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            # contribution of  quali. variables-----------
            
            
            qual<-res$quali.var$contrib
            
            names <- dimnames(qual)[[1]]  
            
            table <- self$results$qual
            
            for (i in 1:5)
                
                table$addColumn(
                    name = paste0("pc", i),
                    title = as.character(i),
                    type = 'number',
                    superTitle = 'Dimension'
                )
            
            
            for (name in names) {
                
                row <- list()
                
                
                for (j in seq_along(1:5)) {
                    row[[paste0("pc", j)]] <- qual[name, j]
                }
                
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            # scree plot(plot)--------
            
            image <- self$results$plot
            image$setState(res) 
            
            
            # Graph of variables
            image <- self$results$plot1
            image$setState(res)
            
            image <- self$results$plot2
            image$setState(res)
            
            image <- self$results$plot3
            image$setState(res)
            
            image <- self$results$plot4
            image$setState(res)
            
            
            
        }
        
        },
          
              .plot = function(image, ggtheme, theme, ...) {
                
                if (length(self$options$vars) <= 2)
                    return()
                
                res <- image$state
                
                # Eigenvalues/variances of dimensions
                
                plot<- factoextra::fviz_screeplot(res)
                
                plot <- plot+ggtheme
                
                print(plot)
                TRUE
                
            },
        
        .plot1 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res <- image$state
            
            plot1<- factoextra::fviz_famd_var(res)
            
            plot1 <- plot1+ggtheme
            
            print(plot1)
            TRUE
            
        },
        
        .plot2 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res <- image$state
            
            # Quantitative variables
             
            plot2<- factoextra::fviz_famd_var(res, "quanti.var", repel = TRUE, col.var = "black")
            
            plot2 <- plot2+ggtheme
            
            print(plot2)
            TRUE
            
        },
        
        .plot3 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res <- image$state
            
            # # Qualitative variables
            
            plot3<- factoextra::fviz_famd_var(res, "quali.var", col.var = "black")
            
            plot3 <- plot3+ggtheme
            
            print(plot3)
            TRUE
            
        },
           
        .plot4 = function(image, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res <- image$state
            
            #  Graph of individuals---------
            
            plot4<- factoextra::fviz_famd_ind(res, 
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
            
            plot4 <- plot4+ggtheme
            
            print(plot4)
            TRUE
            
        }
        
        
        )
)
