
# This file is a generated template, your changes will not be overwritten

#' @importFrom factoextra fviz_pca_var
#' @importFrom factoextra fviz_pca_ind
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom FactoMineR PCA
#' @import ggplot2
#' @export


pcaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcaClass",
    inherit = pcaBase,
    private = list(
        .run = function() {

            
          if (!is.null(self$options$vars)) {
            
            vars <- self$options$vars
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
            
            # Handling id----------
            
            if ( ! is.null(self$options$labels)) {
              rownames(data) <- data[[self$options$labels]]
              data[[self$options$labels]] <- NULL
            }
            
            for (i in seq_along(vars))
              data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            
            # principal component analysis---------
            
            pca <- FactoMineR::PCA(data,  graph = FALSE)
            
            
            ### get eigenvalues------------
            
            eigen <- pca$eig[,1]
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
            
            
            # Variable contributions plot----------
            
            image <- self$results$plot
            image$setState(pca)
            
            # Individual plot-------
            
            
            image1 <- self$results$plot1
            image1$setState(pca)
            
            
            # Biplot--------
            
            image2 <- self$results$plot2
            image2$setState(pca)
            
            
          }
          
        },
        
        # Control variable colors using their contributions----------
        
        .plot = function(image, ggtheme, theme, ...) {
          
          if (length(self$options$vars) < 2)
            return()
          
          pca <- image$state
          
          plot <- factoextra::fviz_pca_var(pca, col.var="contrib",
                                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                           repel = TRUE # Avoid text overlapping
          )
          
          print(plot)
          TRUE
        },
        
        
        .plot1 = function(image1, ggtheme, theme, ...) {
          
          if (length(self$options$vars) < 2)
            return()
          
          
          pca <- image1$state
          
          plot1 <- factoextra::fviz_pca_ind(pca, col.ind = "cos2",
                                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                            repel = TRUE # Avoid text overlapping (slow if many points)
          )
          
          print(plot1)
          TRUE
        },
        
        .plot2 = function(image2, ggtheme, theme, ...) {
          
          if (length(self$options$vars) < 2)
            return()
          
          pca <- image2$state
          
          plot2 <- factoextra::fviz_pca_biplot(pca, repel = TRUE)
          
          print(plot2)
          TRUE
        }
       
        
    )
)
