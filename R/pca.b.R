
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
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                 if ( ! is.null(self$options$labels))
                   rownames(data) <- data[[self$options$labels]]
          
          # principal component analysis---------
           
            pca <- FactoMineR::PCA(data,  graph = FALSE)
            
      
            # Variable contributions plot----------

            image <- self$results$plot
            image$setState(pca)
            
            # Individual plot-------
            
            
            image1 <- self$results$plot1
            image1$setState(pca)
            

            # Biplot--------
            
            image2 <- self$results$plot2
            image2$setState(pca)
            
            # Individuals by groups--------
            
            image3 <- self$results$plot3
            image3$setState(pca)
            
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
       },
       
       .plot3 = function(image3, ggtheme, theme, ...) {
         
         vars <- self$options$vars
         data <- self$data
         data <- jmvcore::naOmit(data)

         for (i in seq_along(vars))
           data[[i]] <- jmvcore::toNumeric(data[[i]])

         if ( ! is.null(self$options$labels))

           rownames(data) <- data[[self$options$labels]]

         if (length(self$options$vars) < 2)
           return()

         pca <- image3$state
       
       plot3 <- factoextra::fviz_pca_ind(pca,
                    
                    label = "none", # hide individual labels
                    habillage = self$data[[self$options$labels]],  # color by groups for example, iris$Species,
                    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    addEllipses = TRUE # Concentration ellipses
       )
         
         print(plot3)
         TRUE
       }

        )
)
