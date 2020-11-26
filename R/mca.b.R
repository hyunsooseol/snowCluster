
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR MCA
#' @importFrom factoextra fviz_mca_var
#' @importFrom factoextra get_mca_ind
#' @import ggplot2
#' @export


mcaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mcaClass",
    inherit = mcaBase,
    private = list(
        .run = function() {
            if(length(self$options$vars>2)){
                
                vars <- self$options$vars
                
                facs <- self$options$facs
                
                #get the data--------
                
                data <- self$data
                
                data <- jmvcore::naOmit(data)
                
                
                # convert to appropriate data types
                
                for (i in seq_along(vars))
                    data[[i]] <- jmvcore::toNumeric(data[[i]])
                
                
                #  data[[vars]] <- jmvcore::toNumeric(data[[vars]])
                
                for (fac in facs)
                    data[[fac]] <- as.factor(data[[fac]])
                
                # data is now all of the appropriate type we can begin!
                
                data <- na.omit(data)
                
                data <- jmvcore::select(data, self$options$vars)
                
                
                # Correspondence analysis---------
                
                res.mca <- FactoMineR::MCA(data, graph = FALSE)
                
                
                #  Correlation between variables plot----------
                
                image1 <- self$results$plot1
                image1$setState(res.mca)
                
                # Coordinates of variable categories plot-------

                image2 <- self$results$plot2
                image2$setState(res.mca)


                # Plot of individuals--------
                
               image3 <- self$results$plot3
               image3$setState(res.mca)
               
               # individuals by groups
               
               image4 <- self$results$plot4
               image4$setState(res.mca)
               
            }
        },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
            if (length(self$options$vars) <= 2)
                return()
            
            res.mca <- image1$state
            
            plot1 <- factoextra::fviz_mca_var(res.mca, choice = "mca.cor", 
                                              repel = TRUE, # Avoid text overlapping (slow)
                                              ggtheme = theme_minimal())
            
            print(plot1)
            TRUE
        
            },
        
        .plot2 = function(image2, ggtheme, theme, ...) {

            if (length(self$options$vars) <= 2)
                return()

            res.mca <- image2$state

            plot2 <- factoextra::fviz_mca_var(res.mca, 
                                              repel = TRUE,
                                              # Avoid text overlapping (slow)
                                              col.var="black", shape.var = 15,
                                              ggtheme = theme_minimal())

            print(plot2)
            TRUE
        },

        .plot3 = function(image3, ggtheme, theme, ...) {

            if (length(self$options$vars) <= 2)
                return()

            res.mca <- image3$state
            
            plot3 <- factoextra::fviz_mca_ind(res.mca, col.ind = "cos2", 
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE, # Avoid text overlapping (slow if many points)
                                              ggtheme = theme_minimal())
            
            print(plot3)
            TRUE
        },
        
        
        .plot4 = function(image4, ggtheme, theme, ...) {
            
            if (is.null(self$options$facs))
                return()
            
            
            res.mca <- image4$state
            
            plot4 <- factoextra::fviz_mca_ind(res.mca, 
                                              label = "none", # hide individual labels
                                              habillage = self$data[[self$options$facs]], # color by groups 
                                              palette = c("#00AFBB", "#E7B800"),
                                              addEllipses = TRUE, ellipse.type = "confidence",
                                              ggtheme = theme_minimal()) 
            print(plot4)
            TRUE
}
        
    ))

