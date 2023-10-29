
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR MCA
#' @importFrom factoextra fviz_mca_var
#' @importFrom factoextra get_mca_ind
#' @importFrom factoextra fviz_screeplot
#' @importFrom factoextra fviz_mca_biplot
#' @import ggplot2
#' @export


mcaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mcaClass",
    inherit = mcaBase,
    private = list(

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
            <p>____________________________________________________________________________________</p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if(isTRUE(self$options$plot5)){
          
          width <- self$options$width
          height <- self$options$height
          
          self$results$plot5$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot1$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot2$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot3)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot3$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot4)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot4$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot6)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot6$setSize(width, height)
        }  
        
        
         
      },
      
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
                
                
                
                ### get eigenvalues------------
                
                eigen <- res.mca$eig[,1]
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
                
                # init. contribution of columns to the dimensions table-------
                # ncp: number of dimensions kept in the results (by default 5)
                # the number of dimension is 5(default in R package)
                
               # loadingvar<- res.mca$var$eta2
                
                colvar <- self$options$colvar
                
                if(colvar=="coordinates"){
                    
                    loadingvar <- res.mca$var$coord
                   
                    
                } else if(colvar=="cos2"){
                    
                    loadingvar <- res.mca$var$cos2
                    
                    
                } else {
                    
                    loadingvar <- res.mca$var$contrib
                    
                   
                }
                
                df <- res.mca$var$coord
                names<- dimnames(df)[[1]]
                
                table <- self$results$loadingvar
                
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
                        row[[paste0("pc", j)]] <- loadingvar[name, j]
                    }
                    
                    
                    table$addRow(rowKey=name, values=row)
                    
                }
                
                # init. contribution of rows to the dimensions table-------
                # the number of dimension is 5(default in R package)
                
              #  loadingind<- res.mca$ind$contrib
               
                rowvar <- self$options$rowvar
                
                if(rowvar=="coordinates"){
                    
                    loadingind <- res.mca$ind$coord
                    
                } else if(rowvar=="cos2"){
                    
                    loadingind <- res.mca$ind$cos2
                    
                } else {
                    
                    loadingind <- res.mca$ind$contrib
                    
                    #    self$results$text$setContent(loadingind)
                    
                }
                
                
                 
                table <- self$results$loadingind
                
                
                for (i in 1:5)
                    
                    table$addColumn(
                        name = paste0("pc", i),
                        title = as.character(i),
                        type = 'number',
                        superTitle = 'Dimension'
                    )
                
                for (i in 1:nrow(data)) {
                    row <- list()
                    
                    for (j in seq_along(1:5)) {
                        row[[paste0("pc", j)]] <- loadingind[i, j]
                    }
                    
                    table$addRow(rowKey = i, values = row)
                    
                }
                
                ###### plot##########################                
                
                #  Correlation between variables plot----------
                
                image1 <- self$results$plot1
                image1$setState(res.mca)
                
                # Coordinates of variable categories plot-------

                image2 <- self$results$plot2
                image2$setState(res.mca)


                # Plot of individuals--------
                
               image3 <- self$results$plot3
               image3$setState(res.mca)
               
               # individuals by groups-----
               
               image4 <- self$results$plot4
               image4$setState(res.mca)
               
               # scree plot-------
               
               image5 <- self$results$plot5
               image5$setState(res.mca)
              
               #Biplot------
               
               image6 <- self$results$plot6
               image6$setState(res.mca)
               
            }
        },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
            
            res.mca <- image1$state
            
            plot1 <- factoextra::fviz_mca_var(res.mca, choice = "mca.cor", 
                                              repel = TRUE, # Avoid text overlapping (slow)
                                              ggtheme = theme_minimal())
            
            plot1 <- plot1+ggtheme
            print(plot1)
            TRUE
        
            },
        
        .plot2 = function(image2, ggtheme, theme, ...) {

          if (is.null(image2$state))
            return(FALSE)

            res.mca <- image2$state

            plot2 <- factoextra::fviz_mca_var(res.mca, 
                                              repel = TRUE,
                                              # Avoid text overlapping (slow)
                                              col.var="black", shape.var = 15,
                                              ggtheme = theme_minimal())

            plot2 <- plot2+ggtheme
            print(plot2)
            TRUE
        },

        .plot3 = function(image3, ggtheme, theme, ...) {

          if (is.null(image3$state))
            return(FALSE)
          
            res.mca <- image3$state
            
            plot3 <- factoextra::fviz_mca_ind(res.mca,  
                                               #col.ind = "contrib", 
                                              # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                               repel = TRUE, # Avoid text overlapping (slow if many points)
                                              #ggtheme = theme_minimal()
                                              )
            
            plot3 <- plot3+ggtheme
            print(plot3)
            TRUE
        },
        
        
        .plot4 = function(image4, ggtheme, theme, ...) {
            
          if (is.null(image4$state))
            return(FALSE)
            
            
            res.mca <- image4$state
            
            plot4 <- factoextra::fviz_mca_ind(res.mca, 
                                              label = "none", # hide individual labels
                                              habillage = self$data[[self$options$facs]], # color by groups 
                                              palette = c("#00AFBB", "#E7B800"),
                                              addEllipses = TRUE, ellipse.type = "confidence",
                                              ggtheme = theme_minimal()) 
            plot4 <- plot4+ggtheme
            print(plot4)
            TRUE
},

.plot5 = function(image5, ggtheme, theme, ...) {
    
  if (is.null(image5$state))
    return(FALSE)
    
    
    res.mca <- image5$state
    
    plot5 <- factoextra::fviz_screeplot(res.mca, addlabels = TRUE) 

    plot5 <- plot5+ggtheme
    print(plot5)
    TRUE
},
 

.plot6 = function(image6, ggtheme, theme, ...) {
    
  if (is.null(image6$state))
    return(FALSE)
    
    res.mca <- image6$state
    
    plot6 <- factoextra::fviz_mca_biplot(res.mca, 
                                         repel = TRUE, # Avoid text overlapping (slow if many point)
                                         ggtheme = theme_minimal())
    
    plot6 <- plot6+ggtheme
    print(plot6)
    TRUE
}       
    ))


