
# This file is a generated template, your changes will not be overwritten

#' @importFrom FactoMineR CA
#' @importFrom factoextra fviz_ca_row
#' @importFrom factoextra fviz_ca_col
#' @importFrom factoextra fviz_ca_biplot
#' @importFrom factoextra fviz_screeplot
#' @import ggplot2
#' @export


correspondenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correspondenceClass",
    inherit = correspondenceBase,
    private = list(
      .htmlwidget = NULL,
      #------------------------------------
      
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     <p>____________________________________________________________________________________</p>
        #     <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
        #     <p>____________________________________________________________________________________</p>
        #     
        #     </div>
        #     </body>
        #     </html>"
        # )
      
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
        )          
        
        if(isTRUE(self$options$plot4)){
          
          width <- self$options$width4
          height <- self$options$height4
          
          self$results$plot4$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot1$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot2$setSize(width, height)
        }  
        
        if(isTRUE(self$options$plot3)){
          
          width <- self$options$width3
          height <- self$options$height3
          
          self$results$plot3$setSize(width, height)
        }  
        
          
      },      
      
      
              .run = function() {

            
            if(length(self$options$vars>2)){
                
              if(length(self$options$vars) < 3)
                return()
              
              
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
                
                
                ##### Correspondence analysis---------
                
                res.ca <- FactoMineR::CA(data, graph = FALSE)
                
                chi <- chisq.test(data)
                
                # get statistic----
                   
                 statistic<- chi$statistic
                
                # get df----------
                
                df<- chi$parameter
                
                # get pvalue------------
                
                p <- chi$p.value
                
               # Chi square table===============
                
                table <- self$results$chi
                
                row <- list()
                
                row[['statistic']] <- statistic
                row[['df']] <- df
                row[['p']] <- p
                
                table$setRow(rowNo = 1, values = row)
                
                ### get eigenvalues------------
                
                 eigen <- res.ca$eig[,1]
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
                
                # loadingvar<- res.ca$col$contrib
                
                colvar <- self$options$colvar
                
                if(colvar=="coordinates"){
                    
                    loadingvar <- res.ca$col$coord
                    
                } else if(colvar=="cos2"){
                    
                    loadingvar <- res.ca$col$cos2
                    
                } else {
                    
                    loadingvar <- res.ca$col$contrib
                    
                     }
              
               
                table <- self$results$loadingvar
                
               
                    for (i in seq_along(eigen))
                        
                        table$addColumn(
                            name = paste0("pc", i),
                            title = as.character(i),
                            type = 'number',
                            superTitle = 'Dimension'
                        )
                    
               
                for (i in seq_along(self$options$vars)) {
                    
                    row <- list()
                  
                    
                    for (j in seq_along(eigen)) {
                        row[[paste0("pc", j)]] <- loadingvar[i, j]
                    }
                    
                   
                    table$setRow(rowNo=i, values=row)
                    
                }
                
                # init. contribution of rows to the dimensions table-------
                
               # loadingind<- res.ca$row$contrib
                
                rowvar <- self$options$rowvar
                
                if(rowvar=="coordinates"){
                    
                    loadingind <- res.ca$row$coord
                    
                } else if(rowvar=="cos2"){
                    
                    loadingind <- res.ca$row$cos2
                    
                } else {
                    
                    loadingind <- res.ca$row$contrib
                    
                    #    self$results$text$setContent(loadingind)
                    
                }
                
               
                table <- self$results$loadingind
                
                
                for (i in seq_along(eigen))
                    
                    table$addColumn(
                        name = paste0("pc", i),
                        title = as.character(i),
                        type = 'number',
                        superTitle = 'Dimension'
                    )
                
                for (i in 1:nrow(data)) {
                    row <- list()
                   
                    for (j in seq_along(eigen)) {
                        row[[paste0("pc", j)]] <- loadingind[i, j]
                    }
                    
                    table$addRow(rowKey = i, values = row)
                    
                }
                
                
                
# Plot==================================================
                
                #  Raw points plot----------
                
                image1 <- self$results$plot1
                image1$setState(res.ca)
            
                # Column points plot-------


                image2 <- self$results$plot2
                image2$setState(res.ca)


                # Biplot--------

                image3 <- self$results$plot3
                image3$setState(res.ca)

                # Scree plot--------
                
                image4 <- self$results$plot4
                image4$setState(res.ca)
                
                
                }
            },
       

        .plot1 = function(image1, ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
          
            
            res.ca <- image1$state
            
          #  rowvar <- self$options$rowvar
            
            plot1 <- factoextra::fviz_ca_row(res.ca, 
                                             # col.row = rowvar,
                                             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                             repel = TRUE)
            
            
            plot1 <- plot1+ggtheme
            
            print(plot1)
            TRUE
        },
        
        .plot2 = function(image2, ggtheme, theme, ...) {
            
          if (is.null(image2$state))
            return(FALSE)
          
            
            res.ca <- image2$state
            
            plot2 <- factoextra::fviz_ca_col(res.ca, repel = TRUE)
            
            plot2 <- plot2+ggtheme
            
            print(plot2)
            TRUE
        },
        
        .plot3 = function(image3, ggtheme, theme, ...) {
            
          if (is.null(image3$state))
            return(FALSE)
          
            
            res.ca <- image3$state
            
            plot3 <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE)
            
            
            plot3 <- plot3+ggtheme
            print(plot3)
            TRUE
        },


   .plot4 = function(image4, ggtheme, theme, ...) {
    
     if (is.null(image4$state))
       return(FALSE)
     
    
    res.ca <- image4$state
    
    plot4 <- factoextra::fviz_screeplot(res.ca, addlabels = TRUE)
    
    
    plot4 <- plot4+ggtheme
    print(plot4)
    TRUE
    
}
       
    ))
            
            
