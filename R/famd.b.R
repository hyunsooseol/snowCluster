
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
      .htmlwidget = NULL,
      
        #------------------------------------
        
        .init = function() {
          private$.htmlwidget <- HTMLWidget$new()
          
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # <p>____________________________________________________________________________________</p>
            # <p> 1. The rationale of Factor Analysis of mixed data is described in the <a href='https://rpkgs.datanovia.com/factoextra/reference/fviz_famd.html' target = '_blank'>page</a>.</p>
            # <p> 2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub</a>.</p>
            # <p>____________________________________________________________________________________</p>
            # 
            # </div>
            # </body>
            # </html>"
            # )
         
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The rationale of Factor Analysis of mixed data is described in the <a href="https://rpkgs.datanovia.com/factoextra/reference/fviz_famd.html" target = "_blank">page</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )          
          
            if(isTRUE(self$options$plot)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
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
            
            if(isTRUE(self$options$plot4)){
              width <- self$options$width4
              height <- self$options$height4
              self$results$plot4$setSize(width, height)
            }  
            
            
               
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
            
            
            # FA analysis of mixed data ##########################
            
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
            
            
            # individuals-----------
            
            
          #  ind<-res$ind$contrib
            
            rowvar <- self$options$rowvar
            
            if(rowvar=="coordinates"){
                
                ind<-res$ind$coord
                
            } else if(rowvar=="cos2"){
                
                ind<-res$ind$cos2
                
            } else {
                
                ind<-res$ind$contrib
                
            }
            
            

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
            
            # variables-----------
            
            
           # v<-res$var$contrib
            
            colvar <- self$options$colvar
            
            if(colvar=="coordinates"){
                
                v<-res$var$coord
                
            } else if(colvar=="cos2"){
                
                v<-res$var$cos2
                
            } else {
                
                v<-res$var$contrib
                
            }
            
            
            
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
            
            # quanti. variables-----------
            
            
         #   quanti<-res$quanti.var$contrib
            
            quanvar <- self$options$quanvar
            
            if(quanvar=="coordinates"){
                
                quanti<-res$quanti.var$coord
                
            } else if(quanvar=="cos2"){
                
                quanti<-res$quanti.var$cos2
                
            } else {
                
                quanti<-res$quanti.var$contrib
                
            }
            
            
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
            
            #  quali. variables-----------
            
            
        #    qual<-res$quali.var$contrib
            
            
            qualvar <- self$options$qualvar
            
            if(qualvar=="coordinates"){
                
                qual<-res$quali.var$coord
                
            } else if(qualvar=="cos2"){
                
                qual<-res$quali.var$cos2
                
            } else {
                
                qual<-res$quali.var$contrib
                
            }
            
            
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
            
            # plot--------
            
            image <- self$results$plot
            image$setState(res) 
            
            image1 <- self$results$plot1
            image1$setState(res)
            
            image2 <- self$results$plot2
            image2$setState(res)
            
            image3 <- self$results$plot3
            image3$setState(res)
            
            image4 <- self$results$plot4
            image4$setState(res)
            
           
            
        }
        
        },
          
              .plot = function(image, ggtheme, theme, ...) {
                
                if (is.null(image$state))
                  return(FALSE)
                
                
                res <- image$state
                
                # Eigenvalues/variances of dimensions
                
                plot<- factoextra::fviz_screeplot(res)
                
                plot <- plot+ggtheme
                
                print(plot)
                TRUE
                
            },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
          
            
            res <- image1$state
            
            plot1<- factoextra::fviz_famd_var(res)
            
            plot1 <- plot1+ggtheme
            
            print(plot1)
            TRUE
            
        },
        
        .plot2 = function(image2, ggtheme, theme, ...) {
            
          if (is.null(image2$state))
            return(FALSE)
          
            
            res <- image2$state
            
            # Quantitative variables
             
            plot2<- factoextra::fviz_famd_var(res, "quanti.var", 
                                              col.var = "black",
                                              repel = TRUE)
            
            plot2 <- plot2+ggtheme
            
            print(plot2)
            TRUE
            
        },
        
        .plot3 = function(image3, ggtheme, theme, ...) {
            
          if (is.null(image3$state))
            return(FALSE)
            
            res <- image3$state
            
            # # Qualitative variables
            
            plot3<- factoextra::fviz_famd_var(res, "quali.var", col.var = "black")
            
            plot3 <- plot3+ggtheme
            
            print(plot3)
            TRUE
            
        },
           
        .plot4 = function(image4, ggtheme, theme, ...) {
            
          if (is.null(image4$state))
            return(FALSE)
            
            res <- image4$state
            
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
