
# This file is a generated template, your changes will not be overwritten

#' @export



mfaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mfaClass",
    inherit = mfaBase,
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
            # <p> 1. Specify 'Model Component'.</p>
            # <p> 2. Type of value indicates 's' for the variables are scaled to unit variance,'n' for categorical variables, or 'f' for frequencies.</p>
            # <p> 3. Move the variables into 'Variables'box.
            # <p> 4. Factor box can be specified for visualizing 'individuals by group' plot. </p>
            # <p> 5. The rationale of Multiple Factor Analysis is described in the <a href='http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/' target = '_blank'>page.</a></p>
            # <p> 6. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
                '<li>Specify <b>Model Component</b>.</li>',
                '<li>Type of value indicates <b>s</b> for the variables are scaled to unit variance,<b>n</b> for categorical variables, or <b>f</b> for frequencies.</li>',
                '<li>Move the variables into Variables box.</li>',
                '<li>Factor box can be specified for visualizing <b>individuals by group</b> plot.</li>',
                '<li>The rationale of Multiple Factor Analysis is described in the <a href="http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/" target = "_blank">page</a>.</li>',
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
            if(isTRUE(self$options$plot5)){
              width <- self$options$width5
              height <- self$options$height5
              self$results$plot5$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot6)){
              width <- self$options$width6
              height <- self$options$height6
              self$results$plot6$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot7)){
              width <- self$options$width7
              height <- self$options$height7
              self$results$plot7$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot8)){
              width <- self$options$width8
              height <- self$options$height8
              self$results$plot8$setSize(width, height)
            }  
            
            
                
        },
        
        #---------------------------------------------
        
        .run = function() {

           
            if(length(self$options$vars>2)){
                
                
                vars <- self$options$vars
                
               
                facs <- self$options$facs

                # for (fac in facs)
                #     data[[fac]] <- as.factor(data[[fac]])
                # 
                # data <- jmvcore::select(data, self$options$vars)
                # 
                
                # model component-------
                
                group <- as.numeric(strsplit(self$options$group, ',')[[1]])
                
                type <- self$options$type # n:category, s:unit variable
                
                type1 <- strsplit(self$options$type, ',')[[1]]
                
                
                gn <- self$options$gn # the name of group
                
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
                #                #num.group.sup =c(1, 4),
                #                graph = FALSE)
                # 
                
                
                mfa <- FactoMineR::MFA(data,
                                       group=group,
                                       type=type1,
                                       name.group = gn1,
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
                
            # contribution for group of variables---------------
                
                # gn <- self$options$gn
                # gn1 <- strsplit(self$options$gn, ',')[[1]]
                # 
                
                grouping <- factoextra::get_mfa_var(mfa, "group")
              
                #  res<- grouping$contrib
                
                
                colvar <- self$options$colvar
                
                if(colvar=="coordinates"){
                    
                    res <- grouping$coord
                    
                } else if(colvar=="cos2"){
                    
                    res <- grouping$cos2
                    
                } else {
                    
                    res <- grouping$contrib
                    
                }
                
                  
                names <- dimnames(res)[[1]]   
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
                        row[[paste0("pc", j)]] <- res[name, j]
                    }


                    table$addRow(rowKey=name, values=row)

                }

               
                
                # contribution of  individuals-----------
                
                
            #    ind<-mfa$ind$contrib
                
                rowvar <- self$options$rowvar
                
                if(rowvar=="coordinates"){
                    
                    ind<-mfa$ind$coord
                    
                } else if(rowvar=="cos2"){
                    
                    ind<-mfa$ind$cos2
                    
                } else {
                    
                    ind<-mfa$ind$contrib
                    
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
                
                # Contribution of quantitative variables--------------
                
                quanti <- factoextra::get_mfa_var(mfa, "quanti.var")
              #  res.quanti<- quanti$contrib
                
                quanvar <- self$options$quanvar
                
                if(quanvar=="coordinates"){
                    
                    res.quanti<- quanti$coord
                    
                } else if(quanvar=="cos2"){
                    
                    res.quanti<- quanti$cos2
                    
                } else {
                    
                    res.quanti<- quanti$contrib
                    
                }
                
                
                names <- dimnames(res.quanti)[[1]]
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
                        row[[paste0("pc", j)]] <- res.quanti[name, j]
                    }
                    
                    
                    table$addRow(rowKey=name, values=row)
                    
                }
                
               
                
                # Plot==================================================
                
                
                #  Groups of variables----------
                
                image <- self$results$plot
                image$setState(mfa)
                
                # Quantitative variables colored by groups-------
                
                image1 <- self$results$plot1
                
                # vars <- length(self$options$vars)
                # 
                # width <- 500 + vars * 30
                # 
                # image$setSize(width, 500)
                
                image1$setState(mfa)
                
                # Contributions to dimension 1------------
                
                image2 <- self$results$plot2
                
                #  vars <- length(self$options$vars)
                #  width <- 300 + vars * 30
                # image$setSize(width, 500)
                
                image2$setState(mfa)
                
                # Contributions to dimension 2------------
                
                image3 <- self$results$plot3
                
                # vars <- length(self$options$vars)
                # width <- 300 + vars * 30
                # image$setSize(width, 500)

                image3$setState(mfa)
                
              
                # Graph of individuals---------
                
                image4 <- self$results$plot4
                
                image4$setState(mfa)
                
                # Individuals by group---------
                
                image5 <- self$results$plot5
                
                image5$setState(mfa)
                  
                # contribution of groups to dimension 1--------
                
                image6 <- self$results$plot6
                
                image6$setState(mfa)
                
                # contribution of groups to dimension 2--------
                
                image7 <- self$results$plot7
                
                image7$setState(mfa)
                
                # scree plot(plot 8)--------
                
                image8 <- self$results$plot8
                image8$setState(mfa)
            }
        },
        
        
        .plot = function(image, ggtheme, theme, ...) {
            
          if (is.null(image$state))
            return(FALSE)
            
            mfa <- image$state
            
            plot <- factoextra::fviz_mfa_var(mfa, "group")
            
            plot <- plot+ggtheme
            
            print(plot)
            TRUE
        
        },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
          if (is.null(image1$state))
            return(FALSE)
          
            mfa <- image1$state
            
            plot1 <- factoextra::fviz_mfa_var(mfa, "quanti.var", 
                                              palette = "jco", 
                                 col.var.sup = "violet", 
                                 repel = TRUE,
                                  geom = c("point", "text"),
                                  legend = "bottom")
            
            plot1 <- plot1+ggtheme
                
            print(plot1)
            TRUE
            
        },
        
        .plot2 = function(image2, ggtheme, theme, ...) {
            
          if (is.null(image2$state))
            return(FALSE)
            
            mfa <- image2$state
            
            plot2 <- factoextra::fviz_contrib(mfa, choice = "quanti.var"
                                              , axes = 1, top = 20,
                                              palette = "jco")
            
            if (self$options$angle > 0) {
              plot2 <- plot2 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            
            print(plot2)
            TRUE
            
        },
        
        .plot3 = function(image3, ggtheme, theme, ...) {
            
            # if (length(self$options$vars) <= 2)
            #     return()
            
          if (is.null(image3$state))
            return(FALSE)
          
            mfa <- image3$state
            
            plot3 <- factoextra::fviz_contrib(mfa, choice = "quanti.var", 
                                              axes = 2, top = 20,
                                              palette = "jco")
                                                                                            
            if (self$options$angle > 0) {
              plot3 <- plot3 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            
            print(plot3)
            TRUE
            
        }, 
        
        .plot4 = function(image4, ggtheme, theme, ...) {
            
            # if (length(self$options$vars) <= 2)
            #     return()
          if (is.null(image4$state))
            return(FALSE)
            
            mfa <- image4$state
            
            plot4 <- factoextra::fviz_mfa_ind(mfa, 
                                             repel=TRUE)
            
            plot4 <- plot4+ggtheme
            
            print(plot4)
            TRUE
        },
       
         .plot5 = function(image5, ggtheme, theme, ...) {
            
           if (is.null(image5$state))
             return(FALSE)
            
            mfa <- image5$state
            
            plot5 <- factoextra::fviz_mfa_ind(mfa, 
                                              habillage = self$options$facs, # color by groups 
                                               #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              addEllipses = TRUE, ellipse.type = "confidence", 
                                              repel=TRUE)
            
            plot5 <- plot5+ggtheme
            
            print(plot5)
            TRUE
         },
        
        .plot6 = function(image6, ggtheme, theme, ...) {
            
          if (is.null(image6$state))
            return(FALSE)
            
            mfa <- image6$state
            
            plot6 <- factoextra::fviz_contrib(mfa,
                                              "group", 
                                              axes = 1)
           
            if (self$options$angle > 0) {
              plot6 <- plot6 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            
            print(plot6)
            TRUE
            
        },
        
        .plot7 = function(image7, ggtheme, theme, ...) {
            
          if (is.null(image7$state))
            return(FALSE)
            
            mfa <- image7$state
            
            plot7 <- factoextra::fviz_contrib(mfa,
                                              "group", 
                                              axes = 2)
            
            if (self$options$angle > 0) {
              plot7 <- plot7 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            
            print(plot7)
            TRUE
            
        },
        
        .plot8 = function(image8, ggtheme, theme, ...) {
            
            # if (length(self$options$vars) <= 2)
            #     return()
            # 
          if (is.null(image8$state))
            return(FALSE)
          
            mfa <- image8$state
            
            plot8 <- factoextra::fviz_screeplot(mfa, addlabels = TRUE)
            
            plot8 <- plot8+ggtheme
            
            print(plot8)
            TRUE
            
            
        }
     
        
        
    )
)
