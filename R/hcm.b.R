
# This file is a generated template, your changes will not be overwritten

hcmClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "hcmClass",
    inherit = hcmBase,
    private = list(

      .htmlwidget = NULL,
      
        .init = function() {
            
          private$.htmlwidget <- HTMLWidget$new()
          
            if (is.null(self$data) | is.null(self$options$podatki)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # <p>____________________________________________________________________________________</p>
            # <p>1. The hierarchical clustering module was created by Gasper Cankar & Hyunsoo Seol.</p>
            # <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowCluster/issues'  target = '_blank'>GitHub.</a></p>
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
                '<li>The hierarchical clustering module was created by Gasper Cankar & Hyunsoo Seol.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )          
          
            
            if(self$options$dend == TRUE)  self$results$plot$setVisible(visible = TRUE)
            if(self$options$heat == TRUE)  self$results$heat$setVisible(visible = TRUE)
            if((self$options$pair == TRUE)&(self$options$case == FALSE)) self$results$pairs$setVisible(visible = TRUE) 

            if(isTRUE(self$options$dend)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
            }
            
            if(isTRUE(self$options$heat)){
              width <- self$options$width1
              height <- self$options$height1
              self$results$heat$setSize(width, height)
            }
            
            if(isTRUE(self$options$pair)){
              width <- self$options$width2
              height <- self$options$height2
              self$results$pairs$setSize(width, height)
            }
             
                    },
        
.run = function() {
    
    
    # check if there are at least 3 variables to cluster with
    if (length(self$options$podatki)<3) return() 
    
    imena <- self$options$imena
    stand <- self$options$stand
    case <- self$options$case
    pair <- self$options$pair
    grp <- self$options$grp
    group <- self$options$group
    dis <- self$options$dis
    method <- self$options$method
    group <- self$options$group
    dend <- self$options$dend
    heat <- self$options$heat
    horiz <- self$options$horiz
    hght <- self$options$hght
    
    # if we have variable for labels do it with labels...
    # scaling the data at the same time...na.omit takes care of NA values...
    if(length(imena) > 0) {
        ime <- as.character(self$data[,which(names(self$data) == imena)])
        pod0 <- self$data[,which(names(self$data) != imena)]
        if(stand == TRUE){
            pod <- scale(na.omit(pod0)) 
        } else {
            pod <- na.omit(pod0)
        }
        rownames(pod) <- ime
    } else {
        if(stand == TRUE){
            pod <- scale(na.omit(self$data))
        } else {
            pod <- na.omit(self$data)
        }
    }
    # change cases/variables if needed
    if(case) pod <- t(pod)
    # calculate distances
    # pod is scaled dataset
    razdalje <- stats::dist(pod, method = dis)
    
    # clustering
    klastri <- stats::hclust(d = jmvcore::toNumeric(razdalje), method = method)
    
    # populate the table izpis
    table <- self$results$izpis
    table$setRow(rowNo=1, values=list(
        var=ifelse((case == TRUE),nrow(pod),ncol(pod)),
        case=ifelse((case == TRUE),ncol(pod),nrow(pod)),
        dist=dis,
        method=method
    ))
    # populate the table Group membership
    table2 <- self$results$groups
    
    #cut the clusters
    if(grp == "height") {
        razrez <- cutree(klastri,h=hght)
    } else {
        razrez <- cutree(klastri,k=group)
    }
    
    tab <- as.data.frame(table(razrez))
    
    for(i in 1:nrow(tab)){
        table2$addRow(rowKey=i, values=list(
            cluster=as.numeric(as.character(tab[i,1])),
            freq=tab[i,2]
        ))	
    }  
    
    #############################################
    # Cluster membership-------------
    
    m<- as.data.frame(razrez)
    mem<- m[,1]
    
    self$results$clust$setValues(mem)
    
    self$results$clust$setRowNums(rownames(data))
    #############################
    
    
    #providing plot		
    if(dend == TRUE){
        image <- self$results$plot
        
        image$setState(list(klastri,razrez))
    }
    
    #providing heatmap		
    if(heat == TRUE){
        image2 <- self$results$heat
        image2$setState(pod)
    }
    
    #providing pairs plot		
    if((pair == TRUE)&(case == FALSE)){
        image3 <- self$results$pairs
        image3$setState(cbind(pod,"cluster"=razrez))
    }
    
    
    #small textual output
    #self$results$text$setContent(pod[1:3,])
},
.plot=function(image,...) {
    if ((length(self$options$podatki)<3) || (self$options$dend == FALSE)) return() 
    
    plotData <- image$state[[1]]
    razrez <- image$state[[2]]
    
    #draw the dendrogram
    if(self$options$horiz == TRUE){
        plott <- plot(stats::as.dendrogram(plotData),type="rectangle",horiz=TRUE)
        #if(self$options$grp == "height"){stats::rect.hclust(plotData,h=self$options$hght,cluster=razrez)} else {stats::rect.hclust(plotData,k=self$options$group,cluster=razrez)}
    } else {
        plott <- plot(stats::as.dendrogram(plotData),hang = -1, cex = 0.6)
        if(self$options$grp == "height"){stats::rect.hclust(plotData,h=self$options$hght,cluster=razrez)} else {stats::rect.hclust(plotData,k=self$options$group,cluster=razrez)}
    }
    
    TRUE
},
.heat=function(image2,...) {
    if ((length(self$options$podatki)<3) || (self$options$heat == FALSE)) return() 
    old <- par(mar=c(7,5,5,3)+.01)
    plotData <- image2$state
    if(self$options$heat == TRUE){
        #draw the heatmap
        stats::heatmap(plotData)
    }
    par(old)
    TRUE
},
.pairs=function(image3,...) {
    if ((length(self$options$podatki)<3) || (self$options$pair == FALSE)) return() 
    plotData <- image3$state
    cluster <- as.numeric(plotData[,ncol(plotData)])
    plotData <- plotData[,-ncol(plotData)]
    #draw pairs plot
    graphics::pairs(plotData, pch=21, bg=grDevices::heat.colors(self$options$group)[cluster])
    TRUE
}		
    ))
        
        
        
        
        
        
 
