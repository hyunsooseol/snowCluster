
# This file is a generated template, your changes will not be overwritten
#' @import jmvcore
#' @import ggplot2
#' @importFrom multipleROC multipleROC
#' @importFrom multipleROC plot_ROC
#' @importFrom rms matinv
#' @export

rocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rocClass",
    inherit = rocBase,
    private = list(
 
      .htmlwidget = NULL,
      
  .init = function() {
        
    private$.htmlwidget <- HTMLWidget$new()
    
    
    if (is.null(self$options$dep) | is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     <p>____________________________________________________________________________________</p>
        #     <p> ROC analysis based on Binomial logistic regression.</p>
        #     <p> Perform ROC curve based on <a href='https://github.com/cardiomoon/multipleROC' target = '_blank'>multipleROC<a> R package.</p>
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
          '<li>ROC analysis based on Binomial logistic regression.</li>',
          '<li>Perform ROC curve based on <a href="https://github.com/cardiomoon/multipleROC" target = "_blank">multipleROC R package</a>.</li>',
          '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
          '</ul></div></div>'
          
        )
        
      )
    )          
        
        
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

     if (is.null(self$data) | is.null(self$options$dep) | is.null(self$options$covs))
       return()    
    
     # Example--------
    # multipleROC::multipleROC(am~wt,data=mtcars)
    
    dep <- self$options$dep
    covs <- self$options$covs
    data <- self$data
    data <- na.omit(data)
    data <- as.data.frame(data)  
          
    #Formula(male~height+weight)------
    covs <- vapply(covs, function(x) jmvcore::composeTerm(x), '')
    formula <- as.formula(paste(paste(dep, paste0(covs, collapse ="+"), sep="~")))
           
    if(isTRUE(self$options$plot1)){
   
    image <- self$results$plot1
    image$setState(formula)
    }   

p2 <- private$.computeP2()
#self$results$text$setContent(p2)

p3 <- private$.computeP3()
 

if ( self$options$auc == TRUE) {
      
      if (length(self$options$covs) < 2){
        stop("Please specify at least two Covariate variables to use DeLong's test.")
      } else{
      
       data <- self$data
      dep <- self$options$dep
      covs <- self$options$covs
      
      class <- self$options$dep
      df <- c(class, covs)
      
      data <- select(self$data, df)
      
      for (cov in covs)
        data[[cov]] <- jmvcore::toNumeric(data[[cov]])
     
      data <- jmvcore::naOmit(data)
     
      #delong test function------------
    
    deLong.test <- function(x, labels, labpos, ref = NULL, conf.level = 0.95) 
      {
     
      if(length(labels) != dim(x)[1])
          stop("\n The number of rows in x must match the length of labels\n")
        id.pos <- labels == labpos
        if(sum(id.pos) < 1)
          stop("\n wrong level specified!\n")
        if(dim(x)[2] < 2)
          stop("\n x must contain at least two columns!\n")
        if(dim(x)[1] < 2)
          stop("\n x must contain at least two rows!\n")
        nn <- sum(!id.pos)
        np <- sum(id.pos)
        nauc <- ncol(x)
        
        if(is.null(ref)) { 
          L <- matrix(0, nrow=nauc*(nauc-1)/2, ncol=nauc)
          newa <- 0
          for(i in 1:(nauc-1)) {
            newl <- nauc - i
            L[(newa+1):(newa+newl),i] <- rep(1, newl)
            L[(newa+1):(newa+newl),((i+1):(i+newl))] <- diag(-1, nrow=newl, ncol=newl)
            newa <- newa + newl
          }
        }
        else { 
          # test for superiority of one method against all others)
          if(ref > nauc) 
            stop(paste("Reference ref must be one of the markers (1...", nauc, " in this case)", sep=""))
          L <- matrix(1, ncol=nauc, nrow=nauc-1)
          L[,-ref] <- diag(-1, nrow=nauc-1, ncol=nauc-1)
        }
        
        markern <- as.matrix(x[!id.pos,])
        markerp <- as.matrix(x[id.pos,])
        
        ###
        ### compute wilcox statistic
        ###
        WK.STAT <- function(x,y){
          r <- rank(c(x, y))
          n.x <- length(x)
          n.y <- length(y)
          STATISTIC <- sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2
          STATISTIC
        }
        
        auc <- vector("numeric", length=nauc)
        for(r in 1:nauc) {
          auc[r] <- WK.STAT(markerp[,r], markern[,r])
        }
        auc <- auc/(nn*np)
        
        ###
        ### if AUCs smaller than 0.5: 1-auc
        ###
        if(any(auc < 0.5)) {
          x[,auc < 0.5] <- -x[,auc<0.5]
          auc[auc < 0.5] <- 1 - auc[auc<0.5]
          markern <- as.matrix(x[!id.pos,])
          markerp <- as.matrix(x[id.pos,])
        }
        
        V10 <- matrix(0, nrow=np, ncol=nauc)
        V01 <- matrix(0, nrow=nn, ncol=nauc)
        
        tmn <- t(markern)
        tmp <- t(markerp)
        for(i in 1:np) {
          V10[i,] <- rowSums(tmn < tmp[,i]) + 0.5 * rowSums(tmn == tmp[,i])
        }
        for(i in 1:nn) {
          V01[i,] <- rowSums(tmp > tmn[,i]) + 0.5 * rowSums(tmp == tmn[,i])
        }
        V10 <- V10/nn
        V01 <- V01/np
        
        W10 <- cov(V10)
        W01 <- cov(V01)
        
        ###
        ### estimated covariance matrix
        ###
        S <- W10/np + W01/nn
        
        ###
        ### compute variances of AUCs and test for AUC > 0.5
        ###
        
        ### Hanley, McNeil (1982)
        q1 <- auc / (2 - auc)
        q2 <- 2*auc^2 / (1 + auc)
        
        ### Haney, McNeil (1982) / Bamber (1975)
        aucvar <- (auc*(1 - auc) + (np - 1)*(q1 - auc^2) + (nn - 1)*(q2 - auc^2)) / (np*nn)
        zhalf <- (auc - 0.5) / sqrt(aucvar)
        phalf <- 1 - pnorm(zhalf)
        zdelong <- (auc - 0.5) / sqrt(diag(S))
        pdelong <- 1 - pnorm(zdelong)
        
        
        ### global p-value
        ###
        aucdiff <- L %*% auc
        z <- t(aucdiff) %*% matinv(L %*% S %*% t(L)) %*% aucdiff
        p <- pchisq(z, df=qr(L %*% S %*% t(L))$rank, lower.tail=FALSE)
        
        if(is.null(ref)) {
          cor.auc <- matrix(ncol=1, nrow=nauc*(nauc-1)/2)
          ci <- matrix(ncol=2, nrow=nauc*(nauc-1)/2)
          ctr <- 1
          rows <- vector("character", length=(nauc*(nauc-1)/2))
          pairp <- matrix(nrow=nauc*(nauc-1)/2, ncol=1)
          quantil <- qnorm(1 - (1 - conf.level)/2)
          for(i in 1:(nauc-1)) {
            for(j in (i+1):nauc) {
              cor.auc[ctr] <- S[i,j] / sqrt(S[i,i]*S[j,j])
              LSL <- t(c(1,-1)) %*% S[c(j,i),c(j,i)] %*% c(1,-1)
              tmpz <- (aucdiff[ctr]) %*% matinv(LSL) %*% aucdiff[ctr]
              pairp[ctr] <- 1 - pchisq(tmpz, df=qr(LSL)$rank)
              ci[ctr,] <- c(aucdiff[ctr] - quantil*sqrt(LSL), aucdiff[ctr] + quantil*sqrt(LSL))
              rows[ctr] <- paste(i, j, sep=" vs. ")
              ctr <- ctr+1
            }
          }
        } else {
          cor.auc <- matrix(ncol=1, nrow=nauc-1)
          ci <- matrix(ncol=2, nrow=nauc-1)
          rows <- vector("character", length=nauc-1)
          pairp <- matrix(nrow=nauc-1, ncol=1)
          comp <- (1:nauc)[-ref]
          for(i in 1:(nauc-1)) {
            cor.auc[i] <- S[ref,comp[i]] / sqrt(S[ref,ref] * S[comp[i],comp[i]])
            LSL <- t(c(1,-1)) %*% S[c(ref,comp[i]),c(ref,comp[i])] %*% c(1,-1)
            tmpz <- aucdiff[i] %*% matinv(LSL) %*% aucdiff[i]
            pairp[i] <- 1 - pchisq(tmpz, df=qr(LSL)$rank)
            ci[i,] <- c(aucdiff[i] - quantil*sqrt(LSL), aucdiff[i] + quantil*sqrt(LSL))
            rows[i] <- paste(ref, comp[i], sep=" vs. ")
          }		
        }
        
        newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
        names(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation")
        rownames(newres) <- rows
        row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
        colnames(ci) <- c(paste0(100*conf.level, "% CI (lower)"), paste0(100*conf.level, "% CI (upper)"))
        names(auc) <- 1:nauc
        auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
        colnames(auc) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")
        
        ERG <- list(AUC = auc, difference = newres, covariance = S, global.z = z, global.p = p)
        class(ERG) <- "DeLong"
        ERG
      }
      
    # delong test ---------------------

              res <- deLong.test(x = data[,-1],
                               labels =data[,1],
                                labpos= "1")

      # self$results$delong$setVisible(visible = TRUE)
      # self$results$delong$setContent(delongres)
     
      #----------------------------------------
      table <- self$results$auc
      res1<- res$AUC
      names <- dimnames(res$AUC)[[1]]
      
        for (name in names) {
          row <- list()
          row[["auc"]] <- res1[name, 1]
          row[["p"]] <- res1[name, 5]
          
          table$addRow(rowKey=name, values=row)
        }
      
 #--------------------------------------
    if(isTRUE(self$options$dif)){  
      table <- self$results$dif
      res1<- res$difference
      names <- dimnames(res$difference)[[1]]
      
      for (name in names) {
        row <- list()
        row[["auc"]] <- res1[name, 1]
        row[["lower"]] <-  res1[name, 2]
        row[["upper"]] <-  res1[name, 3]
        row[["p"]] <- res1[name, 4]
        
        table$addRow(rowKey=name, values=row)
      }
    }
     
    if(isTRUE(self$options$overall)){
      
      table <- self$results$overall
      
      z <- as.vector(res$global.z)
      p <- as.vector(res$global.p)
      
      row <- list()
      
      row[['Z']] <- z
      row[['p']] <- p
      
      table$setRow(rowNo = 1, values = row)
      
    }
      
    
      }
      
      
    }
    },
        
  .plot1 = function(image, ...){
          
            if (is.null(image$state))
              return(FALSE)
          
            formula <- image$state
            
            data <- self$data
            data <- na.omit(data)
            data <- as.data.frame(data)  
            
            plot1 <-  multipleROC::multipleROC(formula, data=data)
          
            print(plot1)
            TRUE
              
          },
          
  .plot2 = function(image,ggtheme,theme, ...){
 
    if(!self$options$plot2)
      return(FALSE)
    
    p2 <- private$.computeP2() 
   
     plot2 <-multipleROC::plot_ROC(p2, show.eta = FALSE, 
                                    show.sens = FALSE)
   
   #  plot2 <- plot2+ggtheme
       
    print(plot2)
    TRUE
    
  },
  
  .plot3 = function(image,ggtheme, theme, ...){

    if(!self$options$plot3)
      return(FALSE)
    
    p3 <- private$.computeP3() 
    plot3 <-multipleROC::plot_ROC(p3, facet=TRUE) 
    #  plot3 <- plot3+ggtheme
    print(plot3)
    TRUE
    
  },
  
#Function---

.computeP2=function(){
  
  dep <- self$options$dep
  covs <- self$options$covs
  data <- self$data
  data <- na.omit(data)
  data <- as.data.frame(data)  
  
  roc <- list()
  
  # Loop through each element in covs
  for (i in seq_along(covs)) {
    # Compute ROC curve for the current covariate and store it in the list
    roc[[i]] <- multipleROC::multipleROC(as.formula(paste(paste(dep, paste0(covs[[i]]), sep="~"))), 
                                         data = data, plot = FALSE)
  }
  
  return(roc)
},

.computeP3=function(){
  
  dep <- self$options$dep
  covs <- self$options$covs
  data <- self$data
  data <- na.omit(data)
  data <- as.data.frame(data)  
  
  roc <- list()
  
  # Loop through each element in covs
  for (i in seq_along(covs)) {
    # Compute ROC curve for the current covariate and store it in the list
    roc[[i]] <- multipleROC::multipleROC(as.formula(paste(paste(dep, paste0(covs[[i]]), sep="~"))), 
                                         data = data, plot = FALSE)
  }
  
  return(roc)
}
 
   
     )
)
