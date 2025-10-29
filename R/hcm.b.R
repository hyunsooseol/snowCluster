
# This file is a generated template, your changes will not be overwritten

hcmClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "hcmClass",
    inherit = hcmBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) |
            is.null(self$options$podatki)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The hierarchical clustering module was created by Gasper Cankar & Hyunsoo Seol.</li>',
            '<li>Missing values in clustering variables will be omitted, and corresponding cases will show NA in the results.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowCluster/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$dend == TRUE)
          self$results$plot$setVisible(visible = TRUE)
        if (self$options$heat == TRUE)
          self$results$heat$setVisible(visible = TRUE)
        if ((self$options$pair == TRUE) & (self$options$case == FALSE))
          self$results$pairs$setVisible(visible = TRUE)
        
        if (isTRUE(self$options$dend)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        if (isTRUE(self$options$heat)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$heat$setSize(width, height)
        }
        if (isTRUE(self$options$pair)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$pairs$setSize(width, height)
        }
      },
      
      .run = function() {
        if (length(self$options$podatki) < 3)
          return()
        
        imena <- self$options$imena
        stand <- self$options$stand
        case <- self$options$case
        pair <- self$options$pair
        grp <- self$options$grp
        group <- self$options$group
        dis <- self$options$dis
        method <- self$options$method
        dend <- self$options$dend
        heat <- self$options$heat
        horiz <- self$options$horiz
        hght <- self$options$hght
        
        data <- self$data
        podatki <- self$options$podatki
        
        # 결측이 없는 행 인덱스 추출
        complete_idx <- which(stats::complete.cases(data[, podatki, drop = FALSE]))
        n_row <- nrow(data)
        
        # (1) 군집분석 데이터 생성
        if (length(imena) > 0) {
          ime <- as.character(data[, which(names(data) == imena)])
          pod0 <- data[, which(names(data) != imena)]
          if (stand == TRUE) {
            pod <- scale(na.omit(pod0))
          } else {
            pod <- na.omit(pod0)
          }
          rownames(pod) <- ime[complete_idx]
        } else {
          if (stand == TRUE) {
            pod <- scale(na.omit(data[, podatki, drop = FALSE]))
          } else {
            pod <- na.omit(data[, podatki, drop = FALSE])
          }
        }
        
        # case 옵션 처리
        if (case)
          pod <- t(pod)
        
        # (2) 거리 계산 및 군집분석
        razdalje <- stats::dist(pod, method = dis)
        klastri <- stats::hclust(d = jmvcore::toNumeric(razdalje), method = method)
        
        # (3) 클러스터 잘라내기
        if (grp == "height") {
          razrez <- cutree(klastri, h = hght)
        } else {
          razrez <- cutree(klastri, k = group)
        }
        
        # (4) 결과 테이블 populate
        table <- self$results$izpis
        table$setRow(
          rowNo = 1,
          values = list(
            var = ifelse((case == TRUE), nrow(pod), ncol(pod)),
            case = ifelse((case == TRUE), ncol(pod), nrow(pod)),
            dist = dis,
            method = method
          )
        )
        
        # (5) 군집 빈도수 테이블
        table2 <- self$results$groups
        tab <- as.data.frame(table(razrez))
        for (i in 1:nrow(tab)) {
          table2$addRow(rowKey = i,
                        values = list(cluster = as.numeric(as.character(tab[i, 1])), freq = tab[i, 2]))
        }
        
        # (6) **군집 membership: 결측 매칭**
        mem <- rep(NA, n_row)
        mem[complete_idx] <- as.numeric(razrez)
        self$results$clust$setValues(mem)
        self$results$clust$setRowNums(rownames(data))
        
        # (7) Dendrogram plot
        if (dend == TRUE) {
          image <- self$results$plot
          image$setState(list(klastri, razrez))
        }
        
        # (8) Heatmap plot
        if (heat == TRUE) {
          image2 <- self$results$heat
          image2$setState(pod)
        }
        
        # (9) Pairs plot
        if ((pair == TRUE) & (case == FALSE)) {
          image3 <- self$results$pairs
          # pair plot: NA매칭된 원본 데이터로 전달
          tmp <- matrix(NA, nrow=n_row, ncol=length(podatki)+1)
          colnames(tmp) <- c(podatki, "cluster")
          tmp[complete_idx, ] <- cbind(pod, razrez)
          image3$setState(tmp)
        }
        # (10) 텍스트 결과 필요 시
        # self$results$text$setContent(pod[1:3,])
      },
      
      .plot = function(image, ...) {
        if ((length(self$options$podatki) < 3) || (self$options$dend == FALSE))
          return()
        
        plotData <- image$state[[1]]
        razrez   <- image$state[[2]]
        
        # 라벨 추출
        labs <- tryCatch({
          if (!is.null(plotData$labels)) plotData$labels else {
            d <- stats::as.dendrogram(plotData)
            unlist(stats::dendrapply(d, function(n)
              if (identical(attr(n, "leaf"), TRUE)) attr(n, "label") else NULL))
          }
        }, error = function(e) character(0))
        
        # 기본값 고정
        lab_cex <- 0.7
        lab_mar <- 0.35
        wlen    <- if (length(labs)) max(nchar(labs, type = "width")) else 8
        
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar), add = TRUE)
        graphics::par(xpd = NA)
        
        dnd <- stats::as.dendrogram(plotData)
        
        if (isTRUE(self$options$horiz)) {
          # 가로형: 라벨 마진 조정만, 빨간선 없음
          rmar <- max(6, ceiling(lab_mar * wlen * lab_cex))
          graphics::par(mar = c(5, 4, 4, rmar))
          graphics::plot(dnd, type = "rectangle", horiz = TRUE, cex = lab_cex)
          
        } else {
          # 세로형: 기존 빨간 사각형 표시
          bmar <- max(6, ceiling(lab_mar * wlen * lab_cex))
          graphics::par(mar = c(bmar, 4, 4, 2))
          graphics::plot(dnd, hang = -1, cex = lab_cex)
          
          if (self$options$grp == "height") {
            stats::rect.hclust(plotData, h = self$options$hght, border = "red")
          } else {
            stats::rect.hclust(plotData, k = self$options$group, border = "red")
          }
        }
        
        TRUE
      },
      
      .heat = function(image2, ...) {
        if ((length(self$options$podatki) < 3) ||
            (self$options$heat == FALSE))
          return()
        old <- par(mar = c(7, 5, 5, 3) + .01)
        plotData <- image2$state
        if (self$options$heat == TRUE) {
          stats::heatmap(plotData)
        }
        par(old)
        TRUE
      },
      
      .pairs = function(image3, ...) {
        if ((length(self$options$podatki) < 3) ||
            (self$options$pair == FALSE))
          return()
        plotData <- image3$state
        # plotData가 NA로만 이루어진 경우 예외처리
        if (all(is.na(plotData))) {
          warning("All rows are NA after removing missing values. Pair plot will not be displayed.")
          return(FALSE)
        }
        # 마지막 열이 cluster 번호임
        cluster <- as.numeric(plotData[, ncol(plotData)])
        plotData2 <- plotData[, -ncol(plotData), drop=FALSE]
        # NA 행 제거 후 pairs plot
        complete_idx <- which(stats::complete.cases(plotData2))
        if (length(complete_idx) < 2) return(FALSE)
        graphics::pairs(plotData2[complete_idx, , drop=FALSE],
                        pch = 21,
                        bg = grDevices::heat.colors(self$options$group)[cluster[complete_idx]])
        TRUE
      }
    )
  )
