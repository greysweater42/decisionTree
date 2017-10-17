#' plotDecisionTree
#'
#' plot a decision tree
#' @param treeResult result of a decision tree
#' @param dict a data.frame of optional names for vertices
#' @keywords plot decision tree, decision tree
#' @export
#' @examples
#' d <- iris[, c("Species", "Sepal.Length", "Sepal.Width")]
#' d$Species <- as.character(d$Species)
#' d$Species[d$Species != "setosa"] <- "non-setosa"
#' x <- d$Sepal.Length
#' x[d$Sepal.Length <= 5.2] <- "Very Short"
#' x[d$Sepal.Length >  5.2 & d$Sepal.Length <= 6.1] <- "Short"
#' x[d$Sepal.Length >  6.1 & d$Sepal.Length <= 7.0] <- "Long"
#' x[d$Sepal.Length >  7.0] <- "Very Long"
#' d$Sepal.Length <- x
#' decTree <- decisionTree(d, eta=5, purity=0.95, minsplit=0)
#' plotDecisionTree(decTree)

plotDecisionTree <- function(treeResult, dict) {
    # ---- prepare data
    nLevel <- max(treeResult$level)
    d <- cbind(treeResult, x=0, y=0)
    d$x[d$leaf != 0] <- d$leaf[d$leaf != 0]
    d$x[d$leaf != 0] <- d$leaf[d$leaf != 0]
    d$y <- d$level

    d <- cbind(d, name=as.character(d$vName), stringsAsFactors=F)
    d$name[d$leaf != 0] <- paste0(d$Lsize[d$leaf != 0], "\n(", 
                                  d$Lleft[d$leaf != 0], "|", 
                                  d$Lright[d$leaf != 0], ")")

    for (i in (nLevel - 1):1) {
        for (j in d$node) {
            if (d$level[j] == i & d$leaf[j] == 0) {
                crow <- d[d$node == j,]
                d$x[j] <- (d$x[d$parent == j & d$LR == "L"] + 
                           d$x[d$parent == j & d$LR == "R"]) / 2
            }
        }
    }

    # ---- plot edges
    plot.new()
    plot.window(c(0.5, nrow(d[d$leaf != 0,]) + 0.5), c(nLevel, 1))

    d <- cbind(d, parx=0, pary=0)
    #     y <- seq(0, pi, 0.001)
    #     x <- -cos(y)
    #     y <- y/pi * 0.8
    #     x <- x/2 + 0.5 # poprawka na szerokość smugi, żeby w child trafiała na środek
    for (i in 2:nrow(d)) {
        cpar <- d[d$node == d[i, "parent"],]  # parent
        width <- (d$Lsize[i] + d$Rsize[i]) / (d[1,"Lsize"] + d[1, "Rsize"]) * 80
        y <- seq(0, pi, length.out=30)
        x <- cos(y)
        lines((x / 2 + 1/2) * (cpar$x - d[i, "x"]) + d[i, "x"],
              y / pi + cpar$level, lwd = width, col="gray") 
        #     me <- d[d$node == i,]  # me
        #     cpar <- d[d$node == d[i, "parent"],]  # parent
        #     bro <- d[d$parent == cpar$node & d$LR != me$LR,]  # brother
        #     width <- me$Lsize + me$Rsize 
        #     fullWidth <- bro$Lsize + bro$Rsize 
        #     
        #     bwidth <-   # brother's width
        #     rect(x * (d[i, "x"] - cpar$x) + cpar$x - fullWidth / 2, 
        #          y + cpar$y + 0.1, 
        #          x * (d[i, "x"] - cpar$x) + cpar$x + width, 
        #          y + cpar$y + 0.1 + 0.001, 
        #          col="gray", border=NA)
        #     lines(x+cpar$x, y+cpar$y+0.1, col="gray")
        #     lines(x+cpar$x+0.3, y+cpar$y+0.1, col="gray")
    }
    # ---- plot vertices
    if (missing(dict)) {
        rect(d$x - strwidth(d$name)/1.8, d$y - 0.1,  # TODO wysokość zależna od nLevel
             d$x + strwidth(d$name)/1.8, d$y + 0.1, col="white", border="gray")
        text(d$x, d$y, d$name)
    } else {
        dd <- merge(d, dict, by="vName", all=T)
        dd$toPlot[dd$vName == "leaf"] <- dd$name[dd$vName == "leaf"]
        rect(dd$x - strwidth(dd$toPlot)/1.8, dd$y - 0.1,  # TODO wysokość zależna od nLevel
             dd$x + strwidth(dd$toPlot)/1.8, dd$y + 0.1, col="white", border="gray")
        text(dd$x, dd$y, dd$toPlot)
    }

    # ---- plot conditions on edges
    for (i in 1:nrow(d)) {
        if (d$v[i] != "") {
            Lchild <- d[d$parent == i & d$LR == "L",]
            Rchild <- d[d$parent == i & d$LR == "R",]
            x <- d$x[i] + (Lchild$x - d$x[i] ) / 2 - 0.1  # TODO dostosowac do liczby liści
            y <- d$y[i] + (Lchild$y - d$y[i] ) / 2 
            text(x, y, d$v[i]) 
            x <- d$x[i] + (Rchild$x - d$x[i] ) / 2 + 0.1
            y <- d$y[i] + (Rchild$y - d$y[i] ) / 2 
            text(x, y, d$vC[i])
        }
    }
}

