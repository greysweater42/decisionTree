#' decision tree 
#'
#' Decision tree algorithm using information gain (entropy), works both for categorical and numerical attributes. Based on Zaki, Meira Jr., Data Mining and Analysis, p.481-496.
#' @param d data frame, first column is dependant variable
#' @param eta stop criterium, size of a leaf
#' @param purity stop criterium, purity of a leaf
#' @param minsplit do not split nodes that are smaller than minsplit
#' @keywords decision tree, information gain
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
#' decisionTree(d, eta=5, purity=0.95, minsplit=0)

# ---- decisionTree
decisionTree <- function(d, eta=10, purity=0.95, minsplit=10) {
    resultDF <- data.frame(node=numeric(), 
                           parent=numeric(),
                           level=numeric(), 
                           RL=character(),
                           Lsize=numeric(), 
                           Lleft=numeric(), 
                           Lright=numeric(), 
                           Rsize=numeric(), 
                           Rleft=numeric(), 
                           Rsize=numeric(),
                           vName=character())
    node <- 0
    baseEnv <- environment()
    .decisionTreeRecursive(d, eta=eta, purity=purity, LR="root", 
                           env=baseEnv, allX=unique(d[[1]]), 
                           minsplit=minsplit)
    resultDF$level <- resultDF$level - min(resultDF$level) + 1  # scaling to 1
    return(resultDF)
}

# ---- .evaluatNumericAttribute
.evaluateNumericAttribute <- function(d, x, minsplit) {
    # should be a private function
    cnames <- unique(d[[1]])
    d <- d[order(d[[x]]),]  # sort D on attribute X
    n <- nrow(d)
    k <- length(unique(d[[1]]))
    vs <- rep(0, n-1)
    for (i in 1:(n-1)) vs[i] <- (d[[x]][i] + d[[x]][i+1]) / 2  # midpoints
    vs <- unique(vs)

    best_H <- Inf
    best_v <- 0
    for (v in vs) {
        dy <- d[d[[x]] <  v,]
        dn <- d[d[[x]] >= v,]
        if (nrow(dy) < minsplit | nrow(dn) < minsplit) next()
        nv1 <- table(dy[[1]])
        nv2 <- table(dn[[1]])
        pcdy <- .entropy(nv1, cnames)
        pcdn <- .entropy(nv2, cnames)
        H1 <- sum(nv1) / n * pcdy + sum(nv2) / n * pcdn
        if (H1 < best_H) {
            best_H <- H1
            best_v <- v
        }
    }
    n0 <- table(d[[1]])
    H0 <- -sum(n0/n * log(n0/n, 2))
    result <- list(v=best_v, score=H0 - best_H, X=x)
    return(result)
}

# ---- .evaluateCategoricalAttribute
.evaluateCategoricalAttribute <- function(d, x, minsplit) {
    # should be a private function
    cnames <- unique(d[[1]])
    n <- nrow(d)
    k <- length(cnames)
    V <- unique(d[[x]])
    m <- length(V)
    vs <- list()
    for (i in 1:floor(m/2)) {
        vs <- c(vs, combn(V, i, simplify=F))     
    }

    best_H <- Inf
    best_v <- 0
    for (v in vs) {
        dy <- d[d[[x]] %in%  v,]
        dn <- d[!d[[x]] %in% v,]
        if (nrow(dy) < minsplit | nrow(dn) < minsplit) next()
        nv1 <- table(dy[[1]])
        nv2 <- table(dn[[1]])
        pcdy <- .entropy(nv1, cnames)
        pcdn <- .entropy(nv2, cnames)
        H1 <- sum(nv1) / n * pcdy + sum(nv2) / n * pcdn
        if (H1 < best_H) {
            best_H <- H1
            best_v <- v
        }
    }
    n0 <- table(d[[1]])
    H0 <- -sum(n0/n * log(n0/n, 2))
    result <- list(v=best_v, score=H0 - best_H, X=x)
    return(result)
}

# ---- .entropy
.entropy <- function(nv, cnames) {
    e <- 0  # entropy
    n <- sum(nv)
    for (cname in cnames)
        if (!is.na(nv[cname])) 
            e <- e - (nv[cname] / n) * log(nv[cname] / n, 2)
    return(e)
}

# ---- .decisionTreeRecursive
.decisionTreeRecursive <- function(d, eta, purity, LR, env, allX, minsplit=10) {
    d_purity <- max(table(d[[1]]) / nrow(d))
    if (nrow(d) < eta | d_purity > purity) return() 
    dd <- ncol(d) - 1  # number of attributes
    result <- list(v=0, score=0, X="")
    for (i in 1:dd) {
        attr_name <- colnames(d)[i+1]
        result0 <- if (is.numeric(d[[attr_name]])) 
            .evaluateNumericAttribute(d, attr_name, minsplit=minsplit) else         
                .evaluateCategoricalAttribute(d, attr_name, minsplit=minsplit)
        if (result0$score > result$score) result <- result0
    }
    if (is.numeric(result$v)) {
        dy <- d[d[[result$X]] <= result$v,]
        dn <- d[d[[result$X]] > result$v,]
    } else {
        dy <- d[d[[result$X]] %in% result$v,]
        dn <- d[!d[[result$X]] %in% result$v,]
    }
    env$node <- env$node + 1
    level <- length(sys.frames())
    parent <- 0
    for (i in nrow(env$resultDF):1) {  # get parent node number
        if (!nrow(env$resultDF)) {
            break
        } else if (env$resultDF$level[i] == level - 1) {
            parent <- env$resultDF$node[i]
            break
        }
    }
    toAppend <- data.frame(node=env$node, 
                           parent=parent,
                           level=level,
                           LR=LR,
                           Lsize=nrow(dy),
                           Lleft=if(is.na(table(dy[[1]])[allX][1])) 0 else table(dy[[1]])[allX][1],
                           LRight=if(is.na(table(dy[[1]])[allX][2])) 0 else table(dy[[1]])[allX][2],
                           Rsize=nrow(dn),
                           Rleft=if(is.na(table(dn[[1]])[allX][1])) 0 else table(dn[[1]])[allX][1],
                           RRight=if(is.na(table(dn[[1]])[allX][2])) 0 else table(dn[[1]])[allX][2],
                           vName=result$X)
    rownames(toAppend) <- NULL
    env$resultDF <- rbind(env$resultDF, toAppend)
    .decisionTreeRecursive(dy, eta=eta, purity=purity, LR="L", env=env, allX=allX, minsplit=minsplit)        
    .decisionTreeRecursive(dn, eta=eta, purity=purity, LR="R", env=env, allX=allX, minsplit=minsplit)
}

