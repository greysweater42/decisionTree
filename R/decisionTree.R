#' decision tree 
#'
#' Decision tree algorithm using information gain (entropy), works both for categorical and numerical attributes. Based on Zaki, Meira Jr., Data Mining and Analysis, p.481-496.
#' @param d data frame, first column is dependant variable
#' @param eta stop criterium, size of a leaf
#' @param purity stop criterium, purity of a leaf
#' @param minsplit do not split nodes that are smaller than minsplit
#' @keywords decision tree, information gain
#' @export

decisionTree <- function(d, eta=10, purity=0.95, minsplit=10) {
    listToPrint <- list()
    baseEnv <- environment()
    .decisionTreeRecursive(d, eta=eta, purity=purity, L="root", 
                           env=baseEnv, allX=unique(d[[1]]), 
                           minsplit=minsplit)
    return(listToPrint)
}

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

.entropy <- function(nv, cnames) {
    e <- 0  # entropy
    n <- sum(nv)
    for (cname in cnames)
        if (!is.na(nv[cname])) 
            e <- e - (nv[cname] / n) * log(nv[cname] / n, 2)
    return(e)
}
#' }}}

.decisionTreeRecursive <- function(d, eta, purity, L, env, allX, minsplit=10) {
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
    level <- paste(rep("   ", length(sys.frames())), collapse="")
    numsDy <- paste0(table(dy[[1]])[allX], collapse="-")
    numsDx <- paste0(table(dn[[1]])[allX], collapse="-")
    toPrint <- paste(level, result$X, paste(result$v, collapse=", "), nrow(dy), 
                     nrow(dn), L, numsDy, "-", numsDx)
    env$listToPrint <- c(env$listToPrint, toPrint)
    .decisionTreeRecursive(dy, eta=eta, purity=purity, L="L", env=env, allX=allX, minsplit=minsplit)        
    .decisionTreeRecursive(dn, eta=eta, purity=purity, L="P", env=env, allX=allX, minsplit=minsplit)
}


