# d <- iris[, c("Species", "Sepal.Length", "Sepal.Width")]
# 
# d$Species <- as.character(d$Species)
# d$Species[d$Species != "setosa"] <- "non-setosa"
# x <- d$Sepal.Length
# x[d$Sepal.Length <= 5.2] <- "Very Short"
# x[d$Sepal.Length >  5.2 & d$Sepal.Length <= 6.1] <- "Short"
# x[d$Sepal.Length >  6.1 & d$Sepal.Length <= 7.0] <- "Long"
# x[d$Sepal.Length >  7.0] <- "Very Long"
# d$Sepal.Length <- x

# library(ggplot2)
# ggplot(data = d, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#     geom_point()


.evaluateNumericAttribute <- function(d, x) {
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

.evaluateCategoricalAttribute <- function(d, x) {
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


decisionTree <- function(d, eta=10, purity=0.95) {
    listToPrint <- list()
    baseEnv <- environment()
    .decisionTreeRecursive(d, eta=eta, purity=purity, L="root", 
                           env=baseEnv, allX=unique(d[[1]]))
    return(listToPrint)
}


.decisionTreeRecursive <- function(d, eta, purity, L, env, allX) {
    d_purity <- max(table(d[[1]]) / nrow(d))
    if (nrow(d) < eta | d_purity > purity) return() 
    dd <- ncol(d) - 1  # number of attributes
    result <- list(v=0, score=0, X="")
    for (i in 1:dd) {
        attr_name <- colnames(d)[i+1]
        result0 <- if (is.numeric(d[[attr_name]])) 
            .evaluateNumericAttribute(d, attr_name) else         
                .evaluateCategoricalAttribute(d, attr_name)
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
    toPrint <- paste(level, result$X, result$v, nrow(dy), nrow(dn), L, numsDy, "-", numsDx)
    env$listToPrint <- c(env$listToPrint, toPrint)
    .decisionTreeRecursive(dy, eta=eta, purity=purity, L="L", env=env, allX=allX)        
    .decisionTreeRecursive(dn, eta=eta, purity=purity, L="P", env=env, allX=allX)
}

.entropy <- function(nv, cnames) {
    e <- 0  # entropy
    n <- sum(nv)
    for (cname in cnames)
        if (!is.na(nv[cname])) 
            e <- e - (nv[cname] / n) * log(nv[cname] / n, 2)
    return(e)
}

decisionTree(d, purity=0.8)

