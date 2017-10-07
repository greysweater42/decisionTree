d <- iris[, c("Species", "Sepal.Length", "Sepal.Width")]
d$Species <- as.character(d$Species)
d$Species[d$Species != "setosa"] <- "non-setosa"

library(ggplot2)
ggplot(data = d, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point()


evaluateNumericAttribute <- function(d, x) {
    # should be a private function
    cnames <- unique(d[[1]])
    d <- d[order(d[[x]]),]  # sort D on attribute X
    n <- nrow(d)
    k <- length(unique(d[[1]]))
    vs <- rep(0, n-1)
    for (i in 1:(n-1)) vs[i] <- (d[[x]][i] + d[[x]][i+1]) / 2  # midpoints
    vs <- unique(vs)

    best_H <- Inf
    m <- 0
    for (v in vs) {
        dy <- d[d[[x]] <  v,]
        dn <- d[d[[x]] >= v,]
        nv1 <- table(dy[[1]])
        nv2 <- table(dn[[1]])
        pcdy <- entropy(nv1, cnames)
        pcdn <- entropy(nv2, cnames)
        H1 <- sum(nv1) / n * pcdy + sum(nv2) / n * pcdn
        if (H1 < best_H) {
            best_H <- H1
            m <- v
        }
    }
    n0 <- table(d[[1]])
    H0 <- -sum(n0/n * log(n0/n, 2))
    result <- list(v=m, score=H0 - best_H, X=x)
    return(result)
}

decisionTree <- function(d, eta=5, purity=0.95) {
    if (!nrow(d)) return()
    d_purity <- max(table(d[[1]]) / nrow(d))
    if (nrow(d) < eta | d_purity > purity) {
        return()
    }
    dd <- ncol(d) - 1  # number of attributes
    result <- list(v=0, score=0, X="")
    for (i in 1:dd) {
        attr_name <- colnames(d)[i+1]
        result0 <- evaluateNumericAttribute(d, attr_name) 
        if (result0$score > result$score) result <- result0
    }
    dy <- d[d[[result$X]] <= result$v,]
    dn <- d[d[[result$X]] > result$v,]
    cat(result$X, "\t", result$v, "\t", nrow(dy), "\t", nrow(dn), "\n")
    decisionTree(dy)
    decisionTree(dn)
}

entropy <- function(nv, cnames) {
    # should be a private function
    e <- 0  # entropy
    n <- sum(nv)
    for (cname in cnames)
        if (!is.na(nv[cname])) 
            e <- e - (nv[cname] / n) * log(nv[cname] / n, 2)
    return(e)
}

decisionTree(d)


