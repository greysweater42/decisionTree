#' decisionTree
#'
#' Decision tree algorithm uses information gain (entropy), works both for categorical and numerical attributes. Based on Zaki, Meira Jr., Data Mining and Analysis, p.481-496.
#' @param d data.frame, dependant variable must be in the first column
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
#' decisionTree(d, eta = 5, purity=0.95, minsplit=0)

# ---- decisionTree
decisionTree <- function(d, eta = 10, purity = 0.95, minsplit = 10) {
  resultDF <- data.frame(level = numeric(), 
                         parent = numeric(),
                         node = numeric(), 
                         leaf = numeric(),
                         RL = character(),
                         Lsize = numeric(), 
                         Lleft = numeric(), 
                         Lright = numeric(), 
                         Rsize = numeric(), 
                         Rleft = numeric(), 
                         Rright = numeric(), 
                         Rsize = numeric(),
                         v = character(),
                         vC = character(),  # complement to v
                         vName = character(),
                         stringsAsFactors = F)
  nodeChoices <- list()
  cs <- list()  # cluster uniques
  for (coln in colnames(d)) {
    if (is.character(d[[coln]])) {
      cs[[coln]] <- unique(d[[coln]])
    }
  }
  node <- 0
  leaf <- 0
  baseEnv <- environment()
  .decisionTreeRecursive(d, eta = eta, purity=purity, LR="root", 
                         env = baseEnv, allX=unique(d[[1]]), 
                         minsplit = minsplit, cs=cs)
  resultDF$level <- resultDF$level - min(resultDF$level) + 1  # scaling to 1
  dto <- new("DecisionTreeObject", 
             resultDF = resultDF, 
             nodeChoices=nodeChoices)
  return(dto)
}

# ---- DecisionTreeObject
DecisionTreeObject <- setClass("DecisionTreeObject", 
                               representation(resultDF = "data.frame", 
                                              nodeChoices = "list"))

# ---- .evaluatNumericAttribute
.evaluateNumericAttribute <- function(d, x, minsplit) {
  cnames <- unique(d[[1]])
  d <- d[(order(d[[x]])),]  # sort D on attribute X
  n <- nrow(d)
  k <- length(unique(d[[1]]))

  # midpoints
  vs <- rep(0, n-1)
  for (i in 1:(n-1)) vs[i] <- (d[[x]][i] + d[[x]][i+1]) / 2
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
  result <- list(v = best_v, score=H0 - best_H, X=x)
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
  for (i in 1:floor(m / 2)) {
    vs <- c(vs, combn(V, i, simplify = F))     
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
  H0 <- -sum(n0 / n * log(n0 / n, 2))
  result <- list(v = best_v, score=H0 - best_H, X=x)
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
.decisionTreeRecursive <- function(d, eta, purity, LR, env, allX, 
                                   minsplit = 10, cs) {
  d_purity <- max(table(d[[1]]) / nrow(d))
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
  if (nrow(d) < eta | d_purity > purity) {  # leaf
    env$node <- env$node + 1
    env$leaf <- env$leaf + 1
    te <- data.frame(level = level,
                     parent = parent,
                     node = env$node,
                     leaf = env$leaf,
                     LR = LR,
                     Lsize = nrow(d),
                     Lleft = if(is.na(table(d[[1]])[allX][1])) 0 else table(d[[1]])[allX][1],
                     Lright = if(is.na(table(d[[1]])[allX][2])) 0 else table(d[[1]])[allX][2],
                     Rsize = 0,
                     Rleft = 0,
                     Rright = 0,
                     v = "",
                     vC = "",
                     vName = "leaf",
                     stringsAsFactors = F)
    rownames(te) <- NULL
    env$resultDF <- rbind(env$resultDF, te)
    return() 
  } else {  # node
    nodeChoices <- data.frame(res0 = character(), score=character(), 
                              v = character(), stringsAsFactors=F)
    dd <- ncol(d) - 1  # number of attributes
    result <- list(v = 0, score=0, X="")
    for (i in 1:dd) {
      attr_name <- colnames(d)[i+1]
      result0 <- if (is.numeric(d[[attr_name]])) 
        .evaluateNumericAttribute(d, attr_name, minsplit = minsplit) else         
          .evaluateCategoricalAttribute(d, attr_name, minsplit = minsplit)
      cNodeChoices <- data.frame(res0 = result0$X, score=result0$score, 
                                 v = paste(result0$v, collapse=","), 
                                 stringsAsFactors = F)
      nodeChoices <- rbind(nodeChoices, cNodeChoices, stringsAsFactors = F)
      if (result0$score > result$score) result <- result0
    }
    colnames(nodeChoices) <- c(paste0("--", result$X, "--"), "score", "v")
    nodeChoices <- nodeChoices[order(nodeChoices$score, decreasing = T),]
    rownames(nodeChoices) <- NULL
    if (is.numeric(result$v)) {
      dy <- d[d[[result$X]] <= result$v,]
      dn <- d[d[[result$X]] > result$v,]
    } else {
      dy <- d[d[[result$X]] %in% result$v,]
      dn <- d[!d[[result$X]] %in% result$v,]
    }
    env$node <- env$node + 1
    env$nodeChoices[[env$node]] <- nodeChoices
    v <- if (is.numeric(result$v)) paste0("less than\n", result$v) else paste(result$v, collapse = ",\n")
    vC <- if (is.numeric(result$v)) paste0("more than\n", result$v) else paste(setdiff(cs[[result$X]], result$v) , collapse = ",\n")
    te <- data.frame(level = level,
                     parent = parent,
                     node = env$node, 
                     leaf = 0,
                     LR = LR,
                     Lsize = nrow(dy),
                     Lleft = if(is.na(table(dy[[1]])[allX][1])) 0 else table(dy[[1]])[allX][1],
                     Lright = if(is.na(table(dy[[1]])[allX][2])) 0 else table(dy[[1]])[allX][2],
                     Rsize = nrow(dn),
                     Rleft = if(is.na(table(dn[[1]])[allX][1])) 0 else table(dn[[1]])[allX][1],
                     Rright = if(is.na(table(dn[[1]])[allX][2])) 0 else table(dn[[1]])[allX][2],
                     v = v,
                     vC = vC,
                     vName = result$X,
                     stringsAsFactors = F)
    rownames(te) <- NULL
    env$resultDF <- rbind(env$resultDF, te)
    .decisionTreeRecursive(dy, eta = eta, purity=purity, LR="L", env=env, 
                           allX = allX, minsplit=minsplit, cs=cs) 
    .decisionTreeRecursive(dn, eta = eta, purity=purity, LR="R", env=env, 
                           allX = allX, minsplit=minsplit, cs=cs)
  }
}

