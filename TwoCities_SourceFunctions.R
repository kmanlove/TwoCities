as.dendrogram.igraph.walktrap <- function (object, hang=-1, use.modularity=FALSE, ...){
  .memberDend <- function(x) {
    r <- attr(x,"x.member")
    if(is.null(r)) {
      r <- attr(x,"members")
      if(is.null(r)) r <- 1:1
    }
    r
  }
      stopifnot(nrow(object$merges)> 0)
      storage.mode(object$merges) <- "integer"
      object$merges <- object$merges + 1L
      if (is.null(object$labels))
          object$labels <- 1:(nrow(object$merges)+1)-1
      z <- list()
      if (!use.modularity || is.null(object$modularity)) {
          object$height <- 1:nrow(object$merges)
        } else {
            object$height <- object$modularity[-1]
            object$height <- cumsum(object$height - min(object$height))
          }
      nMerge <- length(oHgt <- object$height)
      if (nMerge != nrow(object$merges))
          stop("'merge' and 'height' do not fit!")
      hMax <- oHgt[nMerge]
      one <- 1:1;
      two <- 2:2 # integer!
      leafs <- nrow(object$merges)+1
      for (k in 1:nMerge) {
          x <- object$merges[k, ]# no sort() anymore!
          if (any(neg <- x < leafs+1))
              h0 <- if (hang < 0) 0 else max(0, oHgt[k] - hang * hMax)
          if (all(neg)) {                  # two leaves
              zk <- as.list(x)
              attr(zk, "members") <- two
              attr(zk, "midpoint") <- 0.5 # mean( c(0,1) )
              objlabels <- object$labels[x]
              attr(zk[[1]], "label") <- objlabels[1]
              attr(zk[[2]], "label") <- objlabels[2]
              attr(zk[[1]], "members") <- attr(zk[[2]], "members") <- one
              attr(zk[[1]], "height") <- attr(zk[[2]], "height") <- h0
              attr(zk[[1]], "leaf") <- attr(zk[[2]], "leaf") <- TRUE
            }
          else if (any(neg)) {            # one leaf, one node
              X <- as.character(x)
              ## Originally had "x <- sort(..) above => leaf always left, x[1];
                ## don't want to assume this
                isL <- x[1] < leafs+1 ## is leaf left?
              zk <- if(isL) list(x[1], z[[X[2]]])
                else    list(z[[X[1]]], x[2])
              attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + one
              attr(zk, "midpoint") <- (igraph:::.memberDend(zk[[1]]) + attr(z[[X[1 + isL]]], "midpoint"))/2
              attr(zk[[2 - isL]], "members") <- one
              attr(zk[[2 - isL]], "height") <- h0
              attr(zk[[2 - isL]], "label") <- object$labels[x[2 - isL]]
              attr(zk[[2 - isL]], "leaf") <- TRUE
              }
          else {                        # two nodes
              x <- as.character(x)
              zk <- list(z[[x[1]]], z[[x[2]]])
              attr(zk, "members") <- attr(z[[x[1]]], "members") + attr(z[[x[2]]], "members")
              attr(zk, "midpoint") <- (attr(z[[x[1]]], "members") + attr(z[[x[1]]], "midpoint") + attr(z[[x[2]]], "midpoint"))/2
          }
          attr(zk, "height") <- oHgt[k]
          z[[k <- as.character(k+leafs)]] <- zk
        }
      z <- z[[k]]
      class(z) <- "dendrogram"
      z
}
