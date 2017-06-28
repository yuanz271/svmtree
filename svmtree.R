require(foreach)
require(iterators)
require(e1071)

source("http://pastebin.com/raw.php?i=22p7heNA")

.CLASS <- "svmtree"

svmtree <- function(x, ...) UseMethod(.CLASS)

svmtree.formula <- function(formula, data, trace = FALSE, class.weights = "auto", max.depth = 5, ...) {
  mf <- model.frame(formula, data)
  y <- droplevels(model.response(mf))
  lv <- levels(y)
  
  if (nlevels(y) > 2) {
    stop("only supports binary response")
  }
  
  grow <- function(ind, depth) {
    mf <- mf[ind, ]
    y <- model.response(mf)
    
    cweights <- class.weights
    if (!is.null(class.weights)) { 
      if (class.weights == "auto") {
        cweights <-  length(y)/table(y)      
      }
    }
    
    node <- list(svm = svm(formula = formula, data = mf, class.weights = cweights, ...), left = NULL, right = NULL, value = NA)
    class(node) <- .CLASS
    
    node$value <- major(y)
    
    if (trace) {
      print(paste("Depth:", depth))
      print(table(y))
    }
    
    left <- fitted(node$svm) == lv[1]
    
    if (sum(left) != 0 & sum(!left) != 0 & depth < max.depth) {
      # if left branch has two classes
      if (length(unique(y[left])) > 1) {
        node$left <- Recall(left, depth + 1)
      }
      # if right branch has two classes
      if (length(unique(y[!left])) > 1) {
        node$right <- Recall(!left, depth + 1)
      }
    }
    
    node
  }
  
  root <- grow(rep_len(TRUE, length(y)), 1)
  attr(root, "levels") <- lv
  root
}

predict.svmtree <- function(object, newdata, trace = FALSE) {
  
  lv <- attr(object, "levels")
  
  predict2 <- function(node, x) {
    prediction <- predict(node$svm, x)
    if (prediction == lv[1]) {
      if (trace) {
        cat("L -> ")
      }
      if (!is.null(node$left))
        Recall(node$left, x)
      else 
        prediction
    }
    else {
      if (trace) {
        cat("R -> ")
      }
      if (!is.null(node$right))
        Recall(node$right, x)
      else 
        prediction
    }
  }
  
  unlist(foreach(x = iter(newdata, by = "row")) %do% predict2(object, x))
}

print.svmtree <- function(node, indent = "") {
  cat(toString(node$value), "\n")
  if (!is.null(node$left)) {
    cat(indent, "L -> ")
    print(node$left, paste0(indent, "      "))
  }
  if(!is.null(node$right)) {
    cat(indent, "R -> ")
    print(node$right, paste0(indent, "      "))
  }
}