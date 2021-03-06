#' Compute correlation matrix
#'
#' Compute Spearman and Pearson correlation matrix.
#'
#' @param v a data.frame (or list) should be taken.
#'
#' @examples
#' corr(mtcars[,1:5])
#'
#' corr(mtcars[,5:6], normal = T)
#'
#' @export
corr <- function(v, normal = F) {
  options(warn = -1)
  # Common parameter: panel.hist————-----————————————————————-—————————————————————————-—#
  panel.hist <- function(x, no.col=FALSE, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) ); his <- hist(x, plot=FALSE)
    breaks <- his$breaks; nB <- length(breaks)
    y <- his$counts; y <- y/max(y)
    if(no.col) rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
    else rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  }
  # Pearson Correlation Matrix-————-----———————-—————————————-—————————————————————————-—#
  percor <- function(v) {
    panel.cor <- function(x, y, method = 'pearson', digits=3, cex.cor=1.2, no.col=FALSE){
      par(usr = c(0, 1, 0, 1))
      usr <- par("usr"); on.exit(par(usr))
      r <- cor(x, y, method=method); ra <- cor.test(x, y, method=method)$p.value
      txt <- round(r, digits); prefix <- ""
      if(ra <= 0.1) prefix <- "."
      if(ra <= 0.05) prefix <- "*"
      if(ra <= 0.01) prefix <- "**"
      if(ra <= 0.001) prefix <- "***"
      if(no.col){
        color <- 1
        if(r < 0) { if(ra <= 0.001) sig <- 4 else sig <- 3 }
        else { if(ra <= 0.001) sig <- 2 else sig <- 1 }
      }else{
        sig <- 1
        if(ra <= 0.001) sig <- 2
        color <- 2
        if(r < 0) color <- 4
      }
      txt <- paste(txt, prefix, sep="\n")
      text(0.5, 0.5, txt, cex = cex.cor, font=sig, col=color)
    }
    pairs(v, lower.panel = panel.smooth, upper.panel = panel.cor,
          diag.panel = panel.hist,main = 'Pearson Correlation Matrix')
  }
  # Spearman Correlation Matrix-————-----——————-—————————————-—————————————————————————-—#
  specor <- function(v) {
    panel.cor <- function(x, y, method = 'spearman', digits=3, cex.cor=1.2, no.col=FALSE){
      par(usr = c(0, 1, 0, 1))
      usr <- par("usr"); on.exit(par(usr))
      r <- cor(x, y, method=method); ra <- cor.test(x, y, method=method)$p.value
      txt <- round(r, digits); prefix <- ""
      if(ra <= 0.1) prefix <- "."
      if(ra <= 0.05) prefix <- "*"
      if(ra <= 0.01) prefix <- "**"
      if(ra <= 0.001) prefix <- "***"
      if(no.col){
        color <- 1
        if(r < 0) { if(ra <= 0.001) sig <- 4 else sig <- 3 }
        else { if(ra <= 0.001) sig <- 2 else sig <- 1 }
      }else{
        sig <- 1
        if(ra <= 0.001) sig <- 2
        color <- 2
        if(r < 0) color <- 4
      }
      txt <- paste(txt, prefix, sep="\n")
      text(0.5, 0.5, txt, cex = cex.cor, font=sig, col=color)
    }
    pairs(v, lower.panel = panel.smooth, upper.panel = panel.cor,
          diag.panel = panel.hist,main = 'Spearman Correlation Matrix', method='spearman')
  }
  # process-------————-----————————————————————-———————————————————————--------------——-—#
  nor <- sapply(v, shapiro.test)
  if (normal == T) {
    percor(v)
    ans <- list('TIPS' = 'Compute Pearson Correlation Matrix', 'Matrix' = cor(v, method = 'pearson'))
    return(ans)
  }else {
    if (all(data.frame(nor)[2,] > 0.05) == T) {
      percor(v)
      ans <- list('TIPS' = 'Pass the normality test. Pearson Correlation Matrix is suggested',
                  'Matrix' = cor(v, method = 'pearson'))
      return(ans)
    }else {
      specor(v)
      ans <- list('TIPS' = 'Failed to pass the normality test. Spearman Correlation Matrix is suggested',
                  'Matrix' = cor(v, method = 'spearman'))
      return(ans)
    }
  }
}
