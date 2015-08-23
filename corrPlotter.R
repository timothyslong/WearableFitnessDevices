

####### CORRELATIONS #########
library(corrplot)

# Scatter plot with correlations in the upper triangle, smoothing lines in
# the lower triangle, and histograms on the diagonal
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) 
    cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r)/1.5)#/2
}


panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

#tiff(filename='correlation matrix.tiff', width=1920, height=1280)
corrPlot <- function(corrs)
{
  pairs(corrs, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
}
