library(e1071)
library(ggplot2)
library(caret)
library(fastICA)
library(reshape2)
#library(d3heatmap)
library(plotly)

source("helpers.R")

#' The following function displays the skewness of column vectors of a data frame,
#' as bar- or density plot depending of the dimensions of the input data frame.
#' @author Jens Hooge jens.hooge@gmail.com

#' @title Skewness Plot.
#' 
#' @description \code{plotSkewness} plots the skewness of a distribution.
#' 
#' @details
#' \code{plotSkewness} expects a data frame X, and computes the skewness of its
#' column vectors. Skewness is a measure of asymmetry of a distribution. While a
#' skewness of 0 indicates a perfectly symmetrical distribution, larger skewness values
#' indicate that the density of values is higher for smaller values. In that case the
#' distribution would be called right skewed. Vice versa, for negative values of skewness
#' the distribution would be called left skewed. Skewness values between -2 and 2 indicate
#' a roughly symmetric distribution. If the number of columns in X is smaller or equal to 50,
#' skewness displayed in a barplot and a density plot otherwise.
#' 
#' @param X real valued data frame
#' 
#' @examples
#' plotSkewness(mtcars)
#' 
#' @return list{base}
plotSkewness <- function(X) {
  X <- as.data.frame(X) ## in case it is just a numeric vector
  skewValues <- as.data.frame(sapply(X, skewness, na.rm=TRUE, type=1))
  skewValues$variable <- rownames(skewValues)
  rownames(skewValues) <- NULL
  colnames(skewValues) <- c("skewnessValue", "variable")
  skewValues$variable <- as.factor(skewValues$variable)
  
  if (ncol(X) <= 50){
    fig <- ggplot(skewValues, aes(x=variable, y=skewnessValue)) +
      geom_bar(stat="identity", position="dodge") +
      geom_hline(yintercept = 2, linetype = "dashed", color="red") +
      geom_hline(yintercept = -2, linetype = "dashed", color="red") +
      ylab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  } else {
    fig <- ggplot(skewValues, aes(x=skewnessValue)) +
      geom_density() +
      geom_point(aes(x=skewnessValue, y = 0.0005),
                 alpha = 0.25, size=4, colour="darkgray") +
      geom_vline(xintercept = 2, linetype = "dashed", color="red") +
      geom_vline(xintercept = -2, linetype = "dashed", color="red") +
      annotate("text", x = Inf, y = Inf, label = sprintf("n=%i", ncol(X)),
               vjust=1.8, hjust=1.2) +
      xlab(NULL) +
      ylab(NULL) +
      theme_bw()
  }
  return(fig)
}

plotPairs <- function(X) {
  
  #' plot absolute correlation coefficients (pearson) into panel
  #' and adjust the text size according to the correlation
  #' @param x numeric vector
  #' @param y numeric vector
  #' @return plot text element
  #' 
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  
  
  #' plot histogram into diagonal panel of a numeric vector
  #' @param x numeric vector
  #' @return histogram with colored bars
  #' 
  panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "#67000D", ...)
  }
  
  
  #' plot a smoothScatter (color density of a scatter plot) with a loess fit
  #' into the panels 
  #' @param x numeric vector
  #' @param y numeric vector
  #' @return smoothed scatter plot
  #' 
  panel.smoothScatter <- function (x, y, bg = NA, 
                                   cex = 1, col.smooth = "red",
                                   span = 2/3, iter = 3, ...) {
    # colors for the density
    palette <- colorRampPalette(c("blue", "orange", "red"))
    s <- smoothScatter(x, y, colramp = palette, bg = bg, cex = cex, add=T)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
  }
  
  mat <- as.matrix(X)
  fig <- pairs(mat,
               lower.panel = panel.smoothScatter,
               upper.panel = panel.cor,
               diag.panel  = panel.hist)
  return(fig)
  
  
  # ### examples
  # data(iris)
  # myMatrix <- as.matrix(iris[,1:4])
  # ### see ?pairs for more help
  # pairs(myMatrix,
  #       lower.panel = panel.smoothScatter,
  #       upper.panel = panel.cor,
  #       diag.panel  = panel.hist)
  
}

plotCorMat <- function(X) {
  corrmatrix <- cor(X)
  corrdata=as.data.frame(corrmatrix)
  corrdata$Variable1=names(corrdata)
  corrdatamelt=melt(corrdata,id="Variable1")
  names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
  corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
  corrmatplot$addParams(height = 400, width=800)
  corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
  corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
  corrmatplot$guides(x = list(numticks = 3))
  #corrmatplot$addParams(staggerLabels=TRUE)
  corrmatplot
}

plotViolins <- function(X, label) {
  
  molten <- melt(X, id.vars = label)
  
  fig <- ggplot(molten, aes(variable, value, fill=molten[, 1])) + 
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.1) +
    geom_point(alpha = 0.3,  position = "jitter") +
    labs(x=NULL, y=NULL) + 
    coord_flip() +
    theme_bw() +
    guides(fill=guide_legend(title=label))
          
  return(fig)
}

qqPlot <- function(values) {
  df <- data.frame(values=values)
  fig <- ggplot(df, aes(sample = values)) +
    stat_qq(alpha=0.5) + 
    geom_abline(intercept = mean(df$values),
                slope = sd(df$values),
                colour="red") +
    labs(title="QQ Plot") +
    theme_bw() +
    theme(plot.title   = element_text(size=25),
          axis.text.x  = element_text(size=20),
          axis.title.x = element_text(size=25),
          axis.text.y  = element_text(size=20),
          axis.title.y = element_text(size=25),
          legend.title = element_blank(),
          legend.text  = element_text(size=15))
  
  return(fig)
}

featureTypePlot <- function(df) {
  df <- as.data.frame(table(sapply(df, class)))
  colnames(df) <- c("Type", "Freq")
  df<- df[with(df, order(-Freq)), ]
  df$Type <- as.factor(df$Type)
  df$Type <- factor(df$Type, levels=df$Type[order(df$Freq)], ordered=TRUE)
  
  fig <- nPlot(Freq ~ Type, data = df, type = 'pieChart')
  fig$chart(donut = TRUE)
  fig$addParams(dom = "featureTypePlot")

  return(fig)
}

responseClassesPlot <- function(df) {
  label <- colnames(df)[1]
  f <- as.formula(paste("~", label))
  fig <- nPlot(f, data = df, type = 'pieChart')
  fig$chart(donut = TRUE)
  fig$addParams(dom = "responseClassesPlot")

  return(fig)
}

pca2dPlot <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]

  X_projected <- pcaTransform(X)
  # preProc   = preProcess(X, method=c("center", "scale", "pca"))
  # X_projected = predict(preProc, X)[, 1:3] # PCA projection
  
  projection <- data.frame(Label=Y,
                           PC1=X_projected[, 1], 
                           PC2=X_projected[, 2],
                           PC3=X_projected[, 3])
  
  tools <- c("pan", "resize", 
             "wheel_zoom", "box_zoom", 
             "box_select", "lasso_select", 
             "reset", "save")
  
  cols <- 2:ncol(projection)
  nms <- expand.grid(names(projection)[cols], 
                     rev(names(projection)[cols]), 
                     stringsAsFactors = FALSE)
  
  splom_list <- vector("list", 9)
  for(ii in seq_len(nrow(nms))) {
    splom_list[[ii]] <- figure(width = 200, height = 200, tools = tools,
                               xlab = nms$Var1[ii], ylab = nms$Var2[ii]) %>%
      ly_points(nms$Var1[ii], nms$Var2[ii], 
                data = projection,
                color = Label, 
                size = 6, 
                hover=list(Label),
                legend = FALSE)
  }
  grid_plot(splom_list, ncol = 3, same_axes = TRUE, link_data = TRUE)
}

pca2dPlotly <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]

  # print(sprintf("colnames(X): ",colnames(X)))
  # print(sprintf("label: ", label))

  X_projected <- pcaTransform(X)
  Y <- data.frame(Y)
  colnames(Y) <- label
  X_projected <- cbind(X_projected, Y)

  print(colnames(X_projected))
  # print(sprintf("colnames(X_projected): ", label))

  f <- list(
    family = "sans-serif",
    size = 12,
    color = "#000"
  )
  x <- list(
    title = colnames(X_projected)[1],
    titlefont = f
  )
  y <- list(
    title = colnames(X_projected)[2],
    titlefont = f
  )

  fig <- plot_ly(data = X_projected,
                 x = X_projected[, 1],
                 y = X_projected[, 2],
                 mode = "markers",
                 color = X_projected[, 3]) %>%
    layout(xaxis = x, yaxis = y) %>%
    config(displayModeBar = F)
  return(fig)
}

pca3dPlot <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]
  
  X_projected <- pcaTransform(X)
  # preProc   = preProcess(X, method=c("center", "scale", "pca"))
  # X_projected = predict(preProc, X)[, 1:3] # PCA projection
  
  projection <- data.frame(Label=Y,
                           PC1=X_projected[, 1], 
                           PC2=X_projected[, 2],
                           PC3=X_projected[, 3])
  
  plot_ly(projection, x=~PC1, y=~PC2, z=~PC3, 
                 text=~paste("Label: ", Label),
                 color=~Label,
                 opacity=0.6,
                 type="scatter3d", mode="markers") %>%
    config(displayModeBar = F)
}

## does not work anymore since caret has been updated 
## preProc$trace does not exist anymore !!
screePlotPCA <- function(X) {
  preProc    <- preProcess(X, method=c("pca", "center", "scale"))
  components <- colnames(preProc$rotation)
  numComp    <- preProc$numComp
  
  # cumVar     <- preProc$trace[1:numComp]
  cumVar     <- cumsum(preProc$std^2/sum(preProc$std^2))
  screeDF    <- data.frame(Component=1:numComp, 
                           CumulativeVariance=cumVar)
  
  
  fig <- plot_ly(screeDF, 
                 x = Component, 
                 y = CumulativeVariance, 
                 name = "linear", 
                 line = list(shape = "linear")) %>%
         config(displayModeBar = F)
  
  return(fig)
}

screePlotICA <- function(X) {
  preProc    <- preProcess(X, method=c("ica", "center", "scale"), n.comp=ncol(X))
  components <- colnames(preProc$rotation)
  numComp    <- preProc$numComp
  cumVar     <- preProc$trace[1:numComp]
  screeDF    <- data.frame(Component=1:numComp, 
                           CumulativeVariance=cumVar)
  
  fig <- plot_ly(screeDF, 
                 x = Component, 
                 y = CumulativeVariance, 
                 name = "linear", 
                 line = list(shape = "linear")) %>%
    config(displayModeBar = F)
  
  return(fig)
}

ica2dPlot <- function(X, label, n.comp) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]
  X_projected <- icaTransform(X)

  projection <- data.frame(Label=Y,
                           IC1=X_projected[, 1], 
                           IC2=X_projected[, 2],
                           IC3=X_projected[, 3])
  
  tools <- c("pan", "resize", 
             "wheel_zoom", "box_zoom", 
             "box_select", "lasso_select", 
             "reset", "save")
  
  cols <- 2:ncol(projection)
  nms <- expand.grid(names(projection)[cols], 
                     rev(names(projection)[cols]), 
                     stringsAsFactors = FALSE)
  splom_list <- vector("list", 9)
  for(ii in seq_len(nrow(nms))) {
    splom_list[[ii]] <- figure(width = 200, height = 200, tools = tools,
                               xlab = nms$Var1[ii], ylab = nms$Var2[ii]) %>%
      ly_points(nms$Var1[ii], nms$Var2[ii], 
                data = projection,
                color = Label, 
                size = 6, 
                hover=list(Label),
                legend = FALSE)
  }
  grid_plot(splom_list, ncol = 3, same_axes = TRUE, link_data = TRUE)
}
 
ica2dPlotly <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]

  X_projected <- icaTransform(X, n.comp = ncol(X))
  Y <- data.frame(Y)
  colnames(Y) <- label
  X_projected <- cbind(X_projected, Y)

  f <- list(
    family = "sans-serif",
    size = 12,
    color = "#000"
  )
  x <- list(
    title = colnames(X_projected)[1],
    titlefont = f
  )
  y <- list(
    title = colnames(X_projected)[2],
    titlefont = f
  )

  fig <- plot_ly(data = X_projected,
                 x = X_projected[, 1],
                 y = X_projected[, 2],
                 mode = "markers",
                 color = X_projected[, 3]) %>%
    layout(xaxis = x, yaxis = y) %>%
    config(displayModeBar = F)

  return(fig)
}

ica3dPlot <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]
  
  X_projected <- icaTransform(X)

  projection <- data.frame(Label=Y,
                           IC1=X_projected[, 1], 
                           IC2=X_projected[, 2],
                           IC3=X_projected[, 3])
  
  plot_ly(projection, x=~IC1, y=~IC2, z=~IC3, 
                 text=~paste("Label: ", Label),
                 color=~Label,
                 type="scatter3d", mode="markers") %>% 
    config(displayModeBar = F)
}




### STILL TODO
explVarPCAplot <- function(X) {
  preProc <- preProcess(X, method=c("pca", "center", "scale"))
  numComp <- preProc$numComp
  df <- data.frame(Component=colnames(preProc$rotation),
                   ExplVariance=preProc$trace[1:numComp])
  
  fig <- figure(width = 600, height = 600) %>%
    ly_points(df, x=Component, y=ExplVariance, 
              hover = list(ExplVariance))
  fig
}

explVarICAplot <- function(X) {
  preProc <- preProcess(X, method=c("ica", "center", "scale"))
  numComp <- preProc$numComp
  df <- data.frame(Component=colnames(preProc$rotation),
                   ExplVariance=preProc$trace[1:numComp])
  
  fig <- figure(width = 600, height = 600) %>%
    ly_points(df, x=Component, y=ExplVariance, 
              hover = list(ExplVariance))
  fig
}

## This function crashes the RStudio session for some reason. tsne works rtsne does not!
tsnePlot <- function(X, label) {
  Y <- X[, label]
  X <- X[, -which(names(X) == label)]
  X_projected <- tsneTransform(X)
  
  projection <- data.frame(Label=Y,
                           Comp1=X_projected[, 1], 
                           Comp2=X_projected[, 2],
                           Comp3=X_projected[, 3])
  
  tools <- c("pan", "resize", 
             "wheel_zoom", "box_zoom", 
             "box_select", "lasso_select", 
             "reset", "save")
  
  cols <- 2:ncol(projection)
  nms <- expand.grid(names(projection)[cols], 
                     rev(names(projection)[cols]), 
                     stringsAsFactors = FALSE)
  splom_list <- vector("list", 9)
  for(ii in seq_len(nrow(nms))) {
    splom_list[[ii]] <- figure(width = 200, height = 200, tools = tools,
                               xlab = nms$Var1[ii], ylab = nms$Var2[ii]) %>%
      ly_points(nms$Var1[ii], nms$Var2[ii], 
                data = projection,
                color = Label, 
                size = 6, 
                hover=list(Label),
                legend = FALSE)
  }
  grid_plot(splom_list, ncol = 3, same_axes = TRUE, link_data = TRUE)
}
