#' Principal component analysis
#'
#' Given a data frame, perform a principal component analysis
#' @param df The data frame
#' @return a list containing weights of the analysis, principal components,
#' the percent variance of each component, the factor loadings, and projections
#' of the data frame onto the principal components.
#' @export
#' @example pca(iris[,1:4])
pca <- function(df, scale = TRUE){
  # Takes a data frame and performs a PCA
  if(scale){
    covar = cor(df)
  }else{
    covar = cov(df)
  }
  ev <- eigen(covar)
  values <- ev$values
  vectors <- ev$vectors
  pov <- values / sum(values)
  ar = vectors %*% diag(values)^.5
  row.names(ar) <- colnames(df)
  datamat <- as.matrix(df)
  if(scale){
    scaledmat <- apply(datamat, 2, function(v){(v - mean(v))/sd(v)})
    proj <- scaledmat %*% vectors
  }else{
    proj <- datamat %*% vectors
  }
  list('weights' = values, 'components' = vectors, 'percent_variance' = pov,
       'factor_loadings' = ar, 'projection' = proj)
}
