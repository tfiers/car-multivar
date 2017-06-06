# ML power transform of continuous variables

transform = function(XX, ignored_col='co2', verbose=FALSE, XSource=NULL) {
  # The maximum likelihood transforms are calculated using XSource (default: XX)
  if (is.null(XSource)) {
    XSource = XX
  }
  for (col in continuous_cols) {
    name = names(XX)[col]
    pt = powerTransform(XSource[,col])
    pt$lambda = pt$lambda
    transf_coldata = bcPower(XX[,col], pt$lambda)
    if ((abs(pt$lambda) < 3) && (name != ignored_col)) {
      XX[,col] = transf_coldata  
    }
    if (verbose) {
      qqnorm(XX[,col], main=name)
      qqline(XX[,col])
      p_before = shapiro.test(XX[,col])$p
      #
      qqnorm(transf_coldata, main=name)
      qqline(transf_coldata)
      p_after = shapiro.test(transf_coldata)$p
      #
      print(name)
      print(as.numeric(pt$lambda), digits=3)
      print(p_before, digits=3)
      print(p_after, digits=3)
    }
  }
  return(XX)
}
