ex1 = function() {
  alcool = read.table("alcool.dat",header=TRUE);
  attach(alcool);
  
  plot(alcool$Alcool_din_vin,alcool$Decese_datorate_afectiunilor);
  
  text(alcool$Alcool_din_vin,alcool$Decese_datorate_afectiunilor + 9,alcool$Tara);
  
  x = (alcool$Alcool_din_vin - mean(alcool$Alcool_din_vin)) / sd(alcool$Alcool_din_vin);
  y = (alcool$Decese_datorate_afectiunilor - mean(alcool$Decese_datorate_afectiunilor)) / sd(alcool$Decese_datorate_afectiunilor);
  
  corrcoef = cov(x,y);
  
  print(paste("Correlation coefficient =", corrcoef));

  
  detach(alcool);
  
  print("Covariance matrix with standardized data");
  df = data.frame(x,y);
  mat = cov(df);
  print(mat);
}

ex2 = function() {
  iq = read.table("iq.dat",header=TRUE);
  attach(iq);
  
  plot(iq$IQ,iq$Nota);
  
  text(iq$IQ-0.5, iq$Nota, iq$Student);
  
  r = lm(iq$Nota~iq$IQ);
  rsc = summary(r)$coefficients[,1];
  
  estimator = function(a, b, x) {
    y = a + b * x;
    return(y);
  }
  
  print(paste0("Y = ",rsc[1]," + ", rsc[2], " * X"));
  
  print(paste0("Estimated value for IQ=", 115," = ",estimator(rsc[1],rsc[2],115)));
  
  print(paste0("Estimated value for IQ=", 130," = ",estimator(rsc[1],rsc[2],130)));
  
  detach(iq);
}

ex3 = function(m, a, b, xmin, xmax, sigma) {
  eps = rnorm(m, sd=sqrt(sigma));
  x = runif(m, xmin, xmax);
  y = c();
  
  for (i in seq(m)) {
    ytmp = a + b * x[i] + eps[i]; 
    y = c(y,ytmp);
  }
  
  return(y);
}

ex4 = function(obs) {
  
}
