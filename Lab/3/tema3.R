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
  
  abline(rsc[1], rsc[2],col="red");
  legend("topleft", "Y estimator",lty=1,col="red");
  
  print(paste0("Y = ",rsc[1]," + ", rsc[2], " * X"));
  tmp = estimator(rsc[1],rsc[2],115);
  print(paste0("Estimated value for IQ=", 115," = ",tmp));
  points(115,tmp,col="blue");
  
  tmp = estimator(rsc[1],rsc[2],130);
  print(paste0("Estimated value for IQ=", 130," = ",tmp));
  points(130,tmp,col="blue");
  detach(iq);
}

ex3 = function(m, a, b, xmin, xmax, sigma) {
  eps = rnorm(m, sd=sigma);
  x = runif(m, xmin, xmax);
  y = c();
  
  for (i in seq(m)) {
    ytmp = a + b * x[i] + eps[i]; 
    y = c(y,ytmp);
  }
  
  mat = matrix(cbind(x,y),c(length(x),2));
  return(mat);
}

#ex3(10, 10, 0.8, 5, 5.2, 0.01)


ex4 = function(m, x, y) {
  xmean = mean(x);
  ymean = mean(y);
  
  xcor = x - xmean;
  ycor = y - ymean;
  
  xy = sum(xcor * ycor);
  x2 = sum(xcor^2);
  
  b = xy / x2;
  a = ymean - b * xmean;
  
  print(paste("Intercept a =",a));
  print(paste("Slope b =",b));
  
  
  yestimat = x * b + a;
  
  sse = sum((y-yestimat)^2)
  s2 = sse / x2
  
  t = qt(0.975,df=x2);
  
  left = b - t * (sqrt(s2)/sqrt(x2));
  right = b + t * (sqrt(s2)/sqrt(x2));
  
  print(paste0(left," <= Beta <= ", right));
  
  res = cbind(a,b);
  return(res);
}
# ex4(7, seq(100,700,100), c(40,50, 50, 70, 65, 65,80))

ex5 = function() {
  a = 10;
  b = 0.8;
  ms = c(100, 10, 10000, 10, 10000, 10);
  xmins = c(-200,-5,-5,5,5,5);
  xmaxs = c(200,5,5,5.2,5.2,5.2);
  sigmas = c(1.5,1,1,1,1,0.01);
  
  colours = c("blue","red");
  lnames = c("Y = a + bX", "Y^ = a^ + b^ * X");
  
  for(i in seq(1,6,1)){
    mat = ex3(ms[i],a,b,xmins[i],xmaxs[i],sigmas[1]);
    x = mat[,1];
    y = mat[,2];
    ols = ex4(ms[i],x,y);
    beta0 = ols[1];
    beta1 = ols[2];
    
    pdfTitle = paste0("ex5_",i,".pdf");
    pdf(pdfTitle,width = 7, height = 7);
    
    plot(x,y);
    
    
    abline(a,b,col="blue");
    abline(beta0,beta1,col="red",lty=2);
    
    legend("topleft",legend=lnames,col=colours,lty=c(1,2));
    
  }
}

