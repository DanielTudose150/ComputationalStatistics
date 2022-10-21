ex1 = function(a, b) {
  plot(log2,from=a,to=b,main=paste0("f(x) = log2(x), x in [",a,", ",b,"]"), col="red", lwd=2);
}

ex2 = function() {
  x = 1:20;
  for(p in seq(0.1,0.9,0.1))
  {
    photoTitle = floor(p * 10);
    photoTitle = paste0("binom",photoTitle,".png");
    png(photoTitle,width=600, height=600);
    
    plotTitle = paste0("B(x, 20, ",p,")"); 
    plot(x,dbinom(x,20,p), ylab="y", type="o", main=plotTitle);
    
  }
}

ex3 = function() {
  x = seq(-7,7,0.1);
  sig = c(0.5, 1, 2);
  colours = c("green3", "red", "blue");
  
  plot(x, dnorm(x, 0, sig[1]), ylab="y", col=colours[1], type="l");
  lines(x, dnorm(x, 0, sig[2]), col=colours[2], type="l");
  lines(x, dnorm(x, 0, sig[3]), col=colours[3], type="l");
  legend(-7,0.8 , legend=sig,col=colours,lty=1,cex=0.8, title="Standard Deviation");
}

CLT = function(n) {
  vmeans = c();
  for(i in 1:1000){
    v = runif(n, 0, 20);
    m = mean(v);
    vmeans = c(vmeans, m);
  }
  
  return(vmeans);
}

ex4b = function() {

  par(mfrow = c(2,2));
  
  interval = seq(0,20,1);
  
  for(i in c(1,5,10,100))
  {
    vmeans = CLT(i);
    hist(vmeans,main=paste0("Histogram for n=",i," using uniform distribution"), xlab="vmeans with default breaks");
  }
  
  for(i in c(1,5,10,100))
  {
    vmeans = CLT(i);
    hist(vmeans,main=paste0("Histogram for n=",i," using uniform distribution"),breaks=interval, xlab="vmeans with breaks at seq(0, 20, 1)");
  }
  
  # Pentru n=1, histograma numara valorile care se incadreaza intr-un break.
  # Pentru n > 1, histograma incepe sa prinda forma distributiei normale
  
  par(mfrow=c(1,1));
}

CLTc = function(n) {
  vmeans = c();
  for(i in 1:1000){
    v = rbinom(n, 20, 0.1);
    m = mean(v);
    vmeans = c(vmeans, m);
  }

  return(vmeans);
}

ex4c = function() {
  
  par(mfrow = c(2,2));
  
  for(i in c(1,5,10,100))
  {
    vmeans = CLTc(i);
    hist(vmeans,main=paste0("Histogram for n=",i," using binomial distribution"));
  }
  
  # Asemanator cu distributia uniforma pentru n > 1, graficul histrogramei incepe sa prinda forma de distributie normala
  # Valorile se pun intr-o anumita masura simetric fata de valoarea mediana
  # Pentru n=1, graficul histogramei are o forma asemanatoare distributiei binomiale
  
  par(mfrow=c(1,1));
}
