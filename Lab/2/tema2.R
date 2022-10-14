ex1 = function(a, b) {
  plot(log2,from=a,to=b,main=paste0("f(x) = log2(x), x in [",a,", ",b,"]"), col="red", lwd=2);
}

ex2 = function() {
  x = 1:20;
  for(p in seq(0.1,0.9,0.1))
  {
    plot(x,dbinom(x,20,p), ylab="y", type="o");
    title(main=paste0("B(x, 20, ",p,")"));
    png(paste0("binom",p,".png"),width=600, height=600);
  }
}