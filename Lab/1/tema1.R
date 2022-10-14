ex2 = function() {
  print("Exercitiu 2");
  x = c(1,8,2,6,2,8,8,5,5,5);
  
  # a
  xsum = sum(x)/10;
  print(paste("a) Mean of x =", xsum));
  
  # b
  xlog = log2(x);
  print("b) Log2 of x is");
  print(xlog);
  
  # c
  xdiff = max(x) - min(x);
  print(paste("c) Difference between max and min =",xdiff));
  
  # d
  y = (x-5)/2.624669;
  print("d) y array is");
  print(y);
  
  # e
  ymean = mean(y);
  ydev = sd(y);
  print(paste("e) Mean of y =", ymean));
  print(paste("Standard deviation of y =", ydev));
}

ex3 = function() {
  factura = c(46,33,49,37,36,50,48,32,49,35,30,48);
  
  print("Exercitiu 3");
  
  fsum = sum(factura);
  print(paste("a) Sum of factura =",fsum));
  
  fmin = min(factura);
  print(paste("b) Minimum of factura =",fmin));
  
  fmax = max(factura);
  print(paste("c) Maximum of factura =",fmax));
  
  fb40 = length(factura[factura > 40]);
  print(paste("d) Number of values > 40 =",fb40))
  
  # p/ 100 * n = x => p = x * 100 / n
  # n = factura.len; x = fb40
  p = fb40 * 100 / length(factura);
  print(paste("e) Percentage =",p));
  
}

ex4 = function() {
  print("Exercitiu 4");
  print("Please insert 7 real numbers");
  
  x = scan(n=7);
  
  xmax = max(x);
  xmin = min(x);
  xmean = mean(x);
  xmed = median(x);
  xsd = sd(x);
  
  print(paste("a) Max of x=",xmax));
  print(paste("Min of x=",xmin));
  print(paste("Mean of x=",xmean));
  print(paste("Median of x=",xmed));
  print(paste("Standard deviation of x=",xsd));
  
  print("b) x before sorting");
  print(x);
  x = sort(x);
  print("x after sorting");
  print(x);
  
  print("c) x before standardization");
  print(x);
  x = (x-xmean)/xsd;
  print("x after standardization");
  print(x);
  
}