ex = function()
{
  house = read.table("house.dat",header=T);
  RSS = c();
  R2 = c();
  R2a = c();
  Cp = c();
  
  combinationRSS = c();
  combinationR2 = c();
  combinationR2a = c();
  combinationCp = c();
  
  houseNames = names(house);
  houseMatrix = data.matrix(house);
  g = lm(houseMatrix[,1] ~ houseMatrix[,-1]);
  r = summary(g);
  fullRSS = deviance(g);
  m = length(houseMatrix[,1]);
  n = length(houseMatrix[1,]) - 1;
  s2 = fullRSS / (m-n-1);
  
  
  for(p in 1:13)
  {
    result = solveForP(p, house, houseNames, s2, m);
    RSS[p] = result[1];
    combinationRSS[p] = result[2];
    R2[p] = result[3];
    combinationR2[p] = result[4];
    R2a[p] = result[5];
    combinationR2a[p] = result[6];
    Cp[p] = result[7];
    combinationCp[p] = result[8];
  }
  Cp = matrix(Cp,ncol=length(Cp));
  a = unlist(Cp[1,]);
  b = seq(2,14);
  c = abs(a-b);
  Cp = matrix(c,ncol=length(c));
  plot(1:13, RSS,xlab = "p");
  plot(1:13, R2,xlab = "p");
  plot(1:13, R2a,xlab = "p");
  plot(1:13, Cp,xlab = "p");
  
  n
  plot(1:13, RSS,xlab = "p");
  plot(1:13, R2,xlab = "p");
  plot(1:13, R2a,xlab = "p");
  plot(1:13, Cp,xlab = "p");
  
  par(mfrow=c(1,1));
  
  print("RSS");
  print(matrix(RSS,ncol=length(RSS)));
  print(combinationRSS);
  
  print("R2");
  print(matrix(R2,ncol=length(R2)));
  print(combinationR2);
  
  print("R2a");
  print(matrix(R2a,ncol=length(R2a)));
  print(combinationR2a);
  
  print("Cp");
  print(matrix(Cp,ncol=length(Cp)));
  print(combinationCp);
  
  
}

solveForP = function(p, house, houseNames, s2,m)
{
  RSS = 2e+306;
  R2 = 0;
  R2a = 2e-306;
  Cp = 2e+306;
  
  combinationRSS = c();
  combinationR2 = c();
  combinationR2a = c();
  combinationCp = c();
  
  aux = c();
  combinations = combn(2:14,p);
  for(i in 1:dim(combinations)[2])
  {
    matAux = matrix(house[,1],nrow=26);
    for(j in 1:p)
    {
      aux[j] = houseNames[combinations[,i][j]];
      matAux = cbind(matAux, house[,combinations[,i][j]]);
    }
    
    g = lm(matAux[,1]~matAux[,-1]);
    r = summary(g);
    
    currRSS = deviance(g);
    currR2 = r$r.squared;
    currR2a = r$adj.r.squared;
    currCp = currRSS / s2 - (m - 2 * (p+1));
    
    if(currRSS < RSS)
    {
      RSS = currRSS;
      combinationRSS = aux;
    }
    
    if(currR2 > R2)
    {
      R2 = currR2;
      combinationR2 = aux;
    }
    
    if(currR2a > R2a)
    {
      R2a = currR2a;
      combinationR2a = aux;
    }
    
    if(abs((p+1)-currCp) < abs((p+1)-Cp))
    {
      Cp = currCp;
      combinationCp = aux;
    }
  }
  
  return(list(RSS, combinationRSS, R2, combinationR2, R2a, combinationR2a, Cp, combinationCp));
}
