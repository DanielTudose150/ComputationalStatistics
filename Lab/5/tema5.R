forwards = function(dataset)
{
  dataNames = names(dataset);
  dataNames = dataNames[-1];
  dataMatrix = data.matrix(dataset);
  y = dataMatrix[,1];
  xs = dataMatrix[,-1];
  alfa = 0.05;
  solution = c();
  columns = length(dataset[1,]) - 1;
  rows = length(dataset[,1]);
  
  variables = seq(1,columns);
  available = variables;

  solutionMat = matrix(nrow=rows, ncol=0);

  
  for(i in variables)
  {
    best = 0;
    bestCoefficient = 10e+6;
    for(a in available)
    {
      current = c(solution, a);
      
      matAux = solutionMat;
      matAux = cbind(matAux, xs[,a]);
      
      g = lm(y~matAux);
      r = summary(g);
      
      t = tail(r$coefficients, n=1);
      p = t[4];
      if(p < bestCoefficient)
      {
        bestCoefficient = p;
        best = a;
      }
    }

    if(bestCoefficient > alfa)
      break;
    solution = c(solution, best);
    solutionMat = cbind(solutionMat, xs[,best]);
    for(j in seq(1,length(available)))
    {
      if(available[j] == best)
      {
        available = available[-j];
        break;
      }
    }
  }
  

  nume = c();
  for(n in solution)
  {
    nume = c(nume, dataNames[n]);
  }
  print(nume);
}


backwards = function(dataset)
{
  dataNames = names(dataset);
  dataNames = dataNames[-1];
  dataMatrix = data.matrix(dataset);
  y = dataMatrix[,1];
  xs = dataMatrix[,-1];
  alfa = 0.05;
  
  columns = length(dataset[1,]) - 1;
  rows = length(dataset[,1]);
  
  variables = seq(1,columns);
  
  solution = variables;
  solutionMat = xs;
  
  for(i in variables)
  {
    g = lm(y~solutionMat);
    r = summary(g);
    ps = r$coefficients[,4];
    ps = ps[-1];
    
    worst = 0;
    worstCoefficient = -1;
    
    for(a in seq(1, length(solution)))
    {

      if(ps[a] > worstCoefficient)
      {
        worstCoefficient = ps[a];
        worst = a;
      }
    }
    if(worstCoefficient < alfa)
    {
      break;
    }
    
    for(j in seq(1,length(solution)))
      if(j == worst)
      {
        solution = solution[-j];
        solutionMat = solutionMat[,-j];
        break;
      }
  }
  
  nume = c();
  for(n in solution)
  {
    nume = c(nume, dataNames[n]);
  }
  print(nume);
}


stepwise = function(dataset)
{
  dataNames = names(dataset);
  dataNames = dataNames[-1];
  dataMatrix = data.matrix(dataset);
  y = dataMatrix[,1];
  xs = dataMatrix[,-1];
  alfa = 0.05;
  solution = c();
  columns = length(dataset[1,]) - 1;
  rows = length(dataset[,1]);
  
  variables = seq(1,columns);
  available = variables;
  
  solutionMat = matrix(nrow=rows, ncol=0);
  
  best = 0;
  bestCoefficient = 10e+6;
  for(i in variables)
  {
    matAux = xs[,i];
    g = lm(y~matAux);
    r = summary(g);
    
    t = tail(r$coefficients, n=1);
    p = t[4];
    if(p < bestCoefficient)
    {
      bestCoefficient = p;
      best = i;
    }
  }
  
  if(bestCoefficient > alfa)
    return(-1);
  
  solution = c(solution, best);
  solutionMat = cbind(solutionMat, xs[,best]);
  
  available = available[-best];
  
  while (T) {
    g = lm(y~solutionMat);
    r = summary(g);
    ps = r$coefficients[,4];
    ps = ps[-1];
    
    worst = 0;
    worstCoefficient = -1;
    
    for(a in seq(1, length(solution)))
    {
      
      if(ps[a] > worstCoefficient)
      {
        worstCoefficient = ps[a];
        worst = a;
      }
    }
    
    # drop most non-significant one
    if(worstCoefficient > alfa)
    {
      for(j in seq(1,length(solution)))
        if(j == worst)
        {
          available = c(available, solution[j]);
          solution = solution[-j];
          solutionMat = solutionMat[,-j];
          break;
        }
      next;
    }
    
    # check for improvement
    best = 0;
    bestCoefficient = 10e+6;
    for(a in seq(1,length(available)))
    {
      current = c(solution, available[a]);
      matAux = solutionMat;
      matAux = cbind(matAux, xs[,available[a]]);
      
      g = lm(y~matAux);
      r = summary(g);
      
      p = tail(r$coefficients[,4],n=1);
      if(p<bestCoefficient)
      {
        bestCoefficient = p;
        best = a;
      }
    }
    
    if(bestCoefficient > alfa)
      break;
    
    solution = c(solution, available[best]);
    solutionMat = cbind(solutionMat, xs[,available[best]]);
    available = available[-best];
  }
  
  nume = c();
  for(n in solution)
  {
    nume = c(nume, dataNames[n]);
  }
  print(nume);
  
}


ex2 = function(dataset) 
{
  fws = forwards(dataset);
  bws = backwards(dataset);
  sws = stepwise(dataset);
  
  
}


