# Take home problem
# Ben Callaghan
# Stat 540

# Flip a coin n times with probability p of heads
n <- 10
p <- 0.5 
flip <- rbinom(n,1,p)
(flip.prop <- sum(flip)/length(flip))



# Flip a coin n times with probability p of heads
# for tr trials
flip.df <- flipper(100,0.55,200)

flipper <- function(n,p,tr){
  flip.df <- data.frame(matrix(nrow = tr, ncol = n))
  for (i in seq(1:tr)){
    flip <- rbinom(n,1,p)
    flip.df[i,] <- flip
  }
  
  
  trial.prop <- mean(apply(flip.df/n,1,sum))
  print("Mean of proportions: ")
  print((trial.prop))
  trial.var <- var(apply(flip.df/n,1,sum))
  print("Variation across trials: ")
  print(trial.var)
  flip.df<- flip.df
}


