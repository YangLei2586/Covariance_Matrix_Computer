# define the simple function first

covariance_manual <- function(x,mv, sample=T) {
  # This section will do X - X_bar
  nrow <- nrow(x)
  ncol <- ncol(x)
  for (i in 1:nrow) {  
    for (j in 1:ncol) {
      x[i,j] = x[i,j] - mv[j];
    }
  }  
  cv<-t(x) %*% x
  if (sample == TRUE) {
    cv<-cv/(nrow-1)
  } else { 
    cv<-cv/(nrow)
  }
  return(cv);
} 



# Sample Usage for Sample Covariance  (Sigma hat)

X<-matrix(c(4,16,8,7,5,12,9,11), ncol=2, nrow=4, byrow=T)
X_bar <- colMeans(X)
S_hat <- covariance_manual(X, X_bar, F)

S_hat

# Sample Usage for Population Covariance , Sigma _hat_zero

X<-matrix(c(4,16,8,7,5,12,9,11), ncol=2, nrow=4, byrow=T)
Mu_bar <- matrix(c(7,11), nrow=2)
S_hat <- covariance_manual(X, Mu_bar, F)

S_hat