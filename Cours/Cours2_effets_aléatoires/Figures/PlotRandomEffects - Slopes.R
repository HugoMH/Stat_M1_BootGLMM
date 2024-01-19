ToInstall = "lme4" ; if(!any(rownames(installed.packages())==ToInstall)){install.packages(ToInstall)} # String manipulations

#### slope --------
par(mfrow=c(1,2))
n_c = 6 ; set.seed(15) ; x = rep(0:9,n_c) ; col = rep(1:n_c+1, each=10)
µ_ = rnorm(n_c,mean = 1) ; µ_=µ_-mean(µ_)+1 ; y = unlist(lapply(µ_,function(µ)rnorm(10,mean = 0, sd = 0.1) + x[1:10]*µ + pi))
M = lme4::lmer(y ~ x + (0+x|col))
Fix = lme4::fixef(M)
Ran = lme4::ranef(M)[[1]][,1]
plot(x,y, col=col, main = "Un effet aléatoire sur la pente")  ;  abline(Fix, lty=2, lwd=2)
for(i in 1:n_c){    abline(a = Fix[1] , b = Fix[2] + Ran[i] , col=i+1)   }


n_c = 6 ; set.seed(15) ; x = rep(0:9,n_c) ; col = rep(1:n_c+1, each=10)
µ_ = rnorm(n_c,mean = 1) ; µ_=µ_-mean(µ_)+1 ; y = unlist(lapply(µ_,function(µ)rnorm(10,mean = 0, sd = 3.85) + x[1:10]*µ + pi))
M = lme4::lmer(y ~ x + (0+x|col))
Fix = lme4::fixef(M)
Ran = lme4::ranef(M)[[1]][,1]
m = lm(y ~ x + x:as.factor(col)) ; s = c(coef(m)[2], coef(m)[2] + coef(m)[-c(1:2)])
plot(x,y, col=col, main = "Un effet aléatoire sur la pente")  ;  abline(Fix, lty=2, lwd=2)
for(i in 1:n_c){    abline(a = Fix[1] , b = Fix[2] + Ran[i] , col=i+1) 
  w=which(col==(i+1))
  abline( a = rep(coef(m)[1],n_c)
          , b = s[i] , col=i+1, lty=2)
}
