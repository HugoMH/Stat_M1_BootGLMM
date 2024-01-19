ToInstall = "lme4" ; if(!any(rownames(installed.packages())==ToInstall)){install.packages(ToInstall)} # String manipulations

#### Intercept --------
par(mfrow=c(1,2))
n_c = 6 ; set.seed(15) ; x = rep(0:9,n_c) ; col = rep(1:n_c+1, each=10)
µ_ = rnorm(n_c) ; µ_=µ_-mean(µ_) ; y = unlist(lapply(µ_,function(µ)rnorm(10,mean = µ, sd = 0.15))) + x*0.25 + pi
M = lme4::lmer(y ~ x + (1|col))
Fix = lme4::fixef(M)
Ran = lme4::ranef(M)[[1]][,1]
plot(x,y, col=col, main = "Un effet aléatoire sur l'ordonnée à l'origine")  ;  abline(Fix, lty=2, lwd=2)
for(i in 1:n_c){    abline(a = Ran[i] + Fix[1] , b = rep(Fix[2],n_c), col=i+1)   }


n_c = 6 ; set.seed(15) ; x = rep(0:9,n_c) ; col = rep(1:n_c+1, each=10)
µ_ = rnorm(n_c) ; µ_=µ_-mean(µ_) ; y = unlist(lapply(µ_,function(µ)rnorm(10,mean = µ, sd = 0.85))) + x*0.25 + pi
M = lme4::lmer(y ~ x + (1|col))
Fix = lme4::fixef(M)
Ran = lme4::ranef(M)[[1]][,1]
m = lm(y ~ x + as.factor(col)) ; s = c(coef(m)[1], coef(m)[1] + coef(m)[-c(1:2)])
plot(x,y, col=col, main = "Effet fixe vs aléatoire")  ;  abline(Fix, lty=2, lwd=2)
for(i in 1:n_c){    abline(a = Ran[i] + Fix[1] , b = rep(Fix[2],n_c), col=i+1)
  w=which(col==(i+1))
  abline( a = s[i]
          , b=rep(Fix[2],n_c) , col=i+1, lty=2) }