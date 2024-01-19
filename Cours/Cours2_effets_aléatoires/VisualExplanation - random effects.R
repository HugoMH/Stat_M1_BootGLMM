
ToInstall = 'latex2exp' ; if(!any(rownames(installed.packages())==ToInstall)){install.packages(ToInstall)}
library(latex2exp)

N= 30

Expect = rnorm(N,0,0.2)
Obs    = rnorm(N,Expect,0.1)

i = Expect > -1 & Expect < 1 & Obs > -1 & Obs < 1

Expect = Expect[i]
Obs    = Obs[   i]

par(mar=c(0.05,2,0.05,0.05))
plot(c(1, length(Obs)), c(-1, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n')
abline(h=0)
points(Expect, pch=19)
points(Obs, pch=1)
segments(x0 = 1:length(Obs)
        ,x1 = 1:length(Obs)
        ,y0 = Expect
        ,y1 = Obs
        ,lty = 3, col='grey')

legend(x = "bottomright", legend = c('Moyenne observée',TeX("Moyenne 'vraie' : $E(\\circ)=\\bullet$")                                     
,TeX('$\\circ = \\bullet + e$') # the latex expression with \\ instead of \
,TeX('$var(\\circ) = var(\\bullet) + var(e)$') # the latex expression with \\ instead of \
                                     ),pch=c(1,19,NA,NA),cex=1.25)


             
