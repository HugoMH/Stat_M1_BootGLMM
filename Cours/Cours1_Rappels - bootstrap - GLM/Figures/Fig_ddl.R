load('./Figures/Fig_ddl2.RData')
par(mar=c(3, 3, 0.5, 0.5)) # c(bottom, left, top, right)
  plot(x,y,type="n",xlab="",ylab = '',ylim = c(0,10.25))
  mtext("x",side = 1,line = 2,cex=1.4)
  mtext("y",side = 2,line = 2,cex=1.4)
  abline(5,pi)
  abline(lm(y[1:2]~x[1:2]),col=2)
  m3 = lm(y[1:3]~x[1:3])
  abline(m3,col=3)
  # segments(x0 = x[1:3],x1 = x[1:3],y0 = y[1:3], y1 = coef(m3)[1]+coef(m3)[2]*x[1:3],col='darkgreen',lty=3)
  points(x[1:2],y[1:2],col=2, pch=19,cex=2)
  text(x,y,1:n, col=c('darkgreen','darkgreen','darkgreen',rep('grey50',n-3)), cex=c(1.3,1.3,1.3,rep(0.75,n-2)))

  legend("bottomright"
         ,legend = c('les points 1 à 2', 'les points 1 à 3',"'Vérité'")
         ,lty=1,col=c(2,3,1)
         ,title = 'régression sur'
         ,bg = 'white'
         ,cex=0.9)
 
