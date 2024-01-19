# speed = 10

# seed = 1
# seed = seed+1

X11(width=14, height=7)
# X11(width=10, height=5)

set.seed(2)
µ = 1
n = 4

sleep = T

x = sort(rnorm(n))
xlim = max(c(3,abs(x)))
xlim = c(-xlim, xlim)

xP = seq(xlim[1],xlim[2],length = 500)
P = dnorm(xP,mean = µ,sd = 1)
L = dnorm(x,mean = µ,sd = 1)

xL = seq(xlim[1]*0.7,xlim[2]*0.7,length = 500)
Lfunc = sapply(xL,function(mu)prod(dnorm(x,mean = mu,sd = 1)))

par(mfrow=c(1,2))
plot(xP, P, type='l', ylim=c(0,max(P)), xlab = 'x', ylab = "Densité de probabilité",col=2
     , main = paste0('Calule de la vraisemblance pour le modèle : P(X=x|N(µ=',µ,',σ=1))')
     , sub = 'Ceci est une densité de probabilité (somme=1)'
)
legend(x = 'topleft',lty=c(NA,1),pch=c(1,NA),legend = c('données',paste0('model : N(µ=',µ,',σ=1)')),col=c(1,2),bty='n')
points(x,rep(0,n))

if(sleep){Sys.sleep(3/speed)}

for(i in 1:n){
  segments(x0 = x[i],x1 = x[i],y0 = 0,y1 = dnorm(x[i],mean = µ,sd = 1),lty = 2)
  arrows(  x0 = x[i],x1 = xlim[1],y0 = L[i],y1 = L[i],lty = 2, length = 0.1)
  text(x = xlim[1], y = L[i]-max(P)/30,labels = round(L[i],2),adj=0)
  if(sleep){Sys.sleep(1/speed)}
}

mtext(paste0('L = ',paste(round(L,2),collapse = ' x ')," = ",signif(prod(L),2))
     ,side = 3,line = 0.25,cex = 1.5)

if(sleep){Sys.sleep(3/speed)}
plot(xL, Lfunc, type='n', ylim=c(0,max(Lfunc)*1.15), xlab = 'µ', ylab = "Vraissemblance", main = 'Fonction de vraissemblance', sub = "Ceci n'est PAS une densité de probabilité (somme!=1)")
mtext(paste0('Calule de la vraisemblance pour tous les modèles : N(µ= -2 à 2 , σ=1)')
      ,side = 3,line = 0.25,cex = 1
      )


points(µ,prod(L),col=2, pch=19)
segments(x0 = µ,x1 = µ,y0 = 0,y1 = prod(L),lty = 2)
arrows(  x0 = µ,x1 = min(xL),y0 = prod(L),y1 = prod(L),lty = 2, length = 0.1)
text(x = µ-diff(xlim)/100, y = 0,labels = paste0('µ=',µ), adj=1)
text(x = min(xL), y = prod(L)+max(Lfunc)/30,labels = signif(prod(L),2), adj=0,col=2)

if(sleep){Sys.sleep(1.5/speed)}
for(i in round(seq(1,length(xL),length = 30))){
  points(xL[i], Lfunc[i])
  if(sleep){Sys.sleep(0.2/speed)}
}
points(xL, Lfunc, type='l')
if(sleep){Sys.sleep(3/speed)}

arrows(  x0 = xL[which.max(Lfunc)],x1 = xL[which.max(Lfunc)],y0 = max(Lfunc),y1 = 0,lty = 1, length = 0.2, col='darkgreen',lwd=2)
text(x = xL[which.max(Lfunc)]+diff(xlim)/50, y = 0+max(Lfunc)/20,labels = paste0('MLE\n=',signif(xL[which.max(Lfunc)],1)),adj=0, col='darkgreen')

legend(x = 'topleft',legend = c('MLE : Maximum Likelihood Estimate'),text.col=c("darkgreen"),bty='n')

if(sleep){Sys.sleep(1.5/speed)}

LfuncTab = cbind.data.frame('xL'=xL,'Lfunc'=Lfunc,'i'=1:length(Lfunc))
LfuncTab$cumSum = cumsum(LfuncTab$Lfunc) / sum(LfuncTab$Lfunc)

CI = c( LfuncTab$xL[which.min(abs(LfuncTab$cumSum-0.025))] , LfuncTab$xL[which.min(abs(LfuncTab$cumSum-0.975))] )
x = xL[   xL> CI[1] & xL < CI[2]]
y = Lfunc[xL> CI[1] & xL < CI[2]]

polygon(x = c(x,rev(x)), y = c(y,rep(0,length(x))),border = NA,col = "orange")

points(µ,prod(L),col=2, pch=19)
segments(x0 = µ,x1 = µ,y0 = 0,y1 = prod(L),lty = 2)
arrows(  x0 = µ,x1 = min(xL),y0 = prod(L),y1 = prod(L),lty = 2, length = 0.1)
text(x = µ-diff(xlim)/100, y = 0,labels = paste0('µ=',µ), adj=1)
text(x = min(xL), y = prod(L)+max(Lfunc)/30,labels = signif(prod(L),2), adj=0,col=2)

i=round(seq(1,length(xL),length = 30))
points(xL[i], Lfunc[i])
points(xL, Lfunc, type='l')

arrows(  x0 = xL[which.max(Lfunc)],x1 = xL[which.max(Lfunc)],y0 = max(Lfunc),y1 = 0,lty = 1, length = 0.2, col='darkgreen',lwd=2)
text(x = xL[which.max(Lfunc)]+diff(xlim)/50, y = 0+max(Lfunc)/20,labels = paste0('MLE\n=',signif(xL[which.max(Lfunc)],1)),adj=0, col='darkgreen')

legend(x = 'topleft',border = NA,fill=c(NA,"orange"),legend = c('MLE : Maximum Likelihood Estimate', "IC à 95% basé sur le maximum de vraisemblance"),text.col=c("darkgreen", "black"),bg = 'white')

# print(sum(LfuncTab$Lfunc[LfuncTab$xL > CI[1] & LfuncTab$xL < CI[2]]) / sum(LfuncTab$Lfunc))
# print(seed)

