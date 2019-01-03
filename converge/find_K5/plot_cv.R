rm( list=ls() )

n.folds	<- 5
Kmax		<- 8
type		<- 'base'

lls	<- array( NA, dim=c( Kmax, n.folds ) )
for( K in 1:Kmax ){
	savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
	if( ! file.exists( savefile ) ) next
	load( savefile )
	lls	[K,]	<- out[,1]
	rm( out )
}
for( K in Kmax:1 )
	lls	[K,]	<- lls[K,]-lls[1,]

pdf( paste0( '~/figs/mfmr/converge/choose_K_', type, '.pdf' ), width=12, height=7 )
layout( matrix(1:2,1,2), width=c(11,4) )
par( mar=c(5,7,1,1) )

plot( c(1,Kmax), range( lls ), type='n', xlab='# of Subtypes (K)', ylab='', main='', axes=F, cex.lab=1.8, cex.main=1.8 )
mtext( side=2, cex=2, line=4, 'Log-Likelihood Increase over K=1' )
axis(2)
axis(1,at=1:Kmax)

for( K in 1:Kmax )
	points( rep( K, n.folds ), lls[K,], pch=16 )
for( i in 1:n.folds )
	lines( 1:Kmax, lls[,i], pch=16 )

avll	<- apply( lls, 1, mean )
sdll	<- apply( lls, 1, function(x) 1/sqrt(length(x))*sd(x) )*2

maxsd	<- sdll[which.max(avll)]

points( 1:K, avll, pch=16, cex=3, col=2 )
lines(	1:K, avll, lwd=4, col=2 )
points( which.max(avll), max(avll), pch=16, cex=4, col=3 )
kstar	<- min(which(avll>max(avll)-maxsd))

par( mar=c(5,0,1,0) )
plot.new()
legend( 'center', col=c( 1, 2, 3 ), lty=c(1,1,1), pch=c(16,16,16), leg=c('All folds', 'Fold Avg.', 'Maximizer' ), cex=2.1, bty='n', y.intersp=1.5 )

dev.off()
