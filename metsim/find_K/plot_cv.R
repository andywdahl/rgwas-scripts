rm( list=ls() )

Kmax		<- 8
n.folds	<- 5
types		<- 'pc6'

pdf( paste0( '../figs/choose_K_final.pdf' ), width=12, height=7 )
layout( matrix(1:2,1,2), width=c(11,4) )

for( type in types )
	for( jj in 1:3 )
try({

	lls	<- array( NA, dim=c( Kmax, n.folds ) )
	for( K in 1:Kmax ){
		savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
		if( ! file.exists( savefile ) ) next
		load( savefile )
		lls	[K,]	<- out[,jj]
		rm( out )
	}
	if( jj == 1 ){
		ylab	<- 'Log-Likelihood Increase over K=1'
		for( K in Kmax:1 )
			lls	[K,]	<- lls[K,]-lls[1,]
		ylim	<- range( lls )
	} else if( jj == 2 ){
		ylab	<- 'Entropy of clusters'
		lls	[1,]	<- 0
		ylim	<- range( lls )
	} else {
		ylab	<- 'Clustering accuracy'
		ylim	<- 0:1
		lls	[1,]	<- 0
	}

	par( mar=c(5,7,1,1) )
	plot( c(1,Kmax), ylim, type='n', xlab='# of Subtypes (K)', ylab='', main='', axes=F, cex.lab=1.8, cex.main=1.8 )
	mtext( side=2, cex=2, line=4, ylab )
	axis(2)
	axis(1,at=1:Kmax)

	for( K in 1:Kmax )
		points( rep( K, n.folds ), lls[K,], pch=16 )
	for( i in 1:n.folds )
		lines( 1:Kmax, lls[,i], pch=16 )

	avll	<- apply( lls, 1, mean )
	print( lls )

	points( 1:K, avll, pch=16, cex=3, col=2 )
	lines(	1:K, avll, lwd=4, col=2 )
	points( which.max(avll), max(avll), pch=16, cex=4, col=3 )

	par( mar=c(5,0,1,0) )
	plot.new()
	legend( 'center', col=c( 1, 2, 3 ), lty=c(1,1,1), pch=c(16,16,16), leg=c('All folds', 'Fold Avg.', 'Maximizer' ), cex=2.1, bty='n', y.intersp=1.5 )

})
dev.off()
