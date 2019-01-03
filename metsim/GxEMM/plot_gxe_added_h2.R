rm( list=ls() )
load( '../Rdata/setup.Rdata' )

for( K0 in 3 ){

	allout	<- array( NA, dim=c( 2, np ), dimnames=list( c( 'G', 'GxE' ), phens ) )
	for( phen in phens )try({

		savefile	<-	paste0( 'Rdata/iid/', phen, '.Rdata' )
		if( ! file.exists( savefile ) )	next
		load( 				paste0( 'Rdata/hom/', phen, '.Rdata' ) )
		load( savefile )

		allout[1,phen]	<- out_hom$h2
		allout[2,phen]	<- sum(out_iid$h2)
		rm( out_iid, out_hom )
	})
	allout	<- allout * 100

	pdf( paste0( '../figs/added_h2_', K0, '.pdf' ), width=10, height=6 )
	layout( matrix( 1:2, 1, 2 ), width=c(6,4.5) )

	par( mar=c(5,5,1,1) )
	print( range( allout, na.rm=T ) )
	plot( 100*c(0,.65), 100*c(0,.65), type='n', xlab=expression( h[hom]^2 ), ylab=expression( h[GxE+hom]^2 ), cex.lab=1.4, axes=F )
	box()
	mytick	<- (1:4 - 1)*20
	axis( 1, at=mytick, lab=paste0( mytick, '%' ) )
	axis( 2, at=mytick, lab=paste0( mytick, '%' ) )
	abline( a=0, b=1, col=2 )
	text( allout[1,], allout[2,], lab=xnames, cex=.6 )

	par( mar=c(5,5,0,0) )

	hist( -allout[1,]+allout[2,], breaks=6, xlab='Heritability increase from IID GxEMM (%)', main='', ylab='# of Traits' )
	print( mean( allout[1,] ) )
	print( mean( allout[2,] ) )

	dev.off()
}
