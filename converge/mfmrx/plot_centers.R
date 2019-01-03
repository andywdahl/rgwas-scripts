rm( list=ls() )
library(Hmisc)

Ynames	<- c( 'Total MD Episodes', 'Neuroticism', 'Family History', 'Cold Mother', 'Authoritarian Mother', 'Protective Mother', 'Cold Father', 'Authoritarian Father', 'Protective Father','Pre-Menstrual MD', 'Height', 'BMI', 'Mitochondrial DNA', 'Telomere Length' )
Ybnames	<- c( 'Major Depression', 'Melancholia', "Panic Disorder", 'Anxiety Disorder', "Dysthymia", "Postnatal Depression",
	"CSA", "Stress", "Father MD", "Mother MD", 'Agoraphobia', 'Social Phobia', 'Animal Phobia', 'Situational Phobia', 'Blood Phobia',
	'Close Family Death', 'Divorced/Separated', 'Unemployed', 'Fired', 'Finanical Crisis', 'Legal Probems', 'Serious Illness',
	'Serious Accident', 'Natural Disaster', 'Witnessed Violence', 'Raped', 'Physically Attacked', 'Abused as Child ', 'Neglected as Child', 'Violently Threatened', 'Other Terrible Events', 'Widowed' )
Ybnames1	<- Ybnames[ -which( Ybnames == 'Stress' ) ]

type	<- 'base'
	for( K in 2 )
try({

	savefile	<-	paste0( '../mfmrx/Rdata/', type,'_', K, '.Rdata' )
	if( ! file.exists( savefile ) )	next
	load( savefile )
	print( paste( type, K ) )

	load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )
	P			<- ncol(Y)
	if( type %in% c( 'nostress', 'drop_miss_nostress' ) ){
		colnames(Yb)	<- Ybnames1
	} else {
		colnames(Yb)	<- Ybnames
	}
	colnames(Y)		<- Ynames

	pmat	<-  out_mfmr$pmat
	#if( type %in% c( 'drop_miss' ) )
	pmat	<- pmat[,2:1]

	ids		<- apply( pmat, 1, function(x){
		kmax	<- which.max( x )
		ifelse( x[kmax] < .95, NA, kmax )
	})
	print( table( ids ) )
	print( sum( is.na( ids ) ) )

	pdf( paste0( '~/figs/mfmr/converge/centers_', type, '_', K, '.pdf' ), width=22, height=7 )
	layout( matrix( 1:2, 1, 2 ), widths=c( 7.0, 12 ) )

	centroids_q <- t(sapply( 1:K, function(k) colMeans( ( pmat[,k] %o% rep( 1, ncol(Y) ) ) * Y/mean(pmat[,k]), na.rm=T )))
	centroids_b <- t(sapply( 1:K, function(k) colMeans( ( pmat[,k] %o% rep( 1, ncol(Yb)) ) * Yb/mean(pmat[,k]), na.rm=T )))*100
	colnames(centroids_b) <- NULL
	rownames(centroids_b) <- NULL

	#### quant traits
	par( mar=c( 16, 6.3, 3, .3 ) )
	plot( c(1-.7,P+.7), c( -1.6, 2.9 ), type='n', ylab='', xlab='', main='', axes=F )
	axis(2,cex.axis=1.7,padj=.2)
	mtext( side=1, at=1:P, text=colnames(Y), las=2, line=1, cex=1.8 )
	mtext( side=2, line=4, cex=2.0 , 'Cluster Means and 90% IQR' )

	mtext( side=3, line=0, at=1.3, 'a', cex=4.2 )

	for( k in 1:K )
		for( p in 1:P )
	{
		x0	<- seq(-.15,.15,len=K)[k]+p
		points( x0, centroids_q[k,p], col=k, pch=16, cex=3.2 )
    qs  <- wtd.quantile( Y[,p], weights=pmat[,k], probs=c(.1,.9), na.rm=T )

		lines(	rep( x0, 2 )	, qs				, col=k, lwd=3.1, lty=1 )
		lines(	x0+c(-.07,.07), qs[c(1,1)], col=k, lwd=2.5, lty=1 )
		lines(	x0+c(-.07,.07), qs[c(2,2)], col=k, lwd=2.5, lty=1 )
	}
	axis(4,at=c(-1000,1000),labels=NA,line=.6,lwd=10,lty=2,col='lightgrey')


	#### bin traits
	par( mar=c( 16, .3, 3, 6.3 ) )
	mp	<- barplot( centroids_b, beside=T, col=1:K, main='', xlab='', ylab='', axes=F, ylim=c(0,100) )
	axis(4,cex.axis=1.7,padj=.2)
	mtext( side=1, at=colMeans( mp ), text=colnames(Yb), las=2, line=1, cex=1.8 )
	mtext( side=4, line=4, cex=2.0 , 'Prevalence (%)' )

	mtext( side=3, line=0, at=3.5, 'b', cex=4.2 )


	dev.off()
})
