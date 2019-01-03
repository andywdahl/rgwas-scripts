rm( list=ls() )
load( '../parse_converge_data/parsed_data/G_PCs.Rdata' )

type	<- 'base'
for( K in 2:3 )
try({

	savefile	<-	paste0( 'Rdata/', type,'_', K, '_test.Rdata' )
	if( ! file.exists( savefile ) )	next
	load( savefile )
	load( paste0( 'Rdata/', type,'_', K, '.Rdata' ) )

	load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )
	print( paste( type, K ) )

	pvals		<- sapply( out_test, function(x) x$pval )
	rownames(pvals)	<- c( 'hom', 'het', 'hom+het' )

	npvals<- pvals
	pvals	<- apply( pvals, 1:2, function(x) sprintf( '%5.1e', x ) )


	pvec	<- out_mfmr$out$pvec

	betas	<- t(sapply( out_test, function(x) rev( summary(x$fit)$coef[,1] )[1:K] ))
	ses		<- t(sapply( out_test, function(x) rev( summary(x$fit)$coef[,2] )[1:K] ))

	if( ! type %in% c( 'drop_miss' ) ){
		pvec	<- pvec	[2:1]
		betas	<- betas[,2:1]
		ses		<- ses	[,2:1]
	}

	betas	<- t(apply( betas, 1, function(x) -x*sign(x[1]) ))

	pdf( paste0( '~/figs/mfmr/converge/leg_', K, '_', type, '.pdf' ), width=13, height=3.5 )

	layout( matrix( 1:2, 1, 2 ), widths=c(5,2.1) )
	par( mar=c(0,27.5,0,0) )
	pie(pvec,col=1:K,lab='')

	par( mar=c(0,0,0,2) )
	plot.new()
	legend( 'center', bty='n', fill=1:2, leg=c( 'No stress', 'Stress' ), cex=3.2 )

	dev.off()



})

