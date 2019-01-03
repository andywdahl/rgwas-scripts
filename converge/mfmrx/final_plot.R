rm( list=ls() )
load( '../parse_converge_data/parsed_data/G_PCs.Rdata' )
source( 'final_plot_fxn.R' )

type	<- 'base'
for( K in 2 )
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

	betas	<- t(apply( betas, 1, function(x) -x*sign(x[1]) ))

	final_plot_fxn( paste0( '~/figs/mfmr/converge/final_', K, '_', type, '.pdf' ), pvals, npvals, betas, ses, 5, K, pvec=rev(pvec), rsids=rsids )

})
