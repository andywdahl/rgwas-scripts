rm( list=ls() )
library( rgwas )

type	<- 'base'
for( K in 2:5 ){

	loadfile	<- paste0( 'Rdata/', type,'_', K, '.Rdata' )
	if( ! file.exists( loadfile ) )	next
	load(	loadfile )

	savefile	<-	paste0( 'Rdata/', type,'_', K, '_test.Rdata' )
	if( file.exists( savefile ) )	next

	load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )
	Gxpmat	<- t(sapply( 1:nrow(G), function(i) G[i,] %x% out_mfmr$pmat[i,] ))
	pmat    <- out_mfmr$pmat[,-K,drop=F]
	rm( out_mfmr, G, Y )

	out_test	<- lapply( 1:ncol(G_snp), function(i){
		interxn_test( X=cbind( pmat, Gxpmat ), y=Yb[,1], g=G_snp[,i], pmat=pmat, bin=T )
	})

	save( out_test, file=savefile )
	rm( out_test, Yb, Gxpmat, pmat )

	print(warnings())
	print('Done')
	sink()
}
