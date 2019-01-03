rm( list=ls() )

Kmax	<- 4
types	<- 'pc6'
allps	<- matrix( NA, Kmax, 16 )
rownames( allps )	<- paste0( 'K=', 1:Kmax )
for( suff in c( '', '_raw', '_dropT2D' ) )
	for( type in types )
{
	for( K in 2:Kmax )
	tryCatch({

		savefile<- paste0( 'Rdata/', type,'_', K, '_test', suff, '.Rdata' )
		if( ! file.exists( savefile ) )	next
		load( savefile )
		load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
		colnames(allps)	<- names( out_test )

		load( paste0( 'Rdata/', type,'_', K, '.Rdata' ) )
		allps[K,]	<- sapply( out_test, function( out ) out$pvals['Het'] )
	})

	write.csv( allps[-1,], file=paste0( '../figs/statin_pvals', suff, '.csv' ), quote=F )
	print( t(format( allps[-1,], scientific=T, digits=2 ) ))
}
