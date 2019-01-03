rm( list=ls() )
library( rgwas )
library( parallel )

mc.cores	<- as.numeric( commandArgs(TRUE)[[1]] )
K					<- as.numeric( commandArgs(TRUE)[[2]] )

types	<- 'pc6'
for( type in types ){

	savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_', mc.cores, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	sink(	sinkfile )

	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
	
	if( K > 1 ){

	load( paste0( 'Rdata/rerun_mfmr_', type,'_', K, '.Rdata' )  )
	print( names( out ) )
	z_baseline	<- apply( out$pmat, 1, which.max )
	print( table( z_baseline ) )
	rm( out )

	runtime	<- system.time({
		out	<- score_K( Yb, Y, cbind( 1, G ), X=NULL, K, n.folds=5, mc.cores=mc.cores, trace=T, nrun=10, z_baseline = z_baseline )
	})[3]

	} else {
	runtime	<- system.time({
		out	<- score_K( Yb, Y, cbind( 1, G ), X=NULL, K, n.folds=5, mc.cores=mc.cores, trace=T, nrun=10 )
	})[3]
	}
	save( out, runtime, file=savefile )
	rm( out, runtime, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()

}
