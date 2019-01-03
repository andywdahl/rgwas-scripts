rm( list=ls() )
library( mfmr )
library( parallel )

mc.cores<- as.numeric( commandArgs(TRUE)[[1]] )
K				<- as.numeric( commandArgs(TRUE)[[2]] )

types	<- 'pc6'
#types	<- 'pc6_time2'
for( type in types ){
	print( type )

	savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_', mc.cores, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	print( type )
	sink(	sinkfile )

	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )

	runtime	<- system.time({
		out  <- mfmrx_gxe(
			Yb, Y, cbind( 1, G ), X=NULL, K=K, test_inds=1+1:ncol(G),
			sloppy=T,
			accel=F,
			maxit=1e4, tols=1e-4,
			init_runs='null',
			nrun=100, mc.cores=mc.cores, trace=T
		)
	})[3]

	save( out, runtime, file=savefile )
	rm( out, runtime, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()
}
