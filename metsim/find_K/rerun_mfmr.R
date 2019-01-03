rm( list=ls() )
library( rgwas )
library( parallel )

# redefine clusters with new 'rgwas' package instead of old 'mfmr' package
# only done for maximum comparability inside internal accuracy metrics in choose_K in 'rgwas' package

mc.cores<- as.numeric( commandArgs(TRUE)[[1]] )
K				<- as.numeric( commandArgs(TRUE)[[2]] )

types	<- 'pc6'
for( type in types ){
	print( type )

	savefile	<-	paste0( 'Rdata/rerun_mfmr_', type,'_', K, '.Rdata' )
	sinkfile 	<-	paste0( 'Rout/rerun_mfmr_' , type,'_', K, '_', mc.cores, '.Rout' )
	if( file.exists( savefile ) | file.exists( sinkfile ) )	next
	print( sinkfile )
	print( type )
	sink(	sinkfile )

	load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )

	runtime	<- system.time({
		out  <- mfmr( Yb, Y, cbind( 1, G ), X=NULL, K=K, mc.cores=mc.cores, trace=T )
	})[3]

	save( out, runtime, file=savefile )
	rm( out, runtime, Yb, Y, G )
	print(warnings())
	print('Done')
	sink()
}
