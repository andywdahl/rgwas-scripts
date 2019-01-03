rm( list=ls() )
library( rgwas )
library( parallel )

mc.cores	<- as.numeric( commandArgs(TRUE)[[1]] )
K   			<- as.numeric( commandArgs(TRUE)[[2]] )

type	<- 'base'

savefile	<-	paste0( 'Rdata/', type,'_', K, '.Rdata' )
sinkfile 	<-	paste0( 'Rout/'	, type,'_', K, '_', mc.cores, '.Rout' )
if( file.exists( savefile ) | file.exists( sinkfile ) )	next
print( sinkfile )
sink(	sinkfile )

load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )

runtime	<- system.time({
	out_mfmr	<- mfmr(
		Yb, Y, cbind( 1, G ), X=NULL, K,
		nrun=20, mc.cores=mc.cores, trace=T
	)
})[3]
save( out_mfmr, runtime, file=savefile )
rm( out_mfmr, runtime, Yb, Y, G )
print(warnings())
print('Done')
sink()
