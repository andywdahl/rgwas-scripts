rm( list=ls() )
library(mixtools)
library( rgwas )
library( parallel )

#load( paste0( '../parse_converge_data/parsed_data/', types[1], '.Rdata' )  )
#load( paste0( 'Rdata/gmmq_noarbvar_', types[1],'_', 2, '.Rdata' ) )
#cor( Yb[,'stress'], out_gmm[,1] )^2
#summary( lm( Yb[,'stress'] ~ out_gmm[,1] ) )
#stop()

mc.cores	<- as.numeric( commandArgs(TRUE)[[1]] )
K   			<- as.numeric( commandArgs(TRUE)[[2]] )

type	<- 'base'

savefile	<-	paste0( 'Rdata/gmmq_noarbvar_', type,'_', K, '.Rdata' )
sinkfile 	<-	paste0( 'Rout/gmmq_noarbvar_'	, type,'_', K, '_', mc.cores, '.Rout' )
if( file.exists( savefile ) | file.exists( sinkfile ) )	next
print( sinkfile )
sink(	sinkfile )

load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )

runtime	<- system.time({
	out_gmm	<- mvnormalmixEM( Y, k=2, verb=TRUE, arbvar=FALSE )$posterior #crashes when arbvar=T, and/or with binary traits
})[3]
save( out_gmm, runtime, file=savefile )
rm( out_gmm, runtime, Yb, Y, G )
print(warnings())
print('Done')
sink()
