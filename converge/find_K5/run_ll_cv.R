rm( list=ls() )
library( rgwas )
library( parallel )

options(warn=1)

mc.cores  <- as.numeric( commandArgs(TRUE)[[1]] )
K         <- as.numeric( commandArgs(TRUE)[[2]] )

type	<- 'base'

savefile  <-  paste0( 'Rdata/', type,'_', K, '.Rdata' )
sinkfile  <-  paste0( 'Rout/' , type,'_', K, '_', mc.cores, '.Rout' )
if( file.exists( savefile ) | file.exists( sinkfile ) ) next
print( sinkfile )
sink( sinkfile )

load( paste0( '../parse_converge_data/parsed_data/', type, '.Rdata' )  )

runtime <- system.time({
out <- score_K( Yb, Y, cbind( 1, G ), X=NULL, K, n.folds=5, nrun=10, mc.cores=mc.cores, trace=T)
})[3]
save( out, runtime, file=savefile )
rm( out, runtime, Yb, Y, G )
print('Done')
sink()
