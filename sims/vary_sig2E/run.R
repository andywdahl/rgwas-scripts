rm( list=ls() )
source( '../code/main.R' )
source( '../code/sim_fxn.R', chdir=T )
load( 'Rdata/setup.Rdata' )
#library(pryr)

it	<- as.numeric( commandArgs(TRUE)[[1]] )
set.seed( it )

methods	<- c( 'mfmr_fast', #'mfmr+',# 'mfmr_fast3', #'mfmr_fast20','mfmr',
	'cca-Y', 'oracle', 'mvgmmq')
for( method in sample(methods) )
	for( type in sample(types) )
		for( sig2.i in sample(length(sig2hets)) )
			for( truehet in c('', '_K1') )
{

	savefile	<- paste0( 'Rdata/'	,	sig2.i, '_', method, '_', type, '_', it, truehet, '.Rdata')
	sinkfile	<- paste0( 'Rout/'	,	sig2.i, '_', method, '_', type, '_', it, truehet, '.Rout'	)
	if( file.exists(savefile) | file.exists(sinkfile)) next
	print( sinkfile )
	sink( sinkfile )
	try({
		simdat	<- sim_fxn( N=1e4, P=30, P_bin=P_bin, asc=FALSE,
		K=2,
		sig2hom=sig2homs[sig2.i],
		sig2E=	ifelse( truehet=='', sig2Es[type]	   	, 0 ),
		sig2het=ifelse( truehet=='', sig2hets[sig2.i]	, 0 ),
		S=12, S_hom=4, S_het=4,
		Etype='baseline', z_prev=.3, corrtype='Wi', AR_rho=NA, seed=it )

		runtime	<- system.time(
			out	<- snphet_main( Y=simdat$Y, G=cbind( simdat$G ), method=method, K_em=2, z=simdat$z, P_bin=P_bin )
		)[3]
		cat( 'runtime:', runtime, '\n' )

		save( runtime, out, file=savefile )
		rm( simdat, out )
	})
	sink()
}
warnings()
