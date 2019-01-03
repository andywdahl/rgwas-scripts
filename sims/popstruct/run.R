rm( list=ls() )
library(rgwas)
source( '../code/main.R' )
source( '../code/sim_fxn_pop.R' )
source( '../code/sim_fxn_misc.R' )
load( 'Rdata/setup.Rdata' )

it	<- as.numeric( commandArgs(TRUE)[[1]] )
set.seed( it )
for( method in methods )
	for( type in types )
		for( sig2.i in sample(length(sig2hets)) )
			for( truehet in c('_K1','') )
				for( poptype in poptypes[2] )
{

	savefile	<- paste0( 'Rdata/'	,	sig2.i, '_', method, '_', type, '_', it, '_', poptype, truehet, '.Rdata')
	sinkfile	<- paste0( 'Rout/'	,	sig2.i, '_', method, '_', type, '_', it, '_', poptype, truehet, '.Rout'	)
	if( file.exists(savefile) | file.exists(sinkfile)) next
	print( sinkfile )
	sink( sinkfile )
	try({
		simdat	<- sim_fxn_pop( N=1e4, P=30, P_bin=P_bin,
		K=2,
		sig2hom=sig2homs[sig2.i],
		sig2E=	ifelse( truehet=='', sig2Es[poptype]	, 0 ),
		sig2het=ifelse( truehet=='', sig2hets[sig2.i]	, 0 ),
		sig2pop=sig2list[type],
		S=12, S_hom=4, S_het=4,
		Etype='baseline', z_prev=.3, corrtype='Wi', AR_rho=NA, seed=it )

		runtime	<- system.time(
			if( method == 'oracle2+' ){
				out	<- snphet_main( Y=simdat$Y, G=cbind( simdat$G ), method='oracle+'	, K_em=2, z=simdat$z	, P_bin=P_bin, X=simdat$truepop )
			} else if( method == 'mfmr_pc' ){
				out	<- snphet_main( Y=simdat$Y, G=cbind( simdat$G, simdat$pcs ), method='mfmr_fast', K_em=2, z=simdat$z	, P_bin=P_bin )
			} else if( method == 'mfmr_pc20' ){
				out	<- snphet_main( Y=simdat$Y, G=cbind( simdat$G, simdat$pcs ), method='mfmr_fast20', K_em=2, z=simdat$z	, P_bin=P_bin )
			} else {
				out	<- snphet_main( Y=simdat$Y, G=cbind( simdat$G ), method=method		, K_em=2, z=simdat$z	, P_bin=P_bin, X=simdat$pcs )
			}
		)[3]
		cat( 'runtime:', runtime, '\n' )

		save( runtime, out, file=savefile )
		rm( simdat, out )
	})
	sink()
}
warnings()
