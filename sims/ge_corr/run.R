rm( list=ls() )
source( '../code/main.R' )
source( '../code/sim_fxn_ge_corr.R', chdir=T )
load( 'Rdata/setup.Rdata' )

it	<- as.numeric( commandArgs(TRUE)[[1]] )
set.seed( it )

for( method in plotmeth )
	for( type in types )
		for( sig2.i in sample(length(sig2hets)) )
{
	#if( method %in% c( 'mvgmm','allgmm','allgmmq', 'geno_pc_disc', 'geno_pc', 'pheno_pc', 'fmr', 'mfmr+' ) ) next
	savefile	<- paste0( 'Rdata/',	sig2.i, '_', method, '_', type, '_', it, '.Rdata'	)
	sinkfile	<- paste0( 'Rout/',	sig2.i, '_', method, '_', type, '_', it, '.Rout'		)
	if( file.exists(savefile) | file.exists(sinkfile) ) next
	print( sinkfile )
	sink( sinkfile )
	try({
		simdat	<- sim_fxn_ge_cor( N, P=P, P_bin=P_bin,
		ge_cor=ge_corrs[type],
		K=2, zbal=F, sig2E=.1,
		S=12, S_hom=4, S_het=4,
		sig2homs[sig2.i], sig2hets[sig2.i],
		Etype='baseline', z_prev=.3, corrtype='Wi', AR_rho=NA, seed=it )
		gc()

		runtime	<- system.time(
			out	<- snphet_main( Y=simdat$Y, G=simdat$G, method=method, K_em=2, z=simdat$z, P_bin=P_bin )
		)[3]
		cat( 'runtime:', runtime, '\n' )
		gc()

		save( runtime, out, file=savefile )
		rm( simdat, out )
	})
	sink()
}
warnings()
