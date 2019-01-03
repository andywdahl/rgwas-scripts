rm( list=ls() )
load( '../Rdata/setup.Rdata' )

plotmeth	<- c(
	#'geno_pc_disc', 'geno_pc', 'pheno_pc', #'fmr',
	'mfmr_fast', 
	'cca-Y', 'oracle',
	#'mfmr+',# 'mfmr_fast3', #'mfmr_fast20','mfmr',
	#'allgmmq',
	'mvgmmq'
)

for( type in types )
	for( sig2.i in sample(2) )
{
	print( all_savefile	<- paste0( 'Rdata/out_', type, '_', sig2.i, '.Rdata' )	)
	if( file.exists( all_savefile ) ){
		load( all_savefile )
	} else {
		pvals	<- array( NA, dim=c(				S		, P		, Me			, maxit ), 
												dimnames=list(1:S	, 1:P	, methods	, 1:maxit ))
		times	<- array( NA, dim=c(				 Me			, maxit ), 
												dimnames=list( methods, 1:maxit ))
	}

	for( method in plotmeth )
		for( it in 1:maxit )
	tryCatch({

		if( mean( is.na( pvals[-1,,method,it] ) ) < .5 ) next

		loadfile	<- paste0( 'Rdata/',	sig2.i, '_', method, '_', type, '_', it, '.Rdata'	)
		if( !file.exists( loadfile ) ) next
		tryCatch({
		load(	loadfile )
		}, error=function(e){
			file.remove( loadfile )
		})
		print( loadfile )

		pvals[,,method,it]	<- out$pvals[1:S + ifelse( method %in% mfmrmeth, 1, 0 ),]

		times[method,it]		<- runtime
		rm(out,runtime)
	},error=function(e){ print(loadfile); print(e); print( method ) })
	save( times, pvals, file=all_savefile )
}



snptypes	<- c( 'Null', 'Hom', 'Het' )
allp	<- array( NA,
	dim=c(				 3				, 2								,Me			, length(types)	, length(sig2hets) ),
	dimnames=list( snptypes, c('quant','bin')	,methods, types					, sig2hets )
)
allt	<- nruns	<- array( NA,
	dim=c(				  Me			, length(types)	, length(sig2hets) ),
	dimnames=list(  methods, types					, sig2hets )
)

for( type in sample(types) )
	for( sig2.i in sample(2) )
{
	all_savefile	<- paste0( 'Rdata/out_', type, '_', sig2.i, '.Rdata' )
	load( all_savefile )
	for( xx in snptypes )
		for( pp in c('quant','bin') )
	{
		if( pp == 'quant' ){
			phens	<- 1:P_bin
		} else {
			phens	<- (1+P_bin):P
		}

		if( xx == 'Null' ){
			snps	<- 8+1:4
		} else if( xx == 'Hom' ){
			snps	<- 1:4
		} else {
			snps	<- 4+1:4
		}
		allp	[xx,pp,,type,sig2.i]	<- apply( pvals[snps,phens,,,drop=F], 3	, function(x) mean( x<.01, na.rm=T )	)
	}
	nruns	[,type,sig2.i]	<- apply( pvals[1,1,,], 1		, function(x) sum( ! is.na(x) )				)
	allt	[,type,sig2.i]	<- apply( times				, 1		, function(x) mean( x    , na.rm=T )	) / 60

	rm( pvals )
}
times	<- allt
pvals	<- allp

save( times, nruns, pvals, file='Rdata/out.Rdata' )
source( 'plot.R' )
