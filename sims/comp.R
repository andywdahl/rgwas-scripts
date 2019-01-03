rm( list=ls() )
load( 'Rdata/setup.Rdata' )

for( type in types )
	for( dir in dirs )
		for( sig2.i in sample(2) )
{
	print( all_savefile	<- paste0( 'Rdata/out_', dir, '_', type, '_', sig2.i, '.Rdata' )	)
	if( file.exists( all_savefile ) ){
		load( all_savefile )
	} else {
		pvals	<- array( NA, dim=c(				S		, P		, Me			, maxit ), 
												dimnames=list(1:S	, 1:P	, methods	, 1:maxit ))
		times	<- array( NA, dim=c(				 Me			, maxit ), 
												dimnames=list( methods, 1:maxit ))
	}

	for( method in methods )
		for( it in 1:maxit )
	tryCatch({

		if( mean( is.na( pvals[-1,,method,it] ) ) < .5 ) next

		loadfile	<- paste0( dir, '/Rdata/',	sig2.i, '_', method, '_', type, '_', it, '.Rdata'	)
		if( !file.exists( loadfile ) ) next
		load(	loadfile )
		print( loadfile )
		pvals[,,method,it]	<- out$pvals[1:S + ifelse( method %in% mfmrmeth, 1, 0 ),]
		times[method,it]		<- runtime
		rm(out,runtime)
	},error=function(e){ print(loadfile); print(e); print( method ) })
	save( times, pvals, file=all_savefile )
}

snptypes	<- c( 'Null', 'Hom', 'Het' )
allp	<- array( NA,
	dim=c(				 4		, 3				, 3											,Me			, length(types)	, length(sig2hets) ),
	dimnames=list( dirs	, snptypes, c('cc','quant','bin')	,methods, types					, sig2hets )
)
allt	<- nruns	<- array( NA,
	dim=c(				  4																			,Me			, length(types)	, length(sig2hets) ),
	dimnames=list(  dirs																	,methods, types					, sig2hets )
)

for( type in sample(types) )
	for( dir in dirs )
		for( sig2.i in sample(2) )
{
	all_savefile	<- paste0( 'Rdata/out_', dir, '_', type, '_', sig2.i, '.Rdata' )
	load( all_savefile )
	for( xx in snptypes )
		for( pp in c('cc','quant','bin') )
	{
		if( pp == 'cc' ){
			phens	<- 1
		} else if( pp == 'bin' ){
			phens	<- 2:P_bin
		} else {
			phens	<- (P_bin+1):P
		}
		if( xx == 'Null' ){
			snps	<- 8+1:4
		} else if( xx == 'Hom' ){
			snps	<- 1:4
		} else {
			snps	<- 4+1:4
		}
		allp	[dir,xx,pp,,type,sig2.i]	<- apply( pvals[snps,phens,,,drop=F], 3	, function(x) mean( x<.01, na.rm=T )	)
	}
	nruns	[dir,,type,sig2.i]	<- apply( pvals[1,1,,], 1		, function(x) sum( ! is.na(x) )				)
	allt	[dir,,type,sig2.i]	<- apply( times				, 1		, function(x) mean( x    , na.rm=T )	) / 60

	rm( pvals )
}
times	<- allt
pvals	<- allp

save( dirs, times, nruns, pvals, file='Rdata/out.Rdata' )
source( 'fig1.R' )
source( 'plot_times.R' )
