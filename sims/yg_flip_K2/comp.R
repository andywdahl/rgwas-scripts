rm( list=ls() )
load( '../Rdata/setup.Rdata' )
fliptypes	<- c( 'g->y', 'y->g' )

for( type in types )
	for( sig2.i in sample(2) )
		for( fliptype in fliptypes )
{
	print( all_savefile	<- paste0( 'Rdata/out_', type, '_', fliptype, '_', sig2.i, '.Rdata' )	)
	if( file.exists( all_savefile ) ){
		load( all_savefile )
	} else {
		pvals	<- array( NA, dim=c(				S+1			, P+1			, Me			, maxit ), 
												dimnames=list(1:(S+1)	, 1:(P+1)	, methods	, 1:maxit ))
		times	<- array( NA, dim=c(				 Me			, maxit ), 
												dimnames=list( methods, 1:maxit ))
	}

	for( method in methods )
		for( it in 1:maxit )
	tryCatch({

		if( mean( is.na( pvals[-1,,method,it] ) ) < .5 ) next

		loadfile	<- paste0( 'Rdata/',	sig2.i, '_', method, '_', type, '_', fliptype, '_', it, '.Rdata'	)
		if( !file.exists( loadfile ) ) next
		load(	loadfile )
		print( loadfile )

		if( fliptype == 'g->y' ){
			pvals[1:(S-1),,method,it]	<- out$pvals[1:(S-1) + ifelse( method %in% mfmrmeth, 1, 0 ),]
		} else if( fliptype == 'y->g' ){
			pvals[,1:(P-1),method,it]	<- out$pvals[1:(S+1) + ifelse( method %in% mfmrmeth, 1, 0 ),]
		}

		times[method,it]		<- runtime
		rm(out,runtime)
	},error=function(e){ print(loadfile); print(e); print( method ) })
	save( times, pvals, file=all_savefile )
}



snptypes	<- c( 'Null', 'Hom', 'Het', 'False' )
allp	<- array( NA,
	dim=c(				 2  , 2					, 4				, 3													,Me			, length(types)	, length(sig2hets) ),
	dimnames=list( 1:2, fliptypes	, snptypes, c('falsex','quant','bin')	,methods, types					, sig2hets )
)
allt	<- nruns	<- array( NA,
	dim=c(				  2																						,Me			, length(types)	, length(sig2hets) ),
	dimnames=list(  fliptypes																		,methods, types					, sig2hets )
)

for( type in sample(types) )
	for( sig2.i in sample(2) )
		for( fliptype in fliptypes )
			for( fptype in 1:2 )
{
	all_savefile	<- paste0( 'Rdata/out_', type, '_', fliptype, '_', sig2.i, '.Rdata' )
	load( all_savefile )
	for( xx in snptypes )
		for( pp in c('falsex','quant','bin') )
	{
		if( pp == 'falsex' & fliptype == 'g->y' ){
			phens	<- P+1
		} else if( pp == 'bin' ){
			phens	<- 1:P_bin
		} else {
			phens	<- (P_bin+1):P
		}
		if( xx == 'Null' ){
			if( fptype == 2 ){
				snps	<- 8+1:4
			} else {
				snps	<- 4+1:8
			}
		} else if( xx == 'Hom' ){
			snps	<- 1:4
		} else if( xx == 'False' & fliptype == 'y->g' ){
			snps	<- S+1
		} else {
			if( fptype == 2 ){
				snps	<- 4+1:4
			} else {
				next
			}
		}
		allp	[fptype,fliptype,xx,pp,,type,sig2.i]	<- apply( pvals[snps,phens,,,drop=F], 3	, function(x) mean( x<.01, na.rm=T )	)
	}
	nruns	[fliptype,,type,sig2.i]	<- apply( pvals[1,1,,], 1		, function(x) sum( ! is.na(x) )				)
	allt	[fliptype,,type,sig2.i]	<- apply( times				, 1		, function(x) mean( x    , na.rm=T )	) / 60

	rm( pvals )
}
times	<- allt
pvals	<- allp

save( times, nruns, pvals, fliptypes, file='Rdata/out.Rdata' )
source( 'plot.R' )
