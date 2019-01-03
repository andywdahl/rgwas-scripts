rm( list=ls() )
load( 'Rdata/setup.Rdata' )
#if(F)
for( type in types )
	for( sig2.i in sample(2) )
			for( truehet in c('','_K1') )
{
	print( all_savefile	<- paste0( 'Rdata/out_', type, '_', sig2.i, truehet, '.Rdata' )	)
	if( file.exists( all_savefile ) ){
		load( all_savefile )
	} else {
		pvals	<- array( NA, dim=c(				S		, P		, Me			, maxit ), 
												dimnames=list(1:S	, 1:P	, methods	, 1:maxit ))
	}
	for( method in methods )
		for( it in 1:maxit )
	tryCatch({
		if( mean( is.na( pvals[-1,,method,it] ) ) < .5 ) next

		loadfile	<- paste0( 'Rdata/'	,	sig2.i, '_', method, '_', type, '_', it, truehet, '.Rdata')
		if( !file.exists( loadfile ) ) next
		load(	loadfile )
		print( loadfile )
		pvals[,,method,it]	<- out$pvals[1:S + ifelse( method %in% mfmrmeth, 1, 0 ),]
		rm(out,runtime)
	},error=function(e){ print(loadfile); print(e); print( method ); file.remove( loadfile ) })
	save( pvals, file=all_savefile )
}

snptypes	<- c( 'Null', 'K=1', 'Hom', 'Het' )
allp	<- array( NA,
	dim=c(				 4				, 2									,Me			, length(types)	, length(sig2hets) ),
	dimnames=list( snptypes, c('bin','quant'	)	,methods, types					, sig2hets )
)
allt	<- nruns	<- array( NA,
	dim=c(				  Me			, length(types)	, length(sig2hets) ),
	dimnames=list(  methods, types					, sig2hets )
)

for( type in sample(types) )
	for( sig2.i in sample(2) )
		for( pp in c('bin','quant') )
try({
	load( paste0( 'Rdata/out_', type, '_', sig2.i, '.Rdata' ) )

	if( pp == 'bin' ){
		phens	<- 1:P_bins[type]
	} else {
		phens	<- (P_bins[type]+1):P
	}

	for( xx in snptypes[c(1,3,4)] ){
		if( xx == 'Null' ){
			snps	<- 8+1:4
		} else if( xx == 'Hom' ){
			snps	<- 1:4
		} else {
			snps	<- 4+1:4
		}
		allp	[xx,pp,,type,sig2.i]		<- apply( pvals[snps,phens,,,drop=F], 3	, function(x) mean( x<.01, na.rm=T )	)
	}
	nruns	[,type,1]	<- apply( pvals[1,1,,], 1	, function(x) sum( ! is.na(x) )	)
	rm( pvals )

	load( paste0( 'Rdata/out_', type, '_', sig2.i, '_K1.Rdata' ) )
		#allp	['K=1',pp,,type,sig2.i]	<- apply( pvals[4+1:8,phens,,,drop=F], 3, function(x) mean( x<.01, na.rm=T )	)
		allp	['K=1',pp,,type,sig2.i]	<- apply( pvals[1:4,phens,,,drop=F], 3, function(x) mean( x<.01, na.rm=T )	)
	nruns	[,type,2]	<- apply( pvals[1,1,,], 1	, function(x) sum( ! is.na(x) )	)
	rm( pvals )
})
pvals	<- allp
cbind(nruns[meths,,1],nruns[meths,,2])

save( snptypes,  nruns, pvals, file='Rdata/out.Rdata' )
