rm( list=ls() )
library(qvalue)
source( '../code/plot_fxns.R' )
load( '../Rdata/setup.Rdata' )
load( '../parse_data/parsed_data/mafs.Rdata' )
load( '../parse_data/parsed_data/snpids.Rdata' )
load( '../parse_data/parsed_data/allsnps.Rdata' )
load( 'Rdata/lambda_gcs.Rdata' )

Kmax		<- 4
snpset	<- c( is[sub], js )
candsnps<- allsnps1[snpset]
mafs		<- mafs[snpset]

out		<- array( NA, dim=c( 4, Kmax, np ), dimnames=list( c( 'BH', 'Bonf', 'qvalue', 'pi1' ), paste0( 'K=', 1:Kmax ), phens ) )
hits	<- data.frame( snp=NULL, phen=NULL, homp=NULL, hetp=NULL, s=NULL, K=NULL, maf=NULL, gc=NULL ) #mhomp=NULL, 

load( paste0( 'compiled_Rdata/', 1, '.Rdata' ) )
homout	<- allout[,snpset]
rm( allout )

for( K in 2:Kmax )try({
	load( paste0( 'compiled_Rdata/', K, '.Rdata' ) )
	allout	<- allout	[,snpset]
	for( phen in phens ){
		ps	<- 10^-( allout	[phen,] )
		ps0	<- 10^-( homout	[phen,] )

		snpset1	<- which( !is.na( ps ) )
		S				<- length(snpset1)
		ps0			<- ps0[snpset1]
		ps			<- ps [snpset1]
		stopifnot( all( !is.na( ps ) ) )

		out[1,K,phen]	<- sum( .10   > p.adjust( ps, 'BH' ) )
		out[2,K,phen]	<- sum( .05/S > ps )

		qobj    <- qvalue( ps )
		out[3,K,phen]	<- sum( .1 > qobj$qvalues )
		out[4,K,phen]	<- 1-qobj$pi0

		signif	<- which( .05/length(ps) > ps )
		if( length( signif ) == 0 ) next

		for( tt in signif ){
			if( all_lambdas[K,phen] < 1.4 ){
			hits	<- rbind( hits, data.frame( K=K, phen=phen,
				homp=format( ps0[tt]	, nsmall=5, digits=0 ),
				hetp=format( ps [tt]	, nsmall=5, digits=0 ),
				snp=candsnps[snpset1[tt]],
				maf=mafs    [snpset1[tt]],
				gc=all_lambdas[K,phen]
			) )
			} else {
				print( c( K, phen ) )
			}
		}
	}
	rm( allout )
})
hits[hits$K == 3,]
round( out[,3,], 5 )
