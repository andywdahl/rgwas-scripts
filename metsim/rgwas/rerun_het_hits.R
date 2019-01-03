rm( list=ls() )
load( '../Rdata/setup.Rdata' )

K	<- 3
het_hits	<- data.frame(
	snps=c(	     'rs7138803'	, 'rs7528419'	, 'rs780094', 'rs10401969' ),
	phens=c(     'Insulin'		, 'HDL'				, 'NMR PC 3', 'NMR PC 6'	 ),
	phens_full=c('B_P_ins0'   , 'B_S_hdlc'  , 'NMR_PC_3', 'NMR_PC_6'	 )
)

hitfile	<- 'Rdata/hit_barplots.Rdata'
if( !file.exists( hitfile  ) ){
	library( rgwas )
	library(BEDMatrix)

	load( '~/mfmr/parse_data/parsed_data/allsnps.Rdata' )
	print( mean( het_hits$snps %in% allsnps1 ) ) 
	stopifnot( all( het_hits$snps %in% allsnps1 ) ) 

	## load pmat
	load( paste0( '../mfmrx/Rdata/'		, type,'_', K, '.Rdata' ) )
	pmat	<- out$pmat

	## load G/Y/Yb
	load( paste0( '../parse_data/parsed_data/'	, type, '.Rdata' )  )
	allY			<- cbind(Yb,Y)
	Gxpmat		<- t(sapply( 1:nrow(G), function(i) G[i,] %x% pmat[i,] ))
	nullmat   <- cbind( pmat[,-K], Gxpmat )
	#rm( Y, Yb )

	## load SNPs
	dat	<- BEDMatrix( '/u/home/j/joelmeff/project-eeskin/geno/autosomes.U05' ) # 6263 x 665478
	rownames(dat)	<- sapply( rownames(dat), function(x) strsplit( x, '_' )[[1]][2] )
	colnames(dat) <- sapply( colnames( dat ), function(x) strsplit( x, '_' )[[1]][1] )

	## subset
	sub	<- intersect( rownames(G), rownames(dat) )
	Gsnp<- dat[sub,as.character(het_hits$snps)]
	rm(dat)

	rm( out )

	out_hom	<- lapply( 1:ncol(Gsnp), function(ii){
		pp	<- as.character(het_hits$phens_full[ii])
		g		<- Gsnp[,ii]
		y		<- allY[,pp]
		sub1	<- which( !is.na( g ) )
		nullmat	<- G			[sub1,]
		y				<- y			[sub1]
		g				<- g			[sub1]
		pmat		<- pmat		[sub1,]
		glm( y ~ nullmat + g, family=ifelse( (pp %in% colnames(Yb)), 'binomial', 'gaussian' ) )
	})

	out	<- lapply( 1:ncol(Gsnp), function(ii){
		pp	<- as.character(het_hits$phens_full[ii])
		g		<- Gsnp[,ii]
		y		<- allY[,pp]
		sub1	<- which( !is.na( g ) )
		nullmat	<- nullmat[sub1,]
		y				<- y			[sub1]
		g				<- g			[sub1]
		pmat		<- pmat		[sub1,]
		interxn_test( X=nullmat, y=y, g=g, pmat=pmat[,-K,drop=F], bin=(pp %in% colnames(Yb)) )
	})
	save( out_hom, out, file=hitfile )
}
