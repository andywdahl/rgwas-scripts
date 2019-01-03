rm( list=ls() )
load(		'../Rdata/setup.Rdata' )
load( '../parse_data/parsed_data/mafs.Rdata' )
load( '../parse_data/parsed_data/snpids.Rdata' )
load( '../parse_data/parsed_data/allsnps.Rdata' )
source( '../code/plot_fxns.R' )

load( 'compiled_Rdata/1.Rdata' )
psx	<- allout
rm( allout )

load( 'compiled_Rdata/3_Glob.Rdata' )
psy	<- allout
rm( allout )

library(BEDMatrix)
dat						<- BEDMatrix( '/u/home/j/joelmeff/project-eeskin/geno/autosomes.U05' ) # 6263 x 665478
rownames(dat)	<- sapply( rownames(dat), function(x) strsplit( x, '_' )[[1]][2] )
colnames(dat)	<- sapply( colnames( dat ), function(x) strsplit( x, '_' )[[1]][1] )

vals	<- matrix( 0, np, 3 )
rownames(vals)	<- phens
colnames(vals)	<- c( 'GWAS', 'GxEWAS', 'Both' )
for( pp in 1:np ){

	hitfile	<- paste0( 'Rdata/hit_snps_', pp, '.Rdata' )
	if( file.exists( hitfile ) ){
		load( hitfile )
	} else {
		xhits	<- hit_inds( psx[pp,], tol=.2, gc=F )
		yhits	<- hit_inds( psy[pp,], tol=.2, gc=F )
		if( length( c(xhits,yhits) ) == 0 ){
			r2mat	<- NULL
		} else {
			G_snp	<- as.matrix( dat[,c(xhits,yhits),drop=F] )
			r2mat	<- cor( G_snp, use='pairwise.complete.obs' )^2
			rm( G_snp )
		}
		save( r2mat, xhits, yhits, file=hitfile )
	}
	if( is.null(r2mat) ) next

	vals[pp,]	<- c( length( xhits ), length( yhits ), sum( r2mat[upper.tri(r2mat)] > .5 ) )
	rm( r2mat, xhits, yhits )

}
print( t(vals) )
