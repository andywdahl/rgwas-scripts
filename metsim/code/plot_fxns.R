lam_gc_correct	<- function(x,lam){
	chisq	<- qchisq( df=2, p=1-10^-x )
	-log10( 1-pchisq( df=2, q=chisq/lam ) )
}

ldprune	<- function( X, tol ){
	r2mat	<- cor( X, use='pairwise.complete.obs' )^2
	diag(r2mat)	<- 0
	sub			<- 1:ncol(X)
	while( any( r2mat > tol ) ){
		j					<- which( r2mat==max(r2mat, na.rm=T), arr.ind=T )[1,1]
		r2mat[j,]	<- 
		r2mat[,j]	<- 0
		sub				<- setdiff( sub, j )
	}
	sub
}

hit_inds	<- function(x,tol,ptol=-log10( 5e-8 ),gc=F){
	x[ mafs < .05 | mafs > .95 ]	<- NA
	if( gc ){
		sub			<- which( !is.na(x) )
		lam_gc			<- median(qchisq(1-10^-x,1),na.rm=T)/qchisq(0.5,1)
		x[sub]	<- lam_gc_correct( x[sub], lam_gc )
	}
	hits	<- which( x > ptol )
	if( length( hits ) == 0 ) return(NULL)
	if( length( hits ) == 1 ) return(hits)
	hits[ldprune( dat[,hits], tol )]
}
