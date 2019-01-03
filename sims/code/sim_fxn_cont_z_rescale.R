source( 'sim_fxn_misc.R' )
sim_fxn_cont_z <- function(
	N, P, P_bin, K,
	zbal=TRUE,
	S, S_hom, S_het,
	sig2E, sig2hom, sig2het,
	Etype='baseline',
	f=.2, asc=TRUE,
	z_prev,
	nongauss=FALSE,
	corrtype='Wish', AR_rho=NA,
	seed
){
	set.seed( seed )
	stopifnot( sig2E+sig2hom+sig2het < 1 )
	N_univ	<- ifelse( asc, N*1e2, N )
	z		<- rnorm( N_univ ) * sqrt( z_prev * (1-z_prev) )

	# G and effect sizes
	G			<- scale( sapply( 1:S, function(s) rbinom( N_univ, 2, runif(1,.05,.5) ) ) )
	alpha	<- beta <- matrix( 0, S, P )
	alpha[1:S_hom,]				<- sqrt(sig2hom)*rmnorm(S_hom,P)
	beta [1:S_het+S_hom,]	<- sqrt(sig2het)*rmnorm(S_het,P)

	# eps and noise correlations
	Sigma_e	<- Sigfxn( corrtype, P, AR_rho )
	eps			<- sqrt(1-sig2E-sig2hom-sig2het) * rmnorm( N_univ,P ) %*% mat.sqrt( Sigma_e )

	Y	<- G %*% alpha + sqrt(sig2E)*(z %o% rnorm(P)) + (z %o% rep(1,P))*( G %*% beta ) + eps

	# binarization
	if( P_bin > 1 )
		Y[,2:P_bin]	<- bin_thresh( Y[,2:P_bin,drop=F] )

	# ascertainment
	if( asc ){
		inds		<- sort.list(Y[,1],decreasing= T)
		cc			<- rep( 0, N_univ )
		N_cases	<- N_univ*f
		cc[ inds[1:N_cases] ]	<- 1
		sub	<- c( sample( inds[(N_cases+1):N_univ], N/2 ), # ctrls
							sample( inds[1:N_cases]					, N/2 )) # cases
		if( P == 1 ){
			Y		<- as.matrix( cc[sub] )
		} else {
			Y		<- cbind( cc[sub], Y[sub,-1] )
		}
		G		<- G[sub,]
		z		<- z[sub]
	} else {
		cc	<- as.numeric( Y[,1] < quantile( Y[,1], f ) )
		Y		<- cbind( cc, Y[,-1] )
	}

	list( Y=Y, G=G, z=z )
}
