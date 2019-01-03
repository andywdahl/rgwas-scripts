source( 'sim_fxn_misc.R' )
sim_fxn_ge_cor <- function(
	N, P, P_bin, K,
	ge_cor=0,
	zbal=TRUE,
	S, S_hom, S_het,
	sig2E, sig2hom, sig2het,
	Etype='baseline',
	f=.2, asc=TRUE,
	z_prev,
	corrtype='Wish', AR_rho=NA,
	seed
){
	set.seed( seed )
	stopifnot( sig2E+sig2hom+sig2het < 1 )
	N_univ	<- ifelse( asc, N*1e2, N )

	E			<- Efxn( Etype, K, sig2E, z_prev, P )

	# G and effect sizes
	G			<- scale( sapply( 1:S, function(s) rbinom( N_univ, 2, runif(1,.05,.5) ) ) )
	gamma	<- gammafxn( zbal, K, S, P, S_hom, S_het, sig2hom=sig2hom, sig2het=sig2het, z_prev, seed )

	overlap	<- G %*% rnorm(S)
	overlap	<- overlap/sqrt(mean(overlap^2))
	zquant	<- sqrt(ge_cor*sig2E) * overlap + sqrt((1-ge_cor)*sig2E) * rnorm(N_univ)
	cutval	<- quantile(zquant,z_prev)
	z				<- sapply( zquant, function(zi) ifelse( zi > cutval, 2, 1 ) )

	# eps and noise correlations
	Sigma_e	<- Sigfxn( corrtype, P, AR_rho )
	eps			<- sqrt(1-sig2E-sig2hom-sig2het) * rmnorm( N_univ,P ) %*% mat.sqrt( Sigma_e )

	Y	<- matrix( NA, N_univ, P )
	for( k in 1:K ){
		ksub	<- which(z==k)
		Y[ksub,]	<- G[ksub,] %*% gamma[k,,] + matrix( E[k,], length(ksub), P, byrow=T ) + eps[ksub,]
	}

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

	list( Y=Y, G=G, z=z, E=E )
}
