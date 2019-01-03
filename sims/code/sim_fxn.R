source( 'sim_fxn_misc.R' )
sim_fxn <- function(
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

	if( zbal == T ){
		z		<- sample(1:K,N_univ,rep=T)
	} else {
		if( K != 2 ) stop()
		z		<- rbinom(N_univ,1,z_prev) + 1	# 2 wp z_prev, 1 else
	}

	E			<- Efxn( Etype, K, sig2E, z_prev, P )

	# G and effect sizes
	G			<- scale( sapply( 1:S, function(s) rbinom( N_univ, 2, runif(1,.05,.5) ) ) )
	gamma	<- gammafxn( zbal, K, S, P, S_hom, S_het, sig2hom=sig2hom, sig2het=sig2het, z_prev, seed )

	# eps and noise correlations
	Sigma_e	<- Sigfxn( corrtype, P, AR_rho )
	if( nongauss ){
		df		<- 5
		eps		<- rt( N_univ*P, df=df )/sqrt( df/(df-2) )
		eps		<- matrix( eps, N_univ, P )
	} else {
		eps			<- rmnorm( N_univ,P )
	}
	eps			<- sqrt(1-sig2E-sig2hom-sig2het) * eps %*% mat.sqrt( Sigma_e )

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
