sim_fxn_pop <- function(
	N, P, P_bin, K,
	M = 1e3, Fst = 0.1, npc = 3,
	S, S_hom, S_het,
	sig2E, sig2pop, sig2hom, sig2het,
	Etype='baseline',
	z_prev,
	corrtype='Wish', AR_rho=NA,
	seed
){
	set.seed( seed )
	stopifnot( sig2E+sig2pop+sig2hom+sig2het < 1 )

	z			<- rbinom(N,1,z_prev) + 1	# 2 wp z_prev, 1 else
	#pop_id<- rbinom(N,1,.5)			+ 1	# 2 wp .5, 1 else
	truepop	<- as.numeric( scale( rep( 1:2, each=N/2 ) ) )

	genos		<- rgenotype(N/2,M,Fst)
	pcs			<- svd( genos )$u[,1:npc]
	print( cor( pcs, truepop ) )

	E	<- Efxn( Etype, K, sig2E, z_prev, P )

	# G and effect sizes
	G			<- genos[,1:S]
	rm( genos )
	gamma	<- gammafxn( zbal=F, K, S, P, S_hom, S_het, sig2hom=sig2hom, sig2het=sig2het, z_prev, seed )

	# eps and noise correlations
	Sigma_e	<- Sigfxn( corrtype, P, AR_rho )
	eps			<- sqrt(1-sig2E-sig2pop-sig2hom-sig2het) * rmnorm( N,P ) %*% mat.sqrt( Sigma_e )

	Y	<- matrix( NA, N, P )
	for( k in 1:K ){
		ksub	<- which(z==k)
		Y[ksub,]	<- G[ksub,] %*% gamma[k,,] + matrix( E[k,], length(ksub), P, byrow=T ) + eps[ksub,]
	}

	### add hom pop struct effects
	Y		<- Y + sqrt(sig2pop)*truepop %o% rnorm( P )

	#for( pop_j in 1:2 )
	#	if( hetpop ){
	#		pop_sub	<- which(pop_id==pop_j)
	#		for( k in 1:K ){
	#			ksub		<- which(z==k)
	#			locsub	<- intersect( ksub, pop_sub )
	#			Y[locsub,]	<- Y[locsub,] + sqrt(sig2pop)*rnorm(P)
	#		}
	#	} else {
	#		pop_sub	<- which(pop_id==pop_j)
	#		Y[pop_sub,]	<- Y[pop_sub,] + sqrt(sig2pop)*rnorm(P)
	#	}

	# binarization
	if( P_bin > 0 )
		Y[,1:P_bin]	<- bin_thresh( Y[,1+1:P_bin,drop=F] )

	list( Y=Y, G=G, z=z, E=E, truepop=truepop, pcs=pcs )
}
