gammafxn	<- function( zbal, K, S, P, S_hom, S_het, sig2hom, sig2het, z_prev, seed ){
	set.seed( seed )

	gamma	<- array( 0, dim=c( K, S, P ) )
	gamma_hom									<- sqrt(sig2hom/S_hom)						*rnorm(S_hom*P)
	for( k in 1:K )
		gamma[k,1:S_hom,]				<- gamma_hom
	if( ! zbal ){
		gamma[1,S_hom+1:S_het,]	<- sqrt(sig2het/S_het/(1-z_prev))	*rnorm(S_het*P)
	} else {
		for( k in 1:K )
		gamma[k,S_hom+1:S_het,]	<- sqrt(sig2het/S_het)						*rnorm(S_het*P)
	}
	gamma
}

Sigfxn	<- function( corrtype, P, AR_rho )
	if( corrtype == 'Wi' ){
		return( cov2cor( rWishart( 1, P, diag(P) )[,,1] ) )
	} else if( corrtype == 'iid' ){
		return( diag(P) )
	} else if( corrtype == 'AR' ){
		tmp <- diag(P)
		tmp <- AR_rho^abs(row(tmp)-col(tmp))
		return( cov2cor( solve( tmp ) ) )
	} else {
		stop('Bad corrtype')
	}

Efxn	<- function( Etype, K, sig2E, z_prev, P ){
	if( Etype == 'stressonly' ){
		if( K != 2 ) stop()
		E		<- sqrt(sig2E/z_prev) * rbind( rep(0,P), rnorm(P) )
	} else {
		E		<- sqrt(sig2E) * rmnorm(K,P)
	}
	E
}

rmnorm	<- function(N,P)
	matrix( rnorm( N*P ), N, P )

bin_thresh	<- function( X, mode='rand' )
	apply( X, 2, function(x)
{
	cutpoint	<- sample(x[!is.na(x)],1)
	as.numeric( x > cutpoint )
})

rgenotype	<- function(N,M,Fst){

	tmafs = array(0.5,M) #ancestral allelel frequency

	#allele freqs in new populations
	tmafs1= tmafs2 = 0
	for (i in 1:M) {
		tmafs1[i] = rbeta(1,tmafs[i]*(1-Fst)/Fst,(1-tmafs[i])*(1-Fst)/Fst)
		tmafs2[i] = rbeta(1,tmafs[i]*(1-Fst)/Fst,(1-tmafs[i])*(1-Fst)/Fst)
	}

	#haplotypes of each individual at each pair of SNPs
	genos1 = matrix(0,nrow=N,ncol=M);
	genos2 = matrix(0,nrow=N,ncol=M);
	for(i in 1:N) {
		genos1[i,]=rbinom(M,2,tmafs1) 
		genos2[i,]=rbinom(M,2,tmafs2) 
	}

	#estimated freqs
	mafs1 = apply(genos1,2,mean)/2 #pop1
	mafs2 = apply(genos2,2,mean)/2 #pop2
	mafs = (mafs1+mafs2)/2 #combined

	#combine center and scale genotypes
	genos = rbind(genos1,genos2)
	zgenos = scale(genos)
	fzgenos = zgenos[, !is.na(apply(zgenos,2,mean))]
	fzgenos

}
