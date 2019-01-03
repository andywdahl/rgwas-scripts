library(phenix)
library(rgwas)
library(flexmix)
library(mixtools)
snphet_main	<- function( Y, G, method, K_em, P_bin=1, z, Ktrue, Kmax=5, return_pmat=FALSE, X ){

	non_mfmr	<- c( 
		'cca-G', 'cca-Y',
		'cca-Y-oracle',
		'oracle', 'oracle+', 'oracle++', 'oracle-trueK',
		'geno_pc', 'geno_pc_disc',
		'pheno_pc', 'pheno_pc_disc',
		'fmr',
		'gmm', 'allgmmq', 'allgmm',
		'mvgmmq+', 'mvgmmq', 'mvgmm', 'mvgmm+'
	)

	K_em0	<- K_em
	Yb		<- Y[,1:P_bin,drop=F]
	Yq		<- Y[,-(1:P_bin),drop=F]

	if( method %in% non_mfmr ){

		cc		<- Y[,1]
		Y			<- scale(Y)

		if(				method == 'cca-Y' ){
			pmat	<- Y %*% cancor( scale(G), Y )$ycoef[,1:(K_em-1),drop=F]
		} else if(method == 'cca-Y-oracle' ){
			pmat	<- Y %*% cancor( scale(G), Y )$ycoef[,1:(Ktrue-1),drop=F]
		} else if(method == 'cca-G' ){
			pmat	<- G %*% cancor( scale(G), Y )$xcoef[,1:(K_em-1),drop=F]
		} else if(method %in% c( 'oracle', 'oracle++', 'oracle+' ) ){
			pmat		<- matrix( z==1, nrow(Y), 1 )
		} else if(method == 'oracle-trueK' ){
			pmat		<- sapply( 1:(Ktrue-1), function(i){
				xx	<- rep( 0, nrow(Y) )
				xx[ z == i ]	<- 1
				xx
			})
		} else if(method == 'gmm' ){
			pmat	<- normalmixEM( cc, k=2 )$posterior[,1:(K_em-1),drop=F]
		} else if(method %in% c( 'mvgmm', 'mvgmm+' ) ){
			pmat	<- mvnormalmixEM( Y, k=2 )$posterior[,1:(K_em-1),drop=F]
		} else if(method %in% c( 'mvgmmq', 'mvgmmq+' ) ){
			pmat	<- mvnormalmixEM( Yq, k=2 )$posterior[,1:(K_em-1),drop=F]
		} else if(method == 'allgmm' ){
			pmat	<- mvnormalmixEM( cbind( G, Y ), k=2 )$posterior[,1:(K_em-1),drop=F]
		} else if(method == 'allgmmq' ){
			pmat	<- mvnormalmixEM( cbind( G, Yq ), k=2 )$posterior[,1:(K_em-1),drop=F]
		} else if(method == 'fmr' ){
			pmat	<- posterior(flexmix( cc ~ G, k=K_em ))

		} else if(method == 'geno_pc' ){
			pmat		<- svd( G )$u[,1,drop=F]
		} else if(method == 'pheno_pc' ){
			pmat		<- svd( Y )$u[,1,drop=F]
		} else if(method == 'geno_pc_disc' ){
			u		<- svd( G )$u[,1]
			u		<- sapply( u, function(u.i) ifelse( u.i>median(u), 1, 0 ) )
			pmat	<- matrix( u, nrow(Y), 1 )
		}

		if( method == 'oracle+' ){
		pvals	<- sapply( 1:ncol(Y), function(pp)
			sapply( 1:ncol(G), function(ss)
				interxn_test( X=cbind( X, pmat[,-K_em,drop=F] ), y=cbind( Yb, Yq )[,pp], g=G[,ss], pmat=pmat[,-K_em,drop=F], bin=(pp <= ncol(Yb)) )$pvals[2]
		))
		} else if( method %in% c( 'mvgmm+', 'mvgmmq+', 'oracle++' ) ){
		Gxpmat <- t(sapply( 1:nrow(X), function(i) X[i,] %x% c( pmat[i,], 1-pmat[i,] ) ))
		pvals	<- sapply( 1:ncol(Y), function(pp)
			sapply( 1:ncol(G), function(ss)
				interxn_test( X=cbind( pmat[,-K_em,drop=F], Gxpmat ), y=cbind( Yb, Yq )[,pp], g=G[,ss], pmat=pmat[,-K_em,drop=F], bin=(pp <= ncol(Yb)) )$pvals[2]
		))
		} else {
		pvals	<- sapply( 1:ncol(Y), function(pp)
			sapply( 1:ncol(G), function(ss)
				interxn_test( X=pmat[,-K_em,drop=F], y=cbind( Yb, Yq )[,pp], g=G[,ss], pmat=pmat[,-K_em,drop=F], bin=(pp <= ncol(Yb)) )$pvals[2]
		))
		}

	} else {
		#if(method %in% c( 'mfmr_Kstar' ) ){
		#	stop()
		#	print( 'Running Kstar' )
		#	K_em0	<- choose_K()

		#	K_em	<- max( K_em0, 2 )
		#} else if(method == 'mfmr_K6' ){
		#	accel	<- T
		#	K_em	<- 6
		#}

		if(method == 'mfmr' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G) )$pvals[2,,]
		} else if(method == 'mfmr_fast' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G), maxit=1e2, tol=rep(1e-2,2) )$pvals[2,,]
		#} else if(method == 'mfmr_pc' ){
		#	pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=X   , K=K_em, nrun=1, test_inds=1+1:ncol(G), maxit=1e2, tol=rep(1e-2,2) )$pvals[2,,]
		} else if(method == 'mfmr_fast3' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=3, test_inds=1+1:ncol(G), maxit=1e2, tol=rep(1e-2,2) )$pvals[2,,]
		} else if(method == 'mfmr_fast20' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=20, test_inds=1+1:ncol(G), maxit=1e2, tol=rep(1e-2,2) )$pvals[2,,]
		} else if(method == 'mfmr0' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G), init_sd=1e-4 )$pvals[2,,]
		} else if(method == 'mfmr1' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G), init_sd=1e-1 )$pvals[2,,]
		} else if(method == 'mfmrq' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G), qinit=T )$pvals[2,,]
		} else if(method == 'mfmr3' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=3, test_inds=1+1:ncol(G) )$pvals[2,,]
		} else if(method == 'mfmr100' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=100, test_inds=1+1:ncol(G) )$pvals[2,,]
		} else if(method == 'mfmr10' ){
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=10, test_inds=1+1:ncol(G) )$pvals[2,,]
		} else if(method == 'mfmr+' ){
			allout		<- mfmr( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1 )
			pmat			<- allout$pmat
			pvals	<- droptest( Yb, Yq, G=cbind(1,G), X=NULL, K=K_em, nrun=1, test_inds=1+1:ncol(G), mfmrx=TRUE, pmat=pmat, return_pvecs=FALSE )$pvals[2,,]
		}

	}
	if( ! return_pmat )
		pmat	<- NA
	return( list( pmat=pmat, pvals=pvals, K_em=K_em0 ))
}
