rm( list=ls() )
N		<- 1e4

ge_corrs<- seq( .05, .95, len=7 )
types		<- paste0( 'ge_corr_', ge_corrs )
names(ge_corrs) <- types

sig2homs	<- c( .04, .004 )
sig2hets	<- c( .004, .04 )

S			<- 12
P			<- 30
P_bin	<- 3

maxit	<- 300

plotmeth	<- c(
	#'geno_pc_disc', 'geno_pc', 'pheno_pc', #'fmr',
	'mfmr_fast', 
	'cca-Y', 'oracle',
	#'mfmr+',# 'mfmr_fast3', #'mfmr_fast20','mfmr',
	#'allgmmq',
	'mvgmmq'
)

save.image( 'Rdata/setup.Rdata' )
