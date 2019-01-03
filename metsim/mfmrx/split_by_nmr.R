rm( list=ls() )
#load( '../Rdata/setup.Rdata' )
#suppressMessages( library( gplots ) )

#load( '../parse_data/parsed_data/svd.nmr.Rdata' )
load( '../parse_data/parsed_data/softI.Rdata' )
Ynmr	<- Y[,-(1:length( Ynames ))]
#Ynmr	<- Ynmr[,-grep( 'L_C', colnames(Ynmr) )]

bmi			<- Y[,'B_BMI']
bmiwhr	<- resid( lm( Y[,'B_WHR'] ~ bmi ) )
statin	<- G[,'B_statin']
age			<- G[,'Age']
rm( Y, Yb, G )

X	<- cbind( statin, age, age^2 )
#X	<- 1
#X	<- statin

K	<- 3
type	<- 'pc6'

load( paste0( '../mfmrx/Rdata/', type,'_', K, '.Rdata' ) )
#load( paste0( '../parse_data/parsed_data/', type, '.Rdata' )  )
print( paste( type, K ) )




yy	<- apply( out$pmat, 1, which.max ) - 1
colMeans( out$pmat )
colMeans( out$pmat * matrix( bmi, ncol=3, nrow=length(bmi) ) )
table( yy )


yfac	<- as.factor( yy )
table( yfac )
#summary( lm( Ynmr[,'B_NMR_TotCho'] ~ yfac + statin ) )
#summary( lm( Y[,'B_S_ldlc'] ~ yfac + statin ) )
summary( lm( bmiwhr ~ yfac ) )



#mat	<- summary( glm( yy ~ Ynmr, subset=which( yy != 2 ), family='binomial' ) )$coef
#(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]
#.05/nrow(mat)


z1	<- (yy == 0)
#mat	<- summary( glm( z1 ~ Ynmr + X, family='binomial' ) )$coef
#(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .001 ),]
#
#z2	<- (yy == 1)
#mat	<- summary( glm( z2 ~ Ynmr + X, family='binomial' ) )$coef
#(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]

z3	<- (yy == 2)
mat	<- summary( glm( z3 ~ Ynmr + X, family='binomial' ) )$coef
(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]

mat	<- summary( glm( z1 ~ Ynmr + X, family='binomial', subset=which( yy!=2 ) ) )$coef
(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]

#mat	<- summary( glm( z3 ~ Ynmr + X, family='binomial', subset=which( yy!=1 ) ) )$coef
#(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]
#mat	<- summary( glm( z3 ~ Ynmr + X, family='binomial', subset=which( yy!=0 ) ) )$coef
#(mat[sort.list(mat[,4]),])[1:sum( mat[,4] < .01 ),]
