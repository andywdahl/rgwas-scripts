rm( list=ls() )


load( '../parse_data/parsed_data/YG_B.Rdata'  )
G0		<- as.matrix( cbind( G[,c( 'Age', 'Age2',  "B_smoke", "B_statin", "B_betabloc", "B_diuretic", "B_totalcw" )], Y[,c('B_BMI','B_WHR')] ) )
stat0	<- G[,'B_statin']
t2d0	<- Yb[,'T2D']
pt2d0	<- Yb[,'preT2D']
age		<- G[,c('Age','Age2')]
head(age)
rm( G, Y, Yb )
load( '../parse_data/parsed_data/YG_F.Rdata'  )
stat1	<- G[,'F_statin']
t2d1	<- Yb[,'T2D']
pt2d1	<- Yb[,'preT2D']
rm( G, Y, Yb )

N	<- length( t2d1 )
convert_T2D		<- rep( NA, N )
convert_T2D[t2d0 == 0 & pt2d0 == 1 & t2d1	== 1]		<- 0
convert_T2D[t2d0 == 0 & pt2d0 == 1 & t2d1	== 0]		<- 1
table( pt2d0 & !t2d0, exclude=NULL )
table( t2d1[ which(pt2d0 & !t2d0) ], exclude=NULL )
sum( !is.na( t2d1[ which(pt2d0 & !t2d0) ] ) )

statin	<- stat0

mylm	<- function( ... )
	summary( glm( ..., family='binomial' ) )$coef
mylm( convert_T2D	~ statin		)


load( paste0( '../mfmrx/Rdata/', 'pc6', '_', 3, '.Rdata' ) )
Z	<- as.matrix( out$pmat )
colMeans(Z)

myaov	<- function( x, y )
	anova( 
		glm( x, family='binomial' ),
		glm( y, family='binomial' ),
		test='LRT'
	)


myaov(
	convert_T2D	~ 1 ,
	convert_T2D	~ Z	)

myaov(
	convert_T2D	~ G0  ,
	convert_T2D	~ G0	+Z	)


mylm( convert_T2D	~ -1 + Z )
mylm( convert_T2D	~ -1 + Z + G0 )
