rm( list=ls() )
library(ggplot2)
library(scales)
load( '../Rdata/setup.Rdata' )
K0	<- 3

load( '../parse_data/parsed_data/softI.Rdata' )
P			<- 228
phens	<-	colnames(Y)[-(1:7)]
length(phens) == P

h2s			<- array( NA, dim=c( P, 4 ), dimnames=list( phens, c( 'g', 'iid', 'hom', 'het' ) ) )
allps		<- array( NA, dim=c( P, 2 ), dimnames=list( phens, c('hom','iid') ) )
for( pp in 1:P )
try({

	load( savefile1	<- paste0( 'Rdata/hom/' , pp, '.Rdata' ) )
	h2s		[pp,'g']	<- out_hom$h2
	allps[pp,'hom']	<- pchisq( h2s[pp,'g']/( out_hom$h2Covmat[1] ), df=1, lower.tail=F )

	load( savefile2	<- paste0( 'Rdata/iid/' , pp, '.Rdata' ) )
	h2s		[pp,c('hom','het')]	<- out_iid$h2   
	h2s		[pp,'iid']	<- sum(out_iid$h2)
	allps [pp,'iid']	<- pchisq( 2*(out_iid$ll - out_hom$ll), df=1, lower.tail=F )

})
colMeans( h2s, na.rm=T )
colMeans( is.na(h2s) )

allps
colSums( allps < .01/P )
P

pdf( paste0( '../figs/GxEMM_vio_K_', K0, '.pdf' ), width=3.3, height=4.5 )

cols	<- c( 1, 'purple', 'grey', '#FFBB00' )

is	<- 1:4
x		<- as.factor( matrix( rep( is 	, each=P ), 4, P, byrow=T ) )
cols<- c(         matrix( rep( cols	, each=P ), 4, P, byrow=T ) )

y					<- as.numeric( t(h2s) )

print(
	ggplot(data.frame( x=x, y=y ), aes(x=x, y=y,fill=x)) +
	scale_fill_manual(values=cols)+
	geom_violin(trim=TRUE) +
	theme(legend.position="none") +
	scale_y_continuous(labels = percent, name='Variance Explained    ',breaks=c(-.25,0,.25,.5,.75,1),limits=range(h2s)) +
	theme(plot.margin = unit(c(.4,.1,2.9,.5), "cm"),
		panel.background=element_rect(fill = "transparent",colour = NA),
		plot.background=element_rect(fill = "transparent",colour = NA),
		axis.title.y = element_text(size=17),
		axis.ticks = element_blank()
	) +
	scale_x_discrete(name='NMR Traits', labels=c(
		"1" = expression( h[GREML]^2 ),
		"2" = expression( h[iid]^2 ),
		"3" = expression( h[hom]^2 ),
		"4" = expression( h[het]^2 ))) +
	theme( axis.text.y  = element_text(angle=90, size=8) ) +
	theme( axis.text.x  = element_text(          size=14) ) + 
	geom_hline(yintercept=0   , linetype="solid" , color = 1     , size=0.7) +
	geom_hline(yintercept=1   , linetype="solid" , color = 1     , size=0.7) +
	geom_hline(yintercept=.25 , linetype="dotted", color = "grey", size=0.5) +
	geom_hline(yintercept=.5  , linetype="dotted", color = "grey", size=0.5) +
	geom_hline(yintercept=.75 , linetype="dotted", color = "grey", size=0.5) +
	geom_hline(yintercept=-.25, linetype="dotted", color = "grey", size=0.5) 
)
dev.off()
