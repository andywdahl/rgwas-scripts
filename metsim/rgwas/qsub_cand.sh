#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=3G  
#$ -l time=20:02:00       #-- runtime limit (this requests 24 hours)
 
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript run_cands.R
