#!/bin/bash
#
#$ -S /bin/bash
#$ -o ./Rout  ## where to put standard output (to screen)
#$ -e ./Rout  ## where to put error messages
#$ -cwd            #-- start in current working directory
#$ -l h_data=.5G  
#$ -pe shared 5  ## processes=cores
#$ -l time=62:20:00       #-- runtime limit (this requests 24 hours)
 
## these lines will be printed to the output file for the job
uname -a
date

/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 1
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 2
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 3
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 4
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 5
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 6
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 7
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 8
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 9
/u/project/zarlab/joelmeff/joelBin/anaconda/bin/Rscript ./run.R 5 10
