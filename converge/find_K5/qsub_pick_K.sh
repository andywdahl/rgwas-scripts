#!/bin/bash                         #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                     #-- the shell for the job
#$ -o Rout/new					#-- output directory (fill in)
#$ -e Rout/new                     #-- error directory (fill in)
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -r n                            #-- tell the system that if a job crashes, it should be restarted
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=0.5G                  #-- submits on nodes with enough free memory (required)
#$ -l arch=linux-x64               #-- SGE resources (CPU type)
#$ -l netapp=0.5G,scratch=0.5G         #-- SGE resources (home and scratch disks)
#$ -l h_rt=8:20:00                #-- runtime limit (see above; this requests 24 hours)

#$ -pe smp 10

date
hostname

Rscript run_ll_cv.R 10 1
Rscript run_ll_cv.R 10 2
Rscript run_ll_cv.R 10 3
Rscript run_ll_cv.R 10 4
Rscript run_ll_cv.R 10 5
Rscript run_ll_cv.R 10 6
Rscript run_ll_cv.R 10 7
Rscript run_ll_cv.R 10 8
Rscript run_ll_cv.R 10 9
Rscript run_ll_cv.R 10 10
Rscript run_ll_cv.R 10 11
Rscript run_ll_cv.R 10 12
