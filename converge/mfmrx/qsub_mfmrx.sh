#!/bin/bash                         #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                     #-- the shell for the job
#$ -o Rout/2					#-- output directory (fill in)
#$ -e Rout/2e                     #-- error directory (fill in)
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -r n                            #-- tell the system that if a job crashes, it should be restarted
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=0.5G                  #-- submits on nodes with enough free memory (required)
#$ -l arch=linux-x64               #-- SGE resources (CPU type)
#$ -l netapp=0.5G,scratch=0.5G         #-- SGE resources (home and scratch disks)
#$ -l h_rt=2:20:00                #-- runtime limit (see above; this requests 24 hours)
#$ -pe smp 5

date
hostname


Rscript run.R 5 2

Rscript run_gmmq_noarbvar.R 5 2

Rscript run.R 5 3
Rscript run.R 5 4
Rscript run_gmmq_noarbvar.R 5 3
Rscript run_gmmq_noarbvar.R 5 4

