#!/usr/bin/Rscript

chooseCRANmirror( ind=7 )

install.packages( 'filehash' ) # dependecies of 'tikzDevice'
#install.packages( 'tikzDevice', repos='http://R-Forge.R-project.org' )
install.packages('tikzDevice')
install.packages( 'effsize' )
install.packages( 'orddom' )
install.packages( 'e1071' )
install.packages( 'pracma' )
install.packages( 'scales' )
install.packages( 'plotrix' )
install.packages( 'psych' )
install.packages('http://cran.nexr.com/src/contrib/orddom_3.1.tar.gz', repos=NULL, type="source")

