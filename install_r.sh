#!/bin/bash - 
#===============================================================================
#
#          FILE: install_r.sh
# 
#         USAGE: ./install_r.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Peter Belmann (), pbelmann@cebitec.uni-bielefeld.de
#  ORGANIZATION: Computational Metagenomics
#       CREATED: 03/25/16 09:31:16
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

R_DIR = "$HOME/R"
 
if [ ! -d $R_DIR ]; then
   mkdir $R_DIR
   wget http://cran.rstudio.com/src/base/R-3/R-3.2.4.tar.gz
   tar xvf R-3.2.4.tar.gz
   cd R-3.2.4  && ./configure --prefix="$R_DIR"  && make && make install
fi
