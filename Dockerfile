FROM ubuntu:14.04

ADD packrat /packrat
ADD quast_evaluator.r /
ADD .Rprofile /

RUN echo "deb http://ftp5.gwdg.de/pub/misc/cran/bin/linux/ubuntu trusty/"  >> /etc/apt/sources.list

RUN sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com E084DAB9 && \
   sudo apt-get update && \ 
   sudo apt-get install -y r-base r-recommended libicu52 libcurl4-openssl-dev && \
   R -e 'install.packages("packrat" , repos="http://cran.us.r-project.org"); packrat::restore()'

ENTRYPOINT ["/quast_evaluator.r"]
