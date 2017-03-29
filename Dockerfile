FROM ubuntu:14.04

ADD packrat /packrat
ADD quast_evaluator.r /
ADD .Rprofile /
ADD quast_evaluator_cli.r /

ENV TAR_OPTIONS=" --no-same-owner "
RUN echo "deb http://ftp5.gwdg.de/pub/misc/cran/bin/linux/ubuntu trusty/"  >> /etc/apt/sources.list

RUN sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com E084DAB9 && \
   sudo apt-get update && \ 
   sudo apt-get install -y r-base r-recommended libicu52 libcurl4-openssl-dev pandoc && \
   R -e 'install.packages("packrat" , repos="http://cran.us.r-project.org"); packrat::restore()'

ADD /web /web
WORKDIR /web
RUN apt-get install wget
WORKDIR /opt
RUN wget https://nodejs.org/dist/v6.9.5/node-v6.9.5-linux-x64.tar.xz && tar xvf node-v6.9.5-linux-x64.tar.xz 
RUN ln -s  /opt/node-v6.9.5-linux-x64/bin/node   /usr/local/bin/node
RUN ln -s  /opt/node-v6.9.5-linux-x64/bin/npm   /usr/local/bin/npm
ADD cami.R /
ADD biobox.yaml /
WORKDIR /
