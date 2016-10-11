FROM ubuntu:16.04

# upgrade and install tools
RUN apt-get --yes update && apt-get --yes upgrade
RUN apt-get --yes install git curl cmake g++

# build and install aria library
WORKDIR /root
RUN curl http://robots.mobilerobots.com/ARIA/download/current/ARIA-2.9.0+x86_64+gcc4.6.tgz > aria.tar.gz\ 
	&& tar -zxvf aria.tar.gz\
	&& cd Aria*\
	&& sed -i Makefile -e 's#-perm +1#-perm /1#g'\
	&& make\
	&& make install\
	&& rm -rf Aria* aria.tar.gz

COPY mtre3710 ariaracer
