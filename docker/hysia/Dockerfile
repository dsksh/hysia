FROM ocaml/ocaml:ubuntu-14.04
#MAINTAINER Daisuke ISHII <dsksh@acm.org>
WORKDIR /root
RUN apt-get update && \
    apt-get install -y wget libboost-dev && \
    wget https://www.dropbox.com/s/3uf7t2nsizfebno/capdDynSys-201406.zip?dl=0 -O capdDynSys-201406.zip && \
    wget https://github.com/dsksh/hysia/archive/v0.1.2.tar.gz -O hysia-0.1.2.tgz && \
    unzip capdDynSys-201406.zip && \
    tar xvzf hysia-0.1.2.tgz && \
    cd capd_dynsys && \
    patch -p1 -u < ../hysia-0.1.2/tmp/capd_dynsys.20140612.patch && \
    CFLAG="-fPIC" CPPFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure && \
    make install && \
    cd ../hysia-0.1.2 && \
    ./configure && \
    make && \
    install -m 0755 ${PWD}/src_ocaml/hss.opt /usr/local/bin/ && \
    cd .. && \
    apt-get remove -y wget && \
    rm -rf capd_dynsys && \
    rm capdDynSys-201406.zip && \
    rm -rf hysia-0.1.2 && \
    rm hysia-0.1.2.tgz
ADD hysia.sh .
ENTRYPOINT ["/bin/sh", "hysia.sh"]
