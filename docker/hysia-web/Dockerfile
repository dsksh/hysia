FROM dsksh/eliom:latest
#MAINTAINER Daisuke ISHII <dsksh@acm.org>
WORKDIR /home/opam
USER opam
ADD Makefile.opam-4.02.3.patch .
RUN sudo apt-get install -y wget libboost-dev && \
    wget -q https://www.dropbox.com/s/3uf7t2nsizfebno/capdDynSys-201406.zip?dl=0 -O capdDynSys-201406.zip && \
    wget -q https://github.com/dsksh/hysia/archive/v0.1.1.tar.gz -O hysia-0.1.1.tgz && \
    unzip capdDynSys-201406.zip && \
    tar xvzf hysia-0.1.1.tgz && \
    cd capd_dynsys && \
    patch -p1 -u < ../hysia-0.1.1/tmp/capd_dynsys.20140612.patch && \
    CFLAG="-fPIC" CPPFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure && \
    sudo make install && \
    cd ../hysia-0.1.1/src_cpp && \
    make && \
    cd ../src_ocaml && \
    patch < ../../Makefile.opam-4.02.3.patch && \
    PATH=/home/opam/.opam/4.02.3/bin:$PATH make hss && \
    /home/opam/.opam/4.02.3/bin/ocamlfind install hss META hss.o hss.cmxa && \
    cp -a hss.cmxs /home/opam/.opam/4.02.3/lib/hss/ && \
    cd ../eliom && \
    wget -q http://dygraphs.com/1.1.1/dygraph-combined.js -P static && \
    PATH=/home/opam/.opam/4.02.3/bin:$PATH LD_LIBRARY_PATH=/home/opam/.opam/4.02.3/lib/stublibs:$LD_LIBRARY_PATH make && \
    sudo make install && \
    mv static ../.. && \
    cd ../.. && \
    sudo apt-get remove -y wget && \
    rm -rf capd_dynsys && \
    rm capdDynSys-201406.zip && \
    rm -rf hysia-0.1.1 && \
    rm hysia-0.1.1.tgz
ENTRYPOINT ["/usr/bin/sudo", "/home/opam/.opam/4.02.3/bin/ocsigenserver.opt", "-V", "-c", "/usr/local/etc/hssweb/hssweb.conf"]
