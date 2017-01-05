FROM ocaml/ocaml:ubuntu-14.04
MAINTAINER Daisuke ISHII <dsksh@acm.org>
WORKDIR /root
RUN apt-get update && \
    apt-get install -y wget libboost-dev && \
    wget https://www.dropbox.com/s/3uf7t2nsizfebno/capdDynSys-201406.zip?dl=0 -O capdDynSys-201406.zip && \
    wget https://github.com/dsksh/hysia/archive/v0.1.tar.gz -O hysia-0.1.tgz && \
    unzip capdDynSys-201406.zip && \
    tar xvzf hysia-0.1.tgz && \
    cd capd_dynsys && \
    patch -p1 -u < ../hysia-0.1/tmp/capd_dynsys.20140612.patch && \
    CFLAG="-fPIC" CPPFLAGS="-fPIC" CXXFLAGS="-fPIC" ./configure && \
    make install && \
    cd ../hysia-0.1/src_cpp && \
    make && \
    cd ../src_ocaml && \
    ! make && \
    ocamlopt -w Aer-29 -dtypes -g -I . -o hss.opt str.cmxa unix.cmxa nums.cmxa dynlink.cmxa hss.cmxa main.cmx && \
    install -m 0755 ${PWD}/hss.opt /usr/local/bin/ && \
    cd ../.. && \
    apt-get remove -y wget && \
    rm -rf capd_dynsys && \
    rm capdDynSys-201406.zip && \
    rm -rf hysia-0.1 && \
    rm hysia-0.1.tgz
ADD hysia.sh .
ENTRYPOINT ["/bin/sh", "hysia.sh"]
#CMD ["-h"]
