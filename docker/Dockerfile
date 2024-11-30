FROM leviathanch/riscv-toolchain as build

COPY docker/sbt.asc /root/sbt.asc

RUN apt-get update
RUN apt install -y software-properties-common
RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update
RUN apt-get install -y openjdk-21-jdk

# Install SBT - https://www.scala-sbt.org/
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN apt-key add /root/sbt.asc
RUN apt-get update
RUN apt-get install -y sbt

# Verilator (optional, for simulations)
RUN apt-get install -y git make autoconf g++ flex bison help2man
RUN git clone https://github.com/verilator/verilator.git
WORKDIR verilator
RUN autoconf
RUN ./configure
RUN make -j2
RUN make install
WORKDIR ..

# RVLS / Spike dependencies (optional, for simulations)
RUN apt-get install -y device-tree-compiler libboost-all-dev
# Install ELFIO, used to load elf file in the sim
RUN git clone https://github.com/serge1/ELFIO.git
WORKDIR ELFIO
RUN git checkout d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
RUN cp -R elfio /usr/include
WORKDIR ..
RUN rm -rf ELFIO

WORKDIR /root
RUN rm -rf verilator
RUN rm /root/sbt.asc

WORKDIR /root
RUN git clone  --recursive https://github.com/SpinalHDL/VexiiRiscv.git
WORKDIR VexiiRiscv
RUN sbt update
WORKDIR /root
RUN rm -rf VexiiRiscv

#WORKDIR /root
#RUN git clone https://github.com/SpinalHDL/VexiiRiscv.git

#WORKDIR /root/VexiiRiscv
#RUN git submodule update --init --recursive

#WORKDIR /root/VexiiRiscv/ext/riscv-isa-sim
#RUN mkdir build
#WORKDIR /root/VexiiRiscv/ext/riscv-isa-sim/build
#RUN ../configure --prefix=/opt/riscv --enable-commitlog  --without-boost --without-boost-asio --without-boost-regex
#RUN make -j2

#WORKDIR /root/VexiiRiscv/ext/rvls
#RUN make -j2

WORKDIR /work


