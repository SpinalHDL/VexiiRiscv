FROM leviathanch/riscv-toolchain as build

COPY docker/sbt.asc /root/sbt.asc

RUN apt-get update
RUN apt install -y software-properties-common
RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update

# Add SBT repo
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list

RUN apt-key add /root/sbt.asc
RUN apt-get update
RUN apt-get install -y \
        openjdk-21-jdk \
        sbt \
        gtkwave \
        nodejs \
        npm \
        libgtk-3-0 \
        libxss1 \
        libnss3 \
        libx11-xcb1 \
        libcanberra-gtk3-module \
        xvfb \
        x11vnc \
        xfce4 \
        git \
        make \
        autoconf \
        g++ \
        flex \
        bison \
        help2man \
        device-tree-compiler \
        libboost-all-dev \
        dbus-x11 \
        sudo \
        wget \
        gosu

WORKDIR /root
RUN git clone https://github.com/verilator/verilator.git
WORKDIR verilator
RUN autoconf
RUN ./configure
RUN make -j2
RUN make install
WORKDIR /root
RUN rm -rf verilator

WORKDIR /root
RUN git clone https://github.com/serge1/ELFIO.git
WORKDIR ELFIO
RUN git checkout d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
RUN chmod 0644 -R elfio
RUN cp -R elfio /usr/include
WORKDIR /root
RUN rm -rf ELFIO

WORKDIR /root
RUN rm /root/sbt.asc
ENV DISPLAY=:1

COPY docker/setup_userspace.sh /home/ubuntu/setup_userspace.sh
COPY docker/fix_userid.sh /root/fix_userid.sh
COPY docker/init_graphics.sh /home/ubuntu/init_graphics.sh
COPY docker/Konata.desktop /usr/share/applications/Konata.desktop

COPY docker/setup_intellij.sh /root/setup_intellij.sh

RUN chmod 0777 /home/ubuntu/setup_userspace.sh \
    /root/fix_userid.sh \
    /home/ubuntu/init_graphics.sh

RUN gosu ubuntu /home/ubuntu/setup_userspace.sh

WORKDIR /root
RUN bash /root/setup_intellij.sh
RUN rm /root/setup_intellij.sh
RUN mkdir -p /home/ubuntu/Desktop
RUN ln -s /usr/share/applications/IDEA.desktop /home/ubuntu/Desktop/IDEA.desktop
RUN ln -s /usr/share/applications/Konata.desktop /home/ubuntu/Desktop/Konata.desktop

COPY docker/generate_verilog.sh /home/ubuntu/
COPY docker/run_simulation.sh /home/ubuntu/

RUN chown ubuntu /home/ubuntu/*.sh
RUN chmod 0777 /home/ubuntu/*.sh

WORKDIR /work
ENTRYPOINT /root/fix_userid.sh && gosu ubuntu /home/ubuntu/init_graphics.sh
