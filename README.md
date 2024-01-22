# VexiiRiscv

VexiiRiscv is a from scratch second iteration of VexRiscv. Here are the targets : 

- RISCV 32/64 bits IMAFDC
- Could start around as small as VexRiscv, but could scale further in performance
- Optional late-alu
- Optional multi issue
- Optional multi threading
- Cleaning implementation, especially the frontend
- ...

There is a online documentation : 

- https://spinalhdl.github.io/VexiiRiscv-RTD/master/VexiiRiscv/Introduction/VexiiRiscv.html

# Dependencies

```shell
# JAVA JDK
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-19-jdk -y # You don't exactly need that version
sudo update-alternatives --config java
sudo update-alternatives --config javac

# Install SBT - https://www.scala-sbt.org/
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update
sudo apt-get install sbt

# Verilator (optional, for simulations)
sudo apt-get install git make autoconf g++ flex bison
git clone http://git.veripool.org/git/verilator   # Only first time
unsetenv VERILATOR_ROOT  # For csh; ignore error if on bash
unset VERILATOR_ROOT  # For bash
cd verilator
git pull        # Make sure we're up-to-date
git checkout v4.216 # You don't exactly need that version
autoconf        # Create ./configure script
./configure
make
sudo make install

# Getting a RISC-V toolchain (optional)
version=riscv64-unknown-elf-gcc-8.3.0-2019.08.0-x86_64-linux-ubuntu14
wget -O riscv64-unknown-elf-gcc.tar.gz riscv https://static.dev.sifive.com/dev-tools/$version.tar.gz
tar -xzvf riscv64-unknown-elf-gcc.tar.gz
sudo mv $version /opt/riscv
echo 'export PATH=/opt/riscv/bin:$PATH' >> ~/.bashrc

# RVLS / Spike dependencies
sudo apt-get install device-tree-compiler libboost-all-dev
# Install ELFIO, used to load elf file in the sim 
git clone https://github.com/serge1/ELFIO.git 
cd ELFIO
git checkout d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
sudo cp -R elfio /usr/include
cd .. && rm -rf ELFIO
```

# Setup

```shell
git clone --recursive https://github.com/SpinalHDL/VexiiRiscv.git
cd VexiiRiscv

# (optional) Compile riscv-isa-sim (spike), used as a golden model during the sim to check the dut behaviour (lock-step)
cd ext/riscv-isa-sim
mkdir build
cd build
../configure --prefix=$RISCV --enable-commitlog  --without-boost --without-boost-asio --without-boost-regex
make -j$(nproc)
cd ../../..

# (optional) Compile RVLS, (need riscv-isa-sim (spike)
cd ext/rvls
make -j$(nproc)
cd ../..
```

# Generate Verilog

It's currently very very early, but you can run the generation via : 

```shell
sbt "Test/runMain vexiiriscv.Generate"
```

You can get a list of the supported parameters via :

```shell
sbt "Test/runMain vexiiriscv.Generate --help"
```


# Run a simulation

Note that Vexiiriscv use mostly an opt-in configuration. So, most performance related configuration are disabled by default.

```shell
sbt
compile
Test/runMain vexiiriscv.tester.TestBench --load-elf ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf --trace-all
```

This will generate a simWorkspace/VexiiRiscv/test folder which contains :
- test.fst : A wave file which can be open with gtkwave. It shows all the CPU signals
- konata.log : A wave file which can be open with https://github.com/shioyadan/Konata, it shows the pipeline behaviour of the CPU
- spike.log : The execution logs of Spike (golden model)
- tracer.log : The execution logs of VexRiscv (Simulation model)

# Navigating the code

Here are a few key / typical code examples : 

- The CPU toplevel src/main/scala/vexiiriscv/VexiiRiscv.scala
- A cpu configuration generator : dev/src/main/scala/vexiiriscv/Param.scala
- Some globaly shared definitions : src/main/scala/vexiiriscv/Global.scala
- Integer ALU plugin ; src/main/scala/vexiiriscv/execute/IntAluPlugin.scala
- A plugin which probe the CPU at different points for simulation purposes : src/main/scala/vexiiriscv/misc/WhiteboxerPlugin.scala
