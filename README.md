hcprobe
=======

Introduction
=======

  HCProbe is a tool for testing OpenFlow controllers. It includes
  a library in Haskell that provides means to work with OpenFlow
  protocol. Also it includes a reference implementation of an OpenFlow
  software switch and a domain-specific language (EDSL) for constructing
  new custom switches.
  
Build
=======

  To build hcprobe you need cabal.
  
  To build it in a sandbox use cabal-dev tool:

    cabal insatall cabal-dev
  
  Building hcprobe with cabal-dev:
    
    cd src
    cabal update
    cabal-dev install-deps
    cabal-dev configure
    cabal-dev build
    
  Executables can be found in ./dist/build/
  
  To run the reference switch implementation:
    
    ./dist/build/hcprobe/hcprobe
    
  By default it runs 16 OpenFlow switches which try to connect
  to an OpenFlow controller at 127.0.0.1, port 6633.
  Run with --help for more settings.
  
  For more information on writing tests with EDSL see Documentation.

