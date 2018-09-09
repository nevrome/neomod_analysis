#!/bin/bash

mkdir ~/.R
touch ~/.R/Makevars
echo "CC=gcc-4.8 -std=gnu99" >> ~/.R/Makevars
echo "CXX=g++-4.8 -std=gnu++11" >> ~/.R/Makevars
echo "PKG_CXXFLAGS= -std=c++11" >> ~/.R/Makevars
