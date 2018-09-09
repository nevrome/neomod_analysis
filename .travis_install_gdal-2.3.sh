#!/bin/bash

wget http://download.osgeo.org/gdal/2.3.1/gdal-2.3.1.tar.gz 
tar -xzf gdal-2.3.1.tar.gz 
cd gdal-2.3.1
./configure
make
sudo make install
cd ..