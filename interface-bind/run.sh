#!/bin/bash

\rm -f *.o *.mod

gfortran -c field_2rm_util_module.F90 
gfortran -c field_3rm_util_module.F90 
gfortran -c field_util_module.F90 

