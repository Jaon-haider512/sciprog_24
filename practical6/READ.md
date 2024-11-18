# Practical 6

This folder contains practical 6 codes that we performaed in live session.
## Compile:
* there is first code name file mutlimat.f90 which is for A and B and C that is calculated directly without creating function 
* To compile mutlimat.f90 use this  gfortran mutlimat.f90 -o ss2
## Execute 
* ./ss2
* This code is working fine giving correct results 
## Step6 in this practical6 to build a make for compliing the above procedures using function 
* This contains  folder two Fortran codes names as mm.f90 and main.f90 and a Makefile with name "MakefileF"
* This working fine as it it also conatain main.o,mm.o and functions.mod that are generarted after running through makefile
## Compile make
* First step is clean and we used this command " make clean -f MakefileF " This will remove objects file
* 2nd step is compiling by using the following command " make -f MakefileF "

