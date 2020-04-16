Installation :
The program package consists of separate FORTRAN programs. They can be found in
de directory sources. To compile the programs the following environment variable
should be set:

  MOLFDIR_TARGET = <platform> (See the makefile.h file for the available platforms.)

If you want to compile the parallel MOLFDIR modules the following environment variable is required:

  MOLFDIR_PAR = y

LINUX installation:
The linux installation has been tested for Redhat and closely related flavors.

PARALLEL code :
Molfdir, relonel, reltwel, mfdscf, tmoone, rotran and relccsd are now also available
in a parallel version, using MPI for communication. When running parallel, each node 
will need the input file MOLFDIR.INP.   

ARGOS/ECP code :
We can now, using the ECP and spin-orbit integrals of ARGOS, to two-component 
AREP and REP calculations.
One can compile these routines by setting an environment variable ARGOSMAC to 
a value given in Makefile.config. After setting this variable do a 
"make argosecp" and the two codes will be compiled.
Run these codes in the order argos1e.x, cnvrt1e.x 
The input description of argos1e can be found in the directory Doc. 
It works with the default input file argosin. 
The cnvrt1e code uses the input file cnvrtin with one card " /".
A test example (Rn2) is given in the Test directory


PROPERTY code :
We have a code prtran.x that can compute expectation values for all kinds of
properties. Also our coupled-cluster code relccsd.x can perform linear 
response calculations using RPA.
We do NOT have a general integral code to compute the various property 
integrals. In order to compute various properties you need to have access
to the DIRAC code (http://diracprogram.org). 
When properties is set to .true. in the molfdir.x input, molfdir.x will
produce input files for the DIRAC code which then will compute the required
integrals.

Input description :
An input description of the package can be found in the directory 
InputDescription, in the following format : a MS-WORD text file, a postscript 
file. 


Sample run :
To check whether the installation run was succesful, one sample run is provided.
The input and output of this run can be found in the files carbon.molfdir.in and
carbon.molfdir.out.reference.
It consist of an average of configuration calculation on the carbon atom,
followed by COSCI and RASCI calculations on the lowest states (3P, 1D, 1S). 
Also the CCSD program can be tested.


Serial running of the program package :
Some scripts are available to run the standard program sequences. They can be
found in the directory scripts. To run them one needs to define an environment
variable MOLFDIR with as value the path to the main molfdir directory.
The scripts available are :
- molfdir1 :   runs a Dirac-Fock calculation.
- molfdir12a : runs a Dirac-Fock calculation, followed by a COSCI.
- molfdir12b : runs a Dirac-Fock calculation, followed by a RASCI.
- molfdir12c : runs a Dirac-Fock calculation, followed by a CCSD.
- molfdir2a :  runs only the COSCI step.
- molfdir2b :  runs only the RASCI step.
- molfdir2c :  runs only the CCSD  step.
The scripts expect a standard naming convention for input files : 
"molecule.molfdir.in" and write the output in the same directory to the file
"molecule.molfdir.out". 
They expect two or three parameters, the first two compulsory and specifying
the molecule and the scratchdirectory, respectively, a third optional and 
arbitrary. If no third parameter is given the scratch directory is deleted
after completion.
To run the sample calculation on carbon, consisting of a COSCI followed by a
larger RASCI calculation one first runs the script molfdir12a.

"molfdir12a carbon [scratchdir] 1".

The calculation is then restarted to do the RASCI calculation, using trial
vectors from the COSCI step. To do this step one first needs to comment out
the first EDIT card in the input file to account for the larger CI space.

"molfdir2b carbon [scratchdir]".


Basis Sets:
There are some basis sets available that were used in the published papers. 
These basis sets can be found in the directory basis.


Further information :
A detailed description of the package is published in:
Comp. Phys. Comm. 81, 120-144 (1994).


Copyright :
The main authors of the package are (in order of appearance)
P.J.C. Aerts, O. Visser, L. Visscher, H. Merenga, W. A. de Jong, and 
M. Pernpointer. 

