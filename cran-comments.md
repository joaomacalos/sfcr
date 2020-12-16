## Resubmission
This is a resubmission. In this version I have:

* Added more details about the package functionality and implemented
methods in the Description text.

* Added references in the Description text.

* Fixed a bug in the `sfcr_matrix_display()` function.

* Added a validity check to the `sfcr_baseline()` function.

* Added a new unit test to the `sfcr_baseline()` function.

All tests and checks work as before.


## Test environments
* local Ubuntu 20.04.1 LTS, R 4.0.3
* local Windows 10 x64, R 4.0.3
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2

## R CMD check results (Local Ubuntu 20.04.1)
There were no ERRORs or WARNINGs or NOTEs.

## R CMD check results (Local Windows 10x64)
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking compiled code ... NOTE
  Note: information on .o files for i386 is not available
  Note: information on .o files for x64 is not available
  File 'C:/Users/JouJo/AppData/Local/Temp/RtmpmaeNI4/sfcr.Rcheck/sfcr/libs/i386/sfcr.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  File 'C:/Users/JouJo/AppData/Local/Temp/RtmpmaeNI4/sfcr.Rcheck/sfcr/libs/x64/sfcr.dll':
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
    
  Appears to be a false positive on Windows machines with R 4.0.3 or higher
  and using Rtools40 as discussed here: https://stackoverflow.com/a/64419033/7705000
  
## First release
This is the first release of this package.
  
