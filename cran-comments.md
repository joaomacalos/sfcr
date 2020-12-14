## Test environments
* local Windows 10 x64, R 4.0.3
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE on Windows 10 x64:

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
    
  Appears to be a false positive appearing only on Windows machines with R 4.0.3 or higher
  and using Rtools40 as discussed here: https://stackoverflow.com/a/64419033/7705000
  
