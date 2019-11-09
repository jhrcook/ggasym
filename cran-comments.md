# 'ggasym' 0.1.2

submitted November 09, 2019

## Test environments
* (developed on) OS X Mojave install, R 3.6.1 (2019-07-05)
* Ubuntu 16.04.6 LTS (on Travis-CI), R 3.6.1 (2017-01-27)
* Windows Server 2012 R2 x64 (build 9600) (on Appveyor), R 3.6.1 Patched (2019-11-07 r77386)

## R CMD check --as-cran results
There were no ERRORs, WARNINGs, or NOTEs.

## Changes

In this version I added a vignette showing how this package, 'ggasym', works great with 'corrr'.
I also updated the tests to adjust for the version of 'scales' the user has. 
'scales' is getting updated and the gradient calculations have changed slightly.
