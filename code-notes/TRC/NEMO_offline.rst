.. _NEMO-offline:

**************************************
Running NEMO Offline using 5-day Files
**************************************

NEMO is designed to use yearly, monthly, weekly or daily files but Paul Myers' group is producing five-day (fday) files.

We have re-written daymod.F90, dom_oce.F90 and fldread.F90 to handle fday, non-climatology files.  Also see the namelist.  All the files are commited as CindyOff in MY_SRC and EXP00.

To run with fday files you need to:
1) make unspecified data files.  NEMO uses these to look at how many variables are in each type of file.  You can make these by linking to any specific time file.  So, for example,

.. code-block:: bash

    ln -s ANHA4-EXH001_gridT.nc ANHA4-EXH001_y2010m01d05_gridT.nc

You need a gridT, gridU, gridV, gridW and icemod file.

2) reorganize the order in the naming of the files: the date needs to come last
AND
3) the dates need to be the beginning of the five days, not the end. So for example:

.. code-block:: bash

    ln -s ANHA4-EXH001_gridT_y2010m01d01.nc ANHA4-EXH001_y2010m01d05_gridT.nc


