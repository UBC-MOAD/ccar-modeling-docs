.. _NEMO-code:

********************
NEMO-code Repository
********************

These notes describe the GEOTRACES Arctic project.  This project is currently using the same NEMO code base as the Salish Sea MEOPAR `NEMO-code`_ repository

The `NEMO-code`_ repo is a Mercurial repository in which is maintained the merger of the trunk of the main NEMO :command:`svn` repository and the changes made by the Salish Sea MEOPAR project team.

.. note::

    The `NEMO-code`_ repository is a private repository for members of the Salish Sea MEOPAR project team and associated teams: GEOTRACES Arctic included.
    That is because it contains parts of the NEMO_ codebase.
    Although that codebase is openly licensed it's developers require registration_ to access the code.

    If you have completed that registration and would like access to the `NEMO-code`_,
    please contact `Susan Allen`_,
    the Salish Sea MEOPAR project leader.

    .. _NEMO: http://www.nemo-ocean.eu/
    .. _registration: http://www.nemo-ocean.eu/user/register
    .. _Susan Allen: mailto://sallen@eos.ubc.ca


Getting the Code
================

Team members using SSH key authentication on Bitbucket may clone the `NEMO-code`_ repo with:

.. code-block:: bash

    hg clone ssh://hg@bitbucket.org/salishsea/nemo-code NEMO-code

For password authentication use:

.. code-block:: bash

    hg clone https://<you>@bitbucket.org/salishsea/nemo-code NEMO-code

where :kbd:`<you>` is your Bitbucket user id.


Configurations
==============

Making Gyre-Lobster with 2 Extra Tracers
----------------------------------------

To create a this new configuration based on, `GYRE_LOBSTER`_ use:

.. _GYRE_LOBSTER: http://www.nemo-ocean.eu/Using-NEMO/Configurations/GYRE_LOBSTER

.. code-block:: bash

    cd NEMO-code/NEMOGCM/CONFIG
    ./makenemo -r GYRE_LOBSTER -n YourTrcGyreLobster -m ocean add_key "key_netcdf4 key_nosignedzero key_my_trc"

That will use the existing :kbd:`GYRE_LOBSTER` configuration as a basis to build a new configuration called :kbd:`YourTrcGyreLobster` with the :kbd:`ocean` architecture definitions.
The C Pre-Processor (CPP) keys :kbd:`key_netcdf4` and :kbd:`key_nosignedzero` will be added to configurations.  The key :kbd:`key_my_trc` added the code in TOP_SRC/MY_TRC to the compile.
The resulting configuration,
including a compiled and link NEMO executable,
is located in :file:`NEMO-code/NEMOGCM/CONFIG/YourTrcGyreLobster`.

See :command:`./makenemo -h` for details of options and sub-commands.


Running the Model
=================

For now, we will run the model in the :kbd:`EXP00` directory.  In future, once we have stabilized a few model configurations, we will move to running outside.

Inside :kbd:`EXP00` there are two namelist files: :kbd:`namelist_lobster` and :kbd:`namelist_top`, two output definition files: :kbd:`iodef.xml` and :kbd:`xmlio_server.def` and three links.

By including the :kbd:`key_my_trc` flag we have added two tracers but have not defined them in the io file.  We need to add four lines.

.. code-block:: xml

        <field id="TR_7"     description="Northern Source"                               unit="none" />
        <field id="TR_8"     description="Southern Source"                               unit="none" />
        <field ref="TR_7"     />
 	<field ref="TR_8"     />

To get these lines and put them in the correct place, copy into your :kbd:`EXP00` the :kbd:`iodef.xml` from the configuration :kbd:`MyTrcGyreLobster`

.. code-block:: bash

	cd YourGyreLobster/EXP00
	cp ../../MyTrcGyreLobster/EXP00/iodef.xml .

We also need to add these tracers to :kbd:`namelist_top` to initialize them

.. code-block:: fortran

    sn_tracer(7)   = 'TR_7'  , 'Southern Source            ',  'none      ' ,  .false.     ,  .false.
    sn_tracer(8)   = 'TR_8'  , 'Northern Source            ',  'none      ' ,  .false.     ,  .false.

To get these lines and put them in the correct place, copy into your :kbd:`EXP00` the :kbd:`namelist_top` from the configuration :kbd:`MyTrcGyreLobster`

.. code-block:: bash

	cp ../../MyTrcGyreLobster/EXP00/namelist_top .

In addition we need to modify two of the fortran codes.  First we need a version of :kbd:`trcnam_trp.F90` that does not assume tracer damping has been set. Files that are changed from the base configuration go in your :kbd:`MY_SRC` directory.

.. code-block:: bash

	cd ../MY_SRC
	cp ../../MyTrcGyreLobster/MY_SRC/trcnam_trp.F90 .

Second, the generic tracer source sink algorithm put the tracers into the Pacific... but our simulation is the Atlantic.  We need a different :kbd:`trcsms_my_trc.F90`.  This is also the file you should edit to simulate your traces of choice.

.. code-block:: bash

        cp ../../MyTrcGyreLobster/MY_SRC/trcsms_my_trc.F90 .

Now we need to remake the code.  Go back upto CONFIG and run:

.. code-block:: bash

   cd ../../
   ./makenemo -n YourTrcGyreLobster

Then we can run the code by going back into EXP00 and typing

.. code-block:: bash

    cd YourTrcGyreLobster/EXP00
    nice ./opa &

After a good little while, you will see

.. code-block:: bash

    namelist read -->  F F nemo.x                                                                                              ionemo                                                                                              
    filename : iodef.xml
    Le parsing est termine !!! 
    trc_rst_wri_my_trc: No specific variables to write on unit           1  at time         4318        4320
    trc_rst_wri_my_trc: No specific variables to write on unit           1  at time         4319        4320
    trc_rst_wri_my_trc: No specific variables to write on unit           1  at time         4320        4320

and then your job is done.  Results from the tracers are in: :kbd:`GYRE_5d_00010101_00011230_ptrc_T.nc`

you can look at this using a notebook,  An example is at:

:kbd:`/ocean/sallen/allen/research/MEOPAR/NEMO-code/NEMOGCM/CONFIG/MyTrcGyreLobster/EXP00/LookAtTracers.ipynb`
