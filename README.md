Unofficial fork of GExperts 1.01 from the source code found on the [GExperts download page](http://www.gexperts.org/download/). This version was forked because it's the last version that supports Delphi 4.

Compared to the original source, the following custom changes were made. See the commit log for more details.

* Added `postclone.bat` script.
* Added compiler settings (search path, output directories) to Delphi 4 project to allow a quick initial compilation.
* Renamed the project to `GExperts-unofficial` because the license instructs custom versions of GExperts to be distributed under a clearly distinctive name.
* Made the `Multiline editor tabs` enhancement work in Delphi 4.
* Made the `Send OutputDebugString To GExperts` expert work in Delphi 4.
* New editor experts that allow using `Tab`/`Shift+Tab` for indenting/unindenting the selected block of code (in addition to the standard `Ctrl+Shift+I`/`Ctrl+Shift+U` shortcuts).
* For units not part of the current project, the `Project Dependencies` expert resolves their filename on the library path.
* Added an option to the `To Do List` expert to, when scanning units for TODOs, include **all** units in the project file's uses clause (rather than just the units that are explicitly part of the project).
* New `Show in Explorer`command when right-clicking in Delphi's Code Editor, to select the current file in Windows File Explorer.
* Fix for access violations in `bordbk40.dll` on Windows 10 during debugging (disabled by default, requires a registry edit to enable). This fix is implemented as an additional feature of the `Send OutputDebugString To GExperts` expert.

Obtaining the source code
-------------------------

First make sure that you have a recent version of the [Git client](https://git-scm.com/) (`git`) installed. Then open a Windows command prompt window (note that Git Bash isn't supported). In the command prompt, run these commands:
```
> git clone https://github.com/tdebaets/gexperts-unofficial.git gexperts-unofficial
> cd gexperts-unofficial
```

Finally, run the `postclone.bat` script. This will take care of further setting up the repository, creating output directories etc.:
```
> postclone.bat
```

To keep your repository up-to-date, run the `git pull` command.

Building
--------

Borland Delphi 4, including all four of its update packs, is required to build this project. Other versions of Delphi may or may not work but are unsupported.

There's no command-line compile script (yet?), so currently the only way to compile the project is to open it in the Delphi IDE and compile it there.

Installing
----------

There's no dedicated installer for this project, so it's recommended to install the official GExperts 1.01 for Delphi 4 first, which you can download [here](http://www.gexperts.org/download/#GX101) (`GX4-101.exe`). After installation, head to GExpert's installation folder (usually `C:\Program Files (x86)\GExperts`), and replace the `GExpert4.dll` file with the GExperts-unofficial one (which you can find, after successful compilation, in the `Output` folder of the repository).
