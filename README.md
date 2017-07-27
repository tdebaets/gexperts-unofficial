Unofficial fork of GExperts 1.01 from the source code found on the [GExperts download page](http://www.gexperts.org/download/). This version was forked because it's the last version that supports Delphi 4.

Compared to the original source, the following custom changes were made. See the commit log for more details.

* Added postclone.bat script.
* Added compiler settings (search path, output directories) to Delphi 4 project to allow a quick initial compilation.
* Renamed the project to `GExperts-unofficial` because the license instructs custom versions of GExperts to be distributed under a clearly distinctive name.
* Made the `Multiline editor tabs` enhancement work in Delphi 4.
* Made the `Send OutputDebugString To GExperts` expert work in Delphi 4.
* New editor experts that allow using `Tab`/`Shift+Tab` for indenting/unindenting the selected block of code (in addition to the standard `Ctrl+Shift+I`/`Ctrl+Shift+U` shortcuts).
