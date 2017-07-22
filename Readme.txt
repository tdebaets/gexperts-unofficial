GExperts 1.01
Open Source Programming Tools for Delphi and C++Builder

Source code and the latest news are available at:
  http://www.gexperts.org/

Please send bug reports and comments to:
  Erik Berry <eberry@gexperts.org> or <eb@techie.com>


INTRODUCTION
----------------------
  GExperts is a set of tools built to increase the productivity of
Delphi and C++Builder programmers by adding several features to the
IDE.  GExperts is developed as Open Source software and is contributed
as freeware to the development community.  Users are encouraged to
download the source code and submit bug fixes and patches as well as
new features for inclusion in the GExperts distribution.

  Gerald Nunn was the principal author of GExperts for almost two
years.  Gerald has since moved on to other projects, but he kindly
donated the GExperts source code the online community so we
can continue to enhance and maintain it.  Thanks Gerald!


INSTALLATION
----------------------
  GExperts is distributed as a self-installing executable (Setup.exe)
that should automatically install the expert library into the IDE.
Before executing this file, though, you should manually uninstall any
older versions of GExperts.  If the GExperts menu does not appear the
next time you start your IDE, you can install GExperts manually by
setting up a new key using the registry editor (RegEdit.exe).  Just
create the following key:
HKEY_CURRENT_USER\Software\Borland\Delphi\X.0\Experts\ or
HKEY_CURRENT_USER\Software\Borland\C++Builder\X.0\Experts\

Then add a new string value that points to the GExperts DLL:
GExperts=C:\Program Files\GExperts\GExpertX.dll or
GExperts=C:\Program Files\GExperts\GExpertX_bcb.dll

  If you experience problems loading GExperts, it is likely due to
use of an unsupported version of Delphi or C++Builder.  Please
upgrade to the following versions:
  - Delphi 3.02
  - Delphi 4.03 (Update Pack 3)
  - Delphi 5.01 (Update Pack 1)
  - C++Builder 3.01
  - C++Builder 4.02 (Update Pack 2)
  - C++Builder 5.01 (Update Pack 1)

using the free patches available from:
  http://www.borland.com/downloads/


UNINSTALLATION
----------------------
  If you wish to uninstall the Delphi 3 or C++Builder 3 version of
GExperts and you have enabled the Single Instance IDE expert (it is
off by default), you must first unregister this expert using the
Single Instance IDE configuration dialog accessible from the GExperts
Configuration menu item.
  You should also undock all GExperts windows before uninstallation,
or the IDE might temporarily leave a blank area where the docked expert
used to be on the next IDE restart.
  Uninstallation of GExperts can be done from the Control Panel's
Add/Remove Programs Applet.


THE EXPERTS
----------------------
  GExperts contains numerous experts.  See the online help
(GExperts.hlp) for information on the usage of the experts.


CHANGE LOG
----------------------
VERSION 1.01 (May 19, 2001)
 - General: All known bugs are fixed
 - Clean Directories: Recursive deletion was not working under Delphi 3
 - Clipboard History: Coexists with other clipboard history applications
 - Code Librarian: Fixed a possible AV when using "Contract All"
 - Replace Components: Works for data modules now
 - Grep Search: Disabled the "Ignore comments" option until it is fixed
 - PE Information: Allow clipboard copying of the "Exports" tab
 - Project Option Sets: Fixed option filtering/sorting problems


VERSION 1.00 (February 19, 2001)
 - General: All of the experts except the Code Proofreader can be
   dynamically enabled and disabled from the configuration dialog.
   Modified docked window menus to only activate when the docked form
   contains the currently focused control.  GExperts menu items can
   now be placed on the IDE's toolbar. Menu shortcuts are no longer
   lost when loading a project group.
 - Editor Experts: Fixed a possible range check error in the
   delimiter experts when the cursor was near the top of a unit.
   Comment and uncomment experts work around a common IDE AV. Procedure
   header has two new macros: %RESULT% and %ARGUMENTS%.
 - Perfect Layout: Improved the results when the Object Inspector
   is docked into the edit window.  Loads data from the right registry
   key (Thomas Mueller).
 - Grep Results: Fixed some minor problems opening and setting focus
   to DFM files when jumping to a grep match.  It still isn't perfect.
   Fixed a possible memory leak.
 - Replace Components: Works with modules outside the current project.
 - Editor Toolbar: Improved sizing and positioning with respect to
   docked windows.
 - Code Proofreader: The '|' character can be used in AutoCorrect
   entries to determine the cursor position after a correction.
 - Code Librarian: Double-clicking a code snippet node copies the text
   into the code editor and closes the Code Librarian.
 - To Do List: A cosmetic problem left the to do entry's displayed line
   number off by one.  Now supports jumping to one of multiple to do
   entries with the exact same comment text.
 - Class Browser: Works better with method resolution clauses (method
   redirections), multiple interface inheritance, non-standard calling
   conventions, multiple variable declarations on a single line,
   property overrides, non-standard line endings, and is generally more
   accurate when jumping to or viewing the code for class members.
 - Favorite Files: Projects, project groups, and packages will by default
   try to close the current project before opening themselves.
   Splitter position is saved/restored correctly.  The modified flag is
   set after editing the file list.
 - Project Option Sets: Fixed an issue preventing setting environment
   options that were comma delimited strings.
 - Backup Project: Reports files it is unable to find or open.
 - Help File: Updated to reflect the latest changes.


VERSION .98 (Feb. 17, 2000)
- Editor Toolbar: Fixed component/form list toolbar buttons under Delphi 5.
- Clipboard History: works correctly under Delphi 3.
- Component Grid: Curt Krueger added automatic numbering of
  context IDs and printing.
- PE Information: ArentJan Banck added drag and drop support and
  updated the internals of this and several other experts.
- Procedure List: Scott Mattes submitted changes to allow easier
  searching and filtering based on objects.  Works inside Object
  Pascal .inc files now. John Hansen and Stefan Hoffmeister fixed
  focus problems with multiple edit windows open.
- Components to Code: Primoz Gabrijelcic added this new expert which
  allows one to select several components and copy the appropriate
  code to the clipboard which would create those same components at
  runtime.  Note it does not work with some properties like bitmaps.
- Project Option Sets: Stefan Hoffmeister added environment options.
- Clean Directories: Stefan Hoffmeister added recursive scanning.
  Wildcard matching support was added.
- Grep Results: Lee Wenger added refresh capability.
- Class Browser: ArentJan Banck added the ability to print the
  reports without requiring Ace Reporter.
- Code Proofreader: Configuration option to allow matching words based
  on the compiler's list of best matches (Python).
- Source Export: ArentJan Banck completely rewrote this expert.
- Stefan Hoffmeister added IDE current directory protection, implemented
  better native IDE docking, improved delimiter matching, as well as
  several other internal cleanups.

VERSION .97.1 (September 14, 1999)
- Loading toolbar settings from very old versions of GExperts under
  Delphi 3 no longer causes problems.
- Fixed potential AVs when opening batch files and closing with the
  package window open (OpenTools bug workarounds).
- Message view window under Delphi 5 behaves normally with editor
  integration enabled.
- Fixed saving/loading of the Clean Directories settings.
- Opening TLB files no longer raises assertions.

VERSION .97.0 (August 26, 1999)
- To Do List: Recently scanned directories are saved (John Hansen)
- Code Proofreader: The configuration settings are now saved correctly.
  The detection of the current syntax element is more accurate.  Lots of
  other internal changes and fixes (Stefan Hoffmeister).
- Editor Experts: The debug statement expert was removed.  Use an IDE Code
  Template instead for this functionality.
- Editor Enhancements: Several editor enhancements were added to the
  configuration dialog (John Hansen, Gerald Nunn, and Stefan Hoffmeister).
  This includes the cut/copy/paste menu options (see MISCELLANEOUS NOTES
  below) and the editor toolbar.  Daniel Work submitted code to sort the
  component list button's menu items.  The toolbar configuration listbox
  supports auto-scrolling drag and drop.
- IDE Enhancements: New IDE enhancements include flat/3D component palette
  buttons for version 4+ IDEs.  Under Delphi 4 only, you can show the
  "Attach to Process..." menu item.  Remco van Toor submitted some ideas
  to make docked tab controls multiline, and Stefan integrated this feature
  into GExperts.  You can automatically save your DFMs as text files, to
  better integrate into version control systems.
- Priority Booster: This new expert by Stefan Hoffmeister allows you to
  set the Win32 priority for the application/thread while compiling.
- IDE Docking: Support for IDE docking under version 4+ IDEs for certain
  experts.  Many thanks to Python for his hard work with this task, and
  Stefan Hoffmeister for testing and ideas.
- Help File: The new help file is finished.  Thanks to Paul O'Rear for
  layout and technical assistance.
- Project Option Sets: This new expert by John Hansen allows Delphi 4+
  users to save off sets of project options and recall them later.
  It stores the saved data in an OLE Structured Storage file.  Stefan
  Hoffmeister enhanced this expert as well.
- Backup Project: Now scans for $INCLUDE and $RESOURCE directives, as well
  as $I and $R.  Added a project group backup option for those IDEs that
  support project groups and support for wildcards to add and remove files
  (Stefan Hoffmeister).
- Source to HTML: ArentJan Banck enhanced this expert to support copying
  to the clipboard in several formats.
- Replace Components: Replacing across all forms in a project works again
- Clipboard History: The clipboard history is persistent across IDE
  sessions in Delphi 4+ (John Hansen).
- General: Python helped restructure the menu code to be more modular.
- Class Browser: Reworked the class report (ArentJan Banck) and
  hierarchy diagram.

VERSION .96.2 (April 12, 1999)
- IDE Enhancements: Began adding back the many of the old enhancements
  such as tabs as buttons, tab names in the popup menu, custom fonts for
  the component palette and object inspector, and a MDI-like Windows menu.
  Python submitted code to load and unload IDE enhancements at runtime.
- Editor Experts: These experts are all back in GExperts now, and are
  EII compatible (see http://www.toolsfactory.com/eii.html).  You can see
  and configure these experts from the configuration dialog.  Fixed an
  occasional EII crash when dragging and dropping DPR files.
- Grep Results: Better keyboard support, and support for drag and drop of
  files from the results window onto to an icon or a running application
  (notepad, etc.).  Fixed a harmless exception when searching an unsaved
  project.
- Code Librarian: Hides itself when you paste a snippet into the editor
- Hook OutputDebugStr: This expert was added back for Delphi 3, thanks
  to ArentJan Banck and Stefan Hoffmeister.
- Code Proofreader: Minor bugfixes and cleanup to the code.  It sometimes
  misdetected when you were editing inside comments.  Stefan Hoffmeister
  and Alex Petrov fixed most of this.  The default dictionary word list
  was expanded to include more common words.  You can now import a text
  file into the dictionary or export the dictionary to a text file.
  Francois Sorrentino added international keyboard support.  The
  correction history is no longer stored in an exclusive access Paradox
  database to facilitate running multiple IDEs running at once, and when
  this expert is disabled, the database is no longer opened.
- Procedure List: Fixed an AV parsing implementation procedural types and
  procedures with embedded tabs (John Hansen).
- To Do List: Multi-line to do items were not parsed correctly and were
  shown as blank lines.  Now, the first line of the comment is displayed.
  Fixed a bug parsing unterminated comments at the end of a unit.  New
  option to keep the to do window visible after jumping to an item.  John
  Hansen submitted code to scan directories or only the open project
  files for to do items.  You can now copy the to do list to the
  clipboard using Ctrl+C and the expert now supports keyboard commands.
- Clean Directories: You can now add any number of custom extensions to
  be cleaned upon execution (Stefan).
- General: Minor consistency/internal changes to many experts.  Stefan
  enhanced C++Builder support for many experts.
- OneDForAll: Back in Delphi 3 and BCB 3.  It allows you to restrict
  the IDE to one instance when you execute associated files.
- Favorite Files: You can drag and drop files into the favorite files
  manager from explorer.  The drag and drop routines come from Angus
  Johnson, Anders Melander, and Graham Wideman.  You can turn off the
  file preview pane in the options and the folder tree saves its state
  between runs.  Drag and drop inside the form from files to folders
  and from folders to folders works again.
- Expert Manager: Stefan made some internal changes and enhanced the
  interface.  Several minor bugs were fixed.
- ASCII Chart: The hint window can be turned off (Frank Zimmerman)

VERSION .96.1 (Jan 23, 1999)
- General: With lots of help from Stefan Hoffmeister, the source code
  compiles and works in Delphi 3 now, and partially works in BCB.
  Stefan also corrected several minor code problems and moved most of
  the literal strings into ResourceStrings for future localization
  purposes.
  Scott Mattes submitted several minor usability changes/suggestions.
  All conditional defines were moved to GX_CondDefine.inc.
- Edit Writer: Fixed a minor bug that sometimes placed the inserted
  text at the wrong position (affected MessageDlg, and Code Librarian)
- Class Browser: This expert was re-added to GExperts.  It compiles
  both with and without TSyntaxMemo, and several bugs were fixed
  by Stefan and Erik.  It correctly parses methods that are
  reintroduced, overloaded, contain out parameters, etc.
- Procedure List: The display font is now configurable, and the
  search text is highlighted.  This expert should load faster
  because it now uses Martin Waldenberg's new Pascal parser.
  Minor inconsistencies in the statusbar text are fixed.
  You can copy the procedure list to the clipboard with a new button.
- Message Dialog: Stefan added BCB support to this expert.
- Grep: Fixed a memory leak in print preview.  Added an option to
  use (slower) ANSI compatible case insensitive searching.
- Menu: the menu options now appear in the same order they used to
  with the older Delphi 3 versions
- Favorite Files: This expert will compile without TSyntaxMemo,
  but you don't get syntax highlighting.
- PE Information:  Several clipping and spacing problems in the PE
  printout were fixed.  Hex mode is active for import ordinals.
  Stefan Hoffmeister submitted a couple of bugfixes and some great
  code to speed up reading the import/export list.
- Debug Window: You can now copy selected lines to the clipboard,
  save the log to a file, or double-click an item to see it in
  a message dialog.
- Code Librarian: Compiles with/without TSyntaxMemo.  The find and
  find next functions now work properly.
- To Do List:  This expert has been added and enhanced.  Part of
  the code comes from John Hansen <John_Hansen@tcsmgmt.com>.
  You can add comments to your code of the form: {#todo[123] xxx},
  and the to do expert will find these and list them in a list, by
  unit.  You can place to do items anywhere in your projects.
  There is also a new to do keyword editor by ArentJan Banck where
  you can add your own keywords to the list that are scanned for,
  and assign them priorities; then you can sort by any list column.
- Perfect Layout: Scott Mattes submitted changes to make the
  custom layout save/restore windows correctly.
- Printing: GExperts has obtained a registered version of ACE Reporter.
  Thanks to Steve Tyrakowski for his donation.
- TSyntaxMemo: GExperts only compiles with TSyntaxMemo 2.01.22 and up
- Code Proofreader: This is another new expert that corrects your
  typing mistakes while inside the IDE code editor.  It allows you
  to use Microsoft Word style AutoCorrect, fix capitalization, correct
  words that are missing or have one extra character in them, and much
  more.  Alex Petrov was kind enough to donate this code to GExperts.

VERSION .96 (October 19, 1998)
- Clean Directories: Fixed a bug that prevented correctly calculated
  deleted file sizes and counts.  Also fixed a bug where manually added
  directories were not cleaned and errors during deletion were not
  reported.  Added a few more cleanable file types.
- Favorite Files: Bug prevented display of the currently selected fonts
- Grep Results: Toolbar buttons were enabled/disabled incorrectly
- Removed the "GExperts Loaded" addition to the Delphi splash screen
- Clipboard History: You can double-click an item in the list to quickly
  copy it to the clipboard.
- Keyboard Shortcuts: Fixed a rare bug that appeared when trying to set
  shortcuts for menu items created only after a project is loaded
- Favorite Files: Both splitters save their positions.  Syntax
  highlighting works for PAS, DPR, SQL, C, CPP, HTML files.  Other
  minor tweaks.
- Menu: Several of the menu items have different shortcuts, since Delphi
  4 reserves new keys in the code editor.  Customizing the shortcuts is
  still possible using the configuration.  The main GExperts menu
  accelerator was changed to Alt-X, because Alt-G is mapped to Goto Line
  in the default keyboard layout.  Note: Brief and Classic modes use
  Alt-X for File-Exit (I'm not sure how to work around this).
- ASCII Chart: You can press ESC to close the window
- Updated the Pascal parser to use Martin Waldenburg's very fast
  mPasLex 1.3.  Thanks, Martin!
- Procedure List: Correctly saves the selected search mode and uses
  the more standard clWindow and clWindowText color scheme
- Project Dependencies: You can double-click a file that is in the
  current project and Delphi will load it.  Also, the tree layout has
  changed, making more obvious which units are in the project, and which
  have associated forms.  There is a new configuration option to scan the
  entire units for uses clauses, or just scan for the first two.  This is
  useful when you have multiple IFDEFed uses clauses.  Sorting the
  information is now possible by clicking the column headers.
- General: Changed the form centering code to account for the taskbar
  and other system appbars such as the Office toolbar.  Renamed the TFile
  and TFiles classes to TGXFile and TGXFiles to avoid conflicts.
- Source: The GExperts source code is now released with each version.
  If you want to modify the source in a generally useful manner, please
  contact me for instructions on making it easy to integrate into the
  main source tree.

VERSION .95b (October 5, 1998)
- Switched to Jordan Russell's freeware Inno Setup installation utility
  (using the also excellent Inno Setup Express by Brad Stowers)
- Grep regular expressions now handle escaped characters using / in a
  more standard manner.  From Daniel Gerhard <gerhard@parametrix.ch>
- Removed the build requirement for RXLib, TUrl, ACE Reporter,
  TSyntaxMemo, and VCLZip form the source code.  You can still do a more
  complete build of GExperts by defining compiler options as described
  in GExpertX.dpr (if you have some of these products).
- Reformatted all the source code to resemble Borland's VCL and to
  remove all of the hints and warnings produced by the Delphi compiler
- All of the forms were edited for UI and standardization
- Pas to HTML expert allows copying to the clipboard
- Menu shortcut keys now work before dropping down the GExperts menu
- Dependency expert no longer allows editing in the tree, and is faster
- Component Replace is smarter about what you're allowed to do and what
  it selects as defaults
- PE Viewer is much faster and has a couple of bugfixes.  Speedup
  courtesy of Alexey Dynnikov <aldyn@tsagi.rssi.ru>.
- Clipboard history can optionally start capturing upon Delphi startup
- Grep Results paint bug fixed
- Grep search adds a checkbox to disable regular expressions and
  correctly saves your last 20 search parameters
- Clean Directory adds several new cleanable file types
- Set Tab Order gives better feedback and error messages
- Upgraded to VCLZip 2.16, ACE 1.2, and TSyntaxMemo 2.01
- Reorganized Message Dialog Helper and added a new button
- Expand All in Grep Results works now
- Fixed a bug that prevented adding of new entries in Favorite Files
- Stand-alone grep correctly saves and restores settings
- Double-clicking in the stand-alone grep will execute the file
- The GExperts DLL is about 100K smaller
- One potential access violation on shutdown was removed
- Minor bugfixes in Expert Manager, Procedure List, Configuration,
  and several other small changes


KNOWN BUGS  (in approximate order by priority)
-----------------------------------------------
- None

FUTURE ENHANCEMENTS?
----------------------
- [Erik] Integrate file auto-save code
- [Stefan] Allow the toolbar buttons to hook to and be listed from
  actions in D4+.  Use IDE hints, bitmaps, etc for toolbar buttons.
- [Stefan] Fix the timer hack that sets the main menu shortcuts
- [Stefan] Complete removal of EII and reliance on the new OTA
- [Stefan] Port Components to Code to C++Builder
- [Stefan] Verify toolbar button compatibility with BCB
- [Stefan] Change the Windows menu to use native Delphi interfaces
  instead of TIMainMenuIntf and TIMenuItemIntf
- [Stefan] Convert experts to use actions for the toolbars and menu
  items as we have time
- [Stefan] Create a Windows Scripting Host plugin and COM interface
  to allow scripts to access the Delphi 4+ OpenTools API.
- [Stefan] Make the Code Proofreader a TGX_EnhExpert
- [Unassigned] Port TEditReader/TEditWriter to the new IOTA interfaces
- [Unassigned] Add support for hiding the toolbar on dockable experts
- [Unassigned] Add word wrapped printing to the To Do List, etc.
- [Unassigned] Add GX_MessageBox warnings to the Pascal only experts
  when under C++Builder?
- [Unassigned] Grep should skip corrupt DFMs and binary files and report
  them in a GXMessageBox dialog.
- [Unassigned] Modify To Do expert so that it can display Delphi 5 style
  to do items. The basic parsing code exists, but needs to be surfaced
  in the user interface.
- [Unassigned] Add a standalone version of the Code Librarian
- [Unassigned] Translate the rest of the new OTA Project Options
- [Unassigned] Save TStrings/TComponent properties when replacing components
  (we must save component references by name, not using a pointer)
- [Unassigned] Add support for | in header editor experts
- [Unassigned] Integrate a full regular expression parser as a
  compile-time option to the grep search
- [Unassigned] Project group search option in Grep Search
- [Unassigned] To do list filtering support (token, owner, etc.)
- [Unassigned] Add a project form list and project unit list search form
  like the procedure list
- [Unassigned] Component naming property editor (new package)
- [Unassigned] Allow printing in Project Dependencies
- [Unassigned] Enhance the Message Dialog to allow copy to clipboard,
  Borland MessageBoxes, change the captions and default button, and
  generation of case statements.
- [Unassigned] Add support to fire up help for the selected word in the
  Code Librarian
- [Unassigned] Disable certain menu selections when no project is open

KEY:
Erik     = Erik Berry <eberry@gexperts.org>
Stefan   = Stefan Hoffmeister <Stefan.Hoffmeister@econos.de>
ArentJan = ArentJan Banck <Arentjan.Banck@davilex.nl>
Python   = Python <python@softhome.net>
(to claim a task, please email eberry@gexperts.org)


MISCELLANEOUS NOTES
----------------------
- The C++Builder 5 IDE will raise a NULL pointer exception when using the
  LibDir option in the Project Option Sets expert (reported to Borland).
- After using Replace Components on non-visual components, you should
  save the form to force the IDE to refresh the non-visual component
  positions.
- If a DFM is opened as text, a subsequent Grep Search can not scan the
  DFM's associated source files.  Delphi does not provide access to the
  source file buffer.  Under C++Builder 5 you might also see duplicated
  matches for the DFM file when it is being viewed as text.
- The Multiline tabs IDE enhancement and the Code Proofreader will sometimes
  require an IDE restart before they can be activated or deactivated fully.
- Delphi 4 and C++Builder 4 have a bug that keeps the menubar from resizing
  if you add top-level menu items after the main IDE window has been created.
  You should only experience this problem the first time you load GExperts,
  because after this, the menubar will remember its proper width.  To
  fix this the first time you load GExperts, just resize the main window
  once, and the menu will resize appropriately.
- Perfect Layout is really designed for Delphi 3, and as such will probably
  never support docking or undocking of windows in later version IDEs.
  Please use the Desktop saving feature of Delphi 5+ instead if possible.
- Docking the Object Inspector into the Code Editor with editor
  enhancements enabled sometimes causes strange OI/Editor
  focus/cursor problems when first starting the IDE.  The workaround
  is to disable all of the editor experts and editor enhancements or
  undock the Object Inspector.
- The cut/copy/paste menu items don't work very well in Delphi 4 because
  Delphi 4 clears the text selection when right-clicking in the editor.
  Under Delphi 4, use the regular keyboard shortcuts for this, or use the
  menu popup key on newer Windows keyboards.
- An Undo after a Code Proofreader AutoCorrect moves the cursor to the
  top of the file. This appears to be a limitation of Delphi 4's
  CreateUndoableWriter.
- GExperts will cause errors on shutdown when used in conjunction with
  siipatheditor40.bpl (an enhanced path editor) under Delphi 4.  The
  cause is unknown at this time, but is likely because they both hook
  the same IDE event handler.
- The ABC component suite versions before 4.2.4 are incompatible with
  several of the editor experts.  This is because ABC tries to hook
  Screen.OnActiveFormChanged.
- Nothing in GExperts is aware of or takes into account conditional
  defines, so code that is defined out will still be parsed by the
  GExperts lexer/parser.
- Using a high color desktop scheme will cause the help file to show
  colors improperly.  This appears to be a bug in WinHelp.
- Under Delphi 4 and C++Builder 4, many of the possible Project Options
  settings are surfaced by the IDE's OpenTools interface, but they are
  not actually hooked to any code that allows experts to get and set the
  values.  Some of these are fixed in Delphi 5, but not all of them.  If
  you notice compiler options (for example) that have no effect when you
  apply a project option set, you are likely experiencing this IDE bug.
- Borland IDEs before Delphi 5.01 do not correctly support retrieving
  the project group directory for the header experts and backup path.
- Due to an OpenTools API bug, Project Option Sets is disabled under
  C++Builder 4.
- When using the Delphi 3 Expert "Send OutputDebugString to GExperts"
  you may experience problems with other IDE add-ins that attempt to hook
  the debugger kernel (Numega's BoundsChecker is one example).  To
  eliminate conflicts, you can disable this expert or remove the other
  third-party IDE debugging enhancement.  Another possible solution is to
  rearrange the Delphi 3 "Experts" registry entries such that GExperts
  loads before the other add-in.  For a full explanation see the source.


THANKS
----------------------
Donations:

Kevin Boylan donated a copy of VCLZip
http://ourworld.compuserve.com/homepages/boylank/

David Brock donated a copy of TSyntaxMemo
http://www.dbrocksoftware.com/

Steven Tyrakowski donated a copy of ACE Reporter
http://www.sct-associates.com/

Al Grenley donated a copy of Eagle Software's CodeRush for Delphi 3.


GEXPERTS LICENSE
----------------------

  GExperts is copyright 1996-2001 by GExperts, Inc, Erik Berry, and
several other authors who have submitted their code for inclusion.
This license agreement only covers code written by GExperts, Inc,
and Erik Berry.  You should contact the other authors concerning
their respective copyrights and conditions.

  The rules governing the use of GExperts and the GExperts source
code are derived from the official Open Source Definition, available
from http://www.opensource.org/.  The conditions and limitations are
as follows:

  You may not use the GExperts source code to develop a proprietary
commercial or shareware product; this includes the creation of
proprietary plugins and libraries for commercial products.  You may
use the GExperts source code in a non-proprietary (Open Source)
project, under the terms listed below.

  You may not use the GExperts source code to create and distribute
custom versions of GExperts under the "GExperts" name.  If you do
modify and distribute custom versions of GExperts, the binary
distribution must be named differently and clearly marked so users
can tell they are not using the official GExperts distribution.
A visible and unmodified version of this license must appear in
any modified distribution of GExperts.

  Custom distributions of GExperts must include all of the custom
changes as a patch file that can be applied to the original source
code.  This restriction is in place to protect the integrity of the
original authors' source code.  No support for modified versions of
GExperts will be provided by the original authors or on the GExperts
mailing lists.

  All works derived from GExperts must be distributed under a
license compatible with this license and the official Open Source
Definition, which can be obtained from http://www.opensource.org/.

  Please note that GExperts, Inc., and the other contributing authors
hereby state that this package is provided "as is" and without any
express or implied warranties, including, but not without limitation,
the implied warranties of merchantability and fitness for a particular
purpose.  In other words, we accept no liability for any damage that
may result from using GExperts or programs that use the GExperts
source code.

If you have license questions, please send email to Erik Berry at
eberry@gexperts.org.

EOF
