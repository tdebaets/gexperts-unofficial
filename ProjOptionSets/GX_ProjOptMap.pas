unit GX_ProjOptMap;

{$I GX_CondDefine.inc}

 { TODO -oStefan -cImplement :

 Items that need to be completed have either of
  nil
  ''
  [ocUnknown]
 as constants in the option map }

interface

uses
  TypInfo;

type
  // It is possible to specify a translator that
  // translates a passed in string value to a
  // longer, more elaborate option value - e.g.
  // "0" -> False etc.
  TGxOptionValueTranslator = function(const ValueString: string): string;

function GxBoolOptionTranslator(const ValueString: string): string;
function GxStringOptionTranslator(const ValueString: string): string;
function GxIntegerOptionTranslator(const ValueString: string): string;
function GxHexOptionTranslator(const ValueString: string): string;
function GxProcInstructTranslator(const ValueString: string): string;
function GxAlignmentTranslator(const ValueString: string): string;
function GxCallConvTranslator(const ValueString: string): string;
function GxRegVarTranslator(const ValueString: string): string;
function GxMemberPointerTranslator(const ValueString: string): string;
function GxVTableTranslator(const ValueString: string): string;
function GxAtlInstancingTranslator(const ValueString: string): string;
function GxAtlCoinitTranslator(const ValueString: string): string;
function GxAtlThreadTranslator(const ValueString: string): string;
function GxTasmExpTranslator(const ValueString: string): string;
function GxVerInfoModuleAttribTranslator(const ValueString: string): string;
function GxMapFileTranslator(const ValueString: string): string;
function GxReferenceInfoTranslator(const ValueString: string): string;

type
  // The category into which an option belongs
  TGxOptionCategory = (
    // Basic functionality areas
    ocIde, ocCompiler, ocLinker, ocDebugger,
    // Product
    ocDelphi, ocBCB, ocTasm, ocTLib, ocCodeGuard,
    // Language (leaving assembler to ocTasm?)
    ocObjectPascal, ocATL,
    // Directories / Folders
    ocFolders,
    // not yet known
    ocUnknown
  );

  TGxOptionCategorySet = set of TGxOptionCategory;

const
  GxAllOptions = [Low(TGxOptionCategory)..High(TGxOptionCategory)];

const
  GxOptionsCategoryText: array[TGxOptionCategory] of string = (
    'IDE', 'Compiler', 'Linker', 'Debugger',
    'Delphi', 'C++Builder', 'TASM', 'TLib', 'CodeGuard',
    'Object Pascal', 'ATL',
    'Directories',
    'Unknown'
  );

type
  TGxOptionCategoryGroup = (
    ocgDelphi
  );

const
  GxOptionCategoryGroups: array[TGxOptionCategoryGroup] of TGxOptionCategorySet = (
    [ocDelphi]
  );

type
  TGxOptionsMap = record
    Name: string;               // The IDE's option name, e.g. "HostApplication"
    AssumedTypeKind: TTypeKind; // Used for sanity checking
    Description: string;        // Descriptive text about the option
    Categories: TGxOptionCategorySet;
    Translator: TGxOptionValueTranslator;
  end;

{ TODO -oAnyone -cFeature : Add similar maps for the environment options? }

const
  GxOptionsMap: array[0..196] of TGxOptionsMap = (
    ( // 0
      Name: 'HostApplication';
      AssumedTypeKind: tkLString;
      Description: 'Debugger host application';
      Categories: [ocDelphi, ocBCB, ocDebugger];
      Translator: GxStringOptionTranslator;
    ),
    ( // 1
      Name: 'RunParams';
      AssumedTypeKind: tkLString;
      Description: 'Debugger command line parameters';
      Categories: [ocDelphi, ocBCB, ocDebugger];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'RemoteHost';
      AssumedTypeKind: tkLString;
      Description: 'Debugger remote host';
      Categories: [ocDelphi, ocBCB, ocDebugger];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'RemotePath';
      AssumedTypeKind: tkLString;
      Description: 'Debugger remote path';
      Categories: [ocDelphi, ocBCB, ocDebugger, ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'RemoteDebug';
      AssumedTypeKind: tkEnumeration;
      Description: 'Debug on remote host';
      Categories: [ocDelphi, ocBCB, ocDebugger];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'DebugInfo';
      AssumedTypeKind: tkEnumeration;
      Description: 'Include TurboDebugger 32 information in binary';
      Categories: [ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'RemoteSymbols';
      AssumedTypeKind: tkEnumeration;
      Description: 'Include remote debug symbols';
      Categories: [ocDelphi, ocBCB, ocDebugger];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'OutputObj';
      AssumedTypeKind: tkSet;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'HintFlag';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show compiler hints';
      Categories: [ocCompiler, ocDelphi];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'WarnFlag';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show compiler warnings';
      Categories: [ocCompiler, ocDelphi];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 10
      Name: 'StackSize';
      AssumedTypeKind: tkInteger;
      Description: 'Initial stack size';
      Categories: [ocLinker, ocDelphi];
      Translator: GxHexOptionTranslator;
    ),
    (
      Name: 'MaxStackSize';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum stack size';
      Categories: [ocLinker, ocDelphi];
      Translator: GxHexOptionTranslator;
    ),
    (
      Name: 'ImageBase';
      AssumedTypeKind: tkInteger;
      Description: 'Preferred load address of the binary image (usually DLL)';
      Categories: [ocLinker, ocDelphi];
      Translator: GxHexOptionTranslator;
    ),
    (
      Name: 'Target';
      AssumedTypeKind: tkEnumeration;
      Description: ''; // What is this in D5?
      Categories: [ocLinker];
      Translator: nil;
    ),
    (
      Name: 'MapFile';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate map file: Off/Segments/Publics/Detailed';
      Categories: [ocLinker, ocDelphi];
      Translator: GxMapFileTranslator;
    ),
    (
      Name: 'GenDRC';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate DRC file of resource strings';
      Categories: [ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'GenDUI';
      AssumedTypeKind: tkEnumeration;
      Description: ''; // What is a DUI file (BCB)?
      Categories: [ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'CompileName';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'Defines';
      AssumedTypeKind: tkLString;
      Description: 'Conditional defines';
      Categories: [ocBCB, ocDelphi, ocCompiler];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'SysDefines';
      AssumedTypeKind: tkLString;
      Description: 'System defines (defined by IDE)';
      Categories: [ocBCB, ocCompiler];
      Translator: GxStringOptionTranslator;
    ),
    ( // 20
      Name: 'OutputDir';
      AssumedTypeKind: tkLString;
      Description: 'Output directory for compiled binaries';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'UnitOutputDir';
      AssumedTypeKind: tkLString;
      Description: 'Output directory for DCU files';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'UnitDir';
      AssumedTypeKind: tkLString;
      Description: 'Unit search path';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ObjDir';
      AssumedTypeKind: tkLString;
      Description: 'Search path??'; // ????? search path in D5
      Categories: [ocFolders];
      Translator: nil;
    ),
    (
      Name: 'SrcDir';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocFolders];
      Translator: nil;
    ),
    (
      Name: 'ResDir';
      AssumedTypeKind: tkLString;
      Description: 'Search path??'; //???? Search path in D5
      Categories: [ocFolders];
      Translator: nil;
    ),
    (
      Name: 'PkgDllDir';
      AssumedTypeKind: tkLString;
      Description: 'BPL output directory';
      Categories: [ocBCB, ocDelphi, ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'OptionsString';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'PkgDcpDir';
      AssumedTypeKind: tkLString;
      Description: 'DCP/BPI output directory';
      Categories: [ocBCB, ocDelphi, ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'UsePackages';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use runtime packages';
      Categories: [ocLinker, ocDelphi, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 30
      Name: 'Packages';
      AssumedTypeKind: tkLString;
      Description: 'Runtime packages list';
      Categories: [ocBCB, ocDelphi, ocLinker];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'UnitAliases';
      AssumedTypeKind: tkLString;
      Description: 'Aliases for units that have changed names';
      Categories: [ocCompiler, ocDelphi, ocBCB];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ExeDescription';
      AssumedTypeKind: tkLString;
      Description: 'EXE Description (inserted in binary)';
      Categories: [ocLinker];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ImplicitBuild';
      AssumedTypeKind: tkEnumeration;
      Description: 'Implicitly build packages';
      Categories: [ocIde, ocCompiler, ocDelphi, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'RuntimeOnly';
      AssumedTypeKind: tkEnumeration;
      Description: 'This is a runtime only package';
      Categories: [ocLinker, ocDelphi, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'DesigntimeOnly';
      AssumedTypeKind: tkEnumeration;
      Description: 'This is a design-time only package';
      Categories: [ocLinker, ocDelphi, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'DebugSourcePath';
      AssumedTypeKind: tkLString;
      Description: 'Search path for the debugger to find source units';
      Categories: [ocBCB, ocDelphi, ocIde, ocDebugger, ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ImageDebugInfo';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocUnknown];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'HeapSize';
      AssumedTypeKind: tkInteger;
      Description: 'Minimum heap size for application';
      Categories: [ocBCB, ocLinker];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'MaxHeapSize';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum heap size for application';
      Categories: [ocBCB, ocLinker];
      Translator: GxIntegerOptionTranslator;
    ),
    ( // 40
      Name: 'LinkMaxErrors';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum number of linker errors';
      Categories: [ocLinker, ocBCB];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'LinkShowMangle';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show mangled names in MAP file';
      Categories: [ocLinker, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkGenImportLib';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate import library (.LIB or .BPI)';
      Categories: [ocLinker, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkGenLib';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate .lib file - applies to packages only';
      Categories: [ocLinker, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkNoStateFiles';
      AssumedTypeKind: tkEnumeration;
      Description: 'Do not generate linker state files';
      Categories: [ocLinker, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkSubsysMajor';
      AssumedTypeKind: tkInteger;
      Description: 'Major portion of Windows version ID on which you expect your application will be run on';
      Categories: [ocLinker, ocBCB];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'LinkSubsysMinor';
      AssumedTypeKind: tkInteger;
      Description: 'Minor portion of Windows version ID on which you expect your application will be run on';
      Categories: [ocLinker, ocBCB];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'CppDebugInfo';
      AssumedTypeKind: tkEnumeration;
      Description: 'Add debug information to .OBJ files';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LineNumbers';
      AssumedTypeKind: tkEnumeration;
      Description: 'Add line numbers to .OBJ files';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'AutoRegVars'; // see also: RegisterVars
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocBCB, ocCompiler];
      Translator: nil;  // Probably None, Automatic, Register keyword
    ),
    ( // 50
      Name: 'MergeDupStrs';
      AssumedTypeKind: tkEnumeration;
      Description: 'Merge duplicate strings';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EnableInLines';
      AssumedTypeKind: tkEnumeration;
      Description: 'Enable inline expansion of functions';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ShowWarnings';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show BCB compiler warnings'; //????
      Categories: [ocCompiler, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'StdStackFrame';
      AssumedTypeKind: tkEnumeration;
      Description: 'Always generate a standard stack frame';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'PCH';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use pre-compiled headers';
      Categories: [ocIde, ocCompiler, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ShowInfoMsgs';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show general information messages';
      Categories: [ocIde, ocCompiler, ocLinker, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'GenDll';
      AssumedTypeKind: tkEnumeration;
      Description: 'Set DLL module attribute';
      Categories: [ocIde, ocDelphi, ocBCB];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'GenConsoleApp';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate console application';
      Categories: [ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'GenPackage';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate package';
      Categories: [ocUnknown];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'GenStaticLibrary';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate static library';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 60
      Name: 'ShowLinkerWarnings';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show linker warnings';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'UseIncLinker';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use incremental linker';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'InMemoryExe';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate in-memory EXE (Windows NT only)';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkDebugVcl';
      AssumedTypeKind: tkEnumeration;
      Description: 'Link to the debug VCL units';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'InstructionSet';
      AssumedTypeKind: tkEnumeration;
      Description: 'Processor instruction set';
      Categories: [ocBCB, ocCompiler];
      // this is 386..586!
      Translator: GxProcInstructTranslator;
    ),
    (
      Name: 'Alignment';
      AssumedTypeKind: tkEnumeration;
      Description: 'Data alignment';
      Categories: [ocBCB, ocCompiler];
      // 1,2,4,8
      Translator: GxAlignmentTranslator;
    ),
    (
      Name: 'CallingConvention';
      AssumedTypeKind: tkEnumeration;
      Description: 'Calling convention';
      Categories: [ocBCB, ocCompiler];
      // cdecl, register, Pascal, stdcall
      Translator: GxCallConvTranslator;
    ),
    (
      Name: 'RegisterVars';
      AssumedTypeKind: tkEnumeration;
      Description: 'Register variables';
      Categories: [ocBCB, ocCompiler];
      // none, auto, register
      Translator: GxRegVarTranslator;
    ),
    (
      Name: 'Ansi';
      AssumedTypeKind: tkEnumeration;
      Description: 'Language compliance';
      Categories: [ocBCB, ocCompiler];
      Translator: nil; // Borland, ANSI, Unix V, K&R
    ),
    (
      Name: 'AutoDep';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate auto-dependency information in OBJ files';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 70
      Name: 'Underscore';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate underscores';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'FastFloat';
      AssumedTypeKind: tkEnumeration;
      Description: 'Fast floating point code (disregard ANSI conventions)';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'Float';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use no floating point arithmetic';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'PentiumFloat';
      AssumedTypeKind: tkEnumeration;
      Description: 'Correct Pentium FDIV flaw';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'NestedComments';
      AssumedTypeKind: tkEnumeration;
      Description: 'Allow nested C++ comments';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'MFCCompat';
      AssumedTypeKind: tkEnumeration;
      Description: 'Enable MFC compatibility mode';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'IdentLen';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum identifier length in characters';
      Categories: [ocBCB, ocCompiler];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'MemberPrecision';
      AssumedTypeKind: tkEnumeration;
      Description: 'Honor member precision';
      Categories: [ocBCB, ocCompiler];
      Translator: nil;
    ),
    (
      Name: 'ForLoops';
      AssumedTypeKind: tkEnumeration;
      Description: 'Do not restrict scope on for loops';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TwoChar';
      AssumedTypeKind: tkEnumeration;
      Description: 'No distinct char type';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 80
      Name: 'CodeModifiers';
      AssumedTypeKind: tkEnumeration;
      Description: 'Do not mangle code modifiers';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EnableRTTI';
      AssumedTypeKind: tkEnumeration;
      Description: 'Enable generation of RTTI';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EnableExceptions';
      AssumedTypeKind: tkEnumeration;
      Description: 'Enable exceptions';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EHLocalInfo';
      AssumedTypeKind: tkEnumeration;
      Description: 'Enable generation of exception location information';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EHDtor';
      AssumedTypeKind: tkEnumeration;
      Description: 'Perform destructor cleanup on exception';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'EHPrologs';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use fast exception handling prologs';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ZeroBaseClass';
      AssumedTypeKind: tkEnumeration;
      Description: 'Allow zero length base classes';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'MemberPointer';
      AssumedTypeKind: tkEnumeration;
      Description: 'Storage layout of class member pointers';
      Categories: [ocBCB, ocCompiler];
      // All cases, MI, SI, Smallest
      Translator: GxMemberPointerTranslator;
    ),
    (
      Name: 'VTables';
      AssumedTypeKind: tkEnumeration;
      Description: 'Optimize virtual method tables';
      Categories: [ocBCB, ocCompiler];
      // Smart, local, external, public
      Translator: GxVTableTranslator;
    ),
    (
      Name: 'Templates';
      AssumedTypeKind: tkEnumeration;
      Description: 'External templates';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 90
      Name: 'PchPath';
      AssumedTypeKind: tkLString;
      Description: 'Precompiled header path';
      Categories: [ocBCB, ocIde, ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'PchStopAfter';
      AssumedTypeKind: tkLString;
      Description: 'Stop compiling pre-compiled headers after file';
      Categories: [ocBCB, ocIde];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'UseDynamicRtl';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use dynamic Runtime library (DLL)';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ATLMultiUse';
      AssumedTypeKind: tkEnumeration;
      Description: 'ATL Instancing model';
      Categories: [ocBCB, ocATL];
      // Single use, Multiple use
      Translator: GxAtlInstancingTranslator;
    ),
    (
      Name: 'ATLDebugQI';
      AssumedTypeKind: tkEnumeration;
      Description: 'Trace calls to QueryInterface (Debugging)';
      Categories: [ocBCB, ocATL];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ATLCoinitMultiThreaded';
      AssumedTypeKind: tkEnumeration;
      Description: 'OLE Initialization COINIT_xxxx Flag';
      Categories: [ocBCB, ocATL];
      // APARTMENTTHREADED, MULTITHREADED
      Translator: GxAtlCoinitTranslator;
    ),
    (
      Name: 'ATLAutoRegisterInproc';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocBCB, ocATL];
      Translator: nil;
    ),
    (
      Name: 'ATLDebugRefCount';
      AssumedTypeKind: tkEnumeration;
      Description: 'Check reference counts (debugging)';
      Categories: [ocBCB, ocATL];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ATLDebug';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generally trace ATL (debugging)';
      Categories: [ocBCB, ocATL];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ATLThreading';
      AssumedTypeKind: tkEnumeration;
      Description: 'ATL threading model';
      Categories: [ocBCB, ocATL];
      // Single, Apartment, Free, Both
      Translator: GxAtlThreadTranslator;
    ),
    ( // 100
      Name: 'CodeOpt';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocCompiler];
      Translator: nil;
    ),
    (
      Name: 'FloatSupport';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocCompiler];
      Translator: nil;
    ),
    (
      Name: 'IncludePath';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'LibPath';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'DebugPath';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ReleasePath';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocFolders];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'LibraryList';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocUnknown];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'TasmViaCppOpts';
      AssumedTypeKind: tkLString;
      Description: '';
      Categories: [ocTasm];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'ClearPackageCache';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'ClearUnitCache';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    ( // 110
      Name: 'MarkModified'; 
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'CaseSensitive';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate case-sensitive .LIB libraries';
      Categories: [ocTLib];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ExtendedDictionary'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Create extended dictionary';
      Categories: [ocTLib];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'PurgeComment'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Purge comment records';
      Categories: [ocTLib];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'PageSize';
      AssumedTypeKind: tkInteger;
      Description: 'Page size';
      Categories: [ocTLib];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'TasmCrossReference'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Include cross reference in listing';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmSymbolTables'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Show symbol tables in listing';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmGenerateListing'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate listing';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmIncludeConditionals';
      AssumedTypeKind: tkEnumeration;
      Description: 'Include false conditionals in listing';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmIncludeErrors'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Include errors in listing';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 120
      Name: 'TasmExpanded';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate expanded listing';
      Categories: [ocTasm];
      // Normal listing, expanded listing
      Translator: GxTasmExpTranslator;
    ),
    (
      Name: 'TasmCaseCheckingOn'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Check case of symbols';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator; // None, Globals, All !!
    ),
    (
      Name: 'TasmAllCase';
      AssumedTypeKind: tkEnumeration;
      Description: 'Check case of all symbols';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;  // None, Globals, All !!
    ),
    (
      Name: 'TasmDebugOn';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate debug information';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmFullDebug';
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate full debug information';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmWarningsOn'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Generate warnings';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmWarningsLevel1'; 
      AssumedTypeKind: tkEnumeration;
      Description: 'Use warnings Level 1';
      Categories: [ocTasm];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TasmHashTable';
      AssumedTypeKind: tkInteger;
      Description: 'Size of hash table';
      Categories: [ocTasm];
      Translator: nil;
    ),
    (
      Name: 'TasmPasses';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum number of passes';
      Categories: [ocTasm];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'TasmSymbolLength';
      AssumedTypeKind: tkInteger;
      Description: 'Maximum symbol length';
      Categories: [ocTasm];
      Translator: GxIntegerOptionTranslator;
    ),
    ( // 130
      Name: 'TasmDirective';
      AssumedTypeKind: tkLString;
      Description: 'Directives to be passed to TASM';
      Categories: [ocTasm];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'NoAskLibs';
      AssumedTypeKind: tkEnumeration;
      Description: '';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'Align'; // $A
      AssumedTypeKind: tkInteger;
      Description: 'Align fields at ... byte boundaries';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      // 1, 2, 4, 8, 16, 32...
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'BoolEval'; // $B
      AssumedTypeKind: tkInteger;
      Description: 'Boolean short-circuit evaluation';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'Assertions'; // $C
      AssumedTypeKind: tkInteger;
      Description: 'Generate code for assert directives';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'UnitDebugInfo'; // $D
      AssumedTypeKind: tkInteger;
      Description: 'Generate debug information in compiled units';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ImportedData'; // $G
      AssumedTypeKind: tkInteger;
      Description: 'Imported data (packages)';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LongStrings'; // $H
      AssumedTypeKind: tkInteger;
      Description: 'Default to long strings';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'IOChecks'; // $I
      AssumedTypeKind: tkInteger;
      Description: 'Input/output checking';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'WriteableConst'; // $J
      AssumedTypeKind: tkInteger;
      Description: 'Writeable typed constants';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 140
      Name: 'LocalSymbols'; // $L
      AssumedTypeKind: tkInteger;
      Description: 'Generate local symbol information for debugging';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TypeInfo'; // $M
      AssumedTypeKind: tkInteger;
      Description: 'Generate runtime type information';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'Optimization'; // $O
      AssumedTypeKind: tkInteger;
      Description: 'Optimization (Object Pascal)';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'OpenStrings'; // $P
      AssumedTypeKind: tkInteger;
      Description: 'Open String Parameters';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'OverflowChecks'; // $Q
      AssumedTypeKind: tkInteger;
      Description: 'Overflow checking';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'RangeChecks'; // $R
      AssumedTypeKind: tkInteger;
      Description: 'Range checking';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'StackChecks'; // $S
      AssumedTypeKind: tkInteger;
      Description: 'Stack checking';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'TypedAddress'; // $T
      AssumedTypeKind: tkInteger;
      Description: 'Type-checked pointers';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'SafeDivide'; // $U
      AssumedTypeKind: tkInteger;
      Description: 'Pentium-safe FDIV operations';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'VarStringChecks'; // $V
      AssumedTypeKind: tkInteger;
      Description: 'Var-string checking (long vs short var strings)';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 150
      Name: 'StackFrames'; // $W
      AssumedTypeKind: tkInteger;
      Description: 'Compile with stack frames';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ExtendedSyntax'; // $X
      AssumedTypeKind: tkInteger;
      Description: 'Extended syntax (PChar, ignore function results)';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'ReferenceInfo'; // $Y
      AssumedTypeKind: tkInteger;
      Description: 'Symbol declaration and cross-reference information';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      Translator: GxReferenceInfoTranslator;
    ),
    (
      Name: 'MinEnumSize'; // $Z
      AssumedTypeKind: tkInteger;
      Description: 'Minimum size of enumerations in bytes';
      Categories: [ocDelphi, ocBCB, ocCompiler, ocObjectPascal];
      // 1, 2, 4, 8, 16, ...
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'AutoIncBuildNum';
      AssumedTypeKind: tkEnumeration;
      Description: 'Auto-increment build number';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'CodePage';
      AssumedTypeKind: tkInteger;
      Description: '';
      Categories: [];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'IncludeVersionInfo';
      AssumedTypeKind: tkEnumeration;
      Description: 'Include version information in project';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxBoolOptionTranslator;
    ),
    ( // TODO -oAnyone -cIssue: FIXME - VERSIONINFO keys - messy!
      // This isn't correctly surfaced up through D5 (it returns nil)
      Name: 'Keys';
      AssumedTypeKind: tkClass; // (!)
      Description: '';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: nil;
    ),
    (
      Name: 'Locale';
      AssumedTypeKind: tkInteger;
      Description: 'Locale ID (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxHexOptionTranslator;
    ),
    (
      Name: 'MajorVersion';
      AssumedTypeKind: tkInteger;
      Description: 'Module major version (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxIntegerOptionTranslator;
    ),
    ( // 160
      Name: 'MinorVersion';
      AssumedTypeKind: tkInteger;
      Description: 'Module minor version (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'Release';
      AssumedTypeKind: tkInteger;
      Description: 'Release number (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'Build';
      AssumedTypeKind: tkInteger;
      Description: 'Build number (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'ModuleAttribs';
      AssumedTypeKind: tkSet;
      Description: 'Module attributes (VERSIONINFO)';
      Categories: [ocDelphi, ocBCB, ocIDE];
      // Debug build, Pre-release, DLL, Special build, Private build
      Translator: GxVerInfoModuleAttribTranslator;
    ),
    (
      Name: 'AppFileExt';
      AssumedTypeKind: tkLString;
      Description: 'AppFileExt';
      Categories: [ocBCB, ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'LibDir';
      AssumedTypeKind: tkLString;
      Description: 'LibDir';
      Categories: [ocBCB, ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'ShowExtendedMsgs';
      AssumedTypeKind: tkEnumeration;
      Description: 'Show extended compiler messages';
      Categories: [ocBCB, ocCompiler];
      Translator: nil;
    ),
    (
      Name: 'ZeroClassFunction';
      AssumedTypeKind: tkEnumeration;
      Description: 'Allow zero length empty class members';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'MultiThreaded';
      AssumedTypeKind: tkEnumeration;
      Description: 'MultiThreaded';
      Categories: [ocBCB, ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'CGGlobalStackAccesses';
      AssumedTypeKind: tkEnumeration;
      Description: 'CodeGuard: Validate global and stack accesses';
      Categories: [ocBCB, ocCodeGuard, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 170
      Name: 'CGThisPointer';
      AssumedTypeKind: tkEnumeration;
      Description: 'CodeGuard: Validate the "this" pointer on member function entry';
      Categories: [ocBCB, ocCodeGuard, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'CGInlinePointer';
      AssumedTypeKind: tkEnumeration;
      Description: 'CodeGuard: Validate pointer accesses';
      Categories: [ocBCB, ocCodeGuard, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'CGLinkCGLib';
      AssumedTypeKind: tkEnumeration;
      Description: 'CGLinkCGLib';
      Categories: [ocBCB, ocCodeGuard, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkCaseSensitiveLink';
      AssumedTypeKind: tkEnumeration;
      Description: 'Case-insensitive linking';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkCalculateChecksum';
      AssumedTypeKind: tkEnumeration;
      Description: 'Calculate PE header checksum';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkFastTLS';
      AssumedTypeKind: tkEnumeration;
      Description: 'Use fast TLS support';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkReplaceResources';
      AssumedTypeKind: tkEnumeration;
      Description: 'Replace resources';
      Categories: [ocBCB, ocLinker];
      Translator: GxBoolOptionTranslator;
    ),
    (
      Name: 'LinkUserMajor';
      AssumedTypeKind: tkInteger;
      Description: 'User major version (PE header)';
      Categories: [ocBCB, ocLinker];
      Translator: GxIntegerOptionTranslator;
    ),
    (
      Name: 'LinkUserMinor';
      AssumedTypeKind: tkInteger;
      Description: 'User minor version (PE header)';
      Categories: [ocBCB, ocLinker];
      Translator: nil;
    ),
    (
      Name: 'LinkImageComment';
      AssumedTypeKind: tkLString;
      Description: 'Image comment';
      Categories: [ocBCB, ocLinker];
      Translator: GxStringOptionTranslator;
    ),
    ( // 180
      Name: 'LinkDelayLoad';
      AssumedTypeKind: tkLString;
      Description: 'Delay-loaded DLLs';
      Categories: [ocBCB, ocLinker];
      Translator: GxStringOptionTranslator;
    ),
    (
      Name: 'TreatEnumsAsInts';
      AssumedTypeKind: tkEnumeration;
      Description: 'Treat enums as integers';
      Categories: [ocBCB, ocCompiler];
      Translator: GxBoolOptionTranslator;
    ),
    ( // 182
      Name: 'ListFile';
      AssumedTypeKind: tkLString;
      Description: 'ListFile';
      Categories: [ocBCB, ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'EnvVars';
      AssumedTypeKind: tkClass;
      Description: 'Environment variabes';
      Categories: [ocIde];
      Translator: nil;
    ),
    (
      Name: 'SysVars';
      AssumedTypeKind: tkClass;
      Description: 'System defined variabes';
      Categories: [ocIde];
      Translator: nil;
    ),
    (
      Name: 'Launcher';
      AssumedTypeKind: tkLString;
      Description: 'Launcher'; //??
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'UseLauncher';
      AssumedTypeKind: tkENumeration;
      Description: 'Use launcher';
      Categories: [ocUnknown];
      Translator: nil; //GxBoolOptionTranslator??
    ),
    (
      Name: 'DebugCWD';
      AssumedTypeKind: tkLString;
      Description: 'DebugCWD';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'RemoteLauncher';
      AssumedTypeKind: tkLString;
      Description: 'Remote launcher';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'RemoteCWD';
      AssumedTypeKind: tkLString;
      Description: 'Remote CWD';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'ResourceReserve';
      AssumedTypeKind: tkInteger;
      Description: 'Resource reserve';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'SOName';
      AssumedTypeKind: tkLString;
      Description: 'Shared object name';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'SOPrefix';
      AssumedTypeKind: tkLString;
      Description: 'Shared object prefix';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'SOPrefixDefined';
      AssumedTypeKind: tkEnumeration;
      Description: 'Shared object prexix is defined';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'SOSuffix';
      AssumedTypeKind: tkLString;
      Description: 'Shared object suffix';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'SOVersion';
      AssumedTypeKind: tkLString;
      Description: 'Shared object version';
      Categories: [ocUnknown];
      Translator: nil;
    ),
    (
      Name: 'DynamicLoader';
      AssumedTypeKind: tkLString;
      Description: 'Dynamic loader';
      Categories: [ocUnknown];
      Translator: nil;
    )
  );


function GetOptionDescription(const OptionName: string): string;
function GetOptionTranslator(const OptionName: string): TGxOptionValueTranslator;
function GetOptionCategories(const OptionName: string): TGxOptionCategorySet;
function TranslatedValue(const OptionName, OptionValue: string): string;
function CategoryTextToCategory(const CatText: string): TGxOptionCategory;

function OptionIsAppropriateForIde(const OptionCategories: TGxOptionCategorySet): Boolean;
function OptionCategoryIsAppropriateForIde(const OptionCategory: TGxOptionCategory): Boolean;

implementation

uses
  SysUtils;

function GetOptionDescription(const OptionName: string): string;
var
  i: Integer;
begin
  Result := '';

  for i := Low(GxOptionsMap) to High(GxOptionsMap) do
    if CompareText(OptionName, GxOptionsMap[i].Name) = 0 then
    begin
      Result := GxOptionsMap[i].Description;
      Break;
    end;
end;

function GetOptionTranslator(const OptionName: string): TGxOptionValueTranslator;
var
  i: Integer;
begin
  Result := nil;

  for i := Low(GxOptionsMap) to High(GxOptionsMap) do
    if CompareText(OptionName, GxOptionsMap[i].Name) = 0 then
    begin
      Result := GxOptionsMap[i].Translator;
      Break;
    end;
end;

function GetOptionCategories(const OptionName: string): TGxOptionCategorySet;
var
  i: Integer;
begin
  Result := [];

  for i := Low(GxOptionsMap) to High(GxOptionsMap) do
    if CompareText(OptionName, GxOptionsMap[i].Name) = 0 then
    begin
      Result := GxOptionsMap[i].Categories;
      Break;
    end;
end;

function TranslatedValue(const OptionName, OptionValue: string): string;
var
  Xlator: TGxOptionValueTranslator;
begin
  Result := OptionValue;
  Xlator := GetOptionTranslator(OptionName);
  if Assigned(Xlator) then
    Result := Xlator(OptionValue);
end;

function CategoryTextToCategory(const CatText: string): TGxOptionCategory;
var
  i: TGxOptionCategory;
begin
  Result := ocUnknown; // default to Unknown category

  for i := Low(GxOptionsCategoryText) to High(GxOptionsCategoryText) do
    if CompareText(CatText, GxOptionsCategoryText[i]) = 0 then
    begin
      Result := i;
      Break;
    end;
end;

function OptionIsAppropriateForIde(const OptionCategories: TGxOptionCategorySet): Boolean;
begin
  Result := True;

{$IFDEF GX_Delphi}
  Result := Result and not (ocTasm in OptionCategories);
  Result := Result and not (ocTLib in OptionCategories);
  Result := Result and not (ocATL in OptionCategories);
  Result := Result and not (ocCodeGuard in OptionCategories);

  if ocBCB in OptionCategories then
  begin
    Result := Result and (ocDelphi in OptionCategories);
  end;
{$ENDIF GX_Delphi}

{$IFDEF GX_BCB}
  // Nothing at this time.
{$ENDIF GX_Delphi}
end;

function OptionCategoryIsAppropriateForIde(const OptionCategory: TGxOptionCategory): Boolean;
const
  AppropriateDelphiCategories = GxAllOptions - [ocBCB, ocTasm, ocTLib, ocATL, ocCodeGuard];
begin
{$IFDEF GX_Delphi}
  Result := OptionCategory in AppropriateDelphiCategories;
{$ENDIF GX_Delphi}

{$IFDEF GX_BCB}
  Result := True;
{$ENDIF GX_BCB}
end;

//
// Beginning of translators
//

resourcestring
  SCannotTranslateAppendix = ' - cannot translate';

function GxBoolOptionTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'False'
  else
  if ValueString = '1' then
    Result := 'True'
  else
  if ValueString = '-1' then // Used for 'UsePackages' etc.
    Result := 'True'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxStringOptionTranslator(const ValueString: string): string;
begin
  Result := ValueString;
end;

function GxIntegerOptionTranslator(const ValueString: string): string;
begin
  Result := ValueString;
end;

function GxHexOptionTranslator(const ValueString: string): string;
const
  GXHexPrefix = {$IFDEF GX_BCB}'0x'{$ELSE}'$'{$ENDIF};
begin
  Result := Format(GXHexPrefix+'%x', [StrToInt(ValueString)]);
end;

function GxProcInstructTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := '386'
  else if ValueString = '1' then
    Result := '486'
  else if ValueString = '2' then
    Result := '586'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxAlignmentTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := '1'
  else if ValueString = '1' then
    Result := '2'
  else if ValueString = '2' then
    Result := '4'
  else if ValueString = '3' then
    Result := '8'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxCallConvTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'cdecl'
  else if ValueString = '1' then
    Result := 'register'
  else if ValueString = '2' then
    Result := 'Pascal'
  else if ValueString = '3' then
    Result := 'stdcall'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxRegVarTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'none'
  else if ValueString = '1' then
    Result := 'auto'
  else if ValueString = '2' then
    Result := 'register'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxMemberPointerTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'All cases'
  else if ValueString = '1' then
    Result := 'MI'
  else if ValueString = '2' then
    Result := 'SI'
  else if ValueString = '3' then
    Result := 'Smallest'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxVTableTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Smart'
  else if ValueString = '1' then
    Result := 'local'
  else if ValueString = '2' then
    Result := 'external'
  else if ValueString = '3' then
    Result := 'public'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxAtlInstancingTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Single use'
  else if ValueString = '1' then
    Result := 'Multiple use'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxAtlCoinitTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Apartment Threaded'
  else if ValueString = '1' then
    Result := 'Multi-Threaded'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxAtlThreadTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Single'
  else if ValueString = '1' then
    Result := 'Apartment'
  else if ValueString = '2' then
    Result := 'Free'
  else if ValueString = '3' then
    Result := 'Both'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxTasmExpTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Normal listing'
  else if ValueString = '1' then
    Result := 'Expanded listing'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxMapFileTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Off'
  else if ValueString = '1' then
    Result := 'Segments'
  else if ValueString = '2' then
    Result := 'Publics'
  else if ValueString = '3' then
    Result := 'Detailed'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxReferenceInfoTranslator(const ValueString: string): string;
begin
  if ValueString = '0' then
    Result := 'Off'
  else if ValueString = '1' then
    Result := 'On, definitions only'
  else if ValueString = '2' then
    Result := 'On, with usage information'
  else
    Result := ValueString + SCannotTranslateAppendix;
end;

function GxVerInfoModuleAttribTranslator(const ValueString: string): string;
begin
  // Debug build, Pre-release, DLL, Special build, Private build
  // VERSIONINFO module attributes don't make it through in D5?
  Result := ValueString;

  // TODO -oAnyone -cImplement : Implement version info attribute translators
end;

end.

