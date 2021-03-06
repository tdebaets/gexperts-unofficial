        ��  ��                  �S  0   ��
 P A S P A R S E R       0 	        /*
 * TSyntaxMemoParser Script
 * ------------------------
 *
 * Author  :          David Brock
 * Date    :          October 18 1997
 * Language:          Object Pascal
 *
 */


//--------------------------------------------------------------------------------------------------------------------
//
//
//
// Macro definitions. Parameters may be specified and the replacement text terminates with the end of
// line (watch trailing blanks).
//
#define pt_DEFAULT                  0
#define pt_COMMENT_LINE             1
#define pt_IDENTIFIER               2
#define pt_STRING                   3
#define pt_NUMBER                   4
#define pt_COMMENT                  5
#define pt_HEXNUMBER                6
#define pt_RESERVED                 7
#define pt_COMMENT_BRACE            8
#define pt_COMMENT_STAR             9
#define pt_SYMBOL                   10
#define pt_CHAR_DECIMAL             11
#define pt_CHAR_HEX                 12


#define pt_SEMICOLON                20
#define pt_PROPERTY                 21
#define pt_DEFAULT_TOKEN            22
#define pt_READ                     23
#define pt_WRITE                    24
#define pt_STORED                   25
#define pt_EXPORTS                  26
#define pt_NAME                     27
#define pt_INDEX                    28
#define pt_RESIDENT                 29

#define _non_alpha_                 '[^_A-Za-z0-9]'
#define _all_chars_                 '[\x00-\xFF]'
#define _dec_digit_                 '[0-9]'
#define _hex_digit_                 '[a-fA-F0-9]'
#define _no_chars_                  '[]'
#define _dont_care_                 _all_chars_
#define _DEFAULT_BACKGROUND         clWhite
#define _DEFAULT_FOREGROUND         clBlack

#define ss_START                    0
#define ss_PROPERTY                 1
#define ss_EXPORTS                  2



//--------------------------------------------------------------------------------------------------------------------
//
// %%language section
//
// Header section. Describes the textual name of the language, case sensitivity and options used by the language.
//
%%language
Name                      = 'Object Pascal'
Case                      = __INSENSITIVE
Options                   = __DEFAULT_OPTIONS
WordWrapColumn            = _EDGE
Gutter                    = _DEFAULT_GUTTER
Anchor                    = _DEFAULT_START_ANCHOR
MarginWidth               = 8
MarginColor               = [*]clWhite
GutterColor               = [*]clYellow
SelTextColor              = [*]clWhite
SelTextBack               = [*]clBlue
ExampleText               = '(* Syntax Highlighting *)\n\
                            \program test;\n\
                            \var a: string;\n\
                            \    b: integer;\n\
                            \begin\n\
                            \  b := 0;\n\
                            \  a := \'\';\n\
                            \  while b < 10 do begin\n\
                            \    a := a + IntoToStr(b);\n\
                            \    inc(b);\n\
                            \   end;\n\
                            \end.\n'
EditableStyles              ('Reserved word', pt_RESERVED),
                            ('Comment',       pt_COMMENT),
                            ('Identifier',    pt_IDENTIFIER),
                            ('String',        pt_STRING),
                            ('Number',        pt_NUMBER),
                            ('Symbols',       pt_SYMBOL),
                            ('Default',       pt_DEFAULT)





//--------------------------------------------------------------------------------------------------------------------
//
// %%words section
//
// Used to specify simple languge keywords. These are constant value lexemes that always contain the same characters
// and only require the end style to be specified. The words present here will always be tried first. If they fail
// then the entries in the %%tokens section will be allowed to try a match.
//
// %%words table entries have 3 columns:
//     Column 1          Quoted string giving the characters that make up the word
//     Column 2          Quoted string that specifies how the word is terminated
//     Column 3          Token value returned when word is recognised
//
%%words
'\/\/'                  _dont_care_                 pt_COMMENT_LINE
'{'                     _dont_care_                 pt_COMMENT_BRACE
'(*'                    _dont_care_                 pt_COMMENT_STAR
':='                    _dont_care_                 pt_SYMBOL
'+'                     _dont_care_                 pt_SYMBOL
'-'                     _dont_care_                 pt_SYMBOL
'*'                     _dont_care_                 pt_SYMBOL
'\/'                    _dont_care_                 pt_SYMBOL
'='                     _dont_care_                 pt_SYMBOL
'<>'                    _dont_care_                 pt_SYMBOL
'<'                     _dont_care_                 pt_SYMBOL
'>'                     _dont_care_                 pt_SYMBOL
'<='                    _dont_care_                 pt_SYMBOL
'>='                    _dont_care_                 pt_SYMBOL
'('                     _dont_care_                 pt_SYMBOL
')'                     _dont_care_                 pt_SYMBOL
'['                     _dont_care_                 pt_SYMBOL
']'                     _dont_care_                 pt_SYMBOL
'.'                     _dont_care_                 pt_SYMBOL
'..'                    _dont_care_                 pt_SYMBOL
'^'                     _dont_care_                 pt_SYMBOL
','                     _dont_care_                 pt_SYMBOL
';'                     _dont_care_                 pt_SEMICOLON       [ss_START ss_PROPERTY]
':'                     _dont_care_                 pt_SYMBOL
'@'                     _dont_care_                 pt_SYMBOL
'#'                     _dec_digit_                 pt_CHAR_DECIMAL
'#$'                    _hex_digit_                 pt_CHAR_HEX

//
// TSyntaxMemo v2 introduced keyword tables. These are sets of common language
// keywords (normally reserved words in the source language) that share a common
// lexeme definition.
//
// Keyword tables are separated by the active states of the parser. It is possible
// to specify a set of states that must be present for a given keyword table to
// be used. If no state specification is given then the keyword table will be
// used in all cases.
//
// Keyword tables may be changed at run-time by specifying the state(s) in which
// the keywords are valid via the DefineKeywordTable method.
//
%%kwtables

ForState [ss_PROPERTY] endswith _non_alpha_
  ( pt_DEFAULT_TOKEN is 'default'
    pt_READ          is 'read'
    pt_STORED        is 'stored'
    pt_WRITE         is 'write')

ForState [ss_EXPORTS] endswith _non_alpha_
  ( pt_INDEX         is 'index'
    pt_NAME          is 'name'
    pt_RESIDENT      is 'resident')

//
// Default keyword table
//
    ForState [] endswith _non_alpha_
      ( pt_RESERVED  is 'and', 'array', 'as', 'asm', 'absolute', 'abstract', 'assembler', 'at', 'automated',
                        'begin',
                        'case', 'const', 'class', 'constructor', 'cdecl',
                        'div', 'do', 'downto', 'destructor', 'dispid', 'dynamic',
                        'else', 'end', 'except', 'external',
                        'false', 'file', 'for', 'forward', 'function', 'finalization', 'finally',
                        'goto',
                        'if', 'in', 'implementation', 'implements', 'interface', 'inherited', 'initialization', 'inline', 'is',
                        'label', 'library',
                        'mod', 'message',
                        'nil', 'not', 'nodefault',
                        'of', 'or', 'on', 'object', 'out', 'overload', 'override',
                        'procedure', 'program', 'packed', 'pascal', 'private', 'protected', 'public', 'published',
                        'record', 'repeat', 'raise', 'read', 'register', 'reintroduce', 'resident', 'resourcestring',
                        'set', 'string', 'shl', 'shr', 'stdcall',
                        'then', 'to', 'true', 'type', 'threadvar', 'try',
                        'until', 'unit', 'uses',
                        'var', 'virtual',
                        'while', 'with',
                        'xor'

        pt_EXPORTS   is 'exports'
        pt_PROPERTY  is 'property')



//--------------------------------------------------------------------------------------------------------------------
//
// %%handler section
//
// The %%handler section gives rules to be applied once an entry in the %%words table has been recognised. Normally
// no further processing is required but sometimes a token starts with a fixed set of characters but may be followed
// by a character class rather than a known sequence.
//
// %%handler table entries have 4 columns:
//     Column 1          Token value to be processed
//     Column 2          Character specifier that follows recognised word
//     Column 3          Quoted string specifying end sequence
//     Column 4          Whether end sequence is part of lexeme
//
// The <character specifier> is defined as:
//     Column 2          A single character specifier or pre-defined system character macro:
//                         _PASCAL_CHAR         Pascal style character specifier
//                         _C_CHAR              C style character specifier
//                       If the lexeme can optionally have these characters then append '?' to the end
//                       of the quoted string.
//     Column 3          Up to 2 characters may be given as a sequence to terminate the lexeme.
//                       Characters are specified using a simplified regular expression. If this
//                       string is empty then the lexeme will never be matched.
//
%%handler
pt_COMMENT_LINE           '[^\n]'?                    '\n'           _discard_
pt_COMMENT_BRACE          '[^}]'?                     '}'            _use_
pt_COMMENT_STAR           _all_chars_?                '*)'           _use_
pt_CHAR_DECIMAL           _dec_digit_                 '[^0-9]'       _discard_
pt_CHAR_HEX               _hex_digit_                 '[^a-fA-F0-9]' _discard_

//--------------------------------------------------------------------------------------------------------------------
//
// %%tokens section
//
// Used to specify how to recognise non-fixed lexemes. Non-fixed lexemes are only tried if the table entries in the
// %%words section have failed. The non-fixed lexeme is defiened by 5 columns as below:
//     Column 1          Token value
//     Column 2          Single start character specifier
//     Column 3          Single contains character specifier
//     Column 4          End sequence specifier
//     Column 5          Whether end sequence is part of lexeme
// Pre-defined token styles are implemented. If used they should be specified in Column 2. The implemented ones
// are:
//  __STD_PASCALSTRING   Pascal string -- starts with ' ands with ' and uses '' to represent
//                       ' within a string. Does not extend beywond end of line.
//  __STD_IDENTIFIER     Standard identifier. Starts with [_a-zA-Z], contains [_a-zA-Z0-9] and ends with
//                       a non-alpha numeric character that is not part of the lexeme
//  __STD_NUMBER_OR_FP   Integer or floating point constant of syntax:
//                           <Digit String> [ '.' <Digit string> ] [ e|E [+-] <Digit string> ]
//
%%tokens
pt_HEXNUMBER            '$'                         '[0-9a-fA-F]'       '[^0-9a-fA-F]'        _discard_
pt_STRING               __STD_PASCALSTRING
pt_IDENTIFIER           __STD_IDENTIFIER
pt_NUMBER               __STD_NUMBER_OR_FP
pt_DEFAULT              '[\s\t\n]'                  '[\s\t\n]'?         '[^\s\t\n]'           _discard_


//--------------------------------------------------------------------------------------------------------------------
//
// %%effects section
//
// Used to specify the default colors and font styles used for each token
//
//     Column 1          Token value
//     Column 2          Font styles
//     Column 3          Foreground color
//     Column 4          Background color
//     Column 5          Optional column specifying whether map entry is a 'hotspot'
//
// Columns 1-4 must be completed for all rows, Column 5 defaults to non-hotspot.
//
%%effects
#ifdef ver200
//
// Version 2 introduced the overlay of a default color scheme for effects. When active an effect
// will use the colors in effect slot zero (the default color scheme).
// To specify effects that use the default color scheme to override their normal colors, append
// an asterisk to the end of the color value in either the foreground or background column or both.
//
pt_DEFAULT              []                          _DEFAULT_FOREGROUND*        _DEFAULT_BACKGROUND*
pt_IDENTIFIER           []                          _DEFAULT_FOREGROUND*        _DEFAULT_BACKGROUND*
pt_STRING               [fsItalic]                  _DEFAULT_FOREGROUND*        _DEFAULT_BACKGROUND*
pt_COMMENT              [fsItalic]                  clBlue                      _DEFAULT_BACKGROUND*
pt_RESERVED             [fsBold]                    _DEFAULT_FOREGROUND*        _DEFAULT_BACKGROUND*
pt_NUMBER               []                          clGreen                     _DEFAULT_BACKGROUND*
pt_SYMBOL               []                          _DEFAULT_FOREGROUND*        _DEFAULT_BACKGROUND*
#else

pt_DEFAULT              []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
pt_IDENTIFIER           []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
pt_STRING               [fsItalic]                  _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
pt_COMMENT              [fsItalic]                  clBlue                      _DEFAULT_BACKGROUND
pt_RESERVED             [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
pt_NUMBER               []                          clGreen                     _DEFAULT_BACKGROUND
pt_SYMBOL               []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
#endif


//--------------------------------------------------------------------------------------------------------------------
//
// %%map section
//
// Used to specify which entry in the %%effects table each token value uses. By default all token values map onto
// __DEFAULT_TOKEN which is defined as zero. Table has 2 columns:
//     Column 1          Recognised token value
//     Column 2          Map table entry (i.e. %%effects table entry)
// Normally the %%map table consists of identical value entries.
%%map
pt_IDENTIFIER           pt_IDENTIFIER
pt_STRING               pt_STRING
pt_HEXNUMBER            pt_NUMBER
pt_NUMBER               pt_NUMBER
pt_COMMENT              pt_COMMENT
pt_COMMENT_LINE         pt_COMMENT
pt_COMMENT_STAR         pt_COMMENT
pt_COMMENT_BRACE        pt_COMMENT
pt_RESERVED             pt_RESERVED
pt_SYMBOL               pt_SYMBOL
pt_SEMICOLON            pt_SYMBOL
pt_PROPERTY             pt_RESERVED
pt_READ                 pt_RESERVED
pt_WRITE                pt_RESERVED
pt_DEFAULT_TOKEN        pt_RESERVED
pt_STORED               pt_RESERVED
pt_EXPORTS              pt_RESERVED
pt_NAME                 pt_RESERVED
pt_INDEX                pt_RESERVED
pt_RESIDENT             pt_RESERVED
pt_CHAR_DECIMAL         pt_STRING
pt_CHAR_HEX             pt_STRING

%%states
pt_PROPERTY             (+[ss_PROPERTY] -[ss_START])
pt_SEMICOLON            (+[ss_START]    -[ss_PROPERTY ss_EXPORTS])
pt_EXPORTS              (+[ss_EXPORTS])

//
// v2 specific key assignments separated by conditional compilation
//
%%keys
caLEFT                  ([] Left)                     'Cursor Left'
caRIGHT                 ([] Right)                    'Cursor Right'
caLINEHOME              ([] Home)                     'Line start'
caLINEEND               ([] End)                      'Line end'
caUP                    ([] Up)                       'Line up'
caDOWN                  ([] Down)                     'Line down'
caPAGEUP                ([] PgUp)                     'Page up'
caPAGEDOWN              ([] PgDn)                     'Page down'
caWORDLEFT              ([Ctrl] Left)                 'Word left'
caWORDRIGHT             ([Ctrl] Right)                'Word right'
caDOCSTART              ([Ctrl] Home)                 'Document start'
caDOCEND                ([Ctrl] End)                  'Document end'
caCUT                   ([Ctrl] 'X')                  'Cut to clipboard'
caCOPY                  ([Ctrl] 'C'     |
                         [Ctrl]  INSERT)              'Copy to clipboard'
caPASTE                 ([Ctrl] 'V'     |
                         [Shift] INSERT)              'Paste from clipboard'
caDELETE                ([] Delete)                   'Delete at cursor'
caBACKSPACE             ([] Backspace)                'Delete before cursor'
caBLOCKIND              ([Ctrl] 'K', 'I')             'Indent block'
caBLOCKUND              ([Ctrl] 'K', 'U')             'Un-Indent block'
caINSTOGGLE             ([] Insert)                   'Toggle insert / override mode'
caSETBOOKMARK0          ([Ctrl] 'K', '0')             'Set bookmark 0'
caSETBOOKMARK1          ([Ctrl] 'K', '1')             'Set bookmark 1'
caSETBOOKMARK2          ([Ctrl] 'K', '2')             'Set bookmark 2'
caSETBOOKMARK3          ([Ctrl] 'K', '3')             'Set bookmark 3'
caSETBOOKMARK4          ([Ctrl] 'K', '4')             'Set bookmark 4'
caSETBOOKMARK5          ([Ctrl] 'K', '5')             'Set bookmark 5'
caSETBOOKMARK6          ([Ctrl] 'K', '6')             'Set bookmark 6'
caSETBOOKMARK7          ([Ctrl] 'K', '7')             'Set bookmark 7'
caSETBOOKMARK8          ([Ctrl] 'K', '8')             'Set bookmark 8'
caSETBOOKMARK9          ([Ctrl] 'K', '9')             'Set bookmark 9'
caGOTOBOOKMARK0         ([Ctrl] 'Q', '0')             'Goto bookmark 0'
caGOTOBOOKMARK1         ([Ctrl] 'Q', '1')             'Goto bookmark 1'
caGOTOBOOKMARK2         ([Ctrl] 'Q', '2')             'Goto bookmark 2'
caGOTOBOOKMARK3         ([Ctrl] 'Q', '3')             'Goto bookmark 3'
caGOTOBOOKMARK4         ([Ctrl] 'Q', '4')             'Goto bookmark 4'
caGOTOBOOKMARK5         ([Ctrl] 'Q', '5')             'Goto bookmark 5'
caGOTOBOOKMARK6         ([Ctrl] 'Q', '6')             'Goto bookmark 6'
caGOTOBOOKMARK7         ([Ctrl] 'Q', '7')             'Goto bookmark 7'
caGOTOBOOKMARK8         ([Ctrl] 'Q', '8')             'Goto bookmark 8'
caGOTOBOOKMARK9         ([Ctrl] 'Q', '9')             'Goto bookmark 9'
caUNDO                  ([Ctrl] 'Z')                  'Undo'
caREDO                  ([Ctrl Shift] 'Z')            'Redo'
caPRINTSEL              ([CTRL] 'K', 'P')             'Print selection'
//
// Auto-replace specifiers
//
// Format:
//       ActionKeys = <string>
//       <source string> = <replacement string>
//
// Notes: If ActionKeys is not specified then it will default to ';,:.=[]\n\t\s'
//
%%autoreplace           ActionKeys    = ';,:.=()[]\{Return}\{Tab}\{Space}'
                        'teh'         = 'the'
                        '(c)'         = '�'
                        '(r)'         = '�'
                        '(tm)'        = '�'


//
// Code-template specifiers
//
// Format:
//       Hotkey = <Key specifier>
//       <template short name>  [ <template description> ] <template value>
//
// Notes: Within the <template value> specifier, the position of the first '|' (vertical bar) character indicates
//        the position of the caret after the replacement has taken place. If there is no '|' character then the
//        caret will be at the end of the template
//        If Hotkey is not specified then it will default to CTRL J
//
%%templates
Hotkey =  ([Ctrl] 'J')
'headgp' ( 'Procedure header - general' ) = '///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n\
                                            \//\n\
                                            \//  @procedure    |\n\
                                            \//  @author       David Brock - dbrock@cqm.co.uk\n\
                                            \//\n\
                                            \//\n\
                                            \//\n\
                                            \///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n\
                                            procedure '

'casee' ( 'Case with else clause' )       = 'case | of\n\
                                            \   :\n\
                                            \   :\n\
                                            \  else\n\
                                            \ end;\n'

  �  4   ��
 H T M L P A R S E R         0 	        /****************************************************************************\
* TSyntaxMemoParser Script                                                   *
* ------------------------                                                   *
*                                                                            *
* Author  :          David Brock                                             *
* Date    :          March 28 1998                                           *
* Language:          HTML with JavaScript and VBScript                       *
\****************************************************************************/

/*--------------------------------------------------------------------------------------------------------------------
 *
 * Token value definitions
 *
 */
#define ht_DEFAULT                  0
#define ht_NUMBER                   1
#define ht_STRING                   2
#define ht_IDENTIFIER               3
#define ht_TAGNAME                  4
#define ht_ATTRIBUTE                5
#define ht_TAGSTART                 6
#define ht_TAGEND                   7
#define ht_TAGCLOSE                 8
#define ht_TAG_SCRIPT_START         9
#define ht_TAG_SCRIPT_END           10
#define ht_COMMENT                  11
#define ht_EQUAL                    12

#define ht_COMMENT_END              13
#define ht_WHITESPACE               14
#define ht_PLAIN_ID                 15
#define ht_SPECIAL                  16
#define ht_TAG_UNK                  17
#define ht_WEBURL                   18
#define ht_MAILURL                  19
#define ht_FIELD_UNK                20

#define ht_TAGNAME                  21
#define ht_TAGCLOSE_SCRIPT          22

#define jt_RESERVED                 30
#define jt_COMMENT                  31
#define jt_WHITESPACE               32
#define jt_COMMENT_LINE             33
#define jt_STRING                   34
#define jt_STRING_CHAR              35
#define jt_IDENTIFIER               36
#define jt_NUMBER                   37
#define jt_SYMBOL                   38
#define jt_BUILTINMETHOD            39
#define jt_HEXNUMBER                40
#define jt_IDENTIFIER               41

#define vb_FUNCTION                 50
#define vb_METHOD                   51
#define vb_RESERVED                 52
#define vb_COMMENT                  53
#define vb_SYMBOL                   54
#define vb_STRING                   55
#define vb_IDENTIFIER               56
#define vb_NUMBER                   57
#define vb_WHITESPACE               58

#define sv_LANGUAGE                 60
#define sv_JAVA_STRING              61
#define sv_VB_STRING                62
#define ht_JS_SCRIPT                63
#define ht_VB_SCRIPT                64

/*--------------------------------------------------------------------------------------------------------------------
 *
 * State definitions
 *
 */
#define ss_START                    0
#define ss_SCRIPT                   1
#define ss_STRING                   2
#define ss_INTAG                    3
#define ss_ATTRIBUTES               4
#define ss_JAVASCRIPT               5
#define ss_VBSCRIPT                 6
#define ss_LANGTYPE                 7
#define ss_JAVA_LANG                8
#define ss_VB_LANG                  9

/*--------------------------------------------------------------------------------------------------------------------
 *
 * Common script elements
 *
 */
#define _non_alpha_                 '[^_A-Za-z0-9]'
#define _all_chars_                 '[\x00-\xFF]'
#define _no_chars_                  '[]'
#define _dont_care_                 _all_chars_
#define _DEFAULT_BACKGROUND         clWhite
#define _DEFAULT_FOREGROUND         clBlack




/*--------------------------------------------------------------------------------------------------------------------
 *
 * Language style definitions
 *
 * NAME            Short description of the language defined in this script
 *                 Default is <Custom language>
 *
 * CASE            Case-sensitivity of the characters of the language.
 *                 May take the following values:
 *                   __INSENSITIVE     No case sensitivity
 *                   __SENSITIVE       Case sensitive
 *                 Default is __INSENSITIVE
 *
 * OPTIONS         TSyntaxMemo default start-up options.
 *                 May take the following values:
 *                       __DEFAULT_OPTIONS            Component default options, denoted by (*) below
 *                       __OPT_SHOW_RIGHT_MARGIN      (*) Show right margin position
 *                       __OPT_SHOW_GUTTER            (*) Show left gutter area
 *                       __OPT_PRINT_WRAP             (*) Wrap lines when printing
 *                       __OPT_PRINT_LINENOS          (*) Print line numbers
 *                       __OPT_PRINT_FILENAME         (*) Print filename in header
 *                       __OPT_PRINT_DATE             (*) Print date in header
 *                       __OPT_PRINT_PAGENOS          (*) Print page numbers in footer
 *                       __OPT_WORDWRAP               ( ) Word-wrap text
 *                       __OPT_AUTOINDENT             (*) Auto-indent new lines
 *                       __OPT_TABCOLUMN              (*) 'Smart' tab key action
 *                       __OPT_WRAP_OVERRIDE          ( ) Override word-wrap on certain lines
 *                       __WORD_SELECT                (*) Double click selects word at cursor
 *                       __OPT_SHOW_RMARGIN           (*) Show right margin position (Synonym for __OPT_SHOW_RIGHT_MARGIN)
 *                       __OPT_SHOW_WRAPCOLUMN        ( ) Show word-wrap column in word-wrap mode
 *                       __OPT_TITLE_AS_FILENAME      (*) Set print page title to filename when loading/saving
 *                       __OPT_PROCESS_DROP_FILES     ( ) Generate event for dropping of files from explorer
 *                       __OPT_BLOCK_CURSOR           (*) Use block cursor in overwrite mode
 *                 The above options may be combined by adding (+) together the options desired
 *                 Default is __DEFAULT_OPTIONS
 *
 * WORDWRAPCOLUMN  Column number at which word-wrapping will occur.
 *                 May take the following values:
 *                       _EDGE                        Word-wrap at edge of view window
 *                       0                            Word-wrap at edge of view window (Synonym for _EDGE)
 *                       <n>                          Decimal column number
 *                 Default is _EDGE
 *
 * GUTTER          Width of left gutter in pixels.
 *                 May take a velue of:
 *                       0                            Left gutter is not visible
 *                       >= 34                        Gutter width
 *                 The gutter cannot be set to a value in the range 1..33 since space must be available
 *                 for the side by side glyphs on each line.
 *
 * STARTSTATE
 * ANCHOR
 * CONTEXTSTATES
 * EXAMPLETEXT
 * EDITABLESTYLES
 * FONTNAME
 * FONTSIZE
 * TABCOLUMNS
 * TABDEFAULT
 * INDENTSTEP
 * WRAPOVERRIDE
 * SELTEXTCOLOR
 * SELTEXTBACK
 *
 */
%%language
   Name                      = 'HTML'
   Case                      = __INSENSITIVE
   Options                   = __DEFAULT_OPTIONS
   WordWrapColumn            = _EDGE
   Gutter                    = _DEFAULT_GUTTER
   StartState                = [ss_START]
   Anchor                    = [ht_TAGSTART
                                jt_BUILTINMETHOD jt_SYMBOL             jt_RESERVED        jt_COMMENT
                                jt_STRING        jt_STRING_CHAR        jt_IDENTIFIER      jt_NUMBER
                                jt_HEXNUMBER
                                vb_FUNCTION      vb_METHOD             vb_RESERVED        vb_COMMENT
                                vb_SYMBOL        vb_STRING             vb_IDENTIFIER      vb_NUMBER]
   ContextStates             = (ht_JS_SCRIPT,    jt_BUILTINMETHOD,     jt_RESERVED,       jt_SYMBOL,
                                jt_COMMENT,      jt_STRING,            jt_STRING_CHAR,    jt_IDENTIFIER,
                                jt_NUMBER,       jt_HEXNUMBER
                                [ss_SCRIPT ss_JAVASCRIPT]),

                               (ht_VB_SCRIPT,    vb_FUNCTION,          vb_METHOD,         vb_RESERVED,
                                vb_COMMENT,      vb_SYMBOL,            vb_STRING,         vb_IDENTIFIER,
                                vb_NUMBER
                                [ss_SCRIPT ss_VBSCRIPT])
                                
   ExampleText               = '<!-- Syntax Highlighting -->\n\
                               \<HTML>\n\
                               \<HEAD><TITLE>New Page</TITLE></HEAD>\n\
                               \<p align=center>\n\
                               \Plain HTML body text&nbsp;</p>\n\
                               \<a href="mailto:dbrock@cqm.co.uk">Author</a>\n\
                               \</BODY>\n\
                               \</HTML>\n'

   EditableStyles              ('Comment',       ht_COMMENT),
                               ('String',        ht_STRING),
                               ('Tag name',      ht_TAGNAME),
                               ('Field name',    ht_ATTRIBUTE),
                               ('Special char',  ht_SPECIAL),
                               ('Number',        ht_NUMBER),
                               ('Web URLs',      ht_WEBURL),
                               ('JS Comment',    jt_COMMENT),
                               ('JS Keyword',    jt_RESERVED),
                               ('JS Method',     jt_BUILTINMETHOD),
                               ('JS Symbol',     jt_SYMBOL),
                               ('JS String',     jt_STRING),
                               ('JS Indentifier',jt_IDENTIFIER),
                               ('JS Number',     jt_NUMBER),
                               ('VB Comment',    vb_COMMENT),
                               ('VB Keyword',    vb_RESERVED),
                               ('VB Method',     vb_METHOD),
                               ('VB Function',   vb_FUNCTION),
                               ('VB Symbol',     vb_SYMBOL),
                               ('VB String',     vb_STRING),
                               ('VB Indentifier',vb_IDENTIFIER),
                               ('VB Number',     vb_NUMBER),
                               ('Default',       ht_DEFAULT)


/********************************************************************************************************************\
*                                                                                                                    *
*                                 H T M L      SPECIFIC SCRIPT ITEMS                                                 *
*                                 ==================================                                                 *
*                                                                                                                    *
\********************************************************************************************************************/



/*--------------------------------------------------------------------------------------------------------------------
 *
 * Fixed lexeme definitions
 *
 */
%%words
'</'                 _dont_care_              ht_TAGEND
'<'                  _dont_care_              ht_TAGSTART
'>'                  _dont_care_              ht_TAGCLOSE                [ss_INTAG ss_ATTRIBUTES]
'>'                  _dont_care_              ht_TAGCLOSE_SCRIPT         [ss_SCRIPT]
'<!--'               _dont_care_              ht_COMMENT                 [ss_START]
'<!--'               _dont_care_              ht_JS_SCRIPT               [ss_JAVA_LANG ss_JAVASCRIPT]
'<!--'               _dont_care_              ht_VB_SCRIPT               [ss_VB_LANG   ss_VBSCRIPT]
'-->'                _dont_care_              ht_COMMENT_END             [ss_SCRIPT ss_JAVASCRIPT ss_VBSCRIPT]
'<!'                 _dont_care_              ht_TAGSTART
'</script'           _dont_care_              ht_TAG_SCRIPT_END          [ss_START ss_JAVASCRIPT ss_VBSCRIPT]
'<script'            _non_alpha_              ht_TAG_SCRIPT_START
'language'           _non_alpha_              sv_LANGUAGE                [ss_SCRIPT]
'javascript'         _dont_care_              sv_JAVA_STRING             [ss_SCRIPT ss_LANGTYPE]
'javascript1.1'      _dont_care_              sv_JAVA_STRING             [ss_SCRIPT ss_LANGTYPE]
'vbscript'           _dont_care_              sv_VB_STRING               [ss_SCRIPT ss_LANGTYPE]
'"javascript"'       _dont_care_              sv_JAVA_STRING             [ss_SCRIPT ss_LANGTYPE]
'"javascript1.1"'    _dont_care_              sv_JAVA_STRING             [ss_SCRIPT ss_LANGTYPE]
'"vbscript"'         _dont_care_              sv_VB_STRING               [ss_SCRIPT ss_LANGTYPE]

'='                  _dont_care_              ht_EQUAL                   [ss_ATTRIBUTES]
'a'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'address'            _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'align'              _non_alpha_              ht_ATTRIBUTE               [ss_INTAG]
'alt'                _non_alpha_              ht_ATTRIBUTE               [ss_INTAG]
'applet'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'b'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'basefont'           _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'big'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'blink'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'blockquote'         _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'body'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'br'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'caption'            _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'center'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'cite'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'code'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'codebase'           _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'dd'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'dir'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'div'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'dl'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'doctype'            _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'dt'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'em'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'embed'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'font'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'form'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'frame'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'frameset'           _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h1'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h2'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h3'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h4'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h5'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'h6'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'head'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'height'             _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'hr'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'href'               _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'hspace'             _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'html'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'i'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'img'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'input'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'isindex'            _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'kbd'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'li'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'link'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'mark'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'marquee'            _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'menu'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'meta'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'methods'            _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'name'               _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'nextid'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'nobr'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'noframes'           _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'ol'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'option'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'p'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'pre'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'samp'               _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'select'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'small'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'sound'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'strike'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'strong'             _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'sub'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'sup'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'rel'                _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'rev'                _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'table'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'target'             _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'td'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'textarea'           _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'th'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'title'              _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'tr'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'tt'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'u'                  _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'ul'                 _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'urn'                _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'value'              _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'var'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'vspace'             _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]
'wbr'                _non_alpha_              ht_TAGNAME                 [ss_INTAG]
'width'              _non_alpha_              ht_ATTRIBUTE               [ss_ATTRIBUTES]

'&szlig;'            _dont_care_              ht_SPECIAL
'&aacute;'           _dont_care_              ht_SPECIAL
'&agrave;'           _dont_care_              ht_SPECIAL
'&acirc;'            _dont_care_              ht_SPECIAL
'&atilde;'           _dont_care_              ht_SPECIAL
'&aring;'            _dont_care_              ht_SPECIAL
'&auml;'             _dont_care_              ht_SPECIAL
'&aelig;'            _dont_care_              ht_SPECIAL
'&ccedil;'           _dont_care_              ht_SPECIAL
'&copy;'             _dont_care_              ht_SPECIAL
'&eacute;'           _dont_care_              ht_SPECIAL
'&egrave;'           _dont_care_              ht_SPECIAL
'&ecirc;'            _dont_care_              ht_SPECIAL
'&eth;'              _dont_care_              ht_SPECIAL
'&euml;'             _dont_care_              ht_SPECIAL
'&gt;'               _dont_care_              ht_SPECIAL
'&iacute;'           _dont_care_              ht_SPECIAL
'&igrave;'           _dont_care_              ht_SPECIAL
'&icirc;'            _dont_care_              ht_SPECIAL
'&iuml;'             _dont_care_              ht_SPECIAL
'&lt;'               _dont_care_              ht_SPECIAL
'&nbsp;'             _dont_care_              ht_SPECIAL
'&ntilde;'           _dont_care_              ht_SPECIAL
'&oacute;'           _dont_care_              ht_SPECIAL
'&ograve;'           _dont_care_              ht_SPECIAL
'&ocirc;'            _dont_care_              ht_SPECIAL
'&otilde;'           _dont_care_              ht_SPECIAL
'&ouml;'             _dont_care_              ht_SPECIAL
'&oslash;'           _dont_care_              ht_SPECIAL
'&reg;'              _dont_care_              ht_SPECIAL
'&thorn;'            _dont_care_              ht_SPECIAL
'&trade;'            _dont_care_              ht_SPECIAL
'&uacute;'           _dont_care_              ht_SPECIAL
'&ugrave;'           _dont_care_              ht_SPECIAL
'&ucirc;'            _dont_care_              ht_SPECIAL
'&uuml;'             _dont_care_              ht_SPECIAL
'&yacute;'           _dont_care_              ht_SPECIAL
'&yuml;'             _dont_care_              ht_SPECIAL
'http://'            _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'ftp://'             _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'mailto:'            _dont_care_              ht_MAILURL                 [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'news:'              _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'telnet://'          _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'gopher://'          _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'nntp://'            _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'wais://'            _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'newsrc://'          _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]
'file://'            _dont_care_              ht_WEBURL                  [ss_START ss_INTAG ss_STRING ss_ATTRIBUTES]


/*--------------------------------------------------------------------------------------------------------------------
 *
 * Fixed lexeme follow on handlers
 *
 */
%%handler
ht_COMMENT              _all_chars_?                '\-\->'                    _use_
ht_MAILURL              __STD_MAIL_URL
ht_WEBURL               _WEB_CHAR                  _all_chars_               _discard_


/*--------------------------------------------------------------------------------------------------------------------
 *
 * Variable character tokens
 *
 */
%%tokens
ht_WHITESPACE           '[\x00-\s]'                 '[\x00-\s]'?       '[^\x00-\s]'     _discard_    [ss_INTAG ss_START ss_ATTRIBUTES]
ht_PLAIN_ID             __STD_IDENTIFIER                                                             [ss_INTAG ss_ATTRIBUTES]
ht_SPECIAL              '&'                         '[^;\n\s]'?        '[;\n\s]'        _use_

InState ss_INTAG
 (
   ht_TAG_UNK             '[a-zA-Z]'                '[a-zA-Z0-9\-]'    '[^a-zA-Z0-9\-]' _discard_    [ss_INTAG ss_ATTRIBUTES]
 )

InState ss_ATTRIBUTES
 (
   ht_FIELD_UNK           '[a-zA-Z]'                '[a-zA-Z0-9\-]'    '[^a-zA-Z0-9\-]' _discard_    [ss_INTAG ss_ATTRIBUTES]
   ht_STRING              __STD_HTML_STRING  [ss_INTAG ss_ATTRIBUTES]
   ht_NUMBER              __STD_NUMBER_OR_FP [ss_INTAG ss_ATTRIBUTES]
 )


/********************************************************************************************************************\
*                                                                                                                    *
*                                 J A V A S C R I P T   SPECIFIC SCRIPT ITEMS                                        *
*                                 ===========================================                                        *
*                                                                                                                    *
\********************************************************************************************************************/



/*--------------------------------------------------------------------------------------------------------------------
 *
 * Fixed lexeme definitions
 *
 */
%%words
'abstract'                  _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'boolean'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'break'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'byte'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'case'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'catch'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'char'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'class'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'const'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'continue'                  _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'default'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'do'                        _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'double'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'else'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'extends'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'false'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'final'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'finally'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'float'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'for'                       _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'function'                  _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'goto'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'if'                        _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'implements'                _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'import'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'in'                        _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'instanceof'                _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'int'                       _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'interface'                 _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'long'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'native'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'new'                       _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'null'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'package'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'private'                   _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'protected'                 _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'public'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'return'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'short'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'static'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'super'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'switch'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'synchroniz'                _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'this'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'throw'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'throws'                    _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'transient'                 _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'true'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'try'                       _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'var'                       _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'void'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'while'                     _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
'with'                      _non_alpha_       jt_RESERVED        [ss_JAVASCRIPT]
//
//
//
'abs'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'acos'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'alert'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'anchor'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'asin'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'atan'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'back'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'big'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'blink'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'blur'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'bold'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'ceil'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'charAt'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'clear'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'clearTimeout'              _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'click'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'close'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'confirm'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'cos'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'escape'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'eval'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'exp'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'fixed'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'floor'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'focus'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'fontcolor'                 _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'fontsize'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'forward'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getDate'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getDay'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getHours'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getMinutes'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getMonth'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getSeconds'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getTime'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getTimezoneOffset'         _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'getYear'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'go'                        _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'indexOf'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'isNaN'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'italics'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'lastIndexOf'               _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'link'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'log'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'max'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'min'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'open'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'parse'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'parseFloat'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'parseInt'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'pow'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'prompt'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'random'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'round'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'select'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setDate'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setHours'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setMinutes'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setMonth'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setSeconds'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setTimeout'                _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setTime'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'setYear'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'sin'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'small'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'sqrt'                      _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'strike'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'sub'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'submit'                    _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'substring'                 _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'sup'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'tan'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'toGMTString'               _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'toLocaleString'            _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'toLowerCase'               _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'toUpperCase'               _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'unescape'                  _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'UTC'                       _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'write'                     _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]
'writeln'                   _non_alpha_       jt_BUILTINMETHOD   [ss_JAVASCRIPT]

','                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'='                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'+='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'-='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'*='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'/='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'%='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'<<='                       _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>>='                       _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>>>='                      _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'&='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'^='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'|='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'?:'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'||'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'&&'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'|'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'^'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'&'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'=='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'!='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'<'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'<='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>='                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'<<'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>>'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'>>>'                       _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'+'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'-'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'*'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'/'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'%'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'!'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'~'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'++'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'--'                        _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'('                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
')'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'['                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
']'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]
'.'                         _dont_care_       jt_SYMBOL          [ss_JAVASCRIPT]

'0'                         '[xX]'            jt_HEXNUMBER       [ss_JAVASCRIPT]
'\/\/'                      _dont_care_       jt_COMMENT_LINE    [ss_JAVASCRIPT]


/*--------------------------------------------------------------------------------------------------------------------
 *
 * Fixed lexeme follow on handlers
 *
 */
%%handler
jt_COMMENT_LINE         __STD_JS_LINE_COMMENT
jt_HEXNUMBER            '[xX0-9A-Fa-f]'             '[^0-9a-fA-F]'           _discard_


/*--------------------------------------------------------------------------------------------------------------------
 *
 * Variable character tokens
 *
 */
%%tokens

InState ss_JAVASCRIPT
 (
   jt_IDENTIFIER           __STD_IDENTIFIER
   jt_STRING               __STD_C_STRING
   jt_NUMBER               __STD_NUMBER_OR_FP
   jt_WHITESPACE           '[\x00-\s]'      '[\x00-\s]'?  '[^\x00-\s]'  _discard_
 )


%%words
'Abs'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Array'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Asc'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Atn'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CBool'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CByte'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CCur'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CDate'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CDbl'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Chr'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CInt'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CLng'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Cos'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CreateObject'              _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CSng'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'CStr'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Date'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'DateAddFunction'           _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'DateDiff'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'DatePart'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'DateSerial'                _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'DateValue'                 _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Day'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Exp'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Filter'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Fix'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'FormatCurrency'            _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'FormatDateTime'            _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'FormatNumber'              _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'FormatPercent'             _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'GetObject'                 _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Hex'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Hour'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'InputBox'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'InStr'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'InStrRev'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Int'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsArray'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsDate'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsEmpty'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsNull'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsNumeric'                 _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'IsObject'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Join'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'LBound'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'LCase'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Left'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Len'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'LoadPicture'               _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Log'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'LTrim'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Mid'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Minute'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Month'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'MonthName'                 _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'MsgBox'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Now'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Oct'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Replace'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'RGB'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Right'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Rnd'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Round'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'RTrim'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'ScriptEngine'              _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'ScriptEngineBuildVersion'  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'ScriptEngineMajorVersion'  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'ScriptEngineMinorVersion'  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Second'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Sgn'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Sin'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Space'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Split'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Sqr'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'StrComp'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'StrReverse'                _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'String'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Tan'                       _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Time'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'TimeSerial'                _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'TimeValue'                 _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Trim'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'TypeName'                  _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'UBound'                    _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'UCase'                     _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'VarType'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Weekday'                   _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'WeekdayName'               _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Year'                      _non_alpha_       vb_FUNCTION   [ss_VBSCRIPT]
'Add'                       _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'AddFolders'                _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'BuildPath'                 _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Clear'                     _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Close'                     _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Copy'                      _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'CopyFile'                  _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'CopyFolder'                _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'CreateFolder'              _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'CreateTextFile'            _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Delete'                    _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'DeleteFile'                _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'DeleteFolder'              _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'DriveExists'               _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Exists'                    _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'FileExists'                _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'FolderExists'              _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetAbsolutePathName'       _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetBaseName'               _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetDrive'                  _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetDriveName'              _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetExtensionName'          _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetFile'                   _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetFileName'               _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetFolder'                 _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetParentFolderName'       _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetSpecialFolder'          _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'GetTempName'               _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Items'                     _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Keys'                      _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Move'                      _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'MoveFile'                  _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'MoveFolder'                _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'OpenAsTextStream'          _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'OpenTextFile'              _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Raise'                     _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Read'                      _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'ReadAll'                   _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'ReadLine'                  _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Remove'                    _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'RemoveAll'                 _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Skip'                      _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'SkipLine'                  _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'Write'                     _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'WriteBlankLines'           _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'WriteLine'                 _non_alpha_       vb_METHOD     [ss_VBSCRIPT]
'call'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'case'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'const'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'dim'                       _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'do'                        _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'each'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'else'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'elseif'                    _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'end'                       _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'erase'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'error'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'exit'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'explicit'                  _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'for'                       _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'function'                  _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'if'                        _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'in'                        _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'loop'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'next'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'nothing'                   _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'on'                        _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'option'                    _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'preserve'                  _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'private'                   _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'public'                    _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'randomize'                 _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'redim'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'rem'                       _non_alpha_       vb_COMMENT    [ss_VBSCRIPT]
'\''                        _dont_care_       vb_COMMENT    [ss_VBSCRIPT]
'resume'                    _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'select'                    _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'set'                       _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'step'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'sub'                       _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'then'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'to'                        _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'until'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'wend'                      _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]
'while'                     _non_alpha_       vb_RESERVED   [ss_VBSCRIPT]

','                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'='                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'+='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'-='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'*='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'/='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'%='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'<<='                       _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>>='                       _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>>>='                      _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'&='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'^='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'|='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'?:'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'||'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'&&'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'|'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'^'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'&'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'=='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'!='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'<'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'<='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>='                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'<<'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>>'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'>>>'                       _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'+'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'-'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'*'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'/'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'%'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'!'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'~'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'++'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'--'                        _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'('                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
')'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'['                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
']'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]
'.'                         _dont_care_       vb_SYMBOL          [ss_VBSCRIPT]

/*--------------------------------------------------------------------------------------------------------------------
 *
 * Fixed lexeme follow on handlers
 *
 */
%%handler
vb_COMMENT              __STD_JS_LINE_COMMENT



/*--------------------------------------------------------------------------------------------------------------------
 *
 * Variable character tokens
 *
 */
%%tokens

InState ss_VBSCRIPT
 (
   vb_IDENTIFIER           __STD_IDENTIFIER
   vb_STRING               __STD_C_STRING
   vb_NUMBER               __STD_NUMBER_OR_FP
   vb_WHITESPACE           '[\x00-\s]'      '[\x00-\s]'?  '[^\x00-\s]'  _discard_
 )

#ifdef ht_DEFAULT
/*--------------------------------------------------------------------------------------------------------------------
 *
 * Display effects specification
 *
 */
%%effects
ht_DEFAULT              []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ht_TAGNAME              [fsBold]                    clBlue                      _DEFAULT_BACKGROUND
ht_COMMENT              []                          clGreen                     _DEFAULT_BACKGROUND
ht_IDENTIFIER           [fsBOLD]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ht_STRING               []                          clRed                       _DEFAULT_BACKGROUND
ht_NUMBER               []                          clRed                       _DEFAULT_BACKGROUND
ht_ATTRIBUTE            [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ht_SPECIAL              [fsItalic]                  _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ht_WEBURL               [fsUnderline]               clBlue                      _DEFAULT_BACKGROUND  'hotspot'

jt_COMMENT              [fsItalic]                  clBlue                      _DEFAULT_BACKGROUND
jt_RESERVED             [fsBold]                    clGreen                     _DEFAULT_BACKGROUND
jt_BUILTINMETHOD        [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
jt_SYMBOL               []                          clGreen                     _DEFAULT_BACKGROUND
jt_STRING               [fsItalic]                  _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
jt_IDENTIFIER           [fsUnderline]               _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
jt_NUMBER               []                          clFuchsia                   _DEFAULT_BACKGROUND

vb_COMMENT              [fsItalic]                  clBlue                      _DEFAULT_BACKGROUND
vb_RESERVED             [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
vb_METHOD               [fsUnderline]               _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
vb_FUNCTION             [fsUnderline]               _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
vb_SYMBOL               []                          clGreen                     _DEFAULT_BACKGROUND
vb_STRING               [fsItalic]                  _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
vb_IDENTIFIER           [fsUnderline]               _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
vb_NUMBER               []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
#endif

/*--------------------------------------------------------------------------------------------------------------------
 *
 * State switching definitions
 *
 * State switching is used in the following 2 situations:
 * [1] HTML <script TAG
 *     It is required to leave the <script.......> tag with the following states active:
 *          ss_SCRIPT...........In middle of <script>........</script> section
 *          ss_JAVASCRIPT.......Javascript may be encountered after tag close (default)
 *          ss_VB_SCRIPT........VBScript may be encountered after tag close
 *          ss_VB_LANG..........Recognise common HTML/VBScript lexemes
 *          ss_JAVA_LANG........Recognise common HTML/JavaScript lexemes
 *     The scenario is that the '<script' lexeme defined in %%words as ht_TAG_SCRIPT_START triggers the state
 *     machine transition as below:
 *        ht_TAG_SCRIPT_START ----------> +ss_ATTRIBUTES      Allow recognition of tag attribute names
 *                                        +ss_SCRIPT          Now within a <script>....</script> section
 *                                        +ss_JAVASCRIPT      Enable default script language style
 *                                        +ss_JAVA_LANG       Allow common Javascript/HTML lexemes to be separated
 *
 *     Thereafter, the following lexemes may be encountered in the order listed:
 *        sv_LANGUAGE ------------------> +ss_LANGTYPE        Allow recognition of 'language' attribute values
 *        sv_JAVA_STRING ---------------> +ss_JAVA_LANG       Allow common Javascript/HTML lexemes to be separated
 *                                        +ss_JAVASCRIPT      Enable Javascript lexeme recognition
 *        sv_VB_STRING -----------------> +ss_VB_LANG         Allow common VBscript/HTML lexemes to be separated
 *                                        +ss_VBSCRIPT        Enable VBscript lexeme recognition
 *                                        -ss_JAVASCRIPT      Disable Javascript lexeme recognition
 *                                        -ss_JAVA_LANG       Disable common Javascript/HTML lexeme recognition
 *        ht_TAGCLOSE_SCRIPT -----------> -ss_LANGTYPE        Ensure redundant state is off now
 *     Thus at the end of the script tag we will have ss_SCRIPT, one of (ss_JAVA_LANG, ss_VB_LANG) and one
 *     of (ss_JAVASCRIPT, ss_VBSCRIPT) active.
 *     Finally for the recognition of '</script' we must turn off the scripting states:
 *        ht_TAG_SCRIPT_END ------------> -ss_JAVASCRIPT      Disable Javascript lexeme recognition
 *                                        -ss_VBSCRIPT        Disable VBscript lexeme recognition
 *                                        -ss_VB_LANG         Disable common VBScript/HTML lexeme recognition
 *                                        -ss_JAVA_LANG       Disable common Javascript/HTML lexeme recognition
 *                                        -ss_SCRIPT          Disable script lexeme recognition
 *                                        +ss_ATTRIBUTES      Enable recognition of tag attribute names
 *     At the conclusion of the '</script' tag with '>' we will return to the default state:
 *        ht_TAGCLOSE ------------------> -ss_INTAG           Do not recognise tag names now
 *                                        -ss_ATTRIBUTES      Do not recognise attribute names now
 *     Thus the default state [ss_START] will be active at the end of a complete <script>.....</script> section
 *
 * [2] OTHER HTML TAGS
 */
%%states
// ht_TAGEND == '</'
// Turn on ss_INTAG to allow recognition of tag names
ht_TAGEND                     (+[ss_INTAG])

// ht_TAGSTART == '<'
//             == '<!'
// Turn on ss_INTAG to allow recognition of tag names
ht_TAGSTART                   (+[ss_INTAG])

// ht_TAGCLOSE == '>'
// *** NOT inside a <script> tag
// Turn off ss_INTAG and ss_ATTRIBUTES so that normal text will be recognised
ht_TAGCLOSE                   (                                          -[ss_INTAG ss_ATTRIBUTES])

// ht_TAGNAME  == <identifier>
// *** Only within a tag
// Turn on ss_ATTRIBUTES to recognise attribute names
//      off ss_INTAG so that tagnames will not be recognised now
ht_TAGNAME                    (+[ss_ATTRIBUTES]                          -[ss_INTAG])

// ht_TAG_UNK  == <identifier>
// *** Only within a tag
// Treat the same as a known tag....turn on ss_ATTRIBUTES to recognise attribute names
//                                       off ss_INTAG so that tagnames will not be recognised now
ht_TAG_UNK                    (+[ss_ATTRIBUTES]                          -[ss_INTAG])

// ht_TAG_SCRIPT_START == '<script'
// Treat as a combination of ht_TAGSTART followed by ht_TAGNAME:
//    ON:     ss_ATTRIBUTES         Recognise attribute names
//            ss_SCRIPT             We are now inside a <script tag
//            ss_JAVASCRIPT         Contained script is JavaScript by default
ht_TAG_SCRIPT_START           (+[ss_SCRIPT ss_ATTRIBUTES ss_JAVASCRIPT ss_JAVA_LANG]  -[ss_START])

// ht_TAG_SCRIPT_END == '</script'
// Treat as a combination of ht_TAGEND followed by ht_TAGNAME:
//    ON:     ss_ATTRIBUTES         Recognise attribute names
//            ss_START              Re-enable default state again
//    OFF:    ss_JAVASCRIPT         No script items allowed after this
//            ss_VBSCRIPT           -------"-------"------"-----------
//            ss_SCRIPT             -------"-------"------"-----------
ht_TAG_SCRIPT_END             (+[ss_ATTRIBUTES ss_START]                 -[ss_SCRIPT ss_JAVASCRIPT ss_VBSCRIPT ss_JAVA_LANG ss_VB_LANG ss_LANGTYPE])

// ht_COMMENT_END == '-->'
// End of HTML style comment. This is only seen in script sections (since the default ht_COMMENT will
// eat up the end of normal comments)
ht_COMMENT_END                (+[ss_START]                               -[ss_JAVASCRIPT ss_VBSCRIPT])

// ht_TAGCLOSE_SCRIPT == '>'
// Close of '<script....' tag. We require to turn off all states just leaving the ss_SCRIPT and one of
// ss_JAVASCRIPT or ss_VBSCRIPT active
ht_TAGCLOSE_SCRIPT            (                                          -[ss_LANGTYPE ss_ATTRIBUTES ss_INTAG])

// sv_LANGUAGE == 'language'
// An attribute inside the '<script......' tag. Enable ss_LANGTYPE so that any following 'JavaScript' or 'VBScript'
// will be acted upon
sv_LANGUAGE                   (+[ss_LANGTYPE])

// sv_JAVA_STRING == 'JavaScript'
//                == '"JavaScript"'
// Attribute value of 'language' in a '<script.....' tag
// Turn on the ss_JAVA_LANG so that HTML comment styles can be mapped to the JavaScript comment styles. Also
// turn on ss_JAVASCRIPT and make sure that ss_VBSCRIPT is now off.
sv_JAVA_STRING                (+[ss_JAVA_LANG ss_JAVASCRIPT]             -[ss_VBSCRIPT])

// sv_VB_STRING == 'VBScript'
//              == '"VBScript"'
// Attribute value of 'language' in a '<script.....' tag
// Turn on the ss_VB_LANG so that HTML comment styles can be mapped to the VBScript comment styles. Also
// turn on ss_VBSCRIPT and make sure that ss_JAVASCRIPT is now off.
sv_VB_STRING                  (+[ss_VB_LANG ss_VBSCRIPT]               -[ss_JAVASCRIPT ss_JAVA_LANG])

%%containers
//ht_STRING                     (+[ss_STRING]                            -[ss_START ss_INTAG ss_ATTRIBUTES])



%%map
sv_LANGUAGE                   ht_ATTRIBUTE
sv_JAVA_STRING                ht_STRING
sv_VB_STRING                  ht_STRING

ht_DEFAULT                    ht_DEFAULT
ht_TAGNAME                    ht_TAGNAME
ht_COMMENT                    ht_COMMENT
ht_IDENTIFIER                 ht_ATTRIBUTE
ht_STRING                     ht_STRING
ht_ATTRIBUTE                  ht_ATTRIBUTE
ht_TAGSTART                   ht_TAGNAME
ht_TAGEND                     ht_TAGNAME
ht_TAGCLOSE                   ht_TAGNAME
ht_WEBURL                     ht_WEBURL
ht_MAILURL                    ht_WEBURL
ht_NUMBER                     ht_NUMBER
ht_SPECIAL                    ht_SPECIAL
ht_TAG_Script_End             ht_TAGNAME
ht_TAG_SCRIPT_START           ht_TAGNAME
ht_JS_Script                  jt_Comment
ht_Comment_End                jt_Comment
ht_FIELD_UNK                  ht_IDENTIFIER
ht_TAG_UNK                    ht_IDENTIFIER
ht_PLAIN_ID                   ht_DEFAULT
ht_EQUAL                      ht_DEFAULT
ht_TAGCLOSE_SCRIPT            ht_TAGCLOSE

jt_RESERVED                   jt_RESERVED
jt_STRING                     jt_STRING
jt_COMMENT                    jt_COMMENT
jt_IDENTIFIER                 jt_IDENTIFIER
jt_COMMENT_LINE               jt_COMMENT
jt_STRING                     jt_STRING
jt_STRING_CHAR                jt_STRING
jt_BUILTINMETHOD              jt_BUILTINMETHOD
jt_HEXNUMBER                  jt_NUMBER
jt_NUMBER                     jt_NUMBER
jt_IDENTIFIER                 jt_IDENTIFIER
jt_WHITESPACE                 ht_WHITESPACE
jt_SYMBOL                     jt_SYMBOL

vb_FUNCTION                   vb_FUNCTION
vb_METHOD                     vb_METHOD
vb_RESERVED                   vb_RESERVED
vb_COMMENT                    vb_COMMENT
vb_SYMBOL                     vb_SYMBOL
vb_STRING                     vb_STRING
vb_IDENTIFIER                 vb_IDENTIFIER
vb_NUMBER                     vb_NUMBER
vb_WHITESPACE                 ht_WHITESPACE
ht_VB_Script                  vb_COMMENT

%%keys
caLEFT                  ([] Left)                     'Cursor Left'
caRIGHT                 ([] Right)                    'Cursor Right'
caLINEHOME              ([] Home)                     'Line start'
caLINEEND               ([] End)                      'Line end'
caUP                    ([] Up)                       'Line up'
caDOWN                  ([] Down)                     'Line down'
caPAGEUP                ([] PgUp)                     'Page up'
caPAGEDOWN              ([] PgDn)                     'Page down'
caWORDLEFT              ([Ctrl] Left)                 'Word left'
caWORDRIGHT             ([Ctrl] Right)                'Word right'
caDOCSTART              ([Ctrl] Home)                 'Document start'
caDOCEND                ([Ctrl] End)                  'Document end'
caCUT                   ([Ctrl] 'X')                  'Cut to clipboard'
caCOPY                  ([Ctrl] 'C')                  'Copy to clipboard'
caPASTE                 ([Ctrl] 'V')                  'Paste from clipboard'
caDELETE                ([] Delete)                   'Delete at cursor'
caBACKSPACE             ([] Backspace)                'Delete before cursor'
caBLOCKIND              ([Ctrl] 'K', 'I')             'Indent block'
caBLOCKUND              ([Ctrl] 'K', 'U')             'Un-Indent block'
caINSTOGGLE             ([] Insert)                   'Toggle insert / override mode'
caSETBOOKMARK0          ([Ctrl] 'K', '0')             'Set bookmark 0'
caSETBOOKMARK1          ([Ctrl] 'K', '1')             'Set bookmark 1'
caSETBOOKMARK2          ([Ctrl] 'K', '2')             'Set bookmark 2'
caSETBOOKMARK3          ([Ctrl] 'K', '3')             'Set bookmark 3'
caSETBOOKMARK4          ([Ctrl] 'K', '4')             'Set bookmark 4'
caSETBOOKMARK5          ([Ctrl] 'K', '5')             'Set bookmark 5'
caSETBOOKMARK6          ([Ctrl] 'K', '6')             'Set bookmark 6'
caSETBOOKMARK7          ([Ctrl] 'K', '7')             'Set bookmark 7'
caSETBOOKMARK8          ([Ctrl] 'K', '8')             'Set bookmark 8'
caSETBOOKMARK9          ([Ctrl] 'K', '9')             'Set bookmark 9'
caGOTOBOOKMARK0         ([Ctrl] 'Q', '0')             'Goto bookmark 0'
caGOTOBOOKMARK1         ([Ctrl] 'Q', '1')             'Goto bookmark 1'
caGOTOBOOKMARK2         ([Ctrl] 'Q', '2')             'Goto bookmark 2'
caGOTOBOOKMARK3         ([Ctrl] 'Q', '3')             'Goto bookmark 3'
caGOTOBOOKMARK4         ([Ctrl] 'Q', '4')             'Goto bookmark 4'
caGOTOBOOKMARK5         ([Ctrl] 'Q', '5')             'Goto bookmark 5'
caGOTOBOOKMARK6         ([Ctrl] 'Q', '6')             'Goto bookmark 6'
caGOTOBOOKMARK7         ([Ctrl] 'Q', '7')             'Goto bookmark 7'
caGOTOBOOKMARK8         ([Ctrl] 'Q', '8')             'Goto bookmark 8'
caGOTOBOOKMARK9         ([Ctrl] 'Q', '9')             'Goto bookmark 9'
caUNDO                  ([Ctrl] 'Z')                  'Undo'
caREDO                  ([Ctrl Shift] 'Z')            'Redo'

 M  ,   ��
 C P A R S E R       0 	        /*
 * TSyntaxMemoParser Script
 * ------------------------
 *
 * Author  :          David Brock
 * Date    :          October 18 1997
 * Language:          ANSI C
 */

//--------------------------------------------------------------------------------------------------------------------
//
//
//
// Macro definitions. Parameters may be specified and the replacement text terminates with the end of
// line (watch trailing blanks).
//
#define ct_DEFAULT                  0
#define ct_COMMENT_LINE             1
#define ct_COMMENT_STAR             2
#define ct_IDENTIFIER               3
#define ct_STRING                   4
#define ct_NUMBER                   5
#define ct_COMMENT                  6
#define ct_OPERATOR                 7
#define ct_RESERVED                 8
#define ct_CHAR                     9
#define ct_DIRECTIVE                10
#define ct_MISC                     11
#define ct_KEYWORD                  12
#define ct_HEXNUMBER                13

#define _non_alpha_                 '[^_A-Za-z0-9]'
#define _all_chars_                 '[\x00-\xFF]'
#define _no_chars_                  '[]'
#define _dont_care_                 _all_chars_
#define _DEFAULT_BACKGROUND         clWhite
#define _DEFAULT_FOREGROUND         clBlack



//--------------------------------------------------------------------------------------------------------------------
//
// %%language section
//
// Header section. Describes the textual name of the language, case sensitivity and options used by the language.
//
%%language
Name                      = 'ANSI C'
Case                      = __SENSITIVE
Options                   = __DEFAULT_OPTIONS
WordWrapColumn            = _EDGE
Gutter                    = _DEFAULT_GUTTER
Anchor                    = _DEFAULT_START_ANCHOR
ExampleText               = '/* Comment */\n\
                            \#include <stdio.h>\n\
                            \char *documentation[] = {\n\
                            \     "String text\\n"}\n\
                            \a >> 2 > 3 ? 4 ? 5\n'

EditableStyles              ('Comment',       ct_COMMENT),
                            ('String',        ct_STRING),
                            ('Reserved word', ct_KEYWORD),
                            ('Operator',      ct_MISC),
                            ('Identifier',    ct_IDENTIFIER),
                            ('Directive',     ct_DIRECTIVE),
                            ('Number',        ct_NUMBER),
                            ('Default',       ct_DEFAULT)




//--------------------------------------------------------------------------------------------------------------------
//
// %%words section
//
// Used to specify simple languge keywords. These are constant value lexemes that always contain the same characters
// and only require the end style to be specified. The words present here will always be tried first. If they fail
// then the entries in the %%tokens section will be allowed to try a match.
//
// %%words table entries have 3 columns:
//     Column 1          Quoted string giving the characters that make up the word
//     Column 2          Quoted string that specifies how the word is terminated
//     Column 3          Token value returned when word is recognised
//
%%words
'++'                    _dont_care_               ct_OPERATOR
'--'                    _dont_care_               ct_OPERATOR
'+'                     _dont_care_               ct_OPERATOR
'-'                     _dont_care_               ct_OPERATOR
'*'                     _dont_care_               ct_OPERATOR
'/'                     _dont_care_               ct_OPERATOR
'&'                     _dont_care_               ct_OPERATOR
'!'                     _dont_care_               ct_OPERATOR
'~'                     _dont_care_               ct_OPERATOR
'%'                     _dont_care_               ct_OPERATOR
'>'                     _dont_care_               ct_OPERATOR
'<'                     _dont_care_               ct_OPERATOR
'>>'                    _dont_care_               ct_OPERATOR
'<<'                    _dont_care_               ct_OPERATOR
'>='                    _dont_care_               ct_OPERATOR
'<='                    _dont_care_               ct_OPERATOR
'=='                    _dont_care_               ct_OPERATOR
'='                     _dont_care_               ct_OPERATOR
'!='                    _dont_care_               ct_OPERATOR
'^'                     _dont_care_               ct_OPERATOR
'|'                     _dont_care_               ct_OPERATOR
'&&'                    _dont_care_               ct_OPERATOR
'||'                    _dont_care_               ct_OPERATOR
'?'                     _dont_care_               ct_OPERATOR
':'                     _dont_care_               ct_OPERATOR
'+='                    _dont_care_               ct_OPERATOR
'-='                    _dont_care_               ct_OPERATOR
'*='                    _dont_care_               ct_OPERATOR
'/='                    _dont_care_               ct_OPERATOR
'%='                    _dont_care_               ct_OPERATOR
'>>='                   _dont_care_               ct_OPERATOR
'<<='                   _dont_care_               ct_OPERATOR
'&='                    _dont_care_               ct_OPERATOR
'^='                    _dont_care_               ct_OPERATOR
'|='                    _dont_care_               ct_OPERATOR
';'                     _dont_care_               ct_MISC
'('                     _dont_care_               ct_MISC
')'                     _dont_care_               ct_MISC
'['                     _dont_care_               ct_MISC
']'                     _dont_care_               ct_MISC
'{'                     _dont_care_               ct_MISC
'}'                     _dont_care_               ct_MISC
//'"'                   _dont_care_               ct_STRING
//'\''                  _dont_care_               ct_CHAR
'\/*'                   _dont_care_               ct_COMMENT_STAR
'\/\/'                  _dont_care_               ct_COMMENT_LINE
'#'                     _dont_care_               ct_DIRECTIVE
'0'                     '[xX]'                    ct_HEXNUMBER
'auto'                  _non_alpha_               ct_KEYWORD
'break'                 _non_alpha_               ct_KEYWORD
'case'                  _non_alpha_               ct_KEYWORD
'char'                  _non_alpha_               ct_KEYWORD
'continue'              _non_alpha_               ct_KEYWORD
'default'               _non_alpha_               ct_KEYWORD
'do'                    _non_alpha_               ct_KEYWORD
'double'                _non_alpha_               ct_KEYWORD
'else'                  _non_alpha_               ct_KEYWORD
'entry'                 _non_alpha_               ct_KEYWORD
'extern'                _non_alpha_               ct_KEYWORD
'float'                 _non_alpha_               ct_KEYWORD
'for'                   _non_alpha_               ct_KEYWORD
'goto'                  _non_alpha_               ct_KEYWORD
'if'                    _non_alpha_               ct_KEYWORD
'int'                   _non_alpha_               ct_KEYWORD
'long'                  _non_alpha_               ct_KEYWORD
'register'              _non_alpha_               ct_KEYWORD
'return'                _non_alpha_               ct_KEYWORD
'short'                 _non_alpha_               ct_KEYWORD
'sizeof'                _non_alpha_               ct_KEYWORD
'static'                _non_alpha_               ct_KEYWORD
'struct'                _non_alpha_               ct_KEYWORD
'switch'                _non_alpha_               ct_KEYWORD
'typedef'               _non_alpha_               ct_KEYWORD
'union'                 _non_alpha_               ct_KEYWORD
'unsigned'              _non_alpha_               ct_KEYWORD
'while'                 _non_alpha_               ct_KEYWORD




//--------------------------------------------------------------------------------------------------------------------
//
// %%handler section
//
// The %%handler section gives rules to be applied once an entry in the %%words table has been recognised. Normally
// no further processing is required but sometimes a token starts with a fixed set of characters but may be followed
// by a character class rather than a known sequence.
//
// %%handler table entries have 4 columns:
//     Column 1          Token value to be processed
//     Column 2          Character specifier that follows recognised word
//     Column 3          Quoted string specifying end sequence
//     Column 4          Whether end sequence is part of lexeme
//
// The <character specifier> is defined as:
//     Column 2          A single character specifier or pre-defined system character macro:
//                         _PASCAL_CHAR         Pascal style character specifier
//                         _C_CHAR              C style character specifier
//                       If the lexeme can optionally have these characters then append '?' to the end
//                       of the quoted string.
//     Column 3          Up to 2 characters may be given as a sequence to terminate the lexeme.
//                       Characters are specified using a simplified regular expression. If this
//                       string is empty then the lexeme will never be matched.
//
%%handler
ct_COMMENT_LINE         '[^\n]'?                    '\n'           _discard_
ct_COMMENT_STAR         _all_chars_?                '*\/'          _use_
ct_DIRECTIVE            '[^\n]'?                    '\n'           _discard_
ct_HEXNUMBER            '[xX0-9A-Fa-f]'             '[^0-9a-fA-F]' _discard_

//--------------------------------------------------------------------------------------------------------------------
//
// %%tokens section
//
// Used to specify how to recognise non-fixed lexemes. Non-fixed lexemes are only tried if the table entries in the
// %%words section have failed. The non-fixed lexeme is defiened by 5 columns as below:
//     Column 1          Token value
//     Column 2          Single start character specifier
//     Column 3          Single contains character specifier
//     Column 4          End sequence specifier
//     Column 5          Whether end sequence is part of lexeme
// Pre-defined token styles are implemented. If used they should be specified in Column 2. The implemented ones
// are:
//  __STD_PASCALSTRING   Pascal string -- starts with ' ands with ' and uses '' to represent
//                       ' within a string. Does not extend beywond end of line.
//  __STD_IDENTIFIER     Standard identifier. Starts with [_a-zA-Z], contains [_a-zA-Z0-9] and ends with
//                       a non-alpha numeric character that is not part of the lexeme
//  __STD_NUMBER_OR_FP   Integer or floating point constant of syntax:
//                           <Digit String> [ '.' <Digit string> ] [ e|E [+-] <Digit string> ]
//
%%tokens
ct_STRING               __STD_C_STRING
ct_CHAR                 __STD_C_CHAR
ct_IDENTIFIER           '[a-zA-Z_]'  '[a-zA-Z0-9_\.]'?  '[^a-zA-Z0-9_\.]' _discard_ //__STD_IDENTIFIER
ct_NUMBER               __STD_NUMBER_OR_FP

//--------------------------------------------------------------------------------------------------------------------
//
// %%effects section
//
// Used to specify the default colors and font styles used for each token
//
//     Column 1          Token value
//     Column 2          Font styles
//     Column 3          Foreground color
//     Column 4          Background color
//     Column 5          Optional column specifying whether map entry is a 'hotspot'
//
// Columns 1-4 must be completed for all rows, Column 5 defaults to non-hotspot.
//
%%effects
ct_DEFAULT              []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ct_IDENTIFIER           []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ct_STRING               [fsItalic]                  _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ct_COMMENT              [fsItalic]                  clBlue                      _DEFAULT_BACKGROUND
ct_KEYWORD              [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
ct_NUMBER               []                          clRed                       _DEFAULT_BACKGROUND
ct_DIRECTIVE            []                          clGreen                     _DEFAULT_BACKGROUND
ct_MISC                 []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND



//--------------------------------------------------------------------------------------------------------------------
//
// %%map section
//
// Used to specify which entry in the %%effects table each token value uses. By default all token values map onto
// __DEFAULT_TOKEN which is defined as zero. Table has 2 columns:
//     Column 1          Recognised token value
//     Column 2          Map table entry (i.e. %%effects table entry)
// Normally the %%map table consists of identical value entries.
%%map
ct_IDENTIFIER           ct_IDENTIFIER
ct_STRING               ct_STRING
ct_NUMBER               ct_NUMBER
ct_COMMENT              ct_COMMENT
ct_COMMENT_LINE         ct_COMMENT
ct_COMMENT_STAR         ct_COMMENT
ct_KEYWORD              ct_KEYWORD
ct_MISC                 ct_MISC
ct_DIRECTIVE            ct_DIRECTIVE
ct_CHAR                 ct_STRING
ct_OPERATOR             ct_MISC
ct_HEXNUMBER            ct_NUMBER

#ifdef ver200

%%keys
caLEFT                  ([] Left)                     'Cursor Left'
caRIGHT                 ([] Right)                    'Cursor Right'
caLINEHOME              ([] Home)                     'Line start'
caLINEEND               ([] End)                      'Line end'
caUP                    ([] Up)                       'Line up'
caDOWN                  ([] Down)                     'Line down'
caPAGEUP                ([] PgUp)                     'Page up'
caPAGEDOWN              ([] PgDn)                     'Page down'
caWORDLEFT              ([Ctrl] Left)                 'Word left'
caWORDRIGHT             ([Ctrl] Right)                'Word right'
caDOCSTART              ([Ctrl] Home)                 'Document start'
caDOCEND                ([Ctrl] End)                  'Document end'
caCUT                   ([Ctrl] 'X')                  'Cut to clipboard'
caCOPY                  ([Ctrl] 'C')                  'Copy to clipboard'
caPASTE                 ([Ctrl] 'V'     |
                         [CTRL] INSERT)               'Paste from clipboard'
caDELETE                ([] Delete)                   'Delete at cursor'
caBACKSPACE             ([] Backspace)                'Delete before cursor'
caBLOCKIND              ([Ctrl] 'K', 'I')             'Indent block'
caBLOCKUND              ([Ctrl] 'K', 'U')             'Un-Indent block'
caINSTOGGLE             ([] Insert)                   'Toggle insert / override mode'
caSETBOOKMARK0          ([Ctrl] 'K', '0')             'Set bookmark 0'
caSETBOOKMARK1          ([Ctrl] 'K', '1')             'Set bookmark 1'
caSETBOOKMARK2          ([Ctrl] 'K', '2')             'Set bookmark 2'
caSETBOOKMARK3          ([Ctrl] 'K', '3')             'Set bookmark 3'
caSETBOOKMARK4          ([Ctrl] 'K', '4')             'Set bookmark 4'
caSETBOOKMARK5          ([Ctrl] 'K', '5')             'Set bookmark 5'
caSETBOOKMARK6          ([Ctrl] 'K', '6')             'Set bookmark 6'
caSETBOOKMARK7          ([Ctrl] 'K', '7')             'Set bookmark 7'
caSETBOOKMARK8          ([Ctrl] 'K', '8')             'Set bookmark 8'
caSETBOOKMARK9          ([Ctrl] 'K', '9')             'Set bookmark 9'
caGOTOBOOKMARK0         ([Ctrl] 'Q', '0')             'Goto bookmark 0'
caGOTOBOOKMARK1         ([Ctrl] 'Q', '1')             'Goto bookmark 1'
caGOTOBOOKMARK2         ([Ctrl] 'Q', '2')             'Goto bookmark 2'
caGOTOBOOKMARK3         ([Ctrl] 'Q', '3')             'Goto bookmark 3'
caGOTOBOOKMARK4         ([Ctrl] 'Q', '4')             'Goto bookmark 4'
caGOTOBOOKMARK5         ([Ctrl] 'Q', '5')             'Goto bookmark 5'
caGOTOBOOKMARK6         ([Ctrl] 'Q', '6')             'Goto bookmark 6'
caGOTOBOOKMARK7         ([Ctrl] 'Q', '7')             'Goto bookmark 7'
caGOTOBOOKMARK8         ([Ctrl] 'Q', '8')             'Goto bookmark 8'
caGOTOBOOKMARK9         ([Ctrl] 'Q', '9')             'Goto bookmark 9'
caUNDO                  ([Ctrl] 'Z')                  'Undo'
caREDO                  ([Ctrl Shift] 'Z')            'Redo'

#else
caLEFT                  ([] Left)                     'Cursor Left'
caRIGHT                 ([] Right)                    'Cursor Right'
caLINEHOME              ([] Home)                     'Line start'
caLINEEND               ([] End)                      'Line end'
caUP                    ([] Up)                       'Line up'
caDOWN                  ([] Down)                     'Line down'
caPAGEUP                ([] PgUp)                     'Page up'
caPAGEDOWN              ([] PgDn)                     'Page down'
caWORDLEFT              ([Ctrl] Left)                 'Word left'
caWORDRIGHT             ([Ctrl] Right)                'Word right'
caDOCSTART              ([Ctrl] Home)                 'Document start'
caDOCEND                ([Ctrl] End)                  'Document end'
caCUT                   ([Ctrl] 'X')                  'Cut to clipboard'
caCOPY                  ([Ctrl] 'C')                  'Copy to clipboard'
caPASTE                 ([Ctrl] 'V')                  'Paste from clipboard'
caDELETE                ([] Delete)                   'Delete at cursor'
caBACKSPACE             ([] Backspace)                'Delete before cursor'
caBLOCKIND              ([Ctrl] 'K', 'I')             'Indent block'
caBLOCKUND              ([Ctrl] 'K', 'U')             'Un-Indent block'
caINSTOGGLE             ([] Insert)                   'Toggle insert / override mode'
caSETBOOKMARK0          ([Ctrl] 'K', '0')             'Set bookmark 0'
caSETBOOKMARK1          ([Ctrl] 'K', '1')             'Set bookmark 1'
caSETBOOKMARK2          ([Ctrl] 'K', '2')             'Set bookmark 2'
caSETBOOKMARK3          ([Ctrl] 'K', '3')             'Set bookmark 3'
caSETBOOKMARK4          ([Ctrl] 'K', '4')             'Set bookmark 4'
caSETBOOKMARK5          ([Ctrl] 'K', '5')             'Set bookmark 5'
caSETBOOKMARK6          ([Ctrl] 'K', '6')             'Set bookmark 6'
caSETBOOKMARK7          ([Ctrl] 'K', '7')             'Set bookmark 7'
caSETBOOKMARK8          ([Ctrl] 'K', '8')             'Set bookmark 8'
caSETBOOKMARK9          ([Ctrl] 'K', '9')             'Set bookmark 9'
caGOTOBOOKMARK0         ([Ctrl] 'Q', '0')             'Goto bookmark 0'
caGOTOBOOKMARK1         ([Ctrl] 'Q', '1')             'Goto bookmark 1'
caGOTOBOOKMARK2         ([Ctrl] 'Q', '2')             'Goto bookmark 2'
caGOTOBOOKMARK3         ([Ctrl] 'Q', '3')             'Goto bookmark 3'
caGOTOBOOKMARK4         ([Ctrl] 'Q', '4')             'Goto bookmark 4'
caGOTOBOOKMARK5         ([Ctrl] 'Q', '5')             'Goto bookmark 5'
caGOTOBOOKMARK6         ([Ctrl] 'Q', '6')             'Goto bookmark 6'
caGOTOBOOKMARK7         ([Ctrl] 'Q', '7')             'Goto bookmark 7'
caGOTOBOOKMARK8         ([Ctrl] 'Q', '8')             'Goto bookmark 8'
caGOTOBOOKMARK9         ([Ctrl] 'Q', '9')             'Goto bookmark 9'
caUNDO                  ([Ctrl] 'Z')                  'Undo'
caREDO                  ([Ctrl Shift] 'Z')            'Redo'

#endif
 �,  0   ��
 S Q L P A R S E R       0 	        /*
 * TSyntaxMemoParser Script
 * ------------------------
 *
 * Author  :          David Brock
 * Date    :          October 18 1997
 * Language:          SQL
 *
 */

//--------------------------------------------------------------------------------------------------------------------
//
//
//
// Macro definitions. Parameters may be specified and the replacement text terminates with the end of
// line (watch trailing blanks).
//
#define st_DEFAULT                  0
#define st_COMMENT                  1
#define st_IDENTIFIER               2
#define st_STRING                   3
#define st_NUMBER                   4
#define st_SYMBOL                   5
#define st_RESERVED                 6
#define st_COMMENT_LINE             7
#define st_COMMENT_STAR             8


#define _non_alpha_                 '[^_A-Za-z0-9]'
#define _all_chars_                 '[\x00-\xFF]'
#define _no_chars_                  '[]'
#define _dont_care_                 _all_chars_
#define _DEFAULT_BACKGROUND         clNavy
#define _DEFAULT_FOREGROUND         clYellow

%%language
Name                      = 'SQL'
Case                      = __INSENSITIVE
Options                   = __DEFAULT_OPTIONS
WordWrapColumn            = _EDGE
Gutter                    = _DEFAULT_GUTTER
Anchor                    = _DEFAULT_START_ANCHOR
SelTextColor              = clBlue
SelTextBack               = clYellow
FontName                  = 'Courier New'
FontSize                  = 11
ExampleText               = '/* Sample SQL text */\n\
                            \select * from country\n\
                            \         where name like \'%UK%\'\n'
EditableStyles              ('Reserved word', st_RESERVED),
                            ('Comment',       st_COMMENT),
                            ('Identifier',    st_IDENTIFIER),
                            ('String',        st_STRING),
                            ('Number',        st_NUMBER),
                            ('Symbols',       st_SYMBOL),
                            ('Default',       st_DEFAULT)

%%words
'\/\/'           _dont_care_       st_COMMENT_LINE
'/*'             _dont_care_       st_COMMENT_STAR
'!'              _dont_care_       st_SYMBOL
'$'              _dont_care_       st_SYMBOL
'%'              _dont_care_       st_SYMBOL
'^'              _dont_care_       st_SYMBOL
'&'              _dont_care_       st_SYMBOL
'*'              _dont_care_       st_SYMBOL
'('              _dont_care_       st_SYMBOL
')'              _dont_care_       st_SYMBOL
'-'              _dont_care_       st_SYMBOL
'='              _dont_care_       st_SYMBOL
'+'              _dont_care_       st_SYMBOL
'{'              _dont_care_       st_SYMBOL
'}'              _dont_care_       st_SYMBOL
'['              _dont_care_       st_SYMBOL
']'              _dont_care_       st_SYMBOL
':'              _dont_care_       st_SYMBOL
';'              _dont_care_       st_SYMBOL
'@'              _dont_care_       st_SYMBOL
'#'              _dont_care_       st_SYMBOL
'~'              _dont_care_       st_SYMBOL
'<'              _dont_care_       st_SYMBOL
'>'              _dont_care_       st_SYMBOL
','              _dont_care_       st_SYMBOL
'.'              _dont_care_       st_SYMBOL
'/'              _dont_care_       st_SYMBOL
'?'              _dont_care_       st_SYMBOL
'\\'             _dont_care_       st_SYMBOL
'ACTIVE'         _non_alpha_       st_RESERVED
'ALL'            _non_alpha_       st_RESERVED
'AFTER'          _non_alpha_       st_RESERVED
'ALTER'          _non_alpha_       st_RESERVED
'AND'            _non_alpha_       st_RESERVED
'ANY'            _non_alpha_       st_RESERVED
'AS'             _non_alpha_       st_RESERVED
'ASC'            _non_alpha_       st_RESERVED
'ASCENDING'      _non_alpha_       st_RESERVED
'AT'             _non_alpha_       st_RESERVED
'AUTO'           _non_alpha_       st_RESERVED
'AUTOINC'        _non_alpha_       st_RESERVED
'BASE_NAME'      _non_alpha_       st_RESERVED
'BEFORE'         _non_alpha_       st_RESERVED
'BEGIN'          _non_alpha_       st_RESERVED
'BETWEEN'        _non_alpha_       st_RESERVED
'BOTH'           _non_alpha_       st_RESERVED
'BY'             _non_alpha_       st_RESERVED
'CACHE'          _non_alpha_       st_RESERVED
'CAST'           _non_alpha_       st_RESERVED
'CHECK'          _non_alpha_       st_RESERVED
'COLUMN'         _non_alpha_       st_RESERVED
'COMMIT'         _non_alpha_       st_RESERVED
'COMMITTED'      _non_alpha_       st_RESERVED
'COMPUTED'       _non_alpha_       st_RESERVED
'CONDITIONAL'    _non_alpha_       st_RESERVED
'CONNECT'        _non_alpha_       st_RESERVED
'CONSTRAINT'     _non_alpha_       st_RESERVED
'CONTAINING'     _non_alpha_       st_RESERVED
'COUNT'          _non_alpha_       st_RESERVED
'CREATE'         _non_alpha_       st_RESERVED
'CURRENT'        _non_alpha_       st_RESERVED
'CURSOR'         _non_alpha_       st_RESERVED
'DATABASE'       _non_alpha_       st_RESERVED
'DEBUG'          _non_alpha_       st_RESERVED
'DECLARE'        _non_alpha_       st_RESERVED
'DEFAULT'        _non_alpha_       st_RESERVED
'DELETE'         _non_alpha_       st_RESERVED
'DESC'           _non_alpha_       st_RESERVED
'DESCENDING'     _non_alpha_       st_RESERVED
'DISTINCT'       _non_alpha_       st_RESERVED
'DOMAIN'         _non_alpha_       st_RESERVED
'DO'             _non_alpha_       st_RESERVED
'DROP'           _non_alpha_       st_RESERVED
'ELSE'           _non_alpha_       st_RESERVED
'END'            _non_alpha_       st_RESERVED
'ENTRY_POINT'    _non_alpha_       st_RESERVED
'ESCAPE'         _non_alpha_       st_RESERVED
'EXCEPTION'      _non_alpha_       st_RESERVED
'EXECUTE'        _non_alpha_       st_RESERVED
'EXISTS'         _non_alpha_       st_RESERVED
'EXIT'           _non_alpha_       st_RESERVED
'EXTERNAL'       _non_alpha_       st_RESERVED
'EXTRACT'        _non_alpha_       st_RESERVED
'FILTER'         _non_alpha_       st_RESERVED
'FOR'            _non_alpha_       st_RESERVED
'FOREIGN'        _non_alpha_       st_RESERVED
'FROM'           _non_alpha_       st_RESERVED
'FULL'           _non_alpha_       st_RESERVED
'FUNCTION'       _non_alpha_       st_RESERVED
'GENERATOR'      _non_alpha_       st_RESERVED
'GRANT'          _non_alpha_       st_RESERVED
'GROUP'          _non_alpha_       st_RESERVED
'HAVING'         _non_alpha_       st_RESERVED
'IF'             _non_alpha_       st_RESERVED
'IN'             _non_alpha_       st_RESERVED
'INACTIVE'       _non_alpha_       st_RESERVED
'INDEX'          _non_alpha_       st_RESERVED
'INNER'          _non_alpha_       st_RESERVED
'INSERT'         _non_alpha_       st_RESERVED
'INTO'           _non_alpha_       st_RESERVED
'IS'             _non_alpha_       st_RESERVED
'ISOLATION'      _non_alpha_       st_RESERVED
'JOIN'           _non_alpha_       st_RESERVED
'KEY'            _non_alpha_       st_RESERVED
'NULL'           _non_alpha_       st_RESERVED
'LEADING'        _non_alpha_       st_RESERVED
'LEFT'           _non_alpha_       st_RESERVED
'LEVEL'          _non_alpha_       st_RESERVED
'LIKE'           _non_alpha_       st_RESERVED
'MERGE'          _non_alpha_       st_RESERVED
'NAMES'          _non_alpha_       st_RESERVED
'NO'             _non_alpha_       st_RESERVED
'NOT'            _non_alpha_       st_RESERVED
'OF'             _non_alpha_       st_RESERVED
'ON'             _non_alpha_       st_RESERVED
'ONLY'           _non_alpha_       st_RESERVED
'OR'             _non_alpha_       st_RESERVED
'ORDER'          _non_alpha_       st_RESERVED
'OUTER'          _non_alpha_       st_RESERVED
'PARAMETER'      _non_alpha_       st_RESERVED
'PASSWORD'       _non_alpha_       st_RESERVED
'PIVOT'          _non_alpha_       st_RESERVED
'PLAN'           _non_alpha_       st_RESERVED
'POSITION'       _non_alpha_       st_RESERVED
'PROCEDURE'      _non_alpha_       st_RESERVED
'PROTECTED'      _non_alpha_       st_RESERVED
'PRIMARY'        _non_alpha_       st_RESERVED
'PRIVILEGES'     _non_alpha_       st_RESERVED
'READ'           _non_alpha_       st_RESERVED
'REFERENCES'     _non_alpha_       st_RESERVED
'RETAIN'         _non_alpha_       st_RESERVED
'RETURNING_VALUES' _non_alpha_     st_RESERVED
'RETURNS'        _non_alpha_       st_RESERVED
'REVOKE'         _non_alpha_       st_RESERVED
'RIGHT'          _non_alpha_       st_RESERVED
'ROLLBACK'       _non_alpha_       st_RESERVED
'SCHEMA'         _non_alpha_       st_RESERVED
'SELECT'         _non_alpha_       st_RESERVED
'SET'            _non_alpha_       st_RESERVED
'SHARED'         _non_alpha_       st_RESERVED
'SHADOW'         _non_alpha_       st_RESERVED
'SNAPSHOT'       _non_alpha_       st_RESERVED
'SOME'           _non_alpha_       st_RESERVED
'SUSPEND'        _non_alpha_       st_RESERVED
'TABLE'          _non_alpha_       st_RESERVED
'THEN'           _non_alpha_       st_RESERVED
'TO'             _non_alpha_       st_RESERVED
'TRAILING'       _non_alpha_       st_RESERVED
'TRANSFORM'      _non_alpha_       st_RESERVED
'TRANSACTION'    _non_alpha_       st_RESERVED
'TRIGGER'        _non_alpha_       st_RESERVED
'UNCOMMITTED'    _non_alpha_       st_RESERVED
'UNION'          _non_alpha_       st_RESERVED
'UNIQUE'         _non_alpha_       st_RESERVED
'UPDATE'         _non_alpha_       st_RESERVED
'USER'           _non_alpha_       st_RESERVED
'USING'          _non_alpha_       st_RESERVED
'VALUES'         _non_alpha_       st_RESERVED
'VARIABLE'       _non_alpha_       st_RESERVED
'VIEW'           _non_alpha_       st_RESERVED
'WAIT'           _non_alpha_       st_RESERVED
'WHEN'           _non_alpha_       st_RESERVED
'WHERE'          _non_alpha_       st_RESERVED
'WHILE'          _non_alpha_       st_RESERVED
'WITH'           _non_alpha_       st_RESERVED
'WORK'           _non_alpha_       st_RESERVED

%%handler
st_COMMENT_LINE         '[^\n]'?                    '\n'           _discard_
st_COMMENT_STAR         _all_chars_?                '*/'           _use_

%%tokens
st_STRING               __STD_PASCALSTRING
st_STRING               __STD_C_STRING
st_NUMBER               __STD_NUMBER_OR_FP
st_IDENTIFIER           __STD_IDENTIFIER

%%map
st_DEFAULT         st_DEFAULT
st_IDENTIFIER      st_IDENTIFIER
st_STRING          st_STRING
st_COMMENT         st_COMMENT
st_COMMENT_LINE    st_COMMENT
st_COMMENT_STAR    st_COMMENT
st_RESERVED        st_RESERVED
st_SYMBOL          st_SYMBOL
st_NUMBER          st_NUMBER

%%effects
st_DEFAULT              []                          _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
st_IDENTIFIER           []                          clWhite                     _DEFAULT_BACKGROUND
st_STRING               []                          clAqua                      _DEFAULT_BACKGROUND
st_COMMENT              [fsItalic]                  clLtGray                    _DEFAULT_BACKGROUND
st_RESERVED             [fsBold]                    _DEFAULT_FOREGROUND         _DEFAULT_BACKGROUND
st_NUMBER               []                          clLime                      _DEFAULT_BACKGROUND
st_SYMBOL               [fsBold]                    clRed                       _DEFAULT_BACKGROUND


