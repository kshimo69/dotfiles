" Vim syntax file
" Language: ddl
" Current Maintainer:   NEX
" Last Change:  2013 Dec 11

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/cpp.vim
else
  runtime! syntax/cpp.vim
  unlet b:current_syntax
endif

syn keyword DDLStorageClass protocol concrete loopback auto_type_inheritable auto_type custom_type exportable
syn keyword DDLStorageClass in out
syn match   DDLUse          display "^\s*\(%:\|#\)\s*use\>\s*["<]" contains=cIncluded

" OnlineCore/src/DDLTypes/OnlineCore.ddl
syn keyword DDLTypes int64 uint64 int32 uint32 int16 uint16 int8 byte bool float double real
syn keyword DDLTypes char char16 char8 string
syn keyword DDLTypes buffer buffertail cstr
syn keyword DDLTypes datetime qresult variant
syn keyword DDLTypes qvector qlist qqueue qBuffer stationurl

" OnlineCore/src/DDLTypes/STLExt.ddl
syn keyword DDLTypes std_bitset8 std_bitset16 std_bitset32
syn keyword DDLTypes std_char16string std_char8string std_string
syn keyword DDLTypes std_vector std_list std_map

" RendezVous/Core/src/Common/ProtocolFoundation.ddl
syn keyword DDLTypes principalid connectionid gatheringid
syn keyword DDLTypes MessageRecipient NotificationEvent
syn keyword DDLTypes Data DynamicData UserMessage
syn keyword DDLTypes DataHolder PropertyID Property PropertyVariantValue
syn keyword DDLTypes ResultRange PropertyValue PropertyVariant

" Default highlighting
if version >= 508 || !exists("did_ddl_syntax_inits")
  if version < 508
    let did_cpp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink DDLTypes           Type
  HiLink DDLStorageClass    StorageClass
  HiLink DDLIncluded        String
  HiLink DDLUse             Include

  delcommand HiLink
endif

let b:current_syntax = "ddl"
