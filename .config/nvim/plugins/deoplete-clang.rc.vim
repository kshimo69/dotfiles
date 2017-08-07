let OSTYPE = system('uname')
if OSTYPE == "Darwin\n"
  let g:deoplete#sources#clang#libclang_path='/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
  let g:deoplete#sources#clang#clang_header='/Library/Developer/CommandLineTools/usr/lib/clang'
elseif OSTYPE == "Linux\n"
  let g:deoplete#sources#clang#libclang_path='/usr/lib64/llvm/libclang.so'
  let g:deoplete#sources#clang#clang_header='/usr/lib/clang'
endif
