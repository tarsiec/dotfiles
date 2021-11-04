let &path.="src/include,/usr/include/AL,"

set includeexpr=substitute(v:fname,'\\.','/','g')

" path to directory where library can be found
let g:clang_library_path='/usr/lib/clang/12.0.1/lib'
" or path directly to the library file
let g:clang_library_path='/usr/lib64/libclang.so'

