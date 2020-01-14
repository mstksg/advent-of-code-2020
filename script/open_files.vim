"
" open_files.vim
" ==============
"
" use:
"
" :source script/open_files.vim
"
" to load the function into scope, where you can call with:
"
" :call OpenAoC(day)
"
" If you use the :source command in a buffer where the filename has a number
" in it (like Day16.hs), this will automatically open all the files associated
" with that day.
"
" Change s:year below to open test data for a different year
"

let s:year = 2019


function! OpenAoC(day)
    let l:daystr  = printf("%02d",a:day)
    let l:yearstr = printf("%04d",s:year)
    let l:files = [ "prompt/" . l:daystr . "a.md",
                  \"prompt/" . l:daystr . "b.md",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "a.txt",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "b.txt",
                  \"data/" . l:daystr . ".txt",
                  \"src/AOC/Challenge/Day" . l:daystr . ".hs"
                  \]

    for fn in l:files
        execute "e " . fnameescape(fn)
    endfor
endfunction

let s:buffday = str2nr(matchstr(expand('%:t:r'), '\d\+'))

if (s:buffday == 0)
    echo "no valid file found in buffer; use :call OpenAoC(day) to open a day"
else
    echo "found day" . string(s:buffday)
    call OpenAoC(s:buffday)
endif
