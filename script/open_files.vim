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

let s:year = 2020

function! OpenAoC(day)
    let l:daystr  = printf("%02d",a:day)
    let l:yearstr = printf("%04d",s:year)
    let l:files = ["src/AOC/Challenge/Day" . l:daystr . ".hs",
                  \"data/" . l:daystr . ".txt",
                  \"prompt/" . l:daystr . "a.md",
                  \"prompt/" . l:daystr . "b.md",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "a.txt",
                  \"test-data/" . l:yearstr . "/" . l:daystr . "b.txt",
                  \"reflections/day" . l:daystr . ".md",
                  \"bench-out/day" . l:daystr . ".txt"
                  \]

    for fn in reverse(l:files)
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
