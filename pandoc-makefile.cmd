@echo off
echo Current directory: && echo %cd% 
echo ----------------------------------
set /p mdfile="Markdown file: "
set /p outfile="Output file and directory (relative): "
pandoc -s -S %mdfile% --filter pandoc-citeproc ^
 -o %outfile% --mathjax 
pause

start %outfile%