@echo off

@REM Convert a markdown file to pdf. Requires pandoc and latex to be
@REM installed and accessible from the command line.
@REM Usage: md2pdf.cmd <md_filename> <pdf_filename>

call pandoc.exe -f markdown-implicit_figures -t pdf %1 -o %2
