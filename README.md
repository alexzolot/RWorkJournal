RWorkJournal
============

R Work Journal

R code for a large project often contains many tasks  amd may have 500-3000 lines of code that make difficult navigation through the code. Some tasks in a project could have long execution time that makes difficult also usual  literate programming methods (as `sweave`, `knitr`).

We wrote R function `Code2RWorkJournal()` or `rwj()`  that


1.  Transforms .R file into  self-documented  .html file ("R Work Journal"), containing all R code with output pics, headers, table of contents and gallery. 
2.  The titles in body and contents are clickable to navigate from contents to body and back. 
3.  The pics are clickable to resize. 
4.  The html file has R syntax highlighted.   
5.  Parts of the result html file could be folded. 
6.  If you in browser fold TOC, select all, copy and paste from browser to a text editor, you should get the pure original R file (excluding LaTeX inserts). 
7.  If modify .R code,   recreate .html is fast. 
8.  This functionality extends function `spin` of package `knitr` adding additional javascript means for easy navigation through the R code. 

The package also contains other functions t work with R code.


### To install the package 

1. Use command `devtools::install_github('alexzolot/RWorkJournal')` 
2. To see demo, download [demo_RWJ.r](http://alexzolot.github.io/RWJ/demo_RWJ.r) to an empty directory, edit path in line 20 of the file, source the file, and then run `rwj()`. It should create file [demo_RWJ.r.htm](http://alexzolot.github.io/RWJ/demo_RWJ.r.htm) in the same folder and navigate your browser to it.


