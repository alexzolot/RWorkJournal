RWorkJournal
============

R Work Journal

R code for a large project often contains many tasks  amd may have 500-3000 lines of code that make difficult navigation through the code. Some tasks in a project could have long execution time that makes difficult also usual  literate programming methods (as `sweave`, `knitr`).

We wrote R function `Code2RWorkJournal()` that

1.  Transforms  `.R` file into  self-documented  `.html` file, containing all R code with output pics, headers, table of contents and gallery. 
2.  The titles in body and contents are clickable to navigate from contents to body and back. 
3.  The pics are clickable to resize. 
4.  The html file has partly R syntax highlighted.  It is possible to do the full R syntax highlighting in resulting html, but the result file becomes almost twice heavier. 
5.  Parts of the result html file could be folded. 
6.  If in a browser you “select all”, copy and paste from browser to a text editor,   you get the pure original R file. 
7.  If we modify `.R` code,   recreate `.html` is fast. 


### To install the package 

1. Use command `devtools::install_github('alexzolot/RWorkJournal', ref='rel.0.2')` 
2. To see demo, download [demo_RWJ.r](http://alexzolot.github.io/RWJ/demo_RWJ.r) to an empty directory, edit path in line 20 of the file, source the file, and then run `rwj()`. It should create file [demo_RWJ.r.htm](http://alexzolot.github.io/RWJ/demo_RWJ.r.htm) in the same folder and navigate your browser to it.


