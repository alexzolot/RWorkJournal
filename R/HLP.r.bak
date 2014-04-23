# Miscellaneous aliases and functions
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  
#' @name HLP
#' 
#' @title HLP  - Miscellaneous aliases and functions
#' 
#' @description HLP  - Miscellaneous aliases and functions that I use the most often 
#' during project initiation, in progress, leaving and coming back
#' for a book "Handling Large R Projects"
#' 
# abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #e - example 
#' @author Alex Zolotovitski <alex@@zolot.us>
# @Authors@R  Alex Zolotovitski <alex@@zolot.us>
#' @keywords aliases

#' @exportPattern "^[[:alpha:]]+"
#' @export '%+%' '%-%'

#' @import plyr 
#' @docType package
NULL

"Package: HaLaP
Title: HaLaP  - Miscellaneous aliases and functions
Description: HaLaP  - Miscellaneous aliases and functions that I use the most often 
 during project initiation, in progress, leaving and coming back
 for a book \"Handling Large R Projects\"
Version: 0.1
Author: Alex Zolot <alex.zolot@gmail.com>
Maintainer: Alex Zolot <alex.zolot@gmail.com>
Depends: R (>= 3.0.2), plyr
License: GPL-2
LazyData: true"

#=====  Funcs for Tutorial UseR!-2013  =====
#=====  General purpose helper functions and aliases  =====

# xxx : saa, sa() -> sa()

#====  aliases and 1-liners  ====
ch= as.character
fa= factor
nu= as.numeric
df= data.frame
#dtt= data.table

le= length
he= head
sus= subset
summ= summary
sf= sprintf
na= names
fp= file.path
fpa= tools:::file_path_as_absolute
#fpa= function(f)normalizePath(path.expand(f), "/", TRUE)

brr= browser  #function(s='', ...){catt(s); browser(...)}   
trb= traceback

#' fun to split column names
#e  cn('aa bb 2013')
cn= function(cnn,  sep='[ ,\n\t\\+]+') unlist(strsplit(cnn, sep))  # b='',`%a%` = '%c%' =  col.names= cn(cnn)  'aa b 1'  %a% 1  aa b 1'  cn('aa b 1')

#' do nothing
dummy= function(...) invisible()
#' fun to paste column names,  inverse to cn()
#e nc(cars)
nc= nmsv= sNames= function(ds, nm= deparse(substitute(ds)), sep=' ', ...){x=pas(na(ds), sep, sep); catf("\nnames(%s)= cn('%s')\n", nm, x); invisible(x)} 

#' wrapper for subset + paste + colNames
#e ncc('Wi',, iris)
ncc= function(patt = "", pattNeg = "^$", ds) pas(gre2(patt, pattNeg, na(ds)))  #ex: ncc('Wi',, iris)
pr= function(x){catf('==  %s  ==\n', deparse(substitute(x))); print(x)} #ex: pr(cars)

#' print list in 1 column
#e prr(letters[1:5])
#e prr(cars)
prr= function(x, ma='', header=T) {if(header)catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); ns= na(x)
	#for(xx in if(is.null(ns)) 1:le(x) else ns) catt(xx, '=', x[[xx]]); catt('-------------------------\n')} # test with gff ex: prr(cars, 'Cars')
     for(xx in if(is.null(ns)) 1:le(x) else ns) catf('%3s= %s\n', xx,  x[[xx]]); catt('-------------------------\n')} # test with gff ex: prr(cars, 'Cars')

#' wrapper for paste
pas= function(x, sep=' ',collapse=' ')paste(x,sep=sep,collapse=collapse)
#' wrapper for grep
gre2= function(patt='', pattNeg='^$', x, v=T, ...){ a=grepl(patt, x,...) & !grepl(pattNeg, x,...); return( if(v) x[a] else a) }

#' wrapper for grep + names + subset
#ex: suss('Se', 'Wi',  iris, Sepal.Length < 4.5 )
suss= function(patt='', pattNeg='^$', x, ...) {
	cols= grepl(patt, colnames(x)) & !grepl(pattNeg, colnames(x))  # gna(patt, pattNeg, x)  
	if('data.table' %in% class(x)) x[...][, cols, with=F] else subset(x, ...)[cols]
}
if (0) {
	strr(freeny)
	strr(dtt(freeny))
	suss(,, freeny)
	suss(,, dtt(freeny))
	suss('in',,  freeny, y < 8.9)
	suss('in',,  freeny, y < 8.9)
	suss(, 'in', freeny, y < 8.9)
	suss(, 'in', dtt(freeny), y < 8.9)
	suss( 'in', 'p', dtt(freeny), y < 8.9)	
}



#' prepare to detach redundant packages
dett= function(patt= '^pac') catf('\ndetach("%s",  character.only = TRUE)', grep(patt, search(), v=T))  ## prepare detach redundant packages
#' system time + sound
st= function(...){s= system.time(...)[[3]]; aaa(); s}  #ex: st({x= 5})

#w nu * fa
#ex: nuf(ch(CO2$Plant))
#ex: nuf(ch(iris$Species))
nuf= function(...) nu(fa(...))  # char -> fa -> nu

#' compare names
#e comp.na(cars, iris)
comp.na= function(x, y) {
	catt('x & y :', pas(intersect(na(x), na(y))))
	catt('x \\ y :', pas(setdiff(na(x), na(y))))
	catt('y \\ x :', pas(setdiff(na(y), na(x))))  # nin
}


onWin= nchar(Sys.getenv('computername')) > 0
#ww= if(onWin)windows  else x11

ww= function(k=10, ...) if(onWin){
			#ldply(strsplit(system("wmic desktopmonitor get", intern=TRUE),'\r'))
			# 1   Availability  Bandwidth  Caption                    ConfigManagerErrorCode  ConfigManagerUserConfig  CreationClassName     Description                DeviceID         DisplayType  ErrorCleared  ErrorDescription  InstallDate  IsLocked  LastErrorCode  MonitorManufacturer       MonitorType                Name                       PixelsPerXLogicalInch  PixelsPerYLogicalInch  PNPDeviceID                               PowerManagementCapabilities  PowerManagementSupported  ScreenHeight  ScreenWidth  Status  StatusInfo  SystemCreationClassName  SystemName  
			# 2 3                        ThinkPad Display 1600x900  0                       FALSE                    Win32_DesktopMonitor  ThinkPad Display 1600x900  DesktopMonitor1                                                                                     Lenovo                    ThinkPad Display 1600x900  ThinkPad Display 1600x900  96                     96                     DISPLAY\\LEN40B1\\4&2C1ABB3B&0&UID67568640                                                         1080          1920         OK                  Win32_ComputerSystem     T530-1122   
			# 3 3                        Generic PnP Monitor        0                       FALSE                    Win32_DesktopMonitor  Generic PnP Monitor        DesktopMonitor2                                                                                     (Standard monitor types)  Generic PnP Monitor        Generic PnP Monitor        96                     96                     DISPLAY\\SAM0686\\5&24EFB498&0&UID1048850                                                          1080          1920         OK                  Win32_ComputerSystem     T530-1122   
		prr(system("wmic desktopmonitor",  intern=TRUE))
	winprop= function(s='screenwidth') max(nu(system(sf("wmic desktopmonitor get %s", s),  intern=TRUE)), na.rm=T) #[2]
			
	windows( width= k/10 * winprop('screenwidth') / winprop('PixelsPerXLogicalInch')
		  , height= k/10 * winprop('screenheight') / winprop('PixelsPerYLogicalInch'),...)
	}  else x11


#' date &  time 
DT= DateTime= function(format = "%Y-%m-%d %H:%M:%S") strftime(Sys.time(), format) #ex: DT()
#' wrapper for NROW + head
#e hee(cars)
hee= function(ds, h=9){catf('\n%s, %s rows x  %s cols,  %s Mb :\n', deparse(substitute(ds)), NROW(ds), NCOL(ds), round(object.size(ds)/2^20, 1)); print(hh<-head(ds,h)); catf('# he(suss(,, %s[ , cn("%s")]), 5)\n',deparse(substitute(ds)), nmsv(ds, deparse(substitute(ds))));invisible(hh)}

aaa= function(n=20) for(i in 1:n) {cat('\aaa '); flush.console()}  # sound when done 

#' wrapper for cat + sprintf
catf= function(...) cat(sprintf(...))

#' wrapper for cat + '\\n'
#e catt(cars[,1])
catt= function(...) {cat(...); cat('\n'); invisible(flush.console())} 
#' wrapper for paste
'%+%' = function(x, y) paste(x, y, sep= "")

#w shell
exec= function(s) shell(s, wait=T, intern = T)
#w shell + start explorer (win only)
expl= function(x= gw()) shell(sf('start explorer %s', gsub('/','\\\\', x)))

#w getwd
gw= function(){catf('gw: sw("%s");  expl()\n', gw<- getwd()); invisible(gw)} #e  gw()
#w [dir.create] + setwd
sw= function (sDir, ...) {
	dir.create(sDir, rec=T, ...)
	setwd(sDir)
	catf('sw: Work dir set to: %s;  gw()\n', gw())
}

#' x \ y
#r    x  which are not in  y
#e  nin(1:6, 4:9);  1:6 %-% 4:9
nin= '%-%' = function(x, y) x[!(x %in% y)] # x not in y :   1:5 %-%  4:9 # `%-%` 

#' returns matrix of memory consumption
lss= function(verb=T){ #object.sizes <-
	#llss= rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name)
#	llss= (sort(sapply(ls(envir=.GlobalEnv), function (object.name)
#									round(object.size(get(object.name))/1048576,2)) ))
	
	llss= (sort(sapply(ls(envir=.GlobalEnv), function (object.name){x= 1e-11
									try({x=round(object.size(get(object.name))/1048576, 2) },s=T); x
								})))
	
	
	#if(verb)newwin(1,3,'Mem',{
	if(verb)try({
					#barplot(llss, main="Memory usage by object", ylab="Bytes", xlab="Variable name",
					#col=heat.colors(length(object.sizes())))
					#dotchart(llss, main="Memory usage by object, in M", xlab="Bytes")
					pie(llss, labels = paste(names(llss), llss) , main="Memory usage by object, of tot " %+% memory.size()) #round(llss/1e6,2))
				}, s=T)
	print(llss[1])    #print(llss)     
	do.call(str,list(get(names(llss)[1]) ))
	pr(llss)
	catf(':: rm(%s)\n', pas(rev(names(llss)), collapse=','))
	invisible(llss)
}
# x=lss(); x[x== 1e-11]


#' list of data.frames 
#e lsDF(FALSE); a= lsDF(TRUE); hee(a); if(nrow(a)>0)srt(a, ~  + class + ds - size)
lsDF= function(.all=F, ...){ b= mdply(ls(envir= .GlobalEnv, ... ), function(a){if(.all)catt(a); aa=get(a); b=df(); cl<- substr(class(aa)[1],1,4)
				if(.all || any(class(aa) %in% c('data.frame','matrix','list',"metaMDS", "itemsets", "rules", "transactions") | is.list(aa))){
					catf('srm( %-23s ) # %5.1f %5s %5s %3s %-30s\n', a, round(object.size(aa)/1e6, 1), cl, NROW(aa), NCOL(aa), substr(sNames(aa),1,1999))
					b= df(ds=sf('srm(%20s)',a), size=nu(object.size(aa))/1e6, class=cl, nr=NROW(aa), nc=NCOL(aa)
							, vars=substr(sNames(aa),1,99), ds0=a)[1,]
				}
			})
	if(nrow(b)>0){ hee(bs<- srt(b, ~-size)[, -1], 20) #  print(head(bs<- srt(b, ~-size)[, -1], 20), width=1600) 
		#		catf('rm(%s)\n', pas(b$ds0,collapse=', '))
		#		catf('rm(%s)\n', pas(bs$ds0,collapse=', '))
		#		catf('save(%s\n, file="%s")\n', pas(b$ds,collapse=', '), gw())
	}
	
	gc(T,T)
	invisible(b)	
}


#' rm all
rmall= function() rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # rmall()

#' rm  all Data Frames, lists, matrixes
#e  rmDF()
rmDF= function(...) invisible(mdply(ls(envir =.GlobalEnv,... ),function(a){aa= get(a); b= df()
						if( class(aa)[1] %in% c('list')){catf('rm  %-20s %-30s\n', a, sNames(aa)); do.call(rm, list(a), envir =.GlobalEnv)}
						if( class(aa)[1] %in% c('data.frame','matrix')){catf('rm  %-20s %5s %3s %-30s\n', a, nrow(aa), ncol(aa), substr(sNames(aa),1,4999))
							do.call(rm, list(a), envir= .GlobalEnv)
						}}))

#' install + library
libra= function(libs, verb=TRUE){
	libs= if(!grepl('\\"|\'| ', li<- deparse(substitute(libs)))) li else cn(libs)  # now libs is a char vector
	
	misspk= nin(libs, installed.packages()[,1])
	
	if(le(misspk) > 0) {install.packages(misspk, repos= "http://cran.stat.ucla.edu/", dependencies= T)}
	
	for(li in libs){catt('libra:: Call library ', li) 
		do.call(library, list(li, character.only =T))
		if(verb) catf('libra:: demo(%s); example(%1$s); vignette("%1$s")\n', li)
	}
} #--

if(0){ # examples 
	libra(libs= "locfit tkrplot xtable")
	libra(cn("randomForest varSelRF"), verb=T)
	libra("randomForest varSelRF", verb=T)
	libra(randomForest, verb=T)
	
	.libPaths()
	library()
	pas(dir(.libPaths()))
	# [1] "abind akima bak base bitops boot brew Cairo caTools class cluster codetools colorspace datasets DBI Defaults devtools dichromat digest evaluate fImport foreach forecast formatR fracdiff gam gdata ggplot2 gmodels gplots graphics grDevices grid gtable gtools httpuv httr iterators itertools KernSmooth knitr labeling lattice lme4 locfit markdown MASS Matrix memoise methods mgcv munsell nlme nnet plyr png proto quadprog R2HTML randomForest rCharts RColorBrewer Rcpp RcppArmadillo RCurl reshape2 rj rj.gd rJava RJDBC RJSONIO roxygen2 rpart RUnit scales shiny stats stats4 stringr survival testthat timeDate timeSeries tkrplot tools tseries utils whisker XLConnect xtable yaml zoo"
	
	pas(options('defaultPackages')[[1]])
	# [1] "datasets utils grDevices graphics stats methods"
}


#'==   output - input  ==

#' save object or all and print reminder
#' p dsn name for out file
#e 	
#' \dontrun{
#'	saa(,'111')
#'# 2013-06-18 12:15:31:: Saved: load('m:/80_ChurnSim/out/111.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#'	
#'	saa()
#'# 2013-06-18 12:15:38:: Saved: load('m:/80_ChurnSim/out/.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#'	
#e data(cars); saa(cars)
#  2013-06-18 12:15:46:: Saved: load('m:/80_ChurnSim/out/users3.RData'); sw('m:/80_ChurnSim/out')
#' }
saa= function(ds, dsn= deparse(substitute(ds)) , ...){file= sf('%s.RData', dsn)
	save(list = if(missing(ds)) ls(all=TRUE) else  dsn, file=file, ...); 
	catf("%s::  %s  saved: lo('%s/%s'); sw('%3$s')\n", Sys.time(), dsn, getwd(), file)
	if(missing(ds)) catt('  # rmDF(); lsDF(); dir(); expl()')
} 


sa= function(file=''){save.image(file=sf('%s.RData', file)); catf("%s:: Image saved: lo('%s/%s.RData'); sw('%2$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), gw(), file)}
lo= function(file='.RData'){catt('Loaded:', ll<- pas(load(file=file, .GlobalEnv))); lss(); cat('\a\a\a'); alarm();ll} # expl("C:/Users/Public/Music/Sample Music/Kalimba.mp3")} # expl("Z:/exe/testVoice.vbs")}

#' list of ../out/.RData files 
#e  loo()
loo= function(patt='.RData'){gw(); 
	ff= dir(patt=patt, all.files =T)
	if(le(ff) > 0) { 
		srt(ldply(ff, function(f) c(mtime=ch(file.info(f)$mtime), size=round(file.info(f)$size/1e6, 1)
								, lo=sf('lo("%s")',f))), ~mtime)
	} else warning(sf('no %s files in the directory %s', patt, gw()))
	}	

#' save & remove
#e ca= cars; srm(ca); ls(pattern='ca'); dir(patt='ca')	# creates ca.RData
srm= function(x) {dsx= deparse(substitute(x)); saa(x, dsx); rm(list= dsx, envir = .GlobalEnv); catf('\n!!!    %s  is saved & removed  !!!\n', dsx)}

#' save an object to .csv
#w for write.csv 
tocsv= function(ds, dsn= sf('%s.csv', deparse(substitute(ds))), ...){ catt(dsn); write.csv(ds, file=dsn, quote=F, row.names= F); 
	catf("Saved: %s= read.csv('%s/%s'); expl('%2$s/%3$s')\n ",deparse(substitute(ds)), gw(), dsn)} 
totsv= function(ds, dsn= sf('%s.tsv', deparse(substitute(ds))), ...){ write.table(ds, file=dsn, quote=F, row.names= F, sep='\t', ...); 
	catf("Saved: %s= read.delim('%s/%s'); expl('%2$s/%3$s')\n ", deparse(substitute(ds)), gw(), dsn)} 


#' sort.data.frame
#e hee(srt(CO2, ~conc - uptake))
srt= sortt= sort.data.frame= function(x, by){
	# Author: Kevin Wright
	# with some ideas from Andy Liaw
	# http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
	
	# x: A data.frame
	# by: A one-sided formula using + for ascending and - for descending
	#     Sorting is left to right in the formula
	
	# Useage is:
	# library(nlme);
	# data(Oats)
	# sort(Oats, by= ~nitro-Variety)
	#brr()
	
	if(by[[1]] != "~")
		stop("Argument 'by' must be a one-sided formula.")
	
	# Make the formula into character and remove spaces
	formc <- as.character(by[2]) 
	formc <- gsub(" ", "", formc) 
	# If the first character is not + or -, add +
	if(!is.element(substring(formc, 1, 1), c("+", "-")))
		formc <- paste("+", formc, sep = "")
	
	# Extract the variables from the formula
	vars <- unlist(strsplit(formc, "[\\+\\-]"))    
	vars <- vars[vars != ""] # Remove any extra "" terms
	
	# Build a list of arguments to pass to "order" function
	calllist <- list()
	pos <- 1 # Position of + or -
	for(i in 1:length(vars)){
		varsign <- substring(formc, pos, pos)
		pos <- pos + 1 + nchar(vars[i])
		if(is.factor(x[, vars[i]])){
			if(varsign == "-") {
				calllist[[i]] <- -rank(x[, vars[i]])
			} else {
				calllist[[i]] <- rank(x[, vars[i]])
			}
		} else {
			if(varsign == "-") {
				calllist[[i]] <- -x[, vars[i]]
			} else {
				calllist[[i]] <- x[,vars[i]]
			}
		}
	}
	return(x[do.call("order", calllist), ])
}



if(0){   #== Misc
	libra(installr); updateR()
	theFile= 'm:/50_HLP/out/packages/HLP.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	gff('\\bF\\b', theFile)
	
	theFile= fp(proot, '50_HLP.r')
	
	CreateNewProj(newProj.name= 'zzz', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplNa me', root='T:/work/UseR-2013')
	CreateNewProj(newProj.name= '49_2048', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='m:')
} #--




