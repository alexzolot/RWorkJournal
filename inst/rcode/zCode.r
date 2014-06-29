#' @name zCode
#' 
#' @title functions for work with code, excluding R Work Journal  - not for book 
#' 
#' @description  Helper Funcs for work with code and file system
#' not for a  book "Handling Large R Projects"
#' 
#' abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #e - example,  #en - example "\notrun"
#' @author Alex Zolotovitski <alex@@zolot.us>
#' License: GPL-2  

#' @keywords aliases

#' @exportPattern "^[[:alpha:]]+"

#' @import plyr HaLaP data.table
#' @docType package
options(datatable.print.nrows=100)
libra(data.table); dtt= data.table; ad= as.IDate; taa= tables	# vignette("datatable-faq")


###  Common funcs

#'w writeLines
wl= function(s=.Last.value, out, show=T, ...) { message(sf('\n\nwl to file: expl("file://%s")\n', out))
	writeLines(s, out, ...)
	if(show) expl(sf("file://%s", normalizePath(out)))
}


#'  get global variable or option "theFile"
#en get.theFile() 
get.theFile= function() {
	tf= if(exists('theFile', envir= .GlobalEnv)) {get('theFile', envir= .GlobalEnv)
			} else getOption('theFile')
	is.null(tf) && stop('RWJ: Set  options(theFile= "full path to the file")')
	tf
}


#' grep pattern in the file
#e gff('= func', '^\\s*#', f=system.file('rcode/WorkJournal.r', package ='WorkJournal'))
gff= function(patt=' ===', pattNeg='gff', f= get.theFile(), withLineNumb= T){ 
	catt(3099,'============================ gff:', f)
	s= readLines(f, warn=F)
	ii= grepl(patt, s) & !grepl(pattNeg, s)
	x<- sf('%4s %s', if(withLineNumb) 1:le(s) %+% '.' else '', sub('^\\s*', '',s))[ii]
	prr(x)
	invisible(s[ii])   
}

#' set of funcs to extract regex, wrappers for regexpr
#e rege('b.', cn('abcd xy 23b67b8'))
# [1] "bc" ""   "b6"
# or
#e rege('b.', cn('abcd xy 23b67b8'))
# [1] "bc" ""   "b6"

#rege= function(patt, x) {y=regexpr(patt, x, 1);  substr(x, y, y + attr(y, "match.length")-1)}
rege= function(patt, x) {yy= regexec(patt, x, 1); laply(seq_along(yy), function(i){y=yy[[i]][1]; substr(x[i], y, y + attr(yy[[i]], "match.length")[1]-1)})}

#'   wrappers for regexpr
#e grege1('b.', 'abcdbnm')
#e grege1('x', 'abcdbnm')
# [1] "bc" "bn"
#  1 
# "" 
grege1= function(patt, x) {yy= gregexpr(patt, x, 1)[[1]];  laply(seq_along(yy), function(i){y=yy[i];  substr(x, y, y + attr(yy, "match.length")[i]-1)})}

#e grege('b.', cn('abcd xy 23b67b8 absbeb3'))
# nOK grege= function(patt, x) {yy= gregexpr(patt, x); str(yy); llply(yy, function(z)laply(seq_along(z), function(i){y=z[i];  substr(x, y, y + attr(z, "match.length")[i]-1)}))}
grege= function(patt, x) llply(x, function(z)grege1(patt, z))




### for packaging 

#' transform Comments to Roxygen
#en c2r(file= get.theFile(), suffOut='-copy', exec=F) 
c2r= Comments2Roxygen= function(file= get.theFile(), suffOut='-copy'
		, fout= fout<- gsub('\\.r$', suffOut %+% '.r', file)
		, exec=T, ...) {
	s= fsub(fin=file
			, fout
			, fileShow= F
			, `^#w `= "#' wrapper for "
			, `^#p `= "#' @param "
			, `^#' ex:`= "#e "
			, `^#en (.*)`= "#e \\\\dontrun{\n#'  \\1\n#' }" 
			, `^#e (.*)`= "#' @examples \n#'  \\1"
			, '^((([^#]+)= ?)+ ?functi.*)$'= "#\' @title \\3\n#' @aliases \\3 \n\\1"
			, '\\bF\\b'='FALSE'
			, '\\bT\\b'='TRUE'
			, ',([^\\s"\'])'=', \\1'  # insert blanks after comma, excluding quotes and spaces
			, zBase0= 'HaLaP', zCode= 'RWorkJournal')
	
	s= readLines(fout, warn = F)
	s[grepl('^#. @aliases ', s)]= gsub(' *= *', ' ', s[grepl('^#. @aliases ', s)])
	writeLines(head(s,99999), con = fout)
	
	if(exec) roxygenize(fout,...)
}

#	gsub(',([^\\s"\'])', ', \\1', ',xs')
#	gsub(',([^\\s"\'])', ', \\1', ',"')
#	gsub(',([^\\s"\'])', ', \\1', ",'")

if (0) {
	## example: https://github.com/hadley/r2d3/blob/master/R/r2d3.r
	sw('T:/work/zPacks/zBase0')
	# gw: sw("T:/work/zPacks/zBase0");  expl()
	
	theFile= 'T:/work/zPacks/zBase0/R/zBase0.r'
	c2r(file=theFile, suffOut='-copy', exec=F) 
	c2r(file=theFile, suffOut='', fsub=T, exec=T)
	file.remove('T:/work/zPacks/zBase0/R/zBase0-copy.r')
}


###  Analysis of function definitions and usage

# toZcode= function() {
 {   # analysis of function definitions and usage
	 
	 #' list of all functions in memory  ==
	 #e allFunInMem(); prr(allFunInMem())
	 allFunInMem= function() {x= ls(envir= .GlobalEnv)}[class(get(x))=='function']
	 
	 
	 listFuncDef= function(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r'
			 , stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$'){
		 s1= readLines(theFile, warn=F)  
		 
		 u= regexpr('^[^#]*([A-z0-9_\\.]+ *= *)+function\\(|^\\s*(invi(sible)?|return)\\(|^\\}', s1)
		 i= which(u>0)
		 v= dtt(i, s= gsub('\\s', '', s1[i]))
		 v[, fus:= ifelse(grepl('function', s), sub('(.*)=function.*', '\\1', s), '') ]
		 v[, ret:= grepl('(invi(sible)?|return|^\\{)', s) ]
		 suppressWarnings(v[, i2:=  ifelse(ret[-1], i[-1], i[-1]-1) ])
		 if(last(v$i2)==v$i2[1]) v[nrow(v), i2:= le(s1) ]
		 v= v[fus != '', list(fus, i, i2, fu1=unlist(strsplit(fus,'=')[[1]])), by=i]
		 invi(v)
	 }
	 #e d= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
	if (0) { # compare func defs in three files:
		dh= listFuncDef(theFile= 'm:/50_HLP/out/packages/HaLaP/inst/rcode/HLP.r')
		do= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/WorkJournal.r')
		d1= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/RWJ.r')
		d2= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/zCode.r')
		
		prr(nin(d2$fu1, d1$fu1))
		prr(nin(do$fu1, d2$fu1))
		prr(nin(do$fu1, c(dh$fu1, d1$fu1, d2$fu1)))
		
	}
	 
	 
	 
	 listFuncUsage= function(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r', lfd= listFuncDef(theFile)
			 , stoplist='^(le|catf|gsub|attr|grepl|substr|llply|c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|shell|str|unique)$'){
		 s1= readLines(theFile, warn=F)
		 s1= gsub('#.*|\\s','', s1)
		 u= regexpr('[A-z0-9_\\.]+\\(' , s1)
		 
		 i= which(u>0)
		 v= dtt(i, s= gsub('\\($', '', gsub('\\s', '', s1[i])))
		 v= v[, list(i, s, fu= unlist({g=gregexpr('[A-z0-9_\\.]+\\(' , s)[[1]]; ml= attr(g, "match.length");
									 llply(1:le(g), function(j)substr(s, g[j], g[j]+ml[j]-2))})), by=i]
		 
		 v= v[!grepl(stoplist, fu)]
		 v= v[, caller:= lfd$fus[lfd$i <= i  &  i <= lfd$i2] , by=i]
		 v[is.na(caller), caller:='global']
		 strr(v)
		 invi(v[, list(.N, ii= paste0(i, sep='', collapse=',')), by='fu,caller']) 
	 }
	 if (0) {
		 u= listFuncUsage()
		 u[caller !='global']
		 prr(unique(u[, nin(caller, fu)]))
	 }
	 
	 #' for many files, fun = fun(file), ... - args for dir(...)
	 inManyFiles= function(fun=I, patt = "", pattNeg = "^$", ...) { 
		 ff= dir2(patt, pattNeg, ...)
		 invi(dtt(ldply(fp(...,ff), function(f) dtt(f, fun(f)))))
	 }
	 #e ff= inManyFiles(basename, patt = "WorkJournal.r", pattNeg = "^$", 'M:/50_HLP/out/packages/WorkJournal/inst/rcode')
	 
	 
	 if(0){
		 # fun defs
		 dd= inManyFiles(listFuncDef, patt = "WorkJournal.r", pattNeg = "^$", 'M:/50_HLP/out/packages/WorkJournal/inst/rcode')
		 # fun usages
		 uu= inManyFiles(listFuncUsage, patt = "WorkJournal.r", pattNeg = "^$", 'M:/50_HLP/out/packages/WorkJournal/inst/rcode')
		 # redundancy
		 
		 tb= dtt(merge(df(dd), df(uu), by.x='fu1', by.y='fu'))
		 tb[, .N, by='fu1,caller']
		 tb[, .N, by='fu1'][order(N)]
		 uu[fu=='c2r']
		 
		 
		 x= listFuncDef(theFile= get.theFile())
		 hee(srt(x, ~-Freq), 99)
		 in.zBase= unlist(strsplit(x$x, c('=', ' ')))
		 pas(in.zBase)
		 # [1] "` aaa ab all0 AllCategFreqDesc analQuantiles atl BA= BayesAvg calcc CategFreqDesc catf catt cc  code2HTML cc= code2HTMLjQuery classes CleanSpareFigs cn command CorrelCircle CorrelCircle3 CreateNewProj cumsumrev CV CVwt dropp DT= DateTime= timestamp dummy exec execf expl expls ext2 fDate fid fll fregTAG fromUNIXt fsize fsub gcc gf  gtf gff gna gnal=lgna gw gzfile hee  dhe heec heee  dsh hees heta hglm HHa HHd HHf HHInit HHp.bak HHp HHp  HHp2 HHp2 HHpm HHpr= HTMLp HHs HHt hist.pan hists hive_conn hLink hSel hSelC inte isNum JS  JaccardSimilarity last LastDayOfWeek lg1 libra libras Lift LiftWt LiftWtArr LiftWtArr.Old LiftWtArr LiftWtArr2 LiftWtArr3 lo logg logit loo lsDF lss lss0 maxn me merge3 MergeByName mn Model mt nc nc  nmsv  sNames ncc newwin newwin=  function( nmsv  sNames nonUnique nope norm norm0 nuc nuf nut one ord ord2 ordd pas plotGLMCoeff plotl plotm plott pr prinb princ PrintModel prr prrr renamee RF.Importance  RFI rmall rmDF rmmm ROC rou rows.with.na rt rtd rtsv rwith.na sa saa sc.pan sf shorts ShowColorNames showInOpera spectr SQL2vars= Scope2vars srt  sortt  sort.data.frame SS st summar sumn susp suss tab.df tab Timer tocsv toTe totsv toXL= ToXL upper.panel wc week wwc wws xscrollcommand yScore yScoreSc yscrollcommand zeval= evall zlog10 zlogit zqqplot zqqplotWt"
	 }
	 
	 if(0){
		 x= listFuncUsage(theFile=  "m:/80_ChurnSim/80_ChurnSim.r")
		 hee(srt(x, ~-Freq), 30)
		 
		 fs= dir('M:/', recursive=T, patt='\\.[rR]$')
		 fs= fs[!grepl('^(999|scripts)', fs)]
		 fs= 'M:/' %+% fs
		 st({x2= ldply(fs, listFuncUsage)})  # 20.92 1.62
		 
		 x3= ddply(x2, .(x), function(x)df(nf=nrow(x), Freq=sumn(x$Freq)))
		 str(x3)
		 # 'data.frame':	3321 obs. of  3 variables:
		 #  $ x   : chr  "." "..." ".attr" ".C" ...
		 #  $ nf  : int  305 1 1 1 2 1 1 1 2 1 ...
		 #  $ Freq: num  2064 1 1 2 2 ...
		 hee(srt(x3, ~-Freq), 99)
		 # 3321  rows
		 #               x  nf  Freq
		 # 408        catt 580 14951
		 # 2907     tkgrid 341 12077
		 # 2738        sum 545 10368
		 # 1804      names 609  8701
		 # 1993      paste 677  8307
		 # 2194      print 646  7766
		 
		 in.zBase.Freq= sus(srt(x3, ~-Freq), x %in% in.zBase & x %in% allFunInMem()$x)
		 hee(in.zBase.Freq, 99)
		 srt(head(in.zBase.Freq, 19), ~x)
		 #           x  nf  Freq
		 # 404    catf 231  1108
		 # 408    catt 580 14951
		 # 479      cn 163  7469
		 # 1195    gff  77   731
		 # 1282     gw 100   664
		 # 1310    hee  80  1467
	 }
#}
	 
	 
		
	FunctionDependency= function(patt = "test.WorkJournal.r", pattNeg = "^$", path = ".",  ...) { 
		# fun defs
		dd= inManyFiles(listFuncDef, patt, pattNeg, path)
		# fun usages
		uu= inManyFiles(listFuncUsage, patt, pattNeg = "^$", path)
		dd=	setkey(dd, fu1)
		
		redund=	setkey(uu, fu)[dd$fu1][is.na(caller)]
		used= setkey(uu, fu)[dd$fu1][!is.na(caller) & caller != 'global' ]
		
		# graph
		libra(statnet) 
		libra(sna) 
		
		netw= network(used[, list(caller, fu)], directed=T,  matrix.type="edgelist")
		vtx= network.vertex.names(netw)
		
		ww()
		g= plot.network(netw, displaylabels =T, main=sf("Function Dependency in %s", patt)
				, vertex.col = c('red', 'blue')[nuf(dd[vtx]$f)]
		)
		legend('topleft', levels(fa(dd[vtx]$f)), col= c('red', 'blue'), pch=16, bty='n')
		invisible(list(defs=dd, use=uu, used.def=used))
	}
	
	
	plot.fun.usage= function(uu) { catf('\n:\n')
		gw()
		uu= listFuncUsage('M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
		
		libra(d3Network)
		#ht= c('<html>', d3SimpleNetwork(df(uu), file='d3.htm'), '</html>')
		ht=  d3SimpleNetwork(df(uu), fontsize=12, file='d3.htm')
		
		fd= FunctionDependency(patt = "test.WorkJournal.r", pattNeg = "^$", path = "M:/50_HLP/out/packages/WorkJournal/inst/rcode")
		fd= FunctionDependency(patt = "WorkJournal.r", pattNeg = "^$", path = "M:/50_HLP/out/packages/WorkJournal/inst/rcode")
		fd= FunctionDependency(patt = "^WorkJournal.*.r", pattNeg = "^$", path = "M:/50_HLP/out/packages/WorkJournal/inst/rcode")
		
		ht=  d3SimpleNetwork(df(fd$use), Source = 'fu', Target = 'caller', fontsize=12, file='d3.htm')
		ht=  d3SimpleNetwork(df(fd$used.def), Source = 'fu', Target = 'caller', fontsize=12, file='d3.htm')
		#ht=  d3ForceNetwork(df(fd$use), Source = 'fu', Target = 'caller', fontsize=12, file='d3.htm')
		expl('d3.htm')
		
		# for http://www.nomnoml.com/
		s= fd$ used.def[, sf('[%s]->[%s]</br>', caller, fu)]
		s= fd$ used.def[!grepl('code2HTML', caller), sf('[%s]->[%s]</br>', caller, fu)]
		wl(s, 'nn.htm') # then ^A^C
		expl('http://www.nomnoml.com')
		'#fontSize: 9
		#spacing: 4
		#padding: 2
		#direction: right
		#edges: rounded
		#lineWidth: 1
		#zoom: 1
		#gutter:1
		#leading: 1'	
	
	# for http://cpettitt.github.io/project/dagre-d3/latest/demo/interactive-demo.html
	s= unique(fd$ used.def[, sf('"%s" -> "%s";</br>', caller, fu)])
	wl(s, 'nn.htm')
	expl('nn.htm')  # then ^A^C
	expl('http://cpettitt.github.io/project/dagre-d3/latest/demo/interactive-demo.html')
	
	# for http://www.daviddurman.com/automatic-graph-layout-with-jointjs-and-dagre.html
	s= fd$ used.def[, list(sf("'%s': ['%s'],</br>", caller, paste0(unique(fu), sep = '', collapse ="','"))), by=caller]
	wl(s$V1, 'nn.htm')
	expl('http://www.daviddurman.com/automatic-graph-layout-with-jointjs-and-dagre.html')
	# nOK
	
}
	if (0) {
		fd= FunctionDependency(patt = "^RWJ.r", pattNeg = "^$", path = "M:/50_HLP/out/packages/WorkJournal/inst/rcode")
			prr(unique(fd$use$fu))
		
		fd= FunctionDependency(patt = "^WorkJournal.*.r", pattNeg = "^$", path = "M:/50_HLP/out/packages/WorkJournal/inst/rcode")
		fd$defs
		fd$used.def
		strr(fd)
		
		uu= listFuncUsage('M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
		# redundancy
		
		tb= dtt(merge(df(dd), df(uu), by.x='fu1', by.y='fu'))
		tb[, .N, by='fu1,caller']
		tb[, .N, by='fu1'][order(N)]
		uu[fu=='c2r']
		uu[fu=='code2rmd']
		dd[fu1=='code2rmd']
		
		# 	
		# used
		
		
		# graph
		libra(statnet) 
		libra(sna) 
		
		netw= network(used[, list(caller, fu)], directed=T,  matrix.type="edgelist")
		
		ww()
		g= plot.network(netw, displaylabels =T, main="Function Dependency in test.WorkJournal.r")
	}
	
}


if(0){   #== Misc
	theFile= 'R:/work/R-svn-ass/00_commonR/zCodeTools.fun.r'
	listFuncUsage(theFile, stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$')
	listFuncUsage(theFile, stoplist='^(c)$')
	
	gff('saved', theFile)
	gff('sa\\(|===', theFile)

	theFile='M:/50_HLP/out/packages/WorkJournal/inst/rcode/WorkJournal.r'
	gff('pic', theFile)
	gff('Pic', theFile)
	gff('getQuoteCommentStatus', theFile)
	gff('theFile', theFile)
	
	
	u= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
	
	theFile= fp(proot, '80_ChurnSim.r')+ dummy(x)
	#ccc= function()code2HTML(theFile)
	#cc= function()code2HTMLjQuery(theFile) #  cc()
	cc()
	
	shell('start explorer file:\\\\m:\\80_ChurnSim\\80_ChurnSim.r.jQ.htm')
	CreateNewProj(newProj.name= 'HLP_demo', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='m:/50_HLP/out')
} #--

