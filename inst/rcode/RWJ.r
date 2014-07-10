#' @name RWorkJournal
#' 
#' @title R Work Journal, CreateNewProj, and functions for work with code
#' 
#' @description R Work Journal and Helper Funcs for work with code and file system
#' for a book "Handling Large R Projects"
#' 
#' abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #e - example,  #en - example "\notrun"
#' @author Alex Zolotovitski <alex@@zolot.us>
#' License: GPL-2  

#' @keywords aliases

#' @exportPattern "^[[:alpha:]]+"

#' @import plyr HaLaP data.table knitr markdown base64
#' @docType package

#' Created  : 2014-06-18 02:55:55

stopifnot(require(plyr))
stopifnot(require(knitr))
stopifnot(require(markdown))
stopifnot(require(base64))
#libra(data.table); 
stopifnot(require(data.table))
dtt= data.table; ad= as.IDate; taa= tables  # vignette("datatable-faq")
options(datatable.print.nrows=100)


### from HaLaP
'%+%' = paste0
ch= as.character
na= names
sf= sprintf
le= length
fp= file.path
expl= function(x= gw(), ...) browseURL(x, ...)

#'  catt with names
#e  z= 1:5; v= letters[1:2];  catn(z, v, 7, u<-'a', v=88)
catn= function(...) {  # catt('catn:')
  nargs= unlist(strsplit(ch(match.call()),'(),', fixed =T))[-1]  # names of args
  for (i in 1:le(nargs)) catt(nargs[[i]],  '= ', list(...)[[i]])
}

#w cat + '\\n'
#e catt(cars[,1])
catt= function(...) {cat(...); cat('\n'); invisible(flush.console())} 

#' fun to split column names
#e  cn('aa bb 2013')
cn= function(cnn,  sep='[ ,\n\t\\+]+') unlist(strsplit(cnn, sep))  # b='',`%a%` = '%c%' =  col.names= cn(cnn)  'aa b 1'  %a% 1  aa b 1'  cn('aa b 1')
#w cat + sprintf
catf= function(...) cat(sprintf(...))
#w grep
gre2= function(patt='', pattNeg='^$', x, v=T, ...){ a=grepl(patt, x,...) & !grepl(pattNeg, x,...); return( if(v) x[a] else a) }

#w str
#'e strr(cars)
strr= function(x) {catf('\nstr(%s):\n', desu(x)); str(x)}

#' alias for deparse(substitute())
desu= function(x, n=1) deparse(substitute(x, env= parent.frame(n) ))

#' print list in 1 column
#e prr(letters[1:5])
#e prr(cars); prr(dir()); prr(character(0));
prr= function(x, ma='', header=T) {if(header)catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); ns= na(x)
                                   if(le(x)==0){message('length=0'); return(0)}
                                   for(i in if(is.null(ns)) 1:le(x) else ns) catf('%3s= %s\n', i,  x[[i]]); catt('-------------------------\n')
} 

#w dir.create + setwd
#en sw('myTestDir/mySubdir');  sw('../..'); expl()
sw= function (sDir, ...) {
  dir.create(sDir, rec=T, ...)
  setwd(sDir)
  catf('sw: Work dir set to: %s;  gw()\n', gw())
}

#w getwd
#e gw()
gw= function(){catf('gw: sw("%s");  expl()\n', gw<- getwd()); invisible(gw)} 


#w writeLines
wl= function(s=.Last.value, out, show=T, ...) { message(sf('\n\nwl to file: expl("file://%s")\n', out))
	writeLines(s, out, ...)
	if(show) expl(sf("file://%s", normalizePath(out)))
}

#' date &  time 
#e DT()
DT= DateTime= function(format = "%Y-%m-%d %H:%M:%S") strftime(Sys.time(), format) 

	onWin= nchar(Sys.getenv('computername')) > 0
	ww= if(onWin)windows  else x11
	nu= as.numeric
	df= data.frame
	#' x \ y
	#r  x  which are not in  y
	#e  nin(1:6, 4:9);  1:6 %-% 4:9
	nin= '%-%' = function(x, y) x[!(x %in% y)] # x not in y :   1:5 %-%  4:9 # `%-%` 
	
	

countDepth2= function(s= c('{{b}', 'n s{}', 'n s'), ch1='{', ch2='}', woComments=T){
  countChar1= function(s= c('{ {b}', 'n s{}', 'n s'), ch1='{') nchar(gsub(sf('[^%s]',ch1), '', s))
  
  if(woComments) s= gsub('#.*', '', s)
  
  n1= countChar1(s, ch1)
  n2= countChar1(s, ch2)
  invisible(depth<- n1-n2)
}


if(0){
  
  #TODO: treat sequence of  #, ', "  correctly
  
  (countDepth2(s= c('{ {b}', 'n s{}}', 'n s'), ch1='{', ch2='}'))
  # [1]  1 -1  0
  
  s2= gsub('#.*', '', s1)
  hee(x<- df(countDepth2(s1), countDepth2(s2), inc=countDepth2(s1)- countDepth2(s2), cs=cumsum(countDepth2(s2))), 100)
  sus(x, inc>0)
  sum(countDepth2(s2))
  # [1] -1
}



#' first Free Fig Number
#' e firstFreeFigN()
firstFreeFigN= function(dirr='../img', patt='^Fig_(\\d+).*\\.png$') nin(1:999, suppressWarnings(nu(gsub(patt, '\\1', dir(dirr, patt='.png$')))))[1]
firstFreeFigN= function(dirr='../img', patt='^(Fig|Pic)_(\\d+).*\\.png$') nin(1:999, suppressWarnings(nu(gsub(patt, '\\2', dir(dirr, patt='.png$')))))[1]


#' wrapper for dev.print  -  save graphics to .png file
#p  gg   = is.ggplot
sg= saveGraphics= function(capt=.main, Width = dev.size(units = "px")[1] , off= T
		, Height =  dev.size(units = "px")[2], GraphPointSize = 12, dirr='../img', type= "cairo"
		, res=96, dev=0, fNameWithCapt=F, gg=F, ...){ # type= "windows"
	op= options(); on.exit(options(op))  #; options(error=dummy)
	if(!file.exists(dirr)) dir.create(dirr)
	#.iFig= 1 + max(0, nu(gsub('^(Pic|Fig)_(\\d+).*\\.png$', '\\2', dir(dirr, patt='.png$'))), na.rm=T)
	.iFig= firstFreeFigN(dirr)
	# catt('--------------------------------------- HHp: old iFig=', .iFig)
	GraphFileName=  if(fNameWithCapt) sf('Fig_%s. %s', .iFig, capt) else  sf('Fig_%s', .iFig)
	
	AbsGraphFileName= sf('%s/%s/%s.png', gw(), dirr, GraphFileName)
	catt('HHp: printing to ', AbsGraphFileName)
	
	#if(capt > '') title(capt, col.main='blue4')
	
	if(dev>0) dev.set(dev)
	if(gg){try({  	ggsave(AbsGraphFileName)}, s=F)
	} else { 
		dev.copy2pdf(file = sub('png$', 'pdf', AbsGraphFileName)) #, width= 21, height = 10, pointsize= GraphPointSize)
		dev.print(device = png, file = AbsGraphFileName, 
				width= Width, height = Height, pointsize= GraphPointSize, units="px", type= type,...)
	}
	
	if(exists('.HTML.file'))	cat(sf('<p align="left"><img src="img/%s.png"  border="0" width="%s" height="%s"/><br/>
								<span class="caption>%s</span><br/></p>/n', GraphFileName, Width, Height, capt)
				, file = .HTML.file, append = TRUE)
	# dir(fp(.HTML.file,'../../img'))
	
	if(off) dev.off()
	
	options(op)
	catf('Saved to: %s. %s\n', AbsGraphFileName, capt)
	catf('%s. %s\n', GraphFileName, capt)
	invisible(sf('%s. %s', GraphFileName, capt))
}

#' add title to the last graph, then save it.
sgg= function(capt='', ...) {title(capt); sg(capt, ...)}


#'  save rCharts graphics to jFig_dd.htm file
#en sgj(p1)
sgj= function(p= p1, capt='', dirr='../img', absPath=T, fNameWithCapt=F) {
	#.ijFig<<- max(0, nu(gsub('^jFig_(\\d+).*\\.htm$','\\1', dir(dirr, patt='.htm$'))), na.rm=T)
	.ijFig<<- firstFreeFigN(dirr='../img', patt='^jFig_(\\d+).*\\.htm$')
	GraphFileName=  if(fNameWithCapt) sf('jFig_%s. %s', .ijFig, capt) else  sf('jFig_%s', .ijFig)
	AbsGraphFileName= if(absPath) sf('%s/%s.htm', dirr, GraphFileName)  else sf('%s/%s/%s.htm', gw(), dirr, GraphFileName)
	p$save(AbsGraphFileName) 
	catf('Saved to: expl("%s"). %s\n', AbsGraphFileName, capt)
	catf('%s. %s\n', GraphFileName, capt)
	invisible(sf('%s. %s', GraphFileName, capt))
}

# Saved to: expl("m:/80_ChurnSim/out/../img/jFig_10.htm"). 
# jFig_10.


#'  save d3 graphics to jFig_dd.htm file
#en sgj(p1)
sgd3= function(capt='', dirr='../img', absPath=T, fNameWithCapt=F, fin='.htm', save=T) {s=.Last.value
	.ijFig<<- firstFreeFigN(dirr='../img', patt='^jFig_(\\d+).*\\.htm$')
	GraphFileName=  if(fNameWithCapt) sf('jFig_%s. %s', .ijFig, capt) else  sf('jFig_%s', .ijFig)
	AbsGraphFileName= if(absPath) sf('%s/%s.htm', dirr, GraphFileName)  else sf('%s/%s/%s.htm', gw(), dirr, GraphFileName)
	if(save){
		if(is.null(fin)){wl(s, ".htm"); fin=".htm"}
		file.copy(fin, AbsGraphFileName) 
		catf('Saved to: expl("%s"). %s\n', AbsGraphFileName, capt)
		catf('%s. %s\n', GraphFileName, capt)				 
	}
	invisible(sf('%s. %s', GraphFileName, capt))
}

if (0) {
	libra(d3Network)
	
	d3SimpleNetwork(df(source=sample(1:9, 9, re=T),  target=sample(1:9, 9, re=T), fontsize = 15
				, linkColour = '#FF0000'
				, nodeColour = sample(cn('#0000FF #FF0000 #00FF00'), 9, re=T)), file='.htm')
	expl('.htm')
	sgd3()
	# jFig_1.
}



#' Clean spare Figs
CleanSpareFigs= function() { #==  Clean spare Figs  ==
	ff=dir('.', 'png')
	#~  [1] "Pic_1.png"      "Pic_10.png"     "Pic_11 (2).png" "Pic_11.png"     "Pic_12 (2).png" "Pic_12.png"     "Pic_13 (2).png" "Pic_13.png"     "Pic_14 (2).png" "Pic_14.png"     "Pic_15.png"     "Pic_16.png"     "Pic_17.png"     "Pic_18.png"     "Pic_19.png"     "Pic_20.png"     "Pic_21.png"     "Pic_22.png"     "Pic_23.png"     "Pic_24.png"     "Pic_25.png"     "Pic_26.png"     "Pic_27.png"     "Pic_28.png"     "Pic_29.png"     "Pic_3.png"      "Pic_31.png"     "Pic_32.png"     "Pic_33.png"     "Pic_34.png"     "Pic_35.png"     "Pic_36.png"     "Pic_37.png"     "Pic_38.png"     "Pic_39.png"     "Pic_4.png"      "Pic_40.png"     "Pic_41.png"     "Pic_43.png"     "Pic_44.png"     "Pic_45.png"     "Pic_46.png"     "Pic_47.png"     "Pic_48.png"     "Pic_49.png"     "Pic_50.png"     "Pic_51.png"     "Pic_52.png"     "Pic_53.png"     "Pic_54.png"     "Pic_55.png"     "Pic_6.png"      "Pic_7.png"      "Pic_8.png"      "Pic_9.png"     
	
	ps= gsub('^.*((Pic|Fig)_\\d+).*$', '\\1', ff)
	s=readLines(theFile)
	used= gsub('^.*((Pic|Fig)_\\d+).*$', '\\1', grep('(Pic|Fig)_', s, value=TRUE))
	spare= unique(nin(ps, used))
	spare.files= laply(ff, function(f) any(laply(spare, function(s) grepl(s, f))))
	any(laply(spare, function(s) grepl(s, 'Pic_1.png')))
	gw()
	# gw: sw("m:/95_TMob_LTV-2smpAct/in")
	
	dir.create('spare')
	for(f in ff[spare.files])file.rename(f, sf('spare/%s', f))
} #--


#'  get global variable or option "theFile"
#en get.theFile() 
get.theFile= function() {
	tf= if(exists('theFile', envir= .GlobalEnv)) {get('theFile', envir= .GlobalEnv)
			} else getOption('theFile')
	is.null(tf) && stop('RWJ: Set  options(theFile= "full path to the file")')
	tf
}




	
#' side effect: creates .rmd and .kn.htm  files
	code2rmd= function(.file= get.theFile(), s= readLines(.file, warn= F), toTempDir=T, show=T) { catf('\ncode2rmd("%s"): \n', .file)  #, withLineNum=T
		stopifnot(require(knitr))
		stopifnot(require(markdown))
		
		sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
		q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1', s, v=F)  # line start quote
		#q2= gre2('[\'"`]\\s*$', "(['\"`]).*\\1\\s*$|#'\\s*$", s, v=F)  # line end quote, not roxygen
		#q2= gre2('[\'"`]\\s*$', "\\1.*(['\"`])\\s*$|#'\\s*$", s, v=F)  # line end quote, not roxygen
		#q2= gre2('[\'"`]\\s*$', "(.*['\"`]){2,}\\s*$", s, v=F)  # line end quote, not roxygen
		q2= gre2('[^#][\'"`]\\s*$', "('.*'|\".*\"|`.*`)\\s*$", s, v=F)  # line end quote, not roxygen
		
		ich= cumsum(q2 | q1)  # chunk index
		iich= ich - ifelse(ich%%2, 0, c(0, diff(ich)))  # chunk index, from 0; 0 is.code
		is.code= 1- iich%%2
		
		ss= dtt(i= 1:le(s), q1, q2, ich, iich, is.code, s, s2=s)  #, q2a, q2b
		
		dout= if(toTempDir) tempdir() else dirname(.file)
		fout= fp(dout, basename(.file))
		
		#' s2 - code for .rwd
		if(1){
			ss[(q1 | q2) & ich%%2==0,  s2:=  sf("%s\n\n```{r %s}", s, i)]
			ss[(q1 | q2) & ich%%2==1,  s2:=  sf("```\n%s",  s)]
			ss[i==1      & ich%%2==0,  s2:=  sf("\n```{r 1}\n%s", s)]
			ss[i==le(s)  & ich%%2==0,  s2:=  sf("%s\n```",  s)]
			ss[,  s2:=  gsub('(^|[^x"])@', '\\1\\\\', gsub('@@', '\\\\\\\\', s2))]  # escapes for LaTeX @ -> \
			#wl(ss$s2, .file %+% '.rmd')
		}
		options(datatable.print.nrows=600)
		print(ss)
		a.<<- ss   #xxx
		
		rmd= ss$s2
		
		#if(toTempDir) {dout=tempdir();   .file= fp(dout, basename(theFile))} else dout= dirname(theFile)
		
		
		wl(rmd,  fout %+% '.rmd', F)
		
		#' s3 - code for .rwd with line numbers comments in r chunks 
		# insert comments with line numbers in non-empty R chunks
		ss[, s3:=s2]
		ss[is.code & grepl('\\S', s), s3:= s3 %+% sf(" # line %s#", i)]  
		
		if (`fig and r-headers to knit TOC`<- 0) {
			#' transf headers to roxygen comments in r chunks
			# ss[is.code==1 ,  s3:= sub('(.*#)(=+)', "\\1' \\2", s3)]  
			
			ss[is.code==1 ,  s3:= sub('(.*?#)(#+.*)', "\\1\n\n```\n\\2\n\n```{r }", s3)]  
			ss[is.code==1 ,  s3:= sub('(.*#)( j?Fig.*)', "\\1\n\n```\n#####\\2\n\n```{r }", s3)]  
			
			rmd= ss$s3
			wl(rmd,  fout %+% '.rmd', T)			
		}
		
		
		opts_chunk$set(eval=F)
		kn= knit(, text= rmd) 
		#			markdownToHTML(file, output, text,
		#					options = getOption("markdown.HTML.options"),
		#					extensions = getOption("markdown.extensions"), title = "",
		#					stylesheet = getOption("markdown.HTML.stylesheet"),
		#					header = getOption("markdown.HTML.header"),
		#					template = getOption("markdown.HTML.template"), fragment.only = FALSE,
		#					encoding = getOption("encoding"))
		#			expl(getOption("markdown.HTML.header"))
		#			expl(getOption("markdown.HTML.template"))
		
		#knit.htm= markdownToHTML(fout %+% outSuffix, text= kn, fragment.only=F, out=fout %+% '.kn.htm')
		knit.htm= markdownToHTML(fout %+% '.rmd', text= kn, fragment.only=F, out=fout %+% '.kn.htm'
				, options = cn('mathjax highlight_code toc'))  #, extensions= cn('no_intra_emphasis latex_math')  'base64_images'
		expl(fout %+% '.kn.htm')
		
		
		if (1) { #' with line numbers - bad for \] LaTeX
			# grep('\\\\', '\\')
			#s3= ifelse(grepl('(^\\s*$|```|^\\s*\\\\|\\\\\\[|\\\\\\]|= *\\\\)', ss$s2), sf('%s <!--%s-->', ss$s2, 1:le(ss$s2)), sf('%s #zz#%s#', ss$s2, 1:le(ss$s2)))
			s3= ifelse(grepl('(^\\s*$|```|^\\s*\\\\|\\\\\\[|\\\\\\]|= *\\\\)', ss$s2), ss$s2, sf('%s #zz#%s#', ss$s2, 1:le(ss$s2)))
			s3= ss$s3	# prr(ss$s3)
			rmd.nu= c(s3)  #; strr(rmd.nu); prr(rmd.nu)
			wl(rmd.nu,  fout %+% '.nu.rmd', F)
			kn.nu= knit(, text= rmd.nu) 
			kn.nu.htm= markdownToHTML(fout %+% '.nu.rmd', text= kn.nu, fragment.only=F, out=fout %+% '.kn.nu.htm'
					, options = cn('mathjax highlight_code'))  #, extensions= cn('no_intra_emphasis latex_math')
			expl(fout %+% '.kn.nu.htm')
		}
		
		if(show) expl(dout)
		
		# out.kn.htm= sub('$', '.kn.htm', fout)
		invisible(list(file= .file, dout= dout, fout= fout, rmd=rmd, lines=ss, out.rmd= fout %+% '.rmd'
						, out.kn.htm= fout %+% '.kn.htm', out.kn.nu.htm= fout %+% '.kn.nu.htm'))
	}
#e rmd= code2rmd()
#e rmd= code2rmd(toTempDir =F)
	
#e treat.knit.html(theFile)  dout=dirname(theFile), rFile=theFile
	kn.htm2rwj= treat.knit.html= function(theFile, rFile=theFile, dout=dirname(rFile)) { catf('\n:\n')
		'we have 2 knitr produced files: code2rmd() ->  .kn.htm  and cccc= rmd2htm.main -> .html'

		js1='	<!-- script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script-->
				
				<script src="http://code.jquery.com/jquery-2.1.1.js"></script>
				
				<script>
				
				var d= document;  
				var imgFold= "<img src=\'data:image/jpeg;base64,R0lGODlhDQAMAJEAAAAAAP///2NlY////yH5BAEAAAMALAAAAAANAAwAAAIYnI+pK+2OBIvzSHWNfJtin2SDaEUctCwFADs=\' alt=\'-\'/>";
				var srcMinus="data:image/jpeg;base64,R0lGODlhDQAMAJEAAAAAAP///2NlY////yH5BAEAAAMALAAAAAANAAwAAAIYnI+pK+2OBIvzSHWNfJtin2SDaEUctCwFADs=";
				
				$(document).ready(function(){
				
				
				$(".fold").src= srcMinus;
				
				$(".figz").each(function(){   //copy src from gallery to TOC and main - for base64 nOK!
				cl(this.id)
				this.src= $("#tn" + this.id).src
				$("#TOC" + this.id).attr("src",  this.src)
				})
				
				//http://stackoverflow.com/questions/5540561/jquery-click-event-not-firing
				$(".fig").click(function(e) {e.preventDefault(); 
				for(k in e) if(e[k] != null && k.match(/width|css|style/)) cl("fig.e[" + k + "]= " + e[k]); 
				w=this.width; 
				$(this).css({width: (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? "100%" : 700})
				})
				.dblclick(function(e) {e.preventDefault(); ShowImg(this.src, this.alt, e.ctrlKey) });
				
				$("img.imgGal, img.TOCfig")
				.mouseover(function() {this.title= this.alt; show(this) })
				.mouseout(function() { show(0) })
				.click(function() {gotoo(this.id.replace("tn","").replace("TOC","")) })
				.dblclick(function(e) { e.preventDefault(); ShowImg(this.src, this.alt, e.ctrlKey) });
				
				
				$("span.TOC").click(function() {gotoo(this.id.replace("TOC","")) })
				
				
				$(".aToggle.DD1").click(function() {toggleD($(this), $(".H1 + + .aD"), $(".D1")) })
				$(".aToggle.DD2").click(function() {toggleD($(this), $(".H2 + + .aD"), $(".D2")) })
				$(".aToggle.DD3").click(function() {toggleD($(this), $(".H3 + + .aD"), $(".D3")) })
				$(".aToggle.DD4").click(function() {toggleD($(this), $(".H4 + + .aD"), $(".D4")) })
				$(".aToggle.TOContents").click(function() {toggleD($(this), $(".zz"), $("div.TOContents")) })
				$(".aToggle.Gallery").click(function() {toggleD($(this), $(".zz"), $("div.Gallery")) })
				$(".aToggleAllFig").click(function()   {toggleD($(this), $(".zz"), $("img,iframe"))}) //xxx: ; toggleD($(this), $(".zz"), $("iframe")) 
				//$(".linu").click(function() { $("body").scrollTop(0) })
				
				$("#aToggleAll").click(function() {th= $(this)
				if( th.text() =="(+)") {
				$("div").show(); th.html(imgFold); $("a:not(.comments)").html(imgFold)   
				} else { $("div").hide(); th.html("(+)"); $("a:not(.comments)").html("(+)")}
				})
				
				
				$(".aToggleComments").click(function() {ToggleComments2();})
				
				// $(".aD").click(function() //zzz does not work inside <code> can be explicitly call <a href="javascript:alert();" class="aD">
				
				})
				</script>
				<script>
				
				function cl(x){console.log(x)}
				
				function gotoo(i) {w= d.location= "#" + i;}
				function gotop() {d.body.scrollTop = d.documentElement.scrollTop = 0}
				
				function linutoggle() {$("span.linu").toggle()}
				
				function ToggleFold(d){ 
				$("#D" + d).animate({ height:"toggle"}, 600); th= $("#asp" + d); 
				th.html(th.html()== "...}"  ? imgFold : "...}")
				}
				
				function resize0(e) { w= e.width;
				e.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? "100%" : 700;
				}
				
				function resize(i) {e=d.getElementById(i); w= e.width;
				e.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? "100%" : 700;
				}
				
				
				function ShowImg(src, capt, ctrl){ 
				if(ctrl) {src= src.replace("png", "pdf")};
				w= window.open(src, capt, "type=fullWindow,fullscreen,location=\'\',height="+ screen.height+ ",width="+screen.width)
				if(ctrl) { w.document.location= src} else {w.document.write("<html><title>" +  capt + "</title>" + capt + "<br/><img src=\'"  + src + "\' /> </html>")};
				w.title= capt;
				w.document.title= capt;
				w.focus()
				}
				
				
				function show(e){
				$("#show").css({display : (e==0)? "none" : "block", border: (e==0)? "0" : "2px solid blue"})
				
				if(e != 0){
				x= Math.min(e.offsetLeft+ e.width + 10, 500) //x= e.x+ e.width + 10;// ; if(x > 500) x= 500;
				y= e.offsetTop+ e.height + 10               //y= e.y+ e.height + 10

cl("e.src= " + e.src);
				
				$("#show").css({left: x, top: y})
				$("#showFig").attr("src", e.src)
				$("#showTxt").html("<br/>" +  e.alt)  //  +  e.alt)
				
				/* show properties of e 
				for(k in e) if(1 || e[k] > 10 && e[k]+" " < "a") cl("e[" + k + "]= " + e[k]);
				//for(k in e) if(e[k] != null && k.match(/^src$|^id$/)) {cl("e[" + k + "]= " + e[k])}; 
				*/
				}
				}
				
				</script>'	
		
		js.toggle='<script>	
				function toggleD(a, a2, di){ if( a.text() == "(+)") {
				di.show(); a.html(imgFold); a2.html(imgFold)  
				} else { di.hide(); a.html("(+)"); a2.html("...")}
				}
				
				function ToggleComments2(){ 
				var tt = document.getElementsByClassName("comment2"); 
				var t0= tt[0].style.fontSize;
				di= (t0 == "3px" || t0 == "") ? "12px" : ((t0 == "12px")? "1px" : "3px") ;
				fc=  di == "1px" ? "white" : "green";
				for (var i = 0; i < tt.length; i++) {tt[i].style.fontSize = di; tt[i].style.color=fc;}
				}
				</script>
				'
		css='	<style scoped>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */
				//body {max-width: 95%; font-size: 100%; line-height: 100%;}
				div.Gallery {background-color:rgb(255,248,248); }	
				div.TOC {background-color:rgb(248,248,255); }	
				div.main, .r {font-family: monospace; white-space: pre; max-width: 1000px}
				//p{margin-bottom:2%; margin-top:2%;margin-before: 2%; margin-after: 2%;}
				code {border: 0px}
				pre  {border: 1px solid}
				
				.D1, .D2, .D3, .D4, .D5  {background-color:rgba(255,240,240, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} /*div {; opacity: 0.3; background-color:GhostWhite; border-left-style:ridge;}  */
				// code, pre  {background-color:rgba(240,240,255, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} 
				div.D5 {font-size:8px;} 
				//.sq {font-size:12px; font-family: Arial; color:DodgerBlue } 
				center, .capt {font-size:12px; font-weight:bold; font-family: Arial; margin:auto; text-align:center;}
				.captTOC {font-size:100%; font-weight:bold; font-family: Arial; align:left}
				H1,  H2,  H3,  H4,  H5 {color:blue; font-family: Arial; color:teal;}
				.H1, .H2, .H3, .H4, .H5 {color:teal; font-family: Arial; font-weight:bold;  display:inline-block; display:-moz-inline-box;}
				H1, .H1 {font-size:300%;  margin-top:36px;  margin-bottom:8px;  margin-left:0px; } 
				H2, .H2 {font-size:250%;  margin-top:28px;  margin-bottom:5px;  margin-left:20px;} 
				.TOC.H1, H3, .H3 {font-size:200%;  margin-top:15px;  margin-bottom:3px;  margin-left:40px;}
				.TOC.H2, H4, .H4 {font-size:150%;  margin-top:12px;  margin-bottom:2px;  margin-left:60px;}
				.TOC.H3, H5, .H5 {font-size:120%;  margin-top:10px;  margin-bottom:1px;  margin-left:80px;}
				.TOC.H4, H6, .H6 {font-size:105%;  margin-top:7px;  margin-bottom:1px;  margin-left:100px;}
				img.fig, .captTOC, img.tnTOC {margin-top:0px;   margin-bottom:0px;  margin-left:10px;} 
				.TOC.H1, .TOC.H2, .TOC.H3, .TOC.H4, .TOC.H5, span.captTOC, img.tnTOC { display:block;}
				.D77 { display:inline; }
				img, .aD, iframe {border-style:none; border:0}
				/* .dimg{height:90px;  display:inline-block;} /* position:fixed;  position:absolute;*/ 
				.imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:100px} /*height:100%; max-width:100%; max-height:100%; }/* position:absolute; fixed;*/ 
				
				.fun {color:indigo; font-weight:bold;}
				.comment {color:green; align:left}
				.comment2 {color: rgb(100,200,100); font-size:3px; -webkit-text-size-adjust: none;}
				.text, .sq {color: teal; font-size:12px;}
				</style>
				' 
		
		menu.line= '# <a href="#" id= "aToggleAll">Fold</a> All |
				<a href="#" class= "aToggleAllFig">Toggle</a> all figs | 
				<a href="#" class= "aToggle Gallery">Fold</a> Gallery |
				<a href="#" class= "aToggle TOContents">Fold</a> TOC |
				<a href="#" class= "aToggle DD2">Fold</a> H2 |
				<a href="#" class= "aToggle DD3">Fold</a> H3 |
				<a href="#" class= "aToggle DD4">Fold</a> H4 |
				<a href="#" class= "aToggleComments">Toggle</a> Comments |
				<a href="javascript:linutoggle()" >Toggle</a> Line Numbers <span id="out"></span></br>'

		footer=	'<div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
				<span id="showTxt"></span><br/>
				<img id="showFig" src="#" height="401" style="left:0px" alt=""/>
				</div>\n</body>\n</html>'
		
		sk3= readLines(theFile %+% '.kn.nu.htm')  # by  cccc(wchunks=T) 
		sk.tit= grep('<title>', sk3)
		sk3[sk.tit]= sf('<title>%s - RWJ</title>', basename(theFile))
		
		#' insert syntax highlighter
		synhi= readLines(system.file('resources', 'r_highlight.html',  package ='markdown')) # syntax highlighter
		sk.3= append(sk3, c(synhi, css), after= sk.tit)
		
		bd= grep('body>', sk.3)	# [1] 192 364
		in.body= (bd[1]+1):(bd[2]-1)
		
		#fout= fp(tempdir(), basename(theFile) %+% '.sk.3.htm')
		
		s= sk.3[in.body]
		
		#' prep LaTex
		s= gsub('(^.*#.*) \\$([^ ])(.*)([^ ])\\$ ', '\\1 \\\\\\(\\2\\3\\4\\\\\\) ', s)
		
		
		
		#' add line numbers
		# s= gsub('^(.*) # line (\\d+)#', '<span id="sp\\2" class="linu" onclick="gotop()">\\2  </span>\\1', s)
		s= gsub('^(<pre><code class="r \\d*">)?(.*) # line (\\d+)#', '\\1<span id="sp\\3" class="linu" onclick="gotop()">\\3  </span>\\2', s)
		
		#wl(s, '1.htm')
		
		#'  comments, TODO and xxx  ==
#	s= gsub("^(\\s*#\')(.*)$",'<span class="comment2">\\1</span><span class="text">\\2</span>', s)  
#	s= gsub('(#[^\'=-].*?)(<|$)','<span class="comment">\\1</span>\\2', s) 
#	s= gsub('^(.*)(#=+)(.*)$','\\1<span class="comment2">\\2</span>\\3', s)  
#	s= gsub('^(.*)(#\\-\\-)(.*)$','\\1<span class="comment2">\\2</span>\\3', s)   
#	s= gsub('^(.*# ?.*)((xxx|TODO):.*)(</span.*)$','\\1<font color="red">\\2</font>\\4', s)
		
		
		#'  comments, TODO and xxx  ==
		s= gsub("^(.+/span>\\s*#\')(.*)$",'<span class="comment2">\\1</span><span class="text">\\2</span>', s)  
		#wl(s, '1b.htm')
		
		s= gsub('( #[^\'=-].*?)(<|$)','<span class="comment">\\1</span>\\2', s)  
		#wl(s, '1c.htm')
		
		s= gsub('^(.+/span>.*?)( *#=+ *)(.*)$','\\1<span class="comment2">\\2</span>\\3', s)  
		
		#wl(s, '2.htm')
		
		s= gsub('^(.+/span>.*)(#\\-\\-)(.*)$','\\1<span class="comment2">\\2</span>\\3', s)  
		s= gsub('^(.+/span>.*# ?.*)((xxx|TODO):.*)(</span.*)$','\\1<font color="red">\\2</font>\\4', s)
		
		
		#' treat figs
		#	s= gsub('^(.*# )(Fig_\\d+)(.*?)'
		#			, '<br/></code><code class="r"><img id="\\2" class="fig" src="img/\\2.png" width=700 alt="\\2\\3"/>   
		#					<br/><span class="capt">\\1\\2\\3</span><br/>', s)
#		strr(s)
#		u= gre2('# Fig_\\d+',,s)
#		prr(u)
##		u= gre2('^(.*id="sp(\\d+)".*# )(Fig_\\d+)(.*?)(</span.*)',,s)
##		prr(u)
#		u= gre2('^(.*id="sp(\\d+)".*# )(Fig_\\d+)',,s)
#		prr(u)
		#stop('zzzzz')

#		s= gsub('^(.*id="sp(\\d+)".*# )(Fig_\\d+)(.*?)(</span.*)'
#				, '<br/></code><code class="r"><img id="fig\\2" class="fig" src="img/\\3.png" width=700 alt="\\3\\4"/>   
#						<br/><span class="capt">\\3\\4</span><br/>', s)
#s= gsub('^(.*id="sp(\\d+)".*# )(Fig_\\d+)(.*)'
				s= gsub('^(.*id="sp(\\d+)".*# )(Fig_\\d+)([^<>]*)'
								, '<br/></code><code class="r"><img id="fig\\2" class="fig" src="img/\\3.png"
								 width=700 alt="\\3\\4"/>   
						<br/><span class="capt">\\3\\4</span><br/>', s)
		#' js figs (d3)
		s= gsub('^(.*id="sp(\\d+)".*# )(jFig_\\d+)([^<>]*)'
				, '<br/></code><code class="r"><iframe id="fig\\2" class="jfig" src="img/\\3.htm" width="100%"  height="600px" name="\\3\\4"></iframe>  
						<br/><span class="capt">\\3\\4</span><br/>', s)
		
		#' treat Pics
		s= gsub('^(.*id="sp(\\d+)".*# )(Pic_\\d+)([^<>]*)'
				, '<br/></code><code class="r"><img id="fig\\2" class="fig" src="img/\\3.png" width=700 alt="\\3\\4"/>   
						<br/><span class="capt">\\3\\4</span><br/>', s)
		#' js figs (d3)
		s= gsub('^(.*id="sp(\\d+)".*# )(jPic_\\d+)([^<>]*)'
				, '<br/></code><code class="r"><iframe id="fig\\2" class="jfig" src="img/\\3.htm" width="100%"  height="600px" name="\\3\\4"></iframe>  
						<br/><span class="capt">\\3\\4</span><br/>', s)

#s1[i]= gsub('^([^\\~]*# *"?)(jFig_\\d+)(.*)','\n\n <iframe src="img/\\2.htm" width="100%" height="600px"></iframe>\n\n \\1\\2\\3' , s1[i])
		
		#' treat headers
		#p patt.le - regex, it's length -2 is defines header level
		#'  s= gsub('(<span id="sp(\\d+)".*\\s*#== )(.*)( =+.*)$',  '\\1<span class="H2" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>',  s)
		get.r.headers= function(s, find='(<span id="sp(\\d+)".*\\s*#=+ )(.*)( =+.*)$', patt.le='#=+ '
				, replace='\\1<span class="H%s" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>') { 
			h= regexpr(find, s)
			i= which(h>0)
			hh= dtt(i, le=attr(regexpr(patt.le, s[i]),"match.length"), s=s[i])
			hh[, s2:=sub(find, sf(replace, ch(le-2)), s), by=i]
			hh
		}
#	hh= get.headers(s, find='(<span id="sp(\\d+)".*\\s*#=+ )(.*)( =+.*)$', patt.le='#=+ '
#			, replace='\\1<span class="H%s" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>')
		
		
		
#		wl(s, '3.htm')
# <span id="sp6" class="linu" onclick="gotop()">6  </span>if(init&lt;- 1) { <span class="comment2">#===</span> Init ===
# <span id="sp6" class="linu" onclick="gotop()">6  </span>if(init&lt;- 1) {<span class="comment2"> #=== |  </span>Init | ===
#	
#	hh1= get.r.headers(s, find='(<span id="sp(\\d+)".*\\s*#=+ )(.*)( =+.*)?$', patt.le='#=+ '
#			, replace='\\1<span class="H%s" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>')
		hh1= get.r.headers(s, find='(<span id="sp(\\d+)".*#=+ +</span>)(.*?)( =+.*)?$', patt.le='#=+ '
				, replace='\\1<span class="H%s" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>')
		s[hh1$i]= hh1$s2
		
		#brr()
		prr(s[hh1$i])
		
		hh= get.r.headers(s, find='(<span id="sp(\\d+)".*\\s*##+ )(.*?)( =+.*)?$', patt.le='##+ '
				, replace='\\1<span class="H%s" id="\\2" title="\\2">\\3</span> <span class="comment2">\\4</span>')
		s[hh$i]= hh$s2
		#wl(s, theFile %+% '.3.kn.htm')
		
	
		#====  div for code folding  ===
		
		s1= s
		depth= countDepth2(s1)
		#imgFold= '<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\' alt="-"/>'
		# imgFold= '<img src="data:image/jpeg;base64,R0lGODlhDQAMAJEAAAAAAP///2NlY////yH5BAEAAAMALAAAAAANAAwAAAIYnI+pK+2OBIvzSHWNfJtin2SDaEUctCwFADs=" alt="-"/>';
		
		imgFold= '<img src="" alt="-" class="fold" />'
		
		
		if (`split fold divs`<- 0) {
			s1= ifelse(depth!= 0, gsub('^(<span id=.sp(\\d+)[^#]*\\{[^\\{\\}]*)'
							, '\\1 <a href="javascript:ToggleFold(\\2)" id="asp\\2"><img src="" alt="-" class="fold" /></a>
									</code></pre><div class="D-fold" id="D\\2"><pre><code class="r div">', s1), s1)
			
			s1= gsub('^(.* class="H(\\d)".* id="(\\d+).*)D-fold(.*) class="D-fold"' , '\\1D\\3\\4 class="D\\2" id="D\\3"', s1)
			s1= ifelse(depth!= 0, gsub('([^#\\{\\}]*)\\}', '\\1<b>}</b></code></pre></div><pre><code class="r fold">', s1), s1)
			
		}else{
			s1= ifelse(depth!= 0, gsub('^(<span id=.sp(\\d+)[^#]*\\{[^\\{\\}]*)'
							, '\\1 <a href="javascript:ToggleFold(\\2)" id="asp\\2"><img src="" alt="-" class="fold" /></a>
									<div class="D-fold" id="D\\2">', s1), s1)
			
			s1= gsub('^(.* class="H(\\d)".* id="(\\d+).*)D-fold(.*) class="D-fold"' , '\\1D\\3\\4 class="D\\2" id="D\\3"', s1)
			s1= ifelse(depth!= 0, gsub('([^#\\{\\}]*)\\}', '\\1<b>}</b></div>', s1), s1)
			
		}
		
		
		
		#==  prepare Gallery  ==
		#figs= gre2('# (Pic|Fig)_\\d+',, s1) # prr(figs)
#figs= gre2('class="fig"',, s1) # prr(figs)
#figs= sub('.*(<img id=".*?>).*','\\1', figs) # prr(gal)

figs= gre2('class="j?fig"',, s1) ; prr(figs)
figs= sub('.*(<img id=".*?>).*','\\1', figs) ; prr(figs)
figs= sub('.*(<iframe id=".*?iframe>).*','\\1', figs) # prr(gal)

figs= sub('class="jfig"','class="imgGal"', sub('width="100%"  height="600px"', 'width=300 height=140 style="-ms-zoom: 0.25"', figs))


		figs= sub('img id="', 'img id="tn', figs) # prr(gal)
		figs= sub('class="fig"','class="imgGal"', sub('width=700','height=140', figs))
		
		#js0= sub(,'text/x-mathjax-config',, js)
		
		
		#==  prepare Table of Contents  ==	
		
		#' numerate md <h> -> <span class="H
		s1= ifelse(grepl('<h\\d+', s1), sf('<!-- l#%s -->%s', 1:le(s1), s1), s1)
		s1= sub('<!-- l#(\\d+) -->.*<h(\\d)>(.*)</h.>', '<span class="H\\2" id="md\\1">\\3</span>', s1)
		
		toc= grep('img id=|iframe id=|<H\\d+|<h\\d+|"H[1-5]', s1, value=TRUE) 
		toc= gsub('.*(<span class="H.*?)=* *</span>.*', '<br/>\\1</span>', toc)  # clean <H>
		toc= sub('.*(<img id=".* alt="(.*?)").*', '\\1 title="\\2"/>', toc) # clean fig
		toc= sub('.*(<iframe id=".*) name="(.*?)"(.*iframe>).*', '\\1 title="\\2"\\3', toc) # clean fig
		#toc= sub('width="100%"  height="600px"', 'width=300 height=140 style="-ms-zoom: 0.25"', toc) # clean fig
		toc= sub('width=".*?"  height=".*?"', 'width=300 height=140 style="-ms-zoom: 0.25"', toc) # clean fig
		toc= sub(' width=.*? ', ' height=140 ', toc) # clean fig
		toc= sub(' id="', ' id="TOC', toc) # clean fig
		
		
		toc= gsub('"capt"', '"captTOC"', toc)
		#toc= gsub('class="fig"', 'class="tnTOC"', toc)
		toc= gsub('class="fig"', 'class="TOCfig"', toc)
		toc= gsub('class="H', 'class="TOC H', toc)
		toc= gsub('700', '250', toc)  		# figs -> thumbnails
		toc= gsub('resize', 'goto', toc)
		toc= gsub('id="(\\d+)"', 'id="tnTOC\\1"', toc)
		toc= gsub("goto\\('tn(\\d+)'\\)", "goto('\\1')", toc)
		toc= gsub('\\{?( *<H\\d.*</H\\d>).*', '\\1', toc)  # drop braces in TOC
		toc= gsub('(.*)\\{', '<br/>\\1', toc)		# drop braces in TOC
		toc= gsub('<a.*$', '', toc) # drop ends in TOC
		toc= gsub('(.* class= *"H.".*)', '<br/>\\1', toc) # insert new lines     # prr(toc)
		
		toc= gsub('<br/>', '', toc) # insert new lines     # prr(toc)
		
		
		
		css2='<style scoped>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */
				.TOC.fig, .imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:130px}
				.TOC.H1, .TOC.H2, .TOC.H3 {display:block}
				</style> <!-- ================================================================== -->'	
		
		main=s1;  #toc=''; 
		out= c(sk.3[1:bd[1]], css, css2, menu.line, '<div class="Gallery"> <H3>Gallery for  ', rFile, ' </H3>'
				, figs, '\n\n<hr/></div> <div class="TOContents" id="0"> <H2>Table of Contents</H2>', toc, '<hr/></div><br/>
						<!-- pre --><div class="main">', main
				, '</div><!-- /pre --><br>##   The HTML output of ', rFile, ' was created on '
				, DT(),  '; <a href="http://www.mathjax.org/demos/scaling-math/">test MathJax </a>'
				, js1, js.toggle
				, footer)
		
		wl(out, theFile %+% '.6.kn.htm', show=F)
		#if(dirname(theFile) != dout) 
		file.copy(theFile %+% '.6.kn.htm', fp(dout, basename(rFile) %+% '.htm'), overwrite = T) 
		expl(fp(dout, basename(rFile) %+% '.htm'))
		
		invisible(list(s.htm= out, fout.htm= fp(dout, basename(rFile) %+% '.htm')))
	}
	if(0) out= treat.knit.html(theFile)
	
#' R Work Journal
#' .r -> .r.rwd + .r.nu.rwd  -(knit)->  .r.kn.htm + .r.kn.nu.htm  ->  .r.htm (or .r.html with base64 figs)
#en r2rmd2rwj(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
#en r2rmd2rwj(theFile= 'm:/50_HLP/out/HLP_demo/HLP_demo.r')
#en r2rmd2rwj(theFile= 'm:/50_HLP/out/HLP_demo/demo_RWJ.r', base64=T)
rwj= RWorkJournal= r2rmd2rwj= function(theFile= get.theFile(), base64=F, ...) { catf('\nr2rmd2rwj: %s\n', theFile)
	rmd= code2rmd(theFile, ...)
	strr(rmd)
	out= treat.knit.html(rmd$fout, rFile=theFile)
	if(base64) rwjFig2base64(out$fout.htm)
	#strr(out)
}
if(0) r2rmd2rwj(theFile= 'm:/50_HLP/out/HLP_demo/HLP_demo.r', base64=T)
if(0) r2rmd2rwj(theFile= 'm:/50_HLP/out/HLP_demo/demo_RWJ.r', base64=T)


#' Insert base64 instead of src  ( rwj.htm +/img/) -> rwj.html - a singlefile
#en rwjFig2base64(rwj= 'M:/50_HLP/out/HLP_demo/HLP_demo.r.htm')
rwjFig2base64= function(rwj= 'M:/50_HLP/out/HLP_demo/HLP_demo.r.htm') { catf('\nrwjFig2base64:%s\n', rwj)
	#libra(base64)
	s= readLines(rwj)
	gall= grep('div .+(Gall|TOC)', s)
	s2= s[gall[1]:(gall[2]-1)]
	gal= grep('<img', s2)
	srcs= sub('.* src="(.*?)".*', '\\1', s2[gal])
	
	out = tempfile()
	b64= laply(srcs, function(src){ catn('b64.src', src)
				#u = encode(fp(gw(), '..', src),  out, 1e6)   #  	libra(base64)
				u = encode(fp(dirname(rwj),  src),  out, 1e6)   #  	libra(base64)
				u= 'data:image/jpeg;base64,' %+% readLines(out)
			})
	s2gb= laply(1:le(srcs), function(i) sub(' src="(.*?)"', sf(' src="%s"', b64[i]), s2[gal][i]))
	
	s2[gal]= s2gb
	s[gall[1]:(gall[2]-1)]= s2
	js3='<script> $(document).ready(function(){
			$(".fig").each(function(){
			//e=this
			//for(k in e) if(e[k] != null && k.match(/^src$|^id$/)) {cl("e[" + k + "]= " + e[k])}; 
			this.src= $("#tn" + this.id)[0].src
			$("#TOC" + this.id)[0].src= this.src
			})
			})</script>'
	#wl(append(s, js3, grep('</body>',s)[1]-1), fp(dout, "zz.htm"))  # insert before '</head>'
	wl(append(s, js3, grep('</body>',s)[1]-1), rwj %+%  "l")  # insert before '</head>'
	expl(rwj %+%  "l")
}





#ReleaseOut= function(theFile= get.theFile(), vers='', exec=F) {
  ReleaseOut= function(theFile, vers='', exec=F) {
    #stopifnot(basename(gw()) ==  "out" && dirname(gw()) == dirname(theFile))
    d= dirname(theFile)
    stopifnot(file.exists(theFile)  && file.exists(fp(d, 'out')) )
    
              #outRelDir= sf('../out-%s-%s', DT("%Y-%m-%d.%H-%M"), vers)
              outRelDir= sf('%s/out-%s-%s', d, DT("%Y-%m-%d.%H-%M"), vers)
              outFile= sf('%s/%s-%s.htm', gsub( basename(theFile), basename(outRelDir), theFile), basename(theFile), sf('%s%s', DT("%Y-%m-%d.%H-%M"), vers))
	catf('\nreleaseOut: outRelDir= %s, outFile=%s\n', outRelDir, outFile)
	catt('../img', ' --> ', sf('%s/img', outRelDir))
	catt(sf('%s.htm', theFile), ' --> ', outFile)
	
	if(exec){
		dir.create(outRelDir)
		stopifnot(file.copy(theFile, outRelDir))
		stopifnot(file.rename('../img', sf('%s/img', outRelDir)))
		try(file.rename(sf('%s.htm', theFile), outFile), silent=TRUE)
		sw('..', showWarnings=F)
		stopifnot(file.rename('out', gsub('\\.\\.', '.', outRelDir) %+% '/out'))
		sw('out')
	}
}
# ReleaseOut(theFile= 'M:/021_aaaa/021_aaaa.r', vers='.b', exec=T)



#' =  make RWJournals =
#' res<<-  is  produced as a side effect for case of error in cycle!!!
#en RWJournals= MakeRWJournals(root='M:', patt='71_UseR-2013-Tutorial.*59.zz', pattNeg='zExtraPacks|999|scripts|library|lib|fun|Base|code2HTML|86_testShiny', exec=F, show=T, toSave=T, outSuffix='.b.htm')
MakeRWJournals= function(root='.', patt='.*', pattNeg='^$', exec=F, ...) {
	warning('List res<<-  and  .r.htm  files are produced as a side effect for case of error in cycle, if(exec) !!!')
	.res= list(); attr(.res, "par")=list(...)
	if(exec) res <<- .res
	on.exit(invisible(.res))
	for(f in  gre2(patt, pattNeg, dir(root, all.files =T, patt='\\.r$', recursive= T))) {
		catf('%3s. %s\n', le(.res), fp(root, f))
		#if(exec) res[[f]] <<- .res[[f]] <- code2HTMLjQuery(.theFile=fp(root, f), ...)  else .res[[f]]= 1
		if(exec) res[[f]] <<- .res[[f]] <- rwj(.theFile=fp(root, f), ...)  else .res[[f]]= 1
		#try({if(exec) res[[f]] <<- .res[[f]] <- code2HTMLjQuery(.theFile=fp(root, f), ...)  else .res[[f]]= 1}, s=T)
	}
	invisible(.res)
}


#' create RWJ album  - galleries for all RWJ in a folder
#p RWJournals - list of RWJ .html file names 
#' @usage 
#' RWJournals= MakeRWJournals(...)
#' createRWJalbum(RWJournals)
#en createRWJalbum(RWJournals, fout='../all.Fig.35.htm')
#en createRWJalbum(RWJournals.42b, fout='../RAlbum.42b.htm', outSuffix='.htm')
createRWJalbum= function(RWJournals, fout= '../all.Fig.htm', outSuffix= attr(RWJournals, "par")$outSuffix){  # outSuffix='.b.htm', outSuffix='.htm') {
	out= c(RWJournals[[1]]$header
			, sf('<script>
							$(function(){
							$("h3").each( function() {
							var $th = $(this), href =  this.textContent.replace(/.* ([^ ]+) */, "$1")+ "%s";
							$th.wrap("<a href=\'file:///" + href + "\' >")
							});
							$("img.imgGal").each( function() {
							var $th = $(this), href =  this.alt +"%1$s#"  + this.id.replace("tn","");
							$th.wrap("<a href=\'file:///" + href + "\' >")
							});
							$("h3").dblclick(function() {w= window.open(this.textContent.replace(/.* ([^ ]+) */, "$1")+ "%1$s", "", "fullscreen=yes");  w.focus() })
							$("img.imgGal").dblclick(function() {w= window.open(this.alt +"%1$s#" + this.id.replace("tn",""), "","fullscreen=yes");  w.focus() })
							});
							</script>', outSuffix)
			, unlist(llply(RWJournals, function(x)c(' <H3>Gallery <SMALL> for  ', (x$theFile), '</SMALL></H3>'  #basename(x$theFile)
										, gsub('src="', sf(' alt="%s" src="file:///%1$s/../', x$theFile), x$figs)
								)))
			, RWJournals[[1]]$footer)
	writeLines(out, fout)
	expl(fout) ; catf('expl("%s")', tools:::file_path_as_absolute(fout))
}	



#' list RW Journals
ls.RWJ= function(patt='', pattNeg='^$', root='../../') { 
	ff= dir2(sf('(%s).*.r.htm', patt), pattNeg, root, recursive=T); strr(ff); prr(ff)
	fp(root, ff)
}

#' make RWJ.album gathering galeries from RWJs
RWJ.album= function(ff) { catf('\n:\n')
	#out=''
	js= '<script> $(function(){
			$("h3").each( function() {
			var $th = $(this), href =  this.textContent.replace(/.* ([^ ]+) */, "$1")+ ".htm";
			$th.wrap("<a href=\'file:///" + href + "\' >")
			});
			$("img.imgGal").each( function() {
			var $th = $(this), href =  this.alt +"#"  + this.id.replace("tn","");
			$th.wrap("<a href=\'file:///" + href + "\' >")
			});
			$("h3").dblclick(function() {w= window.open(this.textContent.replace(/.* ([^ ]+) */, "$1"), "", "fullscreen=yes");  w.focus() })
			$("img.imgGal").dblclick(function() {w= window.open(this.alt +"#" + this.id.replace("tn",""), "","fullscreen=yes");  w.focus() })
			});
			</script>'
	for(f in ff){catn(f)
		
		s= readLines(f)
		#h1= which(grepl('<head>', s))
		h2= which(grepl('</head>', s))
		hg1= which(grepl('Gallery for', s))
		hg2= which(grepl('>Contents<', s))-1
		
		library(tools)
		if(f==ff[1]) out= c(s[1:h2], js)
		f=file_path_as_absolute(f)
		catn(f)
		
		out= c(out #, ' <H3>Gallery <SMALL> for  ', f, '</SMALL></H3>' 
				,	gsub('src="', sf(' alt="%s" src="file:///%1$s/../', f),  s[hg1 : hg2]), '</div>')
	}
	
	out= c(out, '</br></br></br></br></br></br></br></br></br></br></br></br></br></br></br></br><div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
					<img id="showFig" src="" height="400px" style="left:0px"/><br/>
					<span id="showTxt"></span>
					</div></body></html>')
	
	writeLines(out, '../all.Fig.htm', sep='\n')
	expl(gw() %+% '/../all.Fig.htm')
	
}

if (0) {
	ff= ls.RWJ(patt='TMo|62', pattNeg= 'LTV|Xing|zz|fun|SVN|scri') 
	RWJ.album(ff)
}





### Functions for CreateNewProj

#' sub ... in file, used by CreateProj =
fsub= function(fin="M:/83_ScopeR/AegisCustomDataSourceView.script"
		, fout= gsub('$', '-copy$', fin)
		, fileShow= F, overOut= T
		, ...){
	args <- as.list(substitute(list(...)))[-1L]
	
	catt('fin=', fin, ' fout=', fout)
	if(!overOut && file.exists(fout)){warning(sf('fout %s exists; Do nothing!', fout)); return()}
	
	fi= file(fin, "r")
	s= readLines(fi, warn = F)
	#
	close(fi)
	#	catt(11111)
	#	catt(names(args))
	#	str(args)
	
	for(key in names(args)){catt(key,'->', eval(args[[key]], envir= sys.parent())); s= gsub(key, eval(args[[key]], envir= sys.parent()), s)} 
	
	catt('fsub: Creating OutputFile:', fout)
	#catf('::: s= gsub(\'\', \'\',  s)\n::: writeLines(he(s,9999), con =\'%s\')', fout)
	#print(s[80:100])
	
	#brr()
	
	if(fout != '') {writeLines(head(s,99999), con = fout)
		#if(fileShow)file.edit(fout)
		if(fileShow)browseURL('file://' %+% fout)
	}
	invisible(s)
}

#' sub of multiple patterns in multiple files
#' res<<-  is  produced as a side effect for case of error in cycle!!!
gsubInFiles= function(root='T:', patt='\\.r\\.htm', pattRepl='\\.r\\.htm'
		, pattOutSub='\\.r\\.OO\\.htm', pattNeg='^$', exec=F, ...) {
	#warning('FilesSub produces a side effect if(exec) !!!')
	ff= gre2(patt, pattNeg, dir(root, all.files =T, patt=patt, recursive= T))
	prr(ff)
	for(f in gre2(patt, pattNeg, dir(root, all.files =T, patt=patt, recursive= T))) {
		fa=	tools:::file_path_as_absolute(fp(root,f))
		faOut= gsub(pattRepl, pattOutSub, fa)
		catt(fa, ' --> ', faOut)
		if(exec){
			fsub(fin= fa, fout= faOut, fileShow= T, ...)
			browseURL('file://' %+% fa)
			browseURL('file://' %+% faOut)
		} 
	}
}

if (0) {
	gsubInFiles(root='M:', patt='(95|97).*demo2.*\\.r\\.htm'
			, pattRepl='\\.r\\.htm', pattOutSub='\\.r\\.htm'
			, pattNeg='^$', exec=T, `alex.*zolot.us`= 'azolotovitski@medio.com')
	# m:/71_UseR-2013-Tutorial/out/97_tutorial-demo2.r.html
	# m:/71_UseR-2013-Tutorial/out/97_tutorial-demo2.r.html  -->  m:/71_UseR-2013-Tutorial/out/97_tutorial-demo2.r.OO.html
	browseURL('file://' %+% 'm:/71_UseR-2013-Tutorial/out/97_tutorial-demo2.r.html')
	browseURL('file://' %+% 'M:/71_UseR-2013-Tutorial/out/97_tutorial-demo2.r.OO.html')
	
}


#'   Create New Project from template
#en  CreateNewProj(newProj.name= '01_aaaa', root='m:'); sw('../..')
CreateProj= CreateProject= CreateNewProj= function(newProj.name= 'newProjTemplName'
		, Templ.dir= system.file('newProjTemplName', package ='WorkJournal')  
		, root= gw()  # 'c:/'
		, R2wd= F, overOut=F) {
	sw(sf('%s/%s', root, newProj.name))
	dir.create('in')
	dir.create('out')
	
	gw()
	
	
	for(f in dir(Templ.dir,  patt='newProjTemplName.*|README.*')){
		catt(60, f, sf('%s/%s/%s', root, newProj.name, sub('zz', newProj.name, f)))
		#file.copy(fp(Templ.dir,f), gw())
		catt('f=', f)
		#if(grepl('zz', f)) fsub(fin= f
		if(grepl('newProjTemplName|README', f) & !grepl('doc.?$', f)) {catt('fsub')
			fsub(fin= fp(Templ.dir, f)
					, fout= sub('newProjTemplName', newProj.name, f)
					, fileShow= F, overOut=overOut
					, newProjTemplName= sf('%s', newProj.name)
					, `00-00-00`= DT())
			
		} else 	file.copy(fp(Templ.dir, f), fp(gw(), sub('newProjTemplName', newProj.name, f)), overwrite=overOut)
		
		#file.rename(f, sub('newProjTemplName', newProj.name, f))
		#file.remove(f)
	} 
	
	if(R2wd) { try({
					# http://www.r-bloggers.com/exporting-r-output-to-ms-word-with-r2wd-an-example-session/
					libra(R2wd)
					wdGet(newProj.name %+% '.docx')
					wdTitle(newProj.name)
					wdWrite(sf('Author: Alex Zolot'), T)
					wdWrite(sf('Created  : %s', DT()), T)
					#wdSetFont(fontname="Courier New",fontsize=12,bold=F,italic=F)
					
					
					wdNormal("Chapter 1")
					wdHeading(2, 'Chapter 2')
					wdWrite(sf('Chapter 3'), T)
					
					wdSave(fpa(newProj.name %+% '.docx'))
					wdQuit()
				}, silent=TRUE)}
	expl()	
}

if(0) { #= Jun 23, 2013
	# CreateNewProj(newProj.name= '72_Hack', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Cont', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Tutorial', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Mod2Prod', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= 'newProjTemplName', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	CreateNewProj(newProj.name= '97_tutorial-demo', Templ.dir= 'T:/work/UseR-2013/99_commonR/newProjTemplName', root='T:/work/UseR-2013')
	CreateNewProj(newProj.name= '96_aaa', Templ.dir= 'T:/work/UseR-2013/99_commonR/newProjTemplName', root='T:/work/UseR-2013')
	CreateNewProj(newProj.name= '98_aaa'); sw('../..')
}





if(0){   #== Misc
	theFile= 'R:/work/R-svn-ass/00_commonR/zCodeTools.fun.r'
	listFuncUsage(theFile, stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$')
	listFuncUsage(theFile, stoplist='^(c)$')
	
	gff('saved', theFile)
	gff('sa\\(|===', theFile)

	theFile='M:/50_HLP/out/packages/WorkJournal/inst/rcode/WorkJournal.r'
	theFile='M:/50_HLP/out/packages/WorkJournal/inst/rcode/RWJ.r'
	gff('wl\\(', theFile)
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

