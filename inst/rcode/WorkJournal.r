#' @name RWorkJournal
#' 
#' @title R Work Journal and functions for work with code
#' 
#' @description R Work Journal and Helper Funcs for work with code and file system
#' for a book "Handling Large R Projects"
#' 
#' abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #e - example,  #en - example "\notrun"
#' @author Alex Zolotovitski <alex@@zolot.us>
#' License: GPL-2  

#' @keywords aliases

#' @exportPattern "^[[:alpha:]]+"

#' @import plyr HaLaP 
#' @docType package



#' Created  : 2013-06-24 02:55:55


#=====  Helper Funcs for work with code and file system  =====

# !exists('hee') || !exists('sw') && require(HaLaP) || stop('package HaLaP is required')   # if executed after, overwrites source('zBase')
# !exists('libra') || !exists('sw') || stop('package HaLaP is required')   # if executed after, overwrites source('zBase')


# from HaLaP
prr= function(x, ma='', header=T) {if(header)catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); ns= na(x)
	if(le(x)==0){message('0== le of ', deparse(substitute(x))); return()}
	#for(xx in if(is.null(ns)) 1:le(x) else ns) catt(xx, '=', x[[xx]]); catt('-------------------------\n')} # test with gff ex: prr(cars, 'Cars')
	for(xx in if(is.null(ns)) 1:le(x) else ns) catf('%3s= %s\n', xx,  x[[xx]]); catt('-------------------------\n')}

cn= function(cnn,  sep='[ ,\n\t\\+]+') unlist(strsplit(cnn, sep))

#' wrapper for paste
#e  'a' %+%  1:3
'%+%' = paste0 #function(x, y) paste(x, y, sep= "")



#'w writeLines
wl= function(s=.Last.value, out, show=T, ...) { message(sf('\n\nwl to file: expl("file://%s")\n', out))
	writeLines(s, out, ...)
	if(show) expl(sf("file://%s", normalizePath(out)))
}


getQuoteCommentStatusA= function(s, verb=F) {
	sOut= ss= strsplit(pas(s, '', '\n'), '')[[1]]  # vector of symbols
	
	#brr()
	
	if (verb) catt(pas(ss),'\n-----------------------------------\n')
	state= 'o'  ## out
	s2= s3= ''
	states= c()
	for (i in seq(along = ss)) { s1= ss[i]
		if(state == 'o') {sOut[i]= s1;  s3= ''} else {sOut[i]= ''; s3= s3 %+% s1} 
		if(     s1== '"'  && state == 'o') {s2= '<span class="d">'  ; state='d'; s3= ''}  # use switch ? 
		else if(s1== "'"  && state == 'o') {s2= '<span class="s">'  ; state='s'; s3= ''}
		else if(s1== '`'  && state == 'o') {s2= '<span class="b">'  ; state='b'; s3= ''}
		else if(s1== '#'  && state == 'o') {s2= '<span class="c">'  ; state='#'; s3= ''}
		
		else if(  (s1== '\n' && state == '#')
				|| (s1== '"'  && state == 'd') 
				|| (s1== '`'  && state == 'b') 
				|| (s1== "'"  && state == 's')) {
			state='o'; 
			sOut[i]= s2 %+%  markdownToHTML(text= gsub('[ \t]{2,}', '', s3), stylesheet= '', header= '', fragment.only=T) %+% '</span>'
		}
		#|| (s1== "'"  && state == 's')) {state='o'; sOut[i]= s2 %+%  markdownToHTML(text= gsub('zzzz', '', s3), stylesheet= '', header= '', fragment.only=T) %+% '</span>'}
		
		if (verb) cat('===\n========>', i, s1, state, '||', sOut[i], '\n')                                        
		#catt(s1,state)
		states[i]= state  # redund
		
	}
	if (verb) catt(pas(ss))
	#pas(states)
	#pas(sOut, '', '<br/>')
	sOut= pas(sOut, '', '')
	sOut= x= gsub('\n', '<br/>\n', sOut)
	prr(sOut)
	main= sOut
	
	#main= s
	
	header= c('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"><head><link rel="shortcut icon" href="http://zolot.us/favicon.ico">
					<style>
					H1,  H2,  H3,  H4,  H5, p {display:inline; margin-top:160px;  padding:25px; position:relative;    top:0px;}
					
					</style>
					</head><body>') 
	
	footer=	'<div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
			<img id="showFig" src="" height="399" style="left:0px"/><br/>
			<span id="showTxt"></span>
			</div></body></html>'
	
	out= c(header, '<div class="main">', main, '</div>', footer)	
	
	
	return(out)
}


{ # header.jQuery, footer  for cc()

     js = '	<!-- we need these 2 lines for the case if no Internet during the tutorial to get local js >
					<script src="T:/mathjax-MathJax-v2.2-8-g727332c/mathjax-MathJax-727332c/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
					<script src="T:/work/UseR-2013/lib/jquery-1.10.1.min.js"></script>
					<!--  -->
					

					<!-- script src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script -->
					<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>

					<link rel="stylesheet" type="text/css" href="http://raw.githubusercontent.com/jasonm23/markdown-css-themes/gh-pages/foghorn.css">
					<!-- link rel="stylesheet" type="text/css" href="https://raw.githubusercontent.com/jasonm23/markdown-css-themes/gh-pages/markdown9.css" -->
					
					

					<!--script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.js"></script-->
					

					<!--script type="text/x-mathjax-config">  MathJax.Hub.Config({ tex2jax: { inlineMath: [[" $ "," $ "]] } }) </script>
					<!--script type="text/x-mathjax-config">  MathJax.Hub.Config({ tex2jax: { inlineMath: [[" $","$ "]] } }) </script-->
					
					<!-- script>http://zolot.us/favicon.ico


					$(function(){
					//$(".imgGal").draggable() ; //.parent().resizable();
					$(".dimg").resizable().draggable(); 
					// $(".fig").draggable();  //.resizable();;
					});
					</script -->
					
					


					<script>
					$(function(){
					var d= document
					

					//var imgFold= "<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\'/>"  // "(-)"
					var imgFold= $(".aD:first").html()
					

					$("img.imgGal, img.TOC")
					.mouseover(function() {this.title=this.alt; show(this)} )
					.mouseout(function() {show(0)} )
					.click(function() {goto(this.id.replace("tn",""))})
					.dblclick(function() {ShowImg(this..replace(/\\..*/, ""),  this..replace(/.*?\\./, "")); })
					;

					$(".aD").click(function() { th= $(this)
					th.next("div").toggle();
					//alert(th.html()+ "\\n" + imgFold)
					

					th.html(th.html()== imgFold ? "..." : imgFold)
					})

					$("#aToggleAll").click(function() {th= $(this)
					if( th.text() =="(+)") {
					$("div").show(); th.html(imgFold); $("a:not(.comments)").html(imgFold)   
					} else { $("div").hide(); th.html("(+)"); $("a:not(.comments)").html("(+)")}
					})

					//   $(".aToggleAllFig").click(function() {$("img").toggle() })
					

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
					


					$(".aToggle.DD1").click(function() {toggleD($(this), $(".H1 + + .aD"), $(".D1")) })
					$(".aToggle.DD2").click(function() {toggleD($(this), $(".H2 + + .aD"), $(".D2")) })
					$(".aToggle.DD3").click(function() {toggleD($(this), $(".H3 + + .aD"), $(".D3")) })
					$(".aToggle.DD4").click(function() {toggleD($(this), $(".H4 + + .aD"), $(".D4")) })
					$(".aToggle.TOC").click(function() {toggleD($(this), $(".zz"), $("div.TOC")) })
					$(".aToggle.Gallery").click(function() {toggleD($(this), $(".zz"), $("div.Gallery")) })
					$(".aToggleAllFig").click(function() {toggleD($(this), $(".zz"), $("img")) })
					$(".aToggleComments").click(function() {
					ToggleComments2(); return;
					

					c2= $(".comment2");	fs= c2.eq(0).css("font-size");
					var tt = document.getElementsByClassName("comment2"); var t0= tt[0].style.fontSize;
					//alert(fs + " | " + tt[0].innerHTML + " zz " + t0)
					//alert(fs + " | " + t0.innerHTML)
					//fsn= (fs == "3px" || fs == "") ? "12px" : ((fs == "12px")? "1px" : "3px");
					fsn= (fs == "30px" || fs == "") ? "120px" : ((fs == "120px")? "10px" : "30px");
					//alert(fs + " | " + tt[0].innerHTML + " t0: " + t0 + " fsn:" + fsn+ "|"+ " sp:" + $(".comment2:first + span").text())
					$("#out").text(" || fs:" +fs + " | " + tt[0].innerHTML + " t0: " + t0 + " fsn:" + fsn+ "|"+ " sp:" + $(".comment2:first + span").text());
					$(".comment2").css({"font-size": fsn, "font-size-adjust": .1});
					//$(".comment2").css("font-size", "xx-small");
					$(".comment2").text("zz");
					$(".comment2").css("color",  c2.css("font-size")== "1px" ? "white" : "green")
					})

					});
					</script>
					

					<script><!--
					var d= document;

					function goto(i) {w= d.location= "#" + i;}

					function ShowSVG(p){w= window.open("","","fullscreen=yes")
					w.document.write("<html><iframe src=\'./img/"  + p + ".svg\' width=1200  height=800/></html>")
					w.focus()
					}


					function ShowImg(p, capt){w= window.open("","","type=fullWindow,fullscreen,location=\'\',height="+ screen.height+ ",width="+screen.width)
					w.document.write("<html><img src=\'./img/"  + p + ".png \' /><br/>" + p +  capt + "</html>")
					w.document.title= p + capt;
					w.focus()
					}


					function resize(i) {e=d.getElementById(i); w= e.width;
					e.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? "100%" : 700;
					}


					function ToggleFold(d) {}


					function show(e){
					$("#show").css({display : (e==0)? "none" : "block", border: (e==0)? "0" : "2px solid blue"})

					if(e != 0){
					x= Math.min(e.offsetLeft+ e.width + 10, 500) //x= e.x+ e.width + 10;// ; if(x > 500) x= 500;
					y= e.offsetTop+ e.height + 10               //y= e.y+ e.height + 10
					

					$("#show").css({left: x, top: y})
					$("#showFig").attr("src", e.src)
					$("#showTxt").html("<br/>" +  e.)  //  +  e.alt)
					

					/* show properties of e 
					a=""
					for(k in e) if(1 || e[k] > 10 && e[k]+" " < "a") a= a + "      e[" + k + "]= " + e[k];
					alert (a);
					/**/
					
					};
					}


					//--></script>'
					
	css='	<style scoped>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */
					body {max-width: 95%; font-size: 100%; line-height: 105%;}
					div.Gallery {background-color:rgb(255,248,248); }	
					div.TOC {background-color:rgb(248,248,255); }	
					div.main, .r {font-family: monospace; white-space: pre; max-width: 1000px}
					p{margin-bottom:2%; margin-top:2%;margin-before: 2%; margin-after: 2%;}
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
					<a href="javascript:linutoggle()" >Toggle</a> Line Numbers <span id="out"></span>'
			
			header.jQuery= c('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
							  <head><link rel="shortcut icon" href="">'
							, js, css, '</head><body>')
			
			
			footer=	'<div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
					<span id="showTxt"></span><br/>
					<img id="showFig" src="#" height="401" style="left:0px" alt=""/>
					</div>\n</body>\n</html>'
			
}


#'  get global variable or option "theFile"
#en get.theFile() 
get.theFile= function() {
	tf= if(exists('theFile', envir= .GlobalEnv)) {get('theFile', envir= .GlobalEnv)
			} else getOption('theFile')
	is.null(tf) && stop('RWJ: Set  options(theFile= "full path to the file")')
	tf
}


ccm= code2HTML.md=  function(.theFile= get.theFile(), img='img', FullSyntaxHighlight= FALSE
		, classicHeaders=FALSE, show=TRUE, toSave=TRUE, outSuffix='.htm') {
	s1= readLines(.theFile, warn=F)
	libra(markdown)  # http://daringfireball.net/projects/markdown/basics
	
	{

	s1='## Test1
			
			plot(1:9)  # **the plot**
			"
			* list1
			* list2
			* list3
			
			1. nList1
			2. nList2
			3. nList3
			
			* list4
			
			## Header2 
			"
			
			### Another Header2 
			# Markdown-style *emphasis* inside
			'
	out= getQuoteCommentStatusA(s= s1, verb=T)
	# out
	# [1] "#<span class=\"c\"><!DOCTYPE html>\n<!-- saved from url=(0014)about:internet -->\n<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n<title>Test1</title>\n\n\n\n\n\n<style type=\"text/css\">\nbody, td {\n   font-family: sans-serif;\n   background-color: white;\n   font-size: 12px;\n   margin: 8px;\n}\n\ntt, code, pre {\n   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;\n}\n\nh1 { \n   font-size:2.2em; \n}\n\nh2 { \n   font-size:1.8em; \n}\n\nh3 { \n   font-size:1.4em; \n}\n\nh4 { \n   font-size:1.0em; \n}\n\nh5 { \n   font-size:0.9em; \n}\n\nh6 { \n   font-size:0.8em; \n}\n\na:visited {\n   color: rgb(50%, 0%, 50%);\n}\n\npre {\t\n   margin-top: 0;\n   max-width: 95%;\n   border: 1px solid #ccc;\n   white-space: pre-wrap;\n}\n\npre code {\n   display: block; padding: 0.5em;\n}\n\ncode.r, code.cpp {\n   background-color: #F8F8F8;\n}\n\ntable, td, th {\n  border: none;\n}\n\nblockquote {\n   color:#666666;\n   margin:0;\n   padding-left: 1em;\n   border-left: 0.5em #EEE solid;\n}\n\nhr {\n   height: 0px;\n   border-bottom: none;\n   border-top-width: thin;\n   border-top-style: dotted;\n   border-top-color: #999999;\n}\n\n@media print {\n   * { \n      background: transparent !important; \n      color: black !important; \n      filter:none !important; \n      -ms-filter: none !important; \n   }\n\n   body { \n      font-size:12pt; \n      max-width:100%; \n   }\n       \n   a, a:visited { \n      text-decoration: underline; \n   }\n\n   hr { \n      visibility: hidden;\n      page-break-before: always;\n   }\n\n   pre, blockquote { \n      padding-right: 1em; \n      page-break-inside: avoid; \n   }\n\n   tr, img { \n      page-break-inside: avoid; \n   }\n\n   img { \n      max-width: 100% !important; \n   }\n\n   @page :left { \n      margin: 15mm 20mm 15mm 10mm; \n   }\n     \n   @page :right { \n      margin: 15mm 10mm 15mm 20mm; \n   }\n\n   p, h2, h3 { \n      orphans: 3; widows: 3; \n   }\n\n   h2, h3 { \n      page-break-after: avoid; \n   }\n}\n</style>\n\n\n\n</head>\n\n<body>\n<h1>Test1</h1>\n\n</body>\n\n</html>\n</span><span class=\"c\">\t<span class=\"c\">\t<span class=\"c\">p<span class=\"c\">l<span class=\"c\">o<span class=\"c\">t<span class=\"c\">(<span class=\"c\">1<span class=\"c\">:<span class=\"c\">9<span class=\"c\">)<span class=\"c\">\n<span class=\"c\">\t<span class=\"c\">\t<span class=\"c\">#<span class=\"c\"><!DOCTYPE html>\n<!-- saved from url=(0014)about:internet -->\n<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n<title>Header2</title>\n\n\n\n\n\n<style type=\"text/css\">\nbody, td {\n   font-family: sans-serif;\n   background-color: white;\n   font-size: 12px;\n   margin: 8px;\n}\n\ntt, code, pre {\n   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;\n}\n\nh1 { \n   font-size:2.2em; \n}\n\nh2 { \n   font-size:1.8em; \n}\n\nh3 { \n   font-size:1.4em; \n}\n\nh4 { \n   font-size:1.0em; \n}\n\nh5 { \n   font-size:0.9em; \n}\n\nh6 { \n   font-size:0.8em; \n}\n\na:visited {\n   color: rgb(50%, 0%, 50%);\n}\n\npre {\t\n   margin-top: 0;\n   max-width: 95%;\n   border: 1px solid #ccc;\n   white-space: pre-wrap;\n}\n\npre code {\n   display: block; padding: 0.5em;\n}\n\ncode.r, code.cpp {\n   background-color: #F8F8F8;\n}\n\ntable, td, th {\n  border: none;\n}\n\nblockquote {\n   color:#666666;\n   margin:0;\n   padding-left: 1em;\n   border-left: 0.5em #EEE solid;\n}\n\nhr {\n   height: 0px;\n   border-bottom: none;\n   border-top-width: thin;\n   border-top-style: dotted;\n   border-top-color: #999999;\n}\n\n@media print {\n   * { \n      background: transparent !important; \n      color: black !important; \n      filter:none !important; \n      -ms-filter: none !important; \n   }\n\n   body { \n      font-size:12pt; \n      max-width:100%; \n   }\n       \n   a, a:visited { \n      text-decoration: underline; \n   }\n\n   hr { \n      visibility: hidden;\n      page-break-before: always;\n   }\n\n   pre, blockquote { \n      padding-right: 1em; \n      page-break-inside: avoid; \n   }\n\n   tr, img { \n      page-break-inside: avoid; \n   }\n\n   img { \n      max-width: 100% !important; \n   }\n\n   @page :left { \n      margin: 15mm 20mm 15mm 10mm; \n   }\n     \n   @page :right { \n      margin: 15mm 10mm 15mm 20mm; \n   }\n\n   p, h2, h3 { \n      orphans: 3; widows: 3; \n   }\n\n   h2, h3 { \n      page-break-after: avoid; \n   }\n}\n</style>\n\n\n\n</head>\n\n<body>\n<h2>Header2</h2>\n\n</body>\n\n</html>\n</span><span class=\"c\">\t<span class=\"c\">\t<span class=\"c\">#<span class=\"c\"><!DOCTYPE html>\n<!-- saved from url=(0014)about:internet -->\n<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n<title></title>\n\n\n\n\n\n<style type=\"text/css\">\nbody, td {\n   font-family: sans-serif;\n   background-color: white;\n   font-size: 12px;\n   margin: 8px;\n}\n\ntt, code, pre {\n   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;\n}\n\nh1 { \n   font-size:2.2em; \n}\n\nh2 { \n   font-size:1.8em; \n}\n\nh3 { \n   font-size:1.4em; \n}\n\nh4 { \n   font-size:1.0em; \n}\n\nh5 { \n   font-size:0.9em; \n}\n\nh6 { \n   font-size:0.8em; \n}\n\na:visited {\n   color: rgb(50%, 0%, 50%);\n}\n\npre {\t\n   margin-top: 0;\n   max-width: 95%;\n   border: 1px solid #ccc;\n   white-space: pre-wrap;\n}\n\npre code {\n   display: block; padding: 0.5em;\n}\n\ncode.r, code.cpp {\n   background-color: #F8F8F8;\n}\n\ntable, td, th {\n  border: none;\n}\n\nblockquote {\n   color:#666666;\n   margin:0;\n   padding-left: 1em;\n   border-left: 0.5em #EEE solid;\n}\n\nhr {\n   height: 0px;\n   border-bottom: none;\n   border-top-width: thin;\n   border-top-style: dotted;\n   border-top-color: #999999;\n}\n\n@media print {\n   * { \n      background: transparent !important; \n      color: black !important; \n      filter:none !important; \n      -ms-filter: none !important; \n   }\n\n   body { \n      font-size:12pt; \n      max-width:100%; \n   }\n       \n   a, a:visited { \n      text-decoration: underline; \n   }\n\n   hr { \n      visibility: hidden;\n      page-break-before: always;\n   }\n\n   pre, blockquote { \n      padding-right: 1em; \n      page-break-inside: avoid; \n   }\n\n   tr, img { \n      page-break-inside: avoid; \n   }\n\n   img { \n      max-width: 100% !important; \n   }\n\n   @page :left { \n      margin: 15mm 20mm 15mm 10mm; \n   }\n     \n   @page :right { \n      margin: 15mm 10mm 15mm 20mm; \n   }\n\n   p, h2, h3 { \n      orphans: 3; widows: 3; \n   }\n\n   h2, h3 { \n      page-break-after: avoid; \n   }\n}\n</style>\n\n\n\n</head>\n\n<body>\n<p>Markdown-style <em>emphasis</em> inside</p>\n\n</body>\n\n</html>\n</span><span class=\"c\">\t"
		
	#writeLines(out, .theFile %+% outSuffix)
	writeLines(out, 'tmp.htm')
	expl('tmp.htm')
		
	}
	gw()
	# gw: sw("m:/71_UseR-2013-Tutorial/out");  expl()
	
	
}
#ccm()

cc= code2HTML= code2HTMLjQuery= function(
		     .theFile= get.theFile(), img='img', FullSyntaxHighlight= FALSE
			 , classicHeaders=TRUE, show=TRUE, toSave=TRUE, outSuffix='.htm', wchunks=F) {
	if(FullSyntaxHighlight){   #== Full syntax highlight ==
		catt('FullSyntaxHighlight')
		libra(highlight)
		s1= highlight(.theFile, NULL, renderer = renderer_html( document = TRUE ))
		s1= gsub('\n$','',s1)
		writeLines(s1, .theFile %+% '.FSH.htm')
	}else{s1= readLines(.theFile, warn=F)
	}  #--
	
	
	if (wchunks) {
		#debug(parse.terminal.line.quotes2); undebug(parse.terminal.line.quotes2)
		u= parse.terminal.line.quotes2(s=s1, verb=F, exec=T)
		strr(u)
		# return(list(lines=u, chunks= chunks, s.md= s3, s.htm= s4, s.htm0= s5))
		s2= u$s0.htm
		s2= unlist(strsplit(s2, '\n'))
		#brr()
		strr(s1)
		strr(s2)
		s1= s2
	}
	
	figss= list()
 	s1= gsub('<(\\s)', '&lt;\\1', s1)  # we suppose no blanks after "<" in <tag    in the input R code
	s1= replaceTagsOutSq(s1)  # we suppose  <tag>  only in `` in the  R code
	s1= gsub('@@', '\\\\\\\\', s1)  # drop escapes for LaTeX
	s1= gsub('(^|[^x"])@', '\\1\\\\', s1)  # drop escapes for LaTeX
	
	#brr()
	
	#==  Set id  ==
	for(i in 1:le(s1)){
		if(grepl('^\\s*#\\s* (Pic|Fig)_\\d+', s1[i])) figss[[ch(i)]]= s1[i]
		
		s1[i]= gsub('^([^\\~]*# *"?)(jFig_\\d+)(.*)','\n\n <iframe src="img/\\2.htm" width="100%" height="600px"></iframe>\n\n \\1\\2\\3' , s1[i])
		

		s1[i]= gsub('^([^\\~]*# *"?)(Pic_\\d+)(.*)', sf('
								<br/><img id="%s" class="fig" src="%s/\\2.png" width=700 onClick="resize(%1$s);" ondblclick="ShowImg(\'\\2\', \'\\3\');" ="\\2\\3"/>   
								<br/><span class="capt" onClick="goto(\'tnTOC%1$s\');" ondblclick="ShowImg(\'\\2\', \'\\3\')">\\1\\2\\3</span><br/>',  i, img), s1[i])
		s1[i]= gsub('^([^\\~]*# *"?)(Fig_\\d+)(.*)', sf('
								<br/><img id="%s" class="fig" src="%s/\\2.png" width=700 onClick="resize(%1$s);" ondblclick="ShowImg(\'\\2\', \'\\3\');" ="\\2\\3"/>   
								<br/><span class="capt" onClick="goto(\'tnTOC%1$s\');" ondblclick="ShowImg(\'\\2\', \'\\3\')">\\1\\2\\3</span><br/>',  i, img), s1[i])
		#		s1[i]= gsub('(.*\\s*#==== )(.*)( =+.*)$', sf('</pre>\\1<span class="H5" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#=== )(.*)( =+.*)$',  sf('</pre>\\1<span class="H4" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#== )(.*)( =+.*)$' ,  sf('</pre>\\1<span class="H3" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#= )(.*)( =+.*)$'  ,  sf('</pre>\\1<span class="H2" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#=+)([^=]*)$'      ,  sf('</pre>\\1<span class="H1" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		
		s1[i]= gsub('(.*\\s*#===== )(.*)( =+.*)$', sf('</pre>\\1<span class="H5" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#==== )(.*)( =+.*)$',  sf('</pre>\\1<span class="H4" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#=== )(.*)( =+.*)$' ,  sf('</pre>\\1<span class="H3" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#== )(.*)( =+.*)$'  ,  sf('</pre>\\1<span class="H2" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#= )(.*)( =+.*)?$'  ,  sf('</pre>\\1<span class="H1" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
		
		s1[i]= gsub('(^[^#A-a0-9\\.\\,]*###### )(.*)', sf('</pre><span class="comment2">\\1</span><span class="H5" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
		s1[i]= gsub('(^[^#A-a0-9\\.\\,]*##### )(.*)' , sf('</pre><span class="comment2">\\1</span><span class="H4" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
		s1[i]= gsub('(^[^#A-a0-9\\.\\,]*#### )(.*)'  , sf('</pre><span class="comment2">\\1</span><span class="H3" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
		s1[i]= gsub('(^[^#A-a0-9\\.\\,]*### )(.*)'   , sf('</pre><span class="comment2">\\1</span><span class="H2" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
		s1[i]= gsub('(^[^#A-a0-9\\.\\,]*## )(.*)'    , sf('</pre><span class="comment2">\\1</span><span class="H1" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
		
		s1[i]= gsub('id="(\\d+)" ', 'id="\\1" title="line \\1" ',  s1[i])
	}	
	
	
	if(!classicHeaders) for(i in 1:5) {s1= gsub(sf('class="H%s"', i),  sf('class= "H%s"', 6-i), s1)} 
	
	#= div for code folding ===
	depth= countDepth2(s1)
	imgFold= '<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\'/>'
	
	s1= ifelse(depth!= 0, gsub('^([^#]*\\{[^\\{\\}]*)', sf('\\1 <a href="javascript:ToggleFold(\'D77\')" class="aD">%s</a><div class="D77">', imgFold), s1), s1)

	s1= gsub('^(.* class="H(\\d)".* id="(\\d+).*)D77(.*) class="D77"' , '\\1D\\3\\4 class="D\\2" id="D\\3"', s1)
	s1= ifelse(depth!= 0, gsub('([^#\\{\\}]*)\\}', '\\1</div><b>}</b></pre><pre>', s1), s1)
	
	attr(s1,".theFile")=  .theFile
	s1= subSingleQuote2div(quote='^[^\\`]*`[^\\`]*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<span class="sq">', '</span>'), s1)  # starts from `, single ` in line
	s1= gsub(" \\$ (.+) \\$ ",' &nbsp;&nbsp; $ \\1 $ &nbsp;&nbsp; ', s1)    #  inline math formula
	
	
	#==  comments, TODO and xxx  ==
	s1= gsub("^(\\s*#\')(.*)$",'<span class="comment2">\\1</span><span class="text">\\2</span>', s1)  
	s1= gsub('(#[^\'=-].*?)(<|$)','<span class="comment">\\1</span>\\2', s1)  # code2HTML3()
	s1= gsub('^(.*)(#=+)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML3()
	s1= gsub('^(.*)(#\\-\\-)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML()
	s1= gsub('^(.*# ?.*)((xxx|TODO):.*)(</span.*)$','\\1<font color="red">\\2</font>\\4', s1)
	
	main= gsub('^([^#]+= ?function)(.*)$','<span class="fun">\\1</span>\\2', s1)  
	
	#==  prepare Table of Contents  ==	
	toc= grep('img id=|<H\\d+|"H[1-5]', s1, value=TRUE)
	toc= gsub('"capt"', '"captTOC"', toc)
	toc= gsub('class="fig"', 'class="tnTOC"', toc)
	toc= gsub('700', '400', toc)  		# figs -> thumbnails
	toc= gsub('resize', 'goto', toc)
	toc= gsub('id="(\\d+)"', 'id="tnTOC\\1"', toc)
	toc= gsub("goto\\('tn(\\d+)'\\)", "goto('\\1')", toc)
	toc= gsub('\\{?( *<H\\d.*</H\\d>).*', '\\1', toc)  # drop braces in TOC
	toc= gsub('(.*)\\{', '<br/>\\1', toc)		# drop braces in TOC
	toc= gsub('<a.*$', '', toc) # drop ends in TOC
	toc= gsub('(.* class= *"H.".*)', '<br/>\\1', toc) # insert new lines
	
	
	#==  prepare Gallery  ==
	figs= laply(na(figss), function(np){p=  figss[[np]]; sf('<img id="tn%s" class="imgGal" src="img/%s.png" ="%s" height=100> '
						, np, gsub('.* ((Pic|Fig)_[0-9]+).*', '\\1', p)
						, gsub('# *', '', p))})

	#	    header.jQuery=....
	#
	#		footer=	'<div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
	#				<img id="showFig" src="" height="400px" style="left:0px"/><br/>
	#				<span id="showTxt"></span>
	#				</div></body></html>'
	#
	#		

		out= c(header.jQuery, menu.line, '<div class="TOC" id="0"> <H3>Gallery for  ', .theFile, ' </H3>'
			, figs, '<hr/><H2>Contents</H2>', toc, '<hr/></div> <p/><br/><br/>
			   <!-- pre --><div class="main">', main
			, '</div><!-- /pre --><br>##   The HTML output of ', .theFile, ' was created on ', DT(), '; <a href="http://www.mathjax.org/demos/scaling-math/">test MathJax </a>'
			, footer)

		if(toSave){
			catn(.theFile, outSuffix, .theFile %+% outSuffix)  # xxx del
			writeLines(out, .theFile %+% outSuffix)
			catf('expl("file://%s")', .theFile %+% outSuffix)
			if(show) expl("file://" %+% .theFile %+% outSuffix)
		}
	invisible(list(theFile=.theFile, header=header.jQuery, figs=figs, toc=toc, main=main, footer=footer, out=out))
}
#  cc()



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


ReleaseOut= function(vers='', exec=F) {
	stopifnot(basename(gw()) ==  "out" && dirname(gw()) == dirname(theFile))
	
	outRelDir= sf('../out-%s%s', DT("%Y-%m-%d.%H-%M"), vers)
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
# ReleaseOut(vers='.b', exec=T)



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
		if(exec) res[[f]] <<- .res[[f]] <- code2HTMLjQuery(.theFile=fp(root, f), ...)  else .res[[f]]= 1
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


#{ #==  lists of funcs  ==
	#' list of all functions in memory  ==
	allFunInMem= function() sus(ldply(ls(envir= .GlobalEnv), function(x)df(x, cl=class(get(x)))), cl=='function')
	# hee(allFunInMem())
	# 209  rows
	#                   x       cl
	# 1               %-% function
	# 2               %+% function
	# 8               aaa function
	# 9                ab function
	# 10               ad function
	# 11 AllCategFreqDesc function
	# 12      allFunInMem function
	# 13              atl function
	# 14               BA function
	# # he(sus(allFunInMem(), , sel= cn("x cl")), 5)
	
	
	#e countPatt(s= c('{ {b}ab', 'n s{}', 'n abbab s'), patt='.b')
	# [1] 2 0 2	
	countPatt= function(s= c('{ {b}', 'n s{}', 'n abbab s'), patt='ab'){
		s= gsub( patt, '@', s)
		#catt(s)
		s= gsub('[^@]' , '', s)
		nchar(s)
	} 

	
	#llply(regexec('.b', s<- c('{ {b}ab', 'n s{}', 'n abbcb s')),function(x) {if(x[1]>0) substr(x, x[1], x[1]+attr(x,"match.length"))})

	# gregexpr('.b', c('{ {b}ab', 'n s{}', 'n abbcb s'))
	#e ListPatt(s= c('{ {b}ab', 'n s{}', 'n abbcb s'), patt='.b')
	#    x Freq
	# 1 {b    1
	# 2 ab    2
	# 3 cb    1
	ListPatt= function(s= c('{ {b}ab', 'n s{}', 'n abbab s'), patt='.b'){
		s= gsub( sf('.*?(%s).*?', patt), '#\\1#', s)  #print(s)
		s= grep(patt, unlist(strsplit(s, '#')), value=TRUE)
		x= tab.df(s)
		x
	} 
	
	
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

#' really it used for backticks rather than single quotes : 
subSingleQuote2div= function(quote='^\\s*`\\s*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<div class="sq">', '</div>'), s1= readLines(theFile, warn=F)){
	x= grepl(patt=quote, s1) & !grepl(patt=pattNeg, s1)
	if(sum(x) %% 2 != 0) warning(sf('odd quotes in %s: %s', attr(s1,"theFile"), pas(which(x))))
	
	s1[x]= repl %+%  s1[x]  # %+% repl
	s1
}


#' set tags for single, double quotes, backticks and comments
#e s2= getQuoteCommentStatus(s= c('1abs#bb\n2#a"cc"\n3aa"bb#cc\n4`aa"b`b', '5aa"bb#cc\n6a\'aa#"bb')); prr(s2)
getQuoteCommentStatus= function(s, verb=F) {
	sOut= ss= strsplit(pas(s,'', '\n'), '')[[1]]
	
	if (verb) catt(pas(ss))
	state= 'o'  ## out
	states= c()
	for (i in seq(along = ss)) { s1= ss[i]
		if(     s1== '"'  && state == 'o') {s2= '<d>'  ; state='d'}  # use switch ? 
		else if(s1== "'"  && state == 'o') {s2= '<s>'  ; state='s'}
		else if(s1== '`'  && state == 'o') {s2= '<b>'  ; state='b'}
		else if(s1== '#'  && state == 'o') {s2= '<c>'  ; state='#'}
		else if(s1== '\n' && state == '#') {s2= '</c>' ; state='o'}
		else if(s1== '"'  && state == 'd') {s2= '</d>' ; state='o'}
		else if(s1== '`'  && state == 'b') {s2= '</b>' ; state='o'}
		else if(s1== "'"  && state == 's') {s2= '</s>' ; state='o'}
		else s2= ''
		sOut[i]= s2 %+% s1
		
		if (verb) cat(sOut[i])                                        
		#catt(s1,state)
		states[i]= state  # redund
		
	}
	if (verb) catt(pas(s1))
	#pas(states)
	pas(sOut, '', '')
}



# get status for single, double quotes, backticks - single symbol on the line
# s2= getQuoteCommentStatus(s= c('1abs#bb\n2#a"cc"\n3aa"bb#cc\n4`aa"b`b', '5aa"bb#cc\n6a\'aa#"bb')); prr(s2)
if (0) {
	s= readLines(textConnection(' x=1
							
							"bb **cc** *dd*

## bhbhbh

							"
							y=0
							z=3
							"
							# aa
							# bb"
					"abc"
							
							'))
}

ccc= parse.single.line.quotes= function(file= get.theFile(), s=readLines(file), verb=F, exec=T) {
		#sing.quo= gregexpr('^\\s*([\'"`])\\s*$', s)
		sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
		if (0) {
			state02=ifelse(sing.quo==1, substr(s, attr(sing.quo,"match.length"), attr(sing.quo,"match.length")), '')
			state.text= (sing.quo==1)
			state.text= 0+ grepl('^\\s*([\'"`])\\s*$', s)
			state.text= cumsum(sing.quo==1)%%2
			
			s= gsub('\t','    ', s)
			ident= regexpr('\\S', s) - 1 
			
			
			s3= s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), ''); prr(s3)
			
			s3=c( '<html>\n<pre><code>\n', s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), ''), '</code></pre></html>\n\n')
			s3=c( '\n<pre><code>\n', s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), '')
					, '</code></pre>\n\n')
			
			gw()
			htm= markdownToHTML(text= s3, fragment.only=F)  ; prr(htm)
			markdownToHTML(output='zz.htm', text= s3, fragment.only=F)
			cat(htm, file='zz.htm')
			expl('zz.htm')
			file.remove('zz.htm')		
		}
		s= gsub('\t','    ', s)

		ichunk= cumsum(grepl('^\\s*([\'"`])\\s*$', s))
		u= df(sing.quo, ichunk, style=ifelse(ichunk %%2, 'md', 'r')
		   , ident= regexpr('\\S', s) - 1
		   , ident1= attr(sing.quo,"match.length")-1, s)
		

		s2= split(s, ichunk)
		u2= split(u, ichunk)
	
		s4= list()
		for(i in 1:le(s2)){ # i=3
			if(i%%2==1){#  R code
				#s4[[i]]= c('\n<pre><code>\n', s2[[i]]) # r code
				s4[[i]]= markdownToHTML(text= c('', '    ' %+% s2[[i]], ''),  fragment.only=F) # r code
			} else{ 
				minide=max(0, min(u2[[i]]$ident))
				txt= substr(s2[[i]][-1], minide+1, 999)
				ides= pas(rep(' ', minide))
				htm= ides %+% markdownToHTML(text= txt, fragment.only=T)
				s4[[i]]= c(s2[[i]][1], '\n</code></pre>\n\n', htm) 
			}
		}
		
		#s4= markdownToHTML(text= s %+% '\n', fragment.only=F) #xxx !!!
		s5= c('<html>', unlist(s4) %+%  '\n', '</html>'); if( verb) prr(s5)
		if (exec) {
			cat(s5, file='zz.htm')
			expl('zz.htm')
			#file.remove('zz.htm')
		}
		attr(s5, 'detailes')= u
		invisible(s5)
}
# u= parse.single.line.quotes(file= get.theFile(), verb=F, exec=T)
# gw()


#' split code to chunks by single first or last quote: ' ' `
ccc2= parse.terminal.line.quotes2= function(file= get.theFile(), s= readLines(file), verb=F, exec=T) {
	#s= readLines('m:/50_HLP/out/HLP_demo/HLP_demo.r', warn =0)
	
	#brr()
	
	#sing.quo= gregexpr('^\\s*([\'"`])\\s*$', s)
	s= gsub('\t','    ', s)  # prr(s)
	sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
	#q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1\\S', s, v=F)  # line start quote
	#q2= gre2('[\'"`]\\s*$', '\\S([\'"`]).*\\1\\s*$', s, v=F)  # line end quote
	q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1', s, v=F)  # line start quote
	#q2= gre2('[\'"`]\\s*$', '([\'"`]).*\\1\\s*$', s, v=F)  # line end quote
	q2= gre2('[\'"`]\\s*$', '\\1.*([\'"`])\\s*$', s, v=F)  # line end quote
	if (0) {
		s='#  == Code in top and bottom of the R file.  - "cache", "parking lot"'
		gre2('[\'"`]\\s*$', '([\'"`]).*\\1\\s*$', u$lines$s[180], v=F)
		gre2('[\'"`]\\s*$', '\\1.*([\'"`])\\s*$', u$lines$s[180], v=F)
		grepl('[\'"`]\\s*$', u$lines$s[180])
		grepl('([\'"`]).*\\1\\s*$', u$lines$s[180])
		regexpr('([\'"`]).*\\1\\s*$', u$lines$s[180])
		regexpr('([\'"`])', u$lines$s[180])
		regexpr('([\'"`])\\s*$', u$lines$s[180])
		s[q1]
		prr(s[q2])
		
	}
	ich= cumsum(q2 | q1)
	iich= ich - ifelse(ich%%2, 0, c(0, diff(ich)))  # chunk index, from 0; 0 is.code
	text= ifelse(q1, sub('^\\s*[\'"`]', '', s), s)
	text= ifelse(q2, sub('[\'"`]\\s*$', '', text), text)  # prr(text)
	
	
	u= df(line=1:le(s), s, iich= ich - ifelse(ich%%2, 0, c(0, diff(ich))), ich, is.code=1- iich%%2, text,q1,q2, stringsAsFactors = F)
	chunks= dlply(u, .(iich, is.code), I)  ; strr(chunks)
	
	require(markdown)
#	sh= unlist(sapply(chunks, function(cha){if(cha$is.code[1])c("\n<pre><code>", cha$text, "</code></pre>\n" ) 
#						else markdownToHTML(text=cha$text, fragment.only=T)}))
	
	sh= unname(unlist(sapply(chunks, function(chu){if(chu$is.code[1])c(sf('\n<pre><code chu="%s" line="%s">', chu$iich[1], chu$line[1])
								, chu$text, sf('</code chu="%s" line="%s"></pre>\n', chu$iich[1], last(chu$line)) ) 
								else chu$text}))) ; strr(sh) # prr(sh)
	s3a=sh # %+%  '\n'   ; strr(s3a)# prr(s3)	
	s3= gsub('^ *#', '#', s3a)
	#s4= markdownToHTML(text= s3, fragment.only=F, options = "highlight_code")  
	
	strr(s3)
	
	s4= markdownToHTML(text= s3, fragment.only=F, options = cn("mathjax highlight_code"))  # toc 
	strr(s4)
	s5= markdownToHTML(text= s3, fragment.only=T, options = cn("mathjax highlight_code"))  
	
	strr(s5)
	tf= fp(tempdir(), 's.htm'); cat(s4, file=tf); expl(tf)
	tf0= fp(tempdir(), 's0.htm'); cat(s5, file=tf0); expl(tf0)
	return(list(lines=u, chunks= chunks, s.md= s3, s.htm= s4, s0.htm= s5, tf=tf, tf0=tf0))
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''	
	
	if (0) {
		#df(start.q, end.q, ich=cumsum(start.q + end.q), s)
		#s2= ifelse(start.q, "</code>",'') %+% s %+% ifelse(end.q, "<code>",'')
		#s2= ifelse(start.q, "```\n",'') %+% ifelse(ich%%2, '', '    ') %+%  s %+% ifelse(end.q & !start.q, "\n```",'')
		#s2= ifelse(start.q, "`\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(end.q & !start.q, "\n`",'')
		s2= ifelse(q1, "</code></pre>\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(q2 & !q1, "\n<pre><code>", '') #c('a','b') %+% 1:2
		#s2= ifelse(ich%%2, '', '\t') %+%  s 
		#s3= c("`\n", s2, "`\n\n") %+% '\n'
		s3= c("<pre><code>\n", s2, "</code></pre>\n\n") %+% '\n'
		#s3= s2 #c("```\n", s2, "\n```\n") %+% '\n'
		gw()
		# gw: sw("");  expl()
		
		
		sh= unname(unlist(sapply(chunks, function(chu){if(chu$is.code[1])c("\n'''r\n", chu$text, "\n'''\n" ) 
									else chu$text})))  # prr(sh)
		s3=sh  %+% '\n'   # prr(s3)
		s2= ifelse(q1, "\n'''\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(q2 & !q1, "\n'''r\n", '') #c('a','b') %+% 1:2
		s3= c("'''r\n", s2, "\n'''\n\n") %+% '\n'
		s3= gsub('^ *#', '#', s3)
		
		
		sw('m:/62_MM_dispos/out')
		cat(s3, file='m:/62_MM_dispos/out/zz.md', sep='')
		prr(markdownHTMLOptions())
		expl(system.file('resources', 'markdown.html', package = 'markdown') )
		markdownToHTML('m:/62_MM_dispos/out/zz.md', 'm:/62_MM_dispos/out/zz.htm', fragment.only=F, options = "highlight_code")  # prr('zz.md')
		cc('m:/62_MM_dispos/out/zz.htm')
		expl('m:/62_MM_dispos/out/zz.htm')
		expl('m:/62_MM_dispos/out/')
		
		file.remove('zz.htm')
		
		zzz
		
		execf('d:/z/arc/MultiMarkdown-Windows-Portable-4.3.1/multimarkdown.exe "m:/62_MM_dispos/out/zz.md" >> m:/62_MM_dispos/out/zz.htm')
		expl('zz.htm')
		
	}
	
}
if (0) {
	u= parse.terminal.line.quotes2(s=readLines('m:/50_HLP/out/HLP_demo/HLP_demo.r', warn =0), verb=F, exec=T)
	strr(u)
	u$lines[1:5, ]
	u$lines[81:90, ]
	u$lines[111:115, ]
	u$lines[175:180, ]
	u$lines[192:195, ]
	
}


nope.sg= function(){  # the funcs moved to RWJ.r
	
	
#' first Free Fig Number
#' e firstFreeFigN()
	firstFreeFigN= function(dirr='../img') nin(1:999, suppressWarnings(nu(gsub('^Fig_(\\d+).*\\.png$', '\\1', dir(dirr, patt='.png$')))))[1]
	
	
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
		GraphFileName=  if(fNameWithCapt) sf('Fig_%s. %s', .iFig, capt) 
				else  sf('Fig_%s', .iFig)
		
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
	
}

nope.rCharts= function()
{ #==  try rCharts  ==
	if(!require(rCharts)){  # libra(rCharts)  # - not on CRAN yet
		libra(devtools)
		install_github('rCharts', 'ramnathv')
		
#		install.packages('C:/Users/azolotovitski/Downloads/ramnathv-rCharts-b1061ab.tar.gz', repos = NULL, type='source')
#		install.packages('C:/Users/azolotovitski/Downloads/rCharts-master.zip', repos = NULL, type='source')
#		install.packages('C:/Users/azolotovitski/Downloads/rCharts-master.zip', repos = NULL, type='win.binary')
		#  C:\>z\eclipse\R-2.14.2\bin\x64\R.exe  --vanilla CMD INSTALL C:\Users\azolotovitski\Downloads\rCharts-master\rCharts-master
	}
	
	
	
	
	
	names(iris) = gsub("\\.", "", names(iris))
	p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = "Species", size="PetalWidth", type = "point", dom='zzz')  # http://www.polychartjs.com/demo?bubble_chart
	p1$addParams(width = 550, dom='zzz')
	p1$printChart("chart1")  # use p1$show() from your R console
	str(p1)
	str(p1$show())
	
	#== Example 2: NVD3
	
	p2 <- nvd3Plot(SepalLength ~ SepalWidth, group = 'Species', data = iris, type = 'scatterChart')
	p2$set(width = 550)
	p2$printChart('chart2')
	p2$show()
	
	#Example 3: MorrisJS
	
	data(economics, package = 'ggplot2')
	dat = transform(economics, date = as.character(date))
	p3 <- mPlot(x = "date", y = list("psavert", "uempmed"), data = dat, type = 'Area',
    		labels = list('Savings Rate', 'Median Duration of Unemployment'), pointSize = 0)
	p3$printChart('chart3')
	printChart(p3)
	p3$show()
	str(p3)
	
	#==  Create All Examples  ==
	root= 'C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries'
	df(fs<- dir(root, rec=T,patt='^[^z].*R$'))
	#s= llply(fp(root,fs), readLines)
	s= llply(fp(root,fs), function(f){s= readLines(f); list(f=f, n=le(s), s=s)})
	ldply(s, function(s)with(s, df(f,n)))
	#                                                                                    f   n
	# 1        C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/highcharts/examples.R  85
	# 2  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example1.R   9
	# 3  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example2.R   5
	# 4  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example3.R  12
	# 5  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example4.R  24
	# 6  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example5.R  12
	# 7  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example6.R   6
	# 8  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example7.R   4
	# 9  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example8.R   8
	# 10 C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example9.R   6
	# 11           C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/morris/examples.R  32
	# 12             C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/nvd3/examples.R  40
	# 13       C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/polycharts/examples.R  50
	# 14         C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/rickshaw/example2.R  30
	# 15         C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/rickshaw/examples.R  68
	# 16          C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/xcharts/examples.R  23
	s2= unlist(llply(s, function(s)s$s))  # 414
	writeLines(s2, fp(root,'../az/zAllExamples.R'))
	expl(root)
	expl('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	
	
	
	#HH= function(p1=a) HHjp(p= p1, capt='', dirr='C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	
	fs= dir('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	#s= llply(fp('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img', fs), function(f)s=c('<H2>', f, '</H2></br>', readLines(f), '</br>') )
	s= llply(sf('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img\\%s', fs), function(f) 
				sf('<H2>%s</H2><iframe src="%1$s" width="800px" height="600px"></iframe></br>', f) )
	s= unlist(s)  #chr [1:51830]
	writeLines(s, 'C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\allExamples.htm')
	expl('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\allExamples.htm')
	

		
	#'===  buid all examples from  rCharts   ===
	# 	HH= function(p1=a) HHjp(p= p1, capt='', dirr='C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	

	#'  save rCharts graphics to jFig_dd.htm file
	sgj= HHjp= function(p= p1, capt='', dirr='../img', absPath=T) {
		.ijFig<<- max(0, nu(gsub('^jFig_(\\d+).*\\.htm$','\\1', dir(dirr, patt='.htm$'))), na.rm=T)
		GraphFileName=  sf('jFig_%s', .ijFig<<- .ijFig+1)
		AbsGraphFileName= if(absPath) sf('%s/%s.htm', dirr, GraphFileName)  else sf('%s/%s/%s.htm', gw(), dirr, GraphFileName)
		p$save(AbsGraphFileName) 
		catf('Saved to: expl("%s"). %s\n', AbsGraphFileName, capt)
		catf('%s. %s\n', GraphFileName, capt)
		invisible(sf('%s. %s', GraphFileName, capt))
	}
	#e HHjp(p1)
	# Saved to: expl("m:/80_ChurnSim/out/../img/jFig_10.htm"). 
	# jFig_10. 	

}


if (0) {
	
	#' to source the gist use URL from "<>" (View raw):
	#e source.gist('http://goo.gl/TkRSD2')  # test1.R
	#e source.gist('http://bit.ly/source-gist_R')
	#e source.gist('https://gist.githubusercontent.com/alexzolot/9db6c623622bc0491ee3/raw/858fad86853ab20fd061a024e81d60ea82cdbe66/test1.R')
	
	source.gist= function(url='http://goo.gl/TkRSD2') {
		require("RCurl")
		ur= getURLContent(url, header = TRUE, ssl.verifypeer = FALSE)
		
		code=  if(is.na(loc<- ur$header['Location']) ){ur$body  # url is full
			   }  else getURL(loc, ssl.verifypeer = FALSE)      # url is "short url"
		eval(parse(text= code), envir= .GlobalEnv)
	}

	
}


if(0) {#===  Work with Eclipse installation  ===
	d.st='M:\\71_UseR-2013-Cont\\eclipse-Stand'
	d.te='M:\\71_UseR-2013-Cont\\eclipse-tester'
    pl.st= dir(fp(d.st,'plugins'))
	pl.te= dir(fp(d.te,'plugins'))
	
	nin(pl.st, pl.te)
	nin(pl.te, pl.st)
	
	# no vers
	pl.te.nv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.te)
	
	plst2= df(pl=pl.st, plnv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.st))
	plte2= df(pl=pl.te, plnv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.te))
	nin(plst2$plnv, plte2$plnv)
	nin(plte2$plnv, plst2$plnv)
	comm= intersect(plst2$plnv, plte2$plnv)
	
	tomove= sus(plst2, !(plnv %in% comm)) $pl
	
	for(f in tomove){catt(f); catt(fp(d.st, 'plugins', f), fp(d.st,'plugins/bak', f))}
	for(f in tomove){catt(f); file.rename(fp(d.st, f),fp(d.st,'bak', f))}
	x= file.rename(fp(d.st, 'plugins', tomove),fp(d.st, 'plugins/bak', tomove))
	sum(!x)
	
	
	#  features
	fe.st= dir(fp(d.st,'features'))
	fe.te= dir(fp(d.te,'features'))
	
	fest2= df(fe=fe.st, fenv= gsub('\\.[^\\.]+$', '', fe.st))
	fete2= df(fe=fe.te, fenv= gsub('\\.[^\\.]+$', '', fe.te))
	nin(fest2$fenv, fete2$fenv)
	nin(fete2$fenv, fest2$fenv)
	comm= intersect(fest2$fenv, fete2$fenv)
	tomove= sus(fest2, !(fenv %in% comm)) $fe
	
	x= file.rename(fp(d.st, 'features', tomove),fp(d.st, 'features/bak', tomove))
	sum(!x)
	# [1] 0
}
	

#' for Code2HTML: prevent < and > in R code (outside of single quotes or backticks) render as HTML tags. 
# s= replaceTagsOutSq(readLines('M:/85_Otto/85_Otto.r', warn=F)); prr(tail(s))
#e s= replaceTagsOutSq(c(" <aa '<bb'   `<ee`  <cc", " <aa '<bb'   `<ee`  <cc")); prr(s)
#e s= replaceTagsOutSq(''); prr(s)
replaceTagsOutSq= function(s= readLines('M:/85_Otto/85_Otto.r', warn=F)){
#	comme = ifelse(grepl('#', s), sub('.*?(#.*)', '\\1', s), '')
#	s= no.comme = sub('#.*', '', s)
	
	#s3= unlist(strsplit('~#~' %+% s,''))  # now # is \n
	s3= strsplit(pas(s,'~#~','~#~'),'')[[1]]  # now # is \n
	if(le(s3)==0) return(s)
	
	inbt= cumprod((-1)^ (s3=='`'))== -1
	insq= cumprod((-1)^ (s3=="'"))== -1
	s3[!insq & !inbt]= sub('>', '&gt;', sub('<', '&lt;', s3[!insq & !inbt]))
	s4= strsplit(pas(s3, '', ''), '~#~') [[1]] #[-1]
	#s4= s4 %+% comme
	s4
}


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
	
	theFile= fp(proot, '80_ChurnSim.r')
	#ccc= function()code2HTML(theFile)
	#cc= function()code2HTMLjQuery(theFile) #  cc()
	cc()
	
	shell('start explorer file:\\\\m:\\80_ChurnSim\\80_ChurnSim.r.jQ.htm')
	CreateNewProj(newProj.name= 'HLP_demo', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='m:/50_HLP/out')
} #--

