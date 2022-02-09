################
#              #
# PROGRAM INFO #
#              #
################

	# EDIT THIS SECTION AND SAVE IN APPROPRIATE FOLDER

	# PROJECT       CISCO-19
	# PROGRAM NAME  //Rcb-storage/filestore/Studies/CISCO19/statistics/programs/S1/RPrograms/Current/CISCO19_Paper1_v1.R
	# PURPOSE       Generate report for Paper 1 for CISCO-19
	# AUTHOR        Alex McConnachie
	# STARTED       v1 Draft01 - 6th August 2021
	#               v1 Draft02 - 13th August 2021 - starting on table 2
	#               v1 Draft03 - 25th August 2021 - starting on table 3
	#               v1 Draft03 - 30th August 2021 - starting on table 5
	#               v1 Draft05 - 3rd September 2021 - corrections after checking
	#               v1 Draft06 - 7th September 2021 - more corrections after checking
	#               v1 Draft07 - 14th September 2021 - more corrections after meeting 13/09
	#               v1 Draft08 - 15th September 2021 - last few datasets...
	#               v1 Draft10 - 17th September 2021 - rewind to draft08 then update to draft10 - new CTCA table plus minor tweaks
	#               v1 Draft11 - 30th September 2021 - more minor tweaks + additions
	#               v1 Draft12 - 11th October 2021 - more minor tweaks + additions
	#               v1 Draft13 - 26th October 2021 - more minor tweaks + additions
	#               v1 Draft14 - 10th November 2021 - bits and pieces of extra data
	#               v1 Draft15 - 19th November 2021 - more bits and pieces of extra data
	#               v1 Draft16 - 20th December 2021
	#                            - checking data added in previous version
	#                            - new data for nature medicine
	#               v1 Draft17 - 27th December 2021
	#                            - respond to reviewer comments for nature medicine
	#               v1 Draft18 - 28th December 2021
	#                            - respond to reviewer comments for nature medicine
	#               v1 Draft19 - 31st December 2021
	#                            - respond to reviewer comments for nature medicine
	#                            - add option to move 2 COVID patients into control group
	#               v1 Draft20 - 24th January 2022
	#                            - add option to move remove 2 COVID patients from study
	#               v1 Draft21 - 31st January 2022
	#                            - look at specific criteria in myocarditis adjudication
	#               v1 Draft22 - 4th February 2022
	#                            - radar plot of criteria in myocarditis adjudication
	#                            - update to point to database table
	#               v1 Draft23 - 7th February 2022
	#                            - final draft, using snapshot
	#               v1 - 9th February 2022
	#                    - final version, with unnecessary code removed

#################
#               #
# PROGRAM SETUP #
#               #
#################

	# EDITABLE SECTION

		# NAME OF STATISTICIAN RUNNING PROGRAM
			stats.name<-"Alex McConnachie"

		# PROGRAM DEVELOPMENT VERSION
			Version<-"v1"

		# DATASETS DATE
			data.date<-"20220208"

		# DATABASE NAME
			dataBase<-"CISCO19_20220208"
			privData<-"CISCO19_20220208"

		# EXTERNAL DATA TABLE NAMES
			cec.table<-"extCEC_fin_Data20210803"
			ecg.table<-"ext_ECGFeatures_20210514"
			chestct.table<-"ext_RespUpload_20210720"
			ctca1.table<-"extCTCA_INC_TBL2COVID_20210914"
			ctca2.table<-"ext_Controls_20210917"
			ctca3.table<-"extCTCA_INC_COVID_20210914"
			ctca4.table<-"extCTCA_INC_CONTR_20210914"
			cmr.table<-"ext_MRI_Data_Strain_20210827"
			t1.table<-"ext_MRI_Data_T1_20210914"
			t2.table<-"ext_MRI_Data_T2_20210721"
			blood1.table<-"ext_Biomarkers_20210513"
			blood2.table<-"ext_controls_20210831"
			blood3.table<-"ext_Biomarkers_20210816"
			blood4.table<-"ext_Biomarkers_20211027"
			blood5.table<-"ext_controls_20211027"
			coag.table1<-"ext_CoagulationData160721"
			coag.table2<-"ext_haemostasis_20211020"
			coag.table3<-"ext_visit2_data_v2_20211102"
			coag.table4<-"ext_Results1_20211118"
			coag.table5<-"ext_Results2_20211118"
			coag.table6<-"ext_Results3_20211118"
			extra1.table<-"extCI_CABG_PEAKSODIUM_20210831"
			hcw.table<-"ext_Control_HCW_20210913"
			renal.table<-"ext_Renal_20210730"
			mbf.table<-"ext_QualPerfusion_20210923"
			creat.table<-"ext_CreatinineResults_20211004"
			rdw.table<-"ext_RDW_20211008"
			lge.table<-"ext_LGE_Classification_20211013"
			ddrugs.table<-"ext_Diabetes_Drugs_20210930"
			criteria.table<-"extCEC_Myocarditis_20220128"

		# OUTCOMES DATA
			thromboembolism.table<-"ext_Thromboembolism_20211215"
			events.table<-"ext_Events_20211217"
			medications.table<-"ext_Medications_20211217"
			outpatient.table<-"ext_Outpatient_20211223"
			secondarycare.table<-"ext_SecondaryCare_20211221"
			vitalstatus.table<-"ext_VitalStatus_20211217"
			inhospevents.table<-"ext_IP_Outcomes_30_12_2021"

		# SIMD TABLE NAME
			simd.table<-"SIMD_Summary_20210903"

		# FLAG FOR WHETHER TO COUNT "COVID" PATIENTS WITHOUT PCR CONFIRMATION AS CONTROLS (IF THEY COMPLETE VISIT 2)
			noncovid.crossover<-T

		# FLAG FOR WHETHER TO REMOVE "COVID" PATIENTS WITHOUT PCR CONFIRMATION (IF THEY DO NOT COMPLETE VISIT 2)
			noncovid.remove<-T

	# END OF EDITABLE SECTION

	# PROJECT NAME
		project.title<-"Cardiac Imaging in SARS Coronavirus disease-19"
		project.name<-"CISCO19"

	# ANALYSIS NAME
		analysis.name<-"Paper1"

	# FILES WITHIN STATISTICS DIRECTORY
		fileName<-function(...)paste("//Rcb-storage/filestore/Studies",project.name,"statistics/programs/S1",...,sep="/")
		dataFileName<-function(...)paste("//Rcb-storage/filestore/Studies",project.name,"statistics/data/datstoreCSV",...,sep="/")

	# LIBRARIES
		RLibs<-fileName("RLibraries")
		unlink(RLibs,T,T)
		dir.create(RLibs)
		.libPaths(RLibs)
		install.packages(c("RODBC","chron","QRISK3","eq5d","score","fmsb"),RLibs,"https://cran.ma.imperial.ac.uk/")

		library(RODBC)
		library(chron)
		library(QRISK3)
		library(eq5d)
		library(score)
		library(survival)
		library(nlme)
		library(fmsb)

	# SNAPSHOT DATE
		snapshot.date<-NULL
		snapshot.date<-chron(paste(substring(data.date,7,8),substring(data.date,5,6),substring(data.date,1,4),sep="/"),format="d/m/y",out.format="day mon year")
		if(is.null(snapshot.date)) snapshot.date<-paste(substring(data.date,7,8),substring(data.date,5,6),substring(data.date,1,4),sep="/")

	# SET ANALYSIS NAME AND OUTPUT DIRECTORIES
		outFileName<-function(...)fileName("Reports",analysis.name,...)
		dir.create(outFileName())
		dir.create(outFileName(Version))
		program.name<-paste(project.name,"_",analysis.name,"_",Version,".R",sep="")

	# OUTPUT SUBDIRECTORIES

		# CREATE SUBDIRECTORY FOR TABLES
			table.path<-outFileName(Version,"Tables")
			dir.create(table.path)

		# CREATE SUBDIRECTORY FOR FIGURES
			figure.path<-outFileName(Version,"Figures")
			dir.create(figure.path)

		# CREATE SUBDIRECTORY FOR EXPORTING DATASETS
			export.path<-outFileName(Version,"DataExport")
			dir.create(export.path)

	# DEFAULT FORMATS FOR TABLES
		horiz.fmt<-
			list(
				Style=
					list(
						"font-family"="Arial","font-size"="10pt","page-break-inside"="avoid",
						"border-collapse"="collapse","padding"=paste(rep("0.1cm",4),collapse=" "),
						"border-top"="none","border-bottom"="solid windowtext 1pt","border-left"="none","border-right"="none"),
				Other=
					list(
						align="left",valign="middle"))
		table.fmt<-
			list(
				Style=
					list(
						"font-family"="Arial","font-size"="10pt","page-break-inside"="avoid",
						"border-collapse"="collapse","padding"=paste(rep("0.1cm",4),collapse=" "),
						"border-top"="none","border-bottom"="none","border-left"="none","border-right"="none"),
				Other=
					list(
						align="center",valign="middle"))
		none.fmt<-
			list(
				Style=
					list(
						"font-family"="Arial","font-size"="10pt","page-break-inside"="avoid",
						"border-collapse"="collapse","padding"=paste(rep("0.1cm",4),collapse=" "),
						"border-bottom"="none","border-top"="none","border-left"="none","border-right"="none"),
				Other=
					list(
						align="left",valign="middle"))
		list.fmt<-
			list(
				Style=
					list(
						"font-family"="Arial","font-size"="10pt","page-break-inside"="avoid",
						"border-collapse"="collapse","padding"=paste(rep("0.1cm",4),collapse=" "),
						"border-top"="none","border-bottom"="solid windowtext 1pt","border-left"="none","border-right"="none"),
				Other=
					list(
						align="left",valign="middle"))
	
#############
#           #
# FUNCTIONS #
#           #
#############

	# TABLES FUNCTIONS
	# v1.1 of SPlusTables
		source("//Rcb-file-2000/filestore/Studies/SPlusTables/Share/SPlusTables_v1/SPlusTables_Functions_v1_1.R")

	# FORMATTING
		my.format<-
			function(x,ndp,na.replace="-")
				ifelse(
					is.na(x),
					na.replace,
					format(round(x,ndp),ns=ndp,scientific=F,trim=T))
		p.format<-function(x,pref="p",super=NULL)
			{
				out<-my.format(x,4)
				out<-ifelse(
					out=="0.0000",
					paste(pref,"<0.0001",sep=""),
					paste(pref,"=",out,sep=""))
				if(!is.null(super))
					out<-paste(out,"<sup>",super,"</sup>",sep="")
				out
			}

	# BASIC SUMMARY FUNCTION - REPLACE TEXT PATTERNS
		summary.fun<-function(x,summary.text,ndp=1){
			out<-summary.text
			
			if(length(x)==0){
				sub.n<-0
				sub.nmiss<-sub.pcmiss<-sub.mean<-sub.sd<-sub.count<-sub.percent<-
					sub.median<-sub.lq<-sub.uq<-sub.min<-sub.max<-sub.geomean<-sub.geosd<-sub.lcl<-sub.ucl<-
						"-"
				} else {
				sub.n<-sum(!is.na(x))
				sub.nmiss<-sum(is.na(x))
				sub.pcmiss<-my.format(100*mean(is.na(x)),1)
				sub.mean<-my.format(mean(x,na.rm=TRUE),ndp)

				sub.sd<-sub.lcl<-sub.ucl<-"-"
				if(sub.n>1){
					sub.sd<-my.format(sqrt(var(x,na.rm=TRUE)),ndp)
					if(var(x,na.rm=TRUE)>0){
						sub.lcl<-my.format(t.test(x)$conf.int[1],ndp)
						sub.ucl<-my.format(t.test(x)$conf.int[2],ndp)
					}
				}

				sub.count<-sum(x,na.rm=TRUE)
				sub.percent<-my.format(100*mean(x,na.rm=TRUE),ndp)
				sub.median<-my.format(median(x,na.rm=TRUE),ndp)
				sub.lq<-my.format(quantile(x,0.25,na.rm=TRUE),ndp)
				sub.uq<-my.format(quantile(x,0.75,na.rm=TRUE),ndp)

				if(sub.n){
					sub.min<-my.format(min(x,na.rm=TRUE),ndp)
					sub.max<-my.format(max(x,na.rm=TRUE),ndp)
				} else
					sub.min<-sub.max<-"-"

				if(sum(x<=0,na.rm=TRUE)==0){
					sub.geomean<-my.format(exp(mean(log(x),na.rm=TRUE)),ndp)
					if(sub.n>1){
						sub.geosd<-my.format(exp(sqrt(var(log(x),na.rm=TRUE))),ndp)
					} else
						sub.geosd<-"-"
					}
				}

			out<-gsub("@N",sub.n,out)
			out<-gsub("@MISS",sub.nmiss,out)
			out<-gsub("@PCMISS",sub.pcmiss,out)
			out<-gsub("@MEAN",sub.mean,out)
			out<-gsub("@SD",sub.sd,out)
			out<-gsub("@COUNT",sub.count,out)
			out<-gsub("@PERCENT",sub.percent,out)
			out<-gsub("@MEDIAN",sub.median,out)
			out<-gsub("@LQ",sub.lq,out)
			out<-gsub("@UQ",sub.uq,out)
			out<-gsub("@MIN",sub.min,out)
			out<-gsub("@MAX",sub.max,out)
			out<-gsub("@LCL",sub.lcl,out)
			out<-gsub("@UCL",sub.ucl,out)
			if(sum(x<=0,na.rm=TRUE)==0){
				out<-gsub("@GEOMEAN",sub.geomean,out)
				out<-gsub("@GEOSD",sub.geosd,out)
			}

			return(out)
		}
				
	# END ROW FOR TABLES
	# INPUT PROJECT NAME AND DISCLAIMER INDICATOR
		end.row<-
			function(
				pname=project.name,
				progname=program.name,
				footnote=NULL)
		{
			end.text<-
				c(
					paste("Project:",pname),
					paste("Output created by program:",progname),
					paste("Last run on",date()))

			output<-list(Text=end.text,Other=list(align="right"),Style=list("border-top"="solid windowtext 1pt","border-bottom"="solid windowtext 1pt"))
		
			if(!is.null(footnote))
				output<-rbindTable(row.title(footnote,Style=list("border-top"="solid windowtext 1pt","border-bottom"="solid windowtext 1pt")),output)
		
			output
		}

	# ROW TITLES
		row.title<-
			function(rowtext,anyOther=NULL,Style=NULL)
				list(Text=rowtext,Other=c(list(align="left"),anyOther),Style=Style)
	
	# TABLE TITLE
		table.title<-function(tabnum,tabtext,anyOther=NULL)
			row.title(paste("<b>Table ",tabnum,":<b> ",tabtext,sep=""),Style=list("border-top"="solid windowtext 1pt","border-bottom"="solid windowtext 1pt"),anyOther=anyOther)
	
	# FIGURE TITLE
		figure.title<-function(tabnum,tabtext)
			row.title(paste("<b>Figure ",tabnum,":<b> ",tabtext,sep=""),Style=list("border-top"="solid windowtext 1pt"))
	
	# CBINDTABLE FOR VECTORS
		cbindVector<-function(x,...)do.call("cbindTable",lapply(as.list(x),function(x,...)list(Text=x,...),...=...))
	
	# RBINDTABLE FOR VECTORS
		rbindVector<-function(x,...)do.call("rbindTable",lapply(as.list(x),function(x,...)list(Text=x,...),...=...))
	
	# BOLD TEXT
		bold<-function(x)paste("<b>",x,"</b>",sep="")

	# ITALIC TEXT
		italic<-function(x)paste("<i>",x,"</i>",sep="")

	# READ DATA FROM SQL DATABASE TABLE
		snap.read<-function(database,schema,table.name)
		{
			channel<-odbcConnect(database)
			temp<-as.data.frame(sqlFetch(channel,paste(schema,table.name,sep="."),as.is=T),stringsAsFactors=F)
			odbcClose(channel)
			names(temp)<-casefold(names(temp))
			temp
		}

	# VIEW TABLES FROM SQL DATABASE
		snap.view<-function(database,schema)
		{
			channel<-odbcConnect(database)
			temp<-sqlTables(channel,schema=schema)
			odbcClose(channel)
			temp
		}

	# READ DATA FROM CSV TABLE
		snap.read.csv<-function(table.name)
		{
			temp<-read.csv(table.name,stringsAsFactors=F)
			names(temp)<-casefold(names(temp))
			temp
		}

	# REPLACE SPACES IN TEXT WITH NON-BREAKING SPACES
		nospace<-function(x)gsub(" ","&nbsp;",x)

	# FIGURE FUNCTIONS
		fig.file<-function(fig.num,fig.path)
			paste(fig.path,"/Figure_",gsub("\\.","_",fig.num),".wmf",sep="")
		fig.set<-function(fig.num,fig.path=figure.path,height=5,width=6.8)
			win.metafile(fig.file(fig.num,fig.path),height=height,width=width)
		fig.tag<-function(fig.num,fig.path=figure.path)
			paste("<img src='",fig.file(fig.num,fig.path),"' alt='Figure ",fig.num,"'>",sep="")

	# ROW FUNCTIONS FOR TABLES

	# MEAN AND SD
		msd.row<-function(xname,x,ndp,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T,showAll=F,p3=T)
		{
			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
			}
			summary.text<-"@MEAN (@SD)"

			out<-cbindTable(row.title(xname),row.title("Mean (SD)"))

			if(showAll) out<-cbindTable(out,summary.fun(x[(paper=="No")],summary.text,ndp))

			out<-cbindTable(out,summary.fun(x[(paper=="Yes")&(group=="COVID-19")],summary.text,ndp))

			out<-cbindTable(out,if(showCont) summary.fun(x[(paper=="Yes")&(group=="Control")],summary.text,ndp) else "")

			if(showAll&p3)
			{
				if(all(is.na(x[paper=="No"]))) out<-cbindTable(out,"-")
				if(!all(is.na(x[paper=="No"]))) out<-cbindTable(out,p.format(kruskal.test(x~paper,subset=group=="COVID-19")$p.value))
			}

			if(showCont) out<-cbindTable(out,if(p1) p.format(kruskal.test(x~group,subset=paper=="Yes")$p.value) else "-")

			out<-
				cbindTable(
					out,
					do.call(
						"cbindTable",
						lapply(
							as.list(levels(primary)),
							function(i,x,primary,summary.text,ndp)
								summary.fun(x[primary==i],summary.text=summary.text,ndp=ndp),
							x=x[(paper=="Yes")&(group=="COVID-19")],
							primary=primary[(paper=="Yes")&(group=="COVID-19")],
							summary.text=summary.text,ndp=ndp)),
					if(p2) p.format(kruskal.test(x~primary,subset=(paper=="Yes")&(group=="COVID-19"))$p.value) else "-")

			out
		}

	# MEDIAN AND IQR
		miqr.row<-function(xname,x,ndp,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T,showAll=F,p3=T)
		{
			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
			}
			summary.text<-"@MEDIAN (@LQ, @UQ)"

			out<-cbindTable(row.title(xname),row.title("Median (IQR)"))

			if(showAll) out<-cbindTable(out,summary.fun(x[(paper=="No")],summary.text,ndp))

			out<-cbindTable(out,summary.fun(x[(paper=="Yes")&(group=="COVID-19")],summary.text,ndp))

			out<-cbindTable(out,if(showCont) summary.fun(x[(paper=="Yes")&(group=="Control")],summary.text,ndp) else "")

			if(showAll&p3)
			{
				if(all(is.na(x[paper=="No"]))) out<-cbindTable(out,"-")
				if(!all(is.na(x[paper=="No"]))) out<-cbindTable(out,p.format(kruskal.test(x~paper,subset=group=="COVID-19")$p.value))
			}

			if(showCont) out<-cbindTable(out,if(p1) p.format(kruskal.test(x~group,subset=paper=="Yes")$p.value) else "-")

			out<-
				cbindTable(
					out,
					do.call(
						"cbindTable",
						lapply(
							as.list(levels(primary)),
							function(i,x,primary,summary.text,ndp)
								summary.fun(x[primary==i],summary.text=summary.text,ndp=ndp),
							x=x[(paper=="Yes")&(group=="COVID-19")],
							primary=primary[(paper=="Yes")&(group=="COVID-19")],
							summary.text=summary.text,ndp=ndp)),
					if(p2) p.format(kruskal.test(x~primary,subset=(paper=="Yes")&(group=="COVID-19"))$p.value) else "-")

			out
		}

	# N AND %
		npc.row<-function(xname,x,xlevel=NULL,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T,showAll=F,p3=T,showlevel=NULL,...)
		{
			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
			}
			npc<-function(x,xlevel)
			{
				xtab<-table(x)
				if(all(is.na(x))) rep("-",length(xlevel)) else paste(xtab," (",my.format(100*xtab/sum(xtab),1),"%)",sep="")[match(xlevel,levels(x))]
			}
			if(is.null(showlevel)) showlevel<-is.null(xlevel)
			if(is.null(xlevel)) xlevel<-levels(x)

			out<-if(showlevel) cbindTable(row.title(xname),row.title(nospace(xlevel))) else row.title(xname,anyOther=list(colspan=2))

			if(showAll) out<-cbindTable(out,npc(x[(paper=="No")],xlevel))

			out<-cbindTable(out,npc(x[(paper=="Yes")&(group=="COVID-19")],xlevel))

			out<-cbindTable(out,if(showCont) npc(x[(paper=="Yes")&(group=="Control")],xlevel) else "")

			if(showAll&p3)
			{
				if(all(is.na(x[paper=="No"]))) out<-cbindTable(out,"-")
				if(!all(is.na(x[paper=="No"]))) out<-cbindTable(out,p.format(fisher.test(table(x,group,paper)[,2,],...)$p.value))
			}

			if(showCont) out<-cbindTable(out,if(p1) p.format(fisher.test(table(x,group,paper)[,,2],...)$p.value) else "-")

			out<-
				cbindTable(
					out,
					do.call(
						"cbindTable",
						lapply(
							as.list(levels(primary)),
							function(i,x,primary,xlevel)
								npc(x[primary==i],xlevel),
							x=x[(paper=="Yes")&(group=="COVID-19")],
							primary=primary[(paper=="Yes")&(group=="COVID-19")],
							xlevel=xlevel)),
					if(p2) p.format(fisher.test(table(x,primary,group,paper)[,,2,2],...)$p.value) else "-")

			out
		}

	# N AND % WITH LOG RANK TEST OF TIME TO FIRST EVENT
		npc.lr.row<-function(xname,x,xlevel,disch.date,v1.date,v3.date,event.date,death.date,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T,showAll=F,p3=T,Cens=NULL,...)
		{
			x<-as.numeric(x==xlevel)

			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
				disch.date<-disch.date[!is.na(subset)&subset]
				v1.date<-v1.date[!is.na(subset)&subset]
				v3.date<-v3.date[!is.na(subset)&subset]
				event.date<-event.date[!is.na(subset)&subset]
				death.date<-death.date[!is.na(subset)&subset]
			}

			start.date<-ifelse(group=="COVID-19",disch.date,v1.date)

			if(!is.null(Cens))
			{

				change<-!is.na(event.date-start.date)&(as.numeric(event.date-start.date)>Cens)
				x[change]<-0
				event.date[change]<-NA

				change<-!is.na(death.date-start.date)&(as.numeric(death.date-start.date)>Cens)
				death.date[change]<-NA

				change<-!is.na(v3.date-start.date)&(as.numeric(v3.date-start.date)>Cens)
				v3.date[change]<-start.date[change]+Cens
			}

			npc<-function(x)
				if(all(is.na(x))) "-" else summary.fun(x,"@COUNT (@PERCENT%)",1)

			if(any(is.na(event.date[x>0]))) stop("Missing event date")

			if(sum(x)) tte<-as.numeric(pmin(v3.date,event.date,death.date,na.rm=T)-start.date)
			else tte<-as.numeric(pmin(v3.date,death.date,na.rm=T)-start.date)
			
			surv.obj<-Surv(time=tte,event=x)

			out<-row.title(xname,anyOther=list(colspan=2))

			if(showAll) out<-cbindTable(out,npc(x[(paper=="No")]))

			out<-cbindTable(out,npc(x[(paper=="Yes")&(group=="COVID-19")]))

			out<-cbindTable(out,if(showCont) npc(x[(paper=="Yes")&(group=="Control")]) else "")

			if(showAll&p3) out<-cbindTable(out,p.format(1-pchisq(survdiff(surv.obj~paper,subset=group=="COVID-19")$chisq,1)))
			
			if(showCont) out<-cbindTable(out,if(p1) p.format(1-pchisq(survdiff(surv.obj~group,subset=paper=="Yes")$chisq,1)) else "-")

			out<-
				cbindTable(
					out,
					do.call(
						"cbindTable",
						lapply(
							as.list(levels(primary)),
							function(i,x,primary)
								npc(x[primary==i]),
							x=x[(paper=="Yes")&(group=="COVID-19")],
							primary=primary[(paper=="Yes")&(group=="COVID-19")])),
					if(p2) p.format(1-pchisq(survdiff(surv.obj~primary,subset=(paper=="Yes")&(group=="COVID-19"))$chisq,3)) else "-")

			out
		}

	# EXPANDED CONTINUOUS
		expcont.row<-function(xname,x,ndp,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T)
		{
			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
			}
			summary.text<-"@N (@MISS)<br>@MEAN (@SD)<br>@MEDIAN (@LQ, @UQ)<br>[@MIN, @MAX]"
			rbindTable(
				row.title(xname),
				cbindTable(
					"",
					row.title("N (N<sub>MISSING</sub>)<br>Mean (SD)<br>Median (IQR)<br>[Min, Max]"),
					summary.fun(x[(paper=="Yes")&(group=="COVID-19")],summary.text,ndp),
					if(showCont)
					{
						cbindTable(
							summary.fun(x[(paper=="Yes")&(group=="Control")],summary.text,ndp),
							if(p1) p.format(kruskal.test(x~group,subset=paper=="Yes")$p.value) else "-")
					} else "",
					summary.fun(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Not")],summary.text,ndp),
					summary.fun(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Unlikely")],summary.text,ndp),
					summary.fun(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Probably")],summary.text,ndp),
					summary.fun(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Very")],summary.text,ndp),
					if(p2) p.format(kruskal.test(x~primary,subset=(paper=="Yes")&(group=="COVID-19"))$p.value) else "-"))
		}
	# EXPANDED CATEGORICAL
		expnpc.row<-function(xname,x,paper=cisco$paper,group=cisco$group,primary=cisco$primary,p1=T,p2=T,subset=NULL,showCont=T,...)
		{
			if(!is.null(subset))
			{
				x<-x[!is.na(subset)&subset]
				paper<-paper[!is.na(subset)&subset]
				group<-group[!is.na(subset)&subset]
				primary<-primary[!is.na(subset)&subset]
			}
			npc<-function(x)
			{
				xtab<-table(x)
				c(
					paste(sum(!is.na(x))," (",sum(is.na(x)),")",sep=""),
					if(all(is.na(x))) rep("-",length(levels(x))) else paste(xtab," (",my.format(100*xtab/sum(xtab),1),"%)",sep=""))
			}
			rbindTable(
				row.title(xname),
				cbindTable(
					"",
					row.title(c("N (N<sub>MISSING</sub>)",nospace(levels(x)))),
					npc(x[(paper=="Yes")&(group=="COVID-19")]),
					if(showCont)
					{
						cbindTable(
							npc(x[(paper=="Yes")&(group=="Control")]),
							if(p1) p.format(fisher.test(table(x,group,paper)[,,2],...)$p.value) else "-")
					} else "",
					npc(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Not")]),
					npc(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Unlikely")]),
					npc(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Probably")]),
					npc(x[(paper=="Yes")&(group=="COVID-19")&(primary=="Very")]),
					if(p2) p.format(fisher.test(table(x,primary,group,paper)[,,2,2],...)$p.value) else "-"))
		}

	# FUNCTION TO CALCULATE BRIEF ILLNESS PERCEPTION QUESTIONNAIRE SCORE
		bip.fun<-
			function(q1,q2,q3,q4,q5,q6,q7,q8,imp=0.5)
			{
				q1<-as.numeric(q1)
				q2<-as.numeric(q2)
				q3<-10-as.numeric(q3)
				q4<-10-as.numeric(q4)
				q5<-as.numeric(q5)
				q6<-as.numeric(q6)
				q7<-10-as.numeric(q7)
				q8<-as.numeric(q8)

				apply(
					cbind(q1,q2,q3,q4,q5,q6,q7,q8),
					1,
					function(x)
						if(mean(is.na(x))>imp) NA else length(x)*mean(x,na.rm=T))
			}

	# FUNCTION TO DERIVE IPAQ SCORES
		ipaq.short.fun<-
			function(vigd,vigh,vigm,modd,modh,modm,walkd,walkh,walkm,sith,sitm,id,test=F)
			{
				# IF ZERO DAYS RECORDED, THEN HOURS/MINS=0
					vigm[!is.na(vigd)&(vigd==0)]<-vigh[!is.na(vigd)&(vigd==0)]<-0
					modm[!is.na(modd)&(modd==0)]<-modh[!is.na(modd)&(modd==0)]<-0
					walkm[!is.na(walkd)&(walkd==0)]<-walkh[!is.na(walkd)&(walkd==0)]<-0

				# IF HOURS RECORDED AND MINUTES MISSING, THEN MINUTES=0
					vigm[!is.na(vigh)&is.na(vigm)]<-0
					modm[!is.na(modh)&is.na(modm)]<-0
					walkm[!is.na(walkh)&is.na(walkm)]<-0
					sitm[!is.na(sith)&is.na(sitm)]<-0

				# IF MINUTES RECORDED AND HOURS MISSING, THEN HOURS=0
					vigh[is.na(vigh)&!is.na(vigm)]<-0
					modh[is.na(modh)&!is.na(modm)]<-0
					walkh[is.na(walkh)&!is.na(walkm)]<-0
					sith[is.na(sith)&!is.na(sitm)]<-0

				# CONVERT TO MINUTES PER DAY
					vigperday<-vigh*60+vigm
					modperday<-modh*60+modm
					walkperday<-walkh*60+walkm

				# IF MORE THAN 16 HOURS (960 MINS) OF ACTIVITY IS RECORDED, THEN SET ALL TO MISSING
					totalactive<-vigperday+modperday+walkperday
					vigperday[!is.na(totalactive)&(totalactive>960)]<-
						modperday[!is.na(totalactive)&(totalactive>960)]<-
							walkperday[!is.na(totalactive)&(totalactive>960)]<-NA

				# TRUNCATE AT 3 HOURS
					vigh[vigperday>180]<-3; vigm[vigperday>180]<-0; vigperday<-pmin(180,vigperday)
					modh[modperday>180]<-3; modm[modperday>180]<-0; modperday<-pmin(180,modperday)
					walkh[walkperday>180]<-3; walkm[walkperday>180]<-0; walkperday<-pmin(180,walkperday)

				# IF <10 MINUTES RECORDED, THEN SET TO 0
					vigperday[!is.na(vigperday)&(vigperday<10)]<-0
					modperday[!is.na(modperday)&(modperday<10)]<-0
					walkperday[!is.na(walkperday)&(walkperday<10)]<-0

				# CALCULATE MET MINUTES
					vigmet<-8*vigd*vigperday
					modmet<-4*modd*modperday
					walkmet<-3.3*walkd*walkperday

					totalmet<-vigmet+modmet+walkmet

				# CALCULATE CATEGORIES
					inactive<-ifelse(is.na(totalmet),NA,1)
					minimal<-
						as.numeric(
							((vigd>=3)&(vigm>=20))|
							((modd>=5)&(modm>=30))|
							((walkd>=5)&(walkm>=30))|
							(((vigd+modd+walkd)>=5)&(totalmet>=600)))
					active<-
						as.numeric(
							((vigd>=3)&(vigmet>=1500))|
							(((vigd+modd+walkd)>=7)&(totalmet>=3000)))

				# PUT ALL TOGETHER
					temp<-data.frame(
						id=id,
						met=totalmet,
						ipaq_cat=factor(pmax(inactive,2*minimal,3*active),1:3,c("Low","Moderate","High")),
						vigd=vigd,vigh=vigh,vigm=vigm,vigperday=vigperday,vigmet=vigmet,
						modd=modd,modh=modh,modm=modm,modperday=modperday,modmet=modmet,
						walkd=walkd,walkh=walkh,walkm=walkm,walkperday=walkperday,walkmet=walkmet)

				# IF IN TEST MODE (test=T) THEN OUTPUT EVERYTHING
					if(test) return(me=temp,pack=ipaq(data.frame(id,100,vigd,vigh,vigm,modd,modh,modm,walkd,walkh,walkm,sith,sitm)))

				# OTHERWISE OUTPUT IPAQ CATEGORY AND TOTAL MET MINUTES
					temp[match(id,temp$id),c("met","ipaq_cat")]

			}

	# FUNCTION TO DERIVE DASI SCORE & VO2MAX ESTIMATE
		dasi.fun<-
			function(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12)
			{
				dasi_score<-
					2.75*(q1==1)+
					1.75*(q2==1)+
					2.75*(q3==1)+
					5.5*(q4==1)+
					8*(q5==1)+
					2.7*(q6==1)+
					3.5*(q7==1)+
					8*(q8==1)+
					4.5*(q9==1)+
					5.25*(q10==1)+
					6*(q11==1)+
					7.5*(q12==1)
				dasi_vo2max<-
					0.43*dasi_score+9.6

				data.frame(dasi_score=dasi_score,dasi_vo2max=dasi_vo2max)
			}

	# FUNCTION TO CATEGORISE TROPONIN I
		trop.cat<-function(troponin,sex,whichsex)
		{
			if(whichsex=="Male")
				out<-
					factor(
						ifelse(sex=="Male",as.numeric(cut(troponin,c(-1,1.1999,5.0001,34.0001,1e8))),NA),
						1:4,
						c("&lt;1.2 pg/ml","1.2 - 5.0 pg/ml","&gt;5 - 34 pg/ml","&gt;34 pg/ml"))
			if(whichsex=="Female")
				out<-
					factor(
						ifelse(sex=="Female",as.numeric(cut(troponin,c(-1,1.1999,5.0001,16.0001,1e8))),NA),
						1:4,
						c("&lt;1.2 pg/ml","1.2 - 5.0 pg/ml","&gt;5 - 16 pg/ml","&gt;16 pg/ml"))
			if(whichsex=="All")
				out<-
					factor(
						ifelse(sex=="Female",as.numeric(cut(troponin,c(-1,1.1999,5.0001,16.0001,1e8))),ifelse(sex=="Male",as.numeric(cut(troponin,c(-1,1.1999,5.0001,34.0001,1e8))),NA)),
						1:4,
						c("&lt;1.2 pg/ml","1.2 - 5.0 pg/ml","&gt;5 pg/ml - 99<sup>th</sup> percentile","&gt;99<sup>th</sup> percentile"))
			out
		}

	# FUNCTION TO CATEGORISE NTproBNP
		bnp.cat<-function(bnp)
			cut(bnp,c(-1,125.0001,400.0001,1e8),c("0 - 125pg/ml","&gt;125 - 400pg/ml","&gt;400pg/ml"))

	# FUNCTION TO CALCULATE EGFR
		egfr.fun<-function(creat,age,female,black)
		{
			k<-ifelse(female==1,0.7,0.9)
			a<-ifelse(female==1,-0.329,-0.411)

			creat<-creat/88.42

			round(
				141*
				(pmin(creat/k,1)^a)*
				(pmax(creat/k,1)^(-1.209))*
				(0.993^age)*
				(1.018^female)*
				(1.159^black),
				2)
		}

	# EXTRACT EFFECT ESTIMATE FROM GLM
		lm.summ<-function(fit,term,trans=function(x)x,ndp=1,asTable=T)
		{
			temp<-summary(fit)$coefficients[term,,drop=F]
			est<-my.format(trans(temp[,1]),ndp)
			lcl<-my.format(trans(temp[,1]+qnorm(0.025)*temp[,2]),ndp)
			ucl<-my.format(trans(temp[,1]+qnorm(0.975)*temp[,2]),ndp)
			p.val<-p.format(2*pnorm(-abs(temp[,3])))

			if(asTable)
				cbindTable(paste(est," (",lcl,", ",ucl,")",sep=""),p.val)
			else
				paste(est," (",lcl,", ",ucl,"), ",p.val,sep="")
		}

	# CRP CATEGORIES
		crp.cat<-function(x)
			cut(x,c(0,4.9999,9.9999,1000000),c("&lt;5 mg/l","&ge;5, &lt;10 mg/l","&ge;10 mg/l"))


###############
#             #
# IMPORT DATA #
#             #
###############

	# VIEW DATA TABLES
	#	snap.view(dataBase,NULL)
	#	snap.view(dataBase,"dm")
	#	snap.view(privData,NULL)
	#	head(snap.read(dataBase,"de2","QuickDefFields"))

	# LOOKUPS
	#	snap.read(dataBase,"lookups","LookupTable")
	#	temp<-snap.read(dataBase,"lookups","LookupValue"); temp[temp$name=="JBS004",]
	#	temp<-snap.read(dataBase,"lookups","LookupValue"); temp[temp$lookuptableid==7,]

	# IMPORT SUBJECTS DATA
		subjects.data<-snap.read(dataBase,"web","webSubjects")

	# IMPORT VISITS DATA
		visits.data<-snap.read(dataBase,"web","Visits")

	# IMPORT CRF DATA

		# GET TABLE NAMES
			temp<-casefold(snap.view(dataBase,"crf")$TABLE_NAME,upper=F)

		# IMPORT
			crf.data<-lapply(as.list(temp),function(x)snap.read(dataBase,"crf",x))
			names(crf.data)<-temp

	# SPLIT CRF DATA
		crf1.data<-
			lapply(
				crf.data,
				function(x,vdata)
				{
					x$visittypeid<-vdata$visittypeid[match(paste(x$sno,x$visitid),paste(vdata$sno,vdata$visitid))]
					x[x$visittypeid==1,]
				},
				vdata=visits.data)
		crf2.data<-
			lapply(
				crf.data,
				function(x,vdata)
				{
					x$visittypeid<-vdata$visittypeid[match(paste(x$sno,x$visitid),paste(vdata$sno,vdata$visitid))]
					x[x$visittypeid==2,]
				},
				vdata=visits.data)
		crf4.data<-
			lapply(
				crf.data,
				function(x,vdata)
				{
					x$visittypeid<-vdata$visittypeid[match(paste(x$sno,x$visitid),paste(vdata$sno,vdata$visitid))]
					x[x$visittypeid==4,]
				},
				vdata=visits.data)
		crf10.data<-
			lapply(
				crf.data,
				function(x,vdata)
				{
					x$visittypeid<-vdata$visittypeid[match(paste(x$sno,x$visitid),paste(vdata$sno,vdata$visitid))]
					x[x$visittypeid==10,]
				},
				vdata=visits.data)
		crf1000.data<-
			lapply(
				crf.data,
				function(x,vdata)
				{
					x$visittypeid<-vdata$visittypeid[match(paste(x$sno,x$visitid),paste(vdata$sno,vdata$visitid))]
					x[x$visittypeid==1000,]
				},
				vdata=visits.data)

	# IMPORT PRIV DATA
		priv.data<-snap.read(privData,"web","PD1_Stats")

	# IMPORT SIMD DATA
		simd.data<-snap.read(privData,"stats",simd.table)

	# IMPORT CEC DATA
		cec.data<-snap.read(dataBase,"dm",cec.table)

	# IMPORT ECG DATA
		ecg.data<-snap.read(dataBase,"dm",ecg.table)

	# IMPORT CHEST CT DATA
		chestct.data<-snap.read(dataBase,"dm",chestct.table)

	# IMPORT DERIVED CTCA DATA
		temp1<-snap.read(dataBase,"dm",ctca1.table)
		temp2<-snap.read(dataBase,"dm",ctca2.table)

		temp1<-as.data.frame(lapply(temp1,as.numeric))
		temp2<-as.data.frame(lapply(temp2,as.numeric))
		names(temp2)<-names(temp1)

		ctca.data<-rbind(temp1,temp2)

	# IMPORT RAW CTCA DATA

		ctca.raw1<-snap.read(dataBase,"dm",ctca3.table)
		ctca.raw2<-snap.read(dataBase,"dm",ctca4.table)

	# IMPORT CMR DATA
		cmr.data<-snap.read(dataBase,"dm",cmr.table)

	# IMPORT T1 DATA
		t1.data<-snap.read(dataBase,"dm",t1.table)

	# IMPORT T2 DATA
		t2.data<-snap.read(dataBase,"dm",t2.table)

	# IMPORT CORE BLOODS BIOMARKER DATA
		blood1.data<-snap.read(dataBase,"dm",blood1.table)
		blood2.data<-snap.read(dataBase,"dm",blood2.table)
		blood.data<-rbind(blood1.data,blood2.data)

		blood3.data<-snap.read(dataBase,"dm",blood3.table)

		blood4.data<-snap.read(dataBase,"dm",blood4.table)
		blood5.data<-snap.read(dataBase,"dm",blood5.table)

	# IMPORT CORE BLOODS COAGULATION DATA
		coag.data1<-snap.read(dataBase,"dm",coag.table1)
		coag.data2<-snap.read(dataBase,"dm",coag.table2)
		coag.data3<-snap.read(dataBase,"dm",coag.table3)
		coag.data4<-snap.read(dataBase,"dm",coag.table4)
		coag.data5<-snap.read(dataBase,"dm",coag.table5)
		coag.data6<-snap.read(dataBase,"dm",coag.table6)

	# IMPORT EXTRA DATA (PCI/CABG, HYPERCHOLESTEROLAEMIA, PEAK SODIUM)
		extra1.data<-snap.read(dataBase,"dm",extra1.table)

	# IMPORT CONTROL HCW DATA
		hcw.data<-snap.read(dataBase,"dm",hcw.table)

	# IMPORT RENAL DATA
		renal.data<-snap.read(dataBase,"dm",renal.table)

	# IMPORT MBF DATA
		mbf.data<-snap.read(dataBase,"dm",mbf.table)

	# IMPORT CREATININE DATA
		creat.data<-snap.read(dataBase,"dm",creat.table)

	# IMPORT RDW DATA
		rdw.data<-snap.read(dataBase,"dm",rdw.table)

	# IMPORT LGE DATA
		lge.data<-snap.read(dataBase,"dm",lge.table)

	# IMPORT V3 OUTCOMES
		thromboembolism.data<-snap.read(dataBase,"dm",thromboembolism.table)
		events.data<-snap.read(dataBase,"dm",events.table)
		medications.data<-snap.read(dataBase,"dm",medications.table)
		outpatient.data<-snap.read(dataBase,"dm",outpatient.table)
		secondarycare.data<-snap.read(dataBase,"dm",secondarycare.table)
		vitalstatus.data<-snap.read(dataBase,"dm",vitalstatus.table)
		inhospevents.data<-snap.read(dataBase,"dm",inhospevents.table)

	# IMPORT DIABETES DRUG DATA
		ddrugs.data<-snap.read(dataBase,"dm",ddrugs.table)

	# IMPORT MYOCARDITIS CRITERIA
		criteria.data<-snap.read(dataBase,"dm",criteria.table)


# find0

########################
#                      #
# CREATE ANALYSIS SETS #
#                      #
########################

	# START DATA SPEC
		spec<-list()
		spec.cec<-list()

	# FUNCTION TO ADD LINE TO DATA SPEC
		add.spec<-function(old.spec,varname,vardesc,varformat,assumptions="")
			c(old.spec,list(c(varname,vardesc,varformat,assumptions)))

	# START WITH ALL SUBJECTS
		cisco<-data.frame(id=subjects.data$sno)
		spec<-add.spec(spec,"id","Subject ID","Text")

	# FLAG AS COVID OR CONTROL
		cisco$group<-factor(crf1.data$ptg$ptg001,2:1,c("Control","COVID-19"))[match(cisco$id,crf1.data$ptg$sno)]
		spec<-add.spec(spec,"group","COVID or control","Text","Control, COVID-19")

	# DATE OF DISCHARGE
		cisco$disch_date<-chron(paste(priv.data$pd1018d,priv.data$pd1018m,priv.data$pd1018y,sep="/"),format="d/m/y",out.format="d/m/y")[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"disch_date","Date of discharge","Date","DD/MM/YY")

	# DATE OF VISIT 1 (ENROLMENT)
		cisco$v1_date<-
			chron(paste(visits.data$visitd,visits.data$visitm,visits.data$visity,sep="/"),format="d/m/y",out.format="d/m/y")[match(paste(cisco$id,1),paste(visits.data$sno,visits.data$visittypeid))]
		spec<-add.spec(spec,"v1_date","Date of Visit 1","Date","DD/MM/YY")

	# DATE OF VISIT 2 (MRI)
		cisco$v2_date<-
			chron(paste(visits.data$visitd,visits.data$visitm,visits.data$visity,sep="/"),format="d/m/y",out.format="d/m/y")[match(paste(cisco$id,2),paste(visits.data$sno,visits.data$visittypeid))]
		spec<-add.spec(spec,"v2_date","Date of Visit 2","Date","DD/MM/YY")

	# DATE OF SYMPTOM ONSET
		cisco$onset_date<-chron(paste(priv.data$pd1008d,priv.data$pd1008m,priv.data$pd1008y,sep="/"),format="d/m/y",out.format="d/m/y")[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"onset_date","Date of symptom onset","Date","DD/MM/YY")

	# DATE OF COVID DIAGNOSIS
		cisco$cdiag_date<-chron(paste(priv.data$pd1009d,priv.data$pd1009m,priv.data$pd1009y,sep="/"),format="d/m/y",out.format="d/m/y")[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"cdiag_date","Date of COVID-19 diagnosis","Date","DD/MM/YY")

	# DATE OF ADMISSION
		cisco$adm_date<-chron(paste(priv.data$pd1007d,priv.data$pd1007m,priv.data$pd1007y,sep="/"),format="d/m/y",out.format="d/m/y")[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"adm_date","Date of admission","Date","DD/MM/YY")

	# HOSPITALISED (OVERNIGHT STAY)
		cisco$hosp<-factor(cisco$disch_date>cisco$adm_date,c(T,F),c("Yes","No"))
		spec<-add.spec(spec,"hosp","Hospitalised (overnight stay)","Text","Yes, No")
		
	# DIAGNOSIS

		# CREATE CEC DIAGNOSIS DATASET
			cec<-data.frame(id=substring(100000+as.numeric(cec.data$"study id"),2,6))
			spec.cec<-add.spec(spec.cec,"id","Subject ID","Text")

		# REVIEWER
			cec$reviewer<-cec.data$reviewer
			spec.cec<-add.spec(spec.cec,"reviewer","Reviewer name","Text")

		# GROUP - WILL DROP LATER
			cec$group<-as.numeric(cec.data$group)

		# PRIMARY OUTCOME (LIKELIHOOD OF MYOCARDITIS)
			cec$primary<-factor(as.numeric(cec.data$"likelihood of myocardial inflammation (myocarditis)"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"primary","Primary diagnosis: likelihood of myocarditis","Text","Not, Unlikely, Probably, Very")

		# CERTAINTY OF PRIMARY DIAGNOSIS
			cec$certainty<-factor(as.numeric(cec.data$"certainty of final primary diagnosis1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"primary","Certainty of primary diagnosis","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: MYOCARDIAL INJURY
			cec$sec_myoinj<-factor(as.logical(cec.data$" myocardial injury1"),c(T,F),c("Yes","No"))
			spec.cec<-add.spec(spec.cec,"sec_myoinj","Secondary diagnosis: Myocaridal injury","Text","Yes, No")

		# SECONDARY OUTCOMES: CHRONICICTY OF MYOCARDIAL INJURY
			cec$sec_myoinjchron<-factor(as.numeric(cec.data$"chronicity"),1:2,c("Acute","Chronic"))
			spec.cec<-add.spec(spec.cec,"sec_myoinjchron","Secondary diagnosis: Chronicity of myocaridal injury","Text","Acute, Chronic")

		# SECONDARY OUTCOMES: ACS
			cec$sec_acs<-factor(as.logical(cec.data$"acs?1"),c(T,F),c("Yes","No"))
			spec.cec<-add.spec(spec.cec,"sec_acs","Secondary diagnosis: ACS","Text","Yes, No")

		# SECONDARY OUTCOMES: LIKELIHOOD OF SARS-COV-2 MYOCARDITIS
			cec$sec_sars<-factor(as.numeric(cec.data$"likelihood of sars-cov-2 myocarditis 1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_sars","Secondary diagnosis: likelihood of SARS-COV-2 myocarditis","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: LIKELIHOOD OF ACUTE STRESS CARDIOMYOPATHY
			cec$sec_asc<-factor(as.numeric(cec.data$"likelihood of acute stress cardiomyopathy1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_acs","Secondary diagnosis: likelihood of acute stress cardiomyopathy","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: LIKELIHOOD OF ISCHAEMIA/IMPAIRED PERFUSION AS STRESSOR OF INFLAMMATION
			cec$sec_isch<-factor(as.numeric(cec.data$"likelihood of ischaemia/impaired perfusion as a stressor of inf1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_isch","Secondary diagnosis: likelihood of ischaemia/impaired perfusion as a stressor of inflammation","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: LIKELIHOOD OF INFECTIVE MYOPERICARDITIS (NON-COVID INFECTION)
			cec$sec_noncov<-factor(as.numeric(cec.data$"likelihood of infective myopericarditis (non-covid infection)1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_noncov","Secondary diagnosis: likelihood of infective myopericarditis (non-COVID infection)","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: LIKELIHOOD OF DRUG-INDUCED (TOXIC) MYOCARDIAL INFLAMMATION
			cec$sec_drug<-factor(as.numeric(cec.data$"likelihood of drug-induced (toxic) myocardial inflammation1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_drug","Secondary diagnosis: likelihood of drug-induced (toxic) myocardial inflammation","Text","Not, Unlikely, Probably, Very")

		# SECONDARY OUTCOMES: LIKELIHOOD OF IDIOPATHIC MYOCARDIAL +/- PERICARDIAL INFLAMMATION
			cec$sec_idio<-factor(as.numeric(cec.data$"likelihood of idiopathic myocardial ± pericardial inflammation1"),1:4,c("Not","Unlikely","Probably","Very"))
			spec.cec<-add.spec(spec.cec,"sec_idio","Secondary diagnosis: likelihood of idiopathic myocardial &plusminus; pericardial inflammation1","Text","Not, Unlikely, Probably, Very")

		# KEEP THOSE IN CISCO DATA	
			cec<-cec[is.element(cec$id,cisco$id),]

		# KEEP FIRST READS
			cec<-cec[cec$group==1,]
			cec$group<-NULL

		# FUNCTION TO DERIVE CONSENSUS VIEW
			consensus<-function(x,id,target.id)
			{
				factor(unlist(by(as.numeric(x),factor(as.character(id),as.character(target.id)),median)),1:length(levels(x)),levels(x))
			}

		# ADD PRIMARY OUTCOME
			cisco$primary<-consensus(cec$primary,cec$id,cisco$id)
			spec<-add.spec(spec,"primary","Primary outcome: likelihood of myocarditis (CEC consensus)","Text","Not, Unlikely, Probably, Very")

		# ADD BINARY PRIMARY OUTCOME (>= probably)
			cisco$primary_bin<-factor(as.numeric(cisco$primary)>2.5,c(F,T),c("No","Yes"))
			spec<-add.spec(spec,"primary_bin","Primary outcome (binary version): at least probable likelihood of myocarditis","Text","No, Yes")

		# ADD SECONDARY OUTCOMES
			cisco$sec_myoinj<-consensus(cec$sec_myoinj,cec$id,cisco$id)
			cisco$sec_sars<-consensus(cec$sec_sars,cec$id,cisco$id)
			cisco$sec_asc<-consensus(cec$sec_asc,cec$id,cisco$id)
			cisco$sec_isch<-consensus(cec$sec_isch,cec$id,cisco$id)
			cisco$sec_noncov<-consensus(cec$sec_noncov,cec$id,cisco$id)
			cisco$sec_drug<-consensus(cec$sec_drug,cec$id,cisco$id)
			cisco$sec_idio<-consensus(cec$sec_idio,cec$id,cisco$id)

			spec<-add.spec(spec,"myoinj","Secondary diagnosis: myocardial injury (CEC consensus)","Text","Yes, No")
			spec<-add.spec(spec,"sec_sars","Secondary diagnosis: likelihood of SARS-COV-2 myocarditis (CEC consensus)","Text","Not, Unlikely, Probably, Very")
			spec<-add.spec(spec,"sec_acs","Secondary diagnosis: likelihood of acute stress cardiomyopathy (CEC consensus)","Text","Not, Unlikely, Probably, Very")
			spec<-add.spec(spec,"sec_isch","Secondary diagnosis: likelihood of ischaemia/impaired perfusion as a stressor of inflammation (CEC consensus)","Text","Not, Unlikely, Probably, Very")
			spec<-add.spec(spec,"sec_noncov","Secondary diagnosis: likelihood of infective myopericarditis (non-COVID infection) (CEC consensus)","Text","Not, Unlikely, Probably, Very")
			spec<-add.spec(spec,"sec_drug","Secondary diagnosis: likelihood of drug-induced (toxic) myocardial inflammation (CEC consensus)","Text","Not, Unlikely, Probably, Very")
			spec<-add.spec(spec,"sec_idio","Secondary diagnosis: likelihood of idiopathic myocardial &plusminus; pericardial inflammation1 (CEC consensus)","Text","Not, Unlikely, Probably, Very")

		# ADD CONSENSUS PRIMARY OUTCOME BACK INTO CEC DATA
			cec$primary_consensus<-cisco$primary[match(cec$id,cisco$id)]
			spec.cec<-add.spec(spec.cec,"primary_consensus","Primary outcome: likelihood of myocarditis (CEC consensus)","Text","Not, Unlikely, Probably, Very")

		# FLAG POPULATIONS
			cisco$paper<-factor((cisco$group=="Control")|!is.na(cisco$primary),c(F,T),c("No","Yes"))
			spec<-add.spec(spec,"paper","Included in paper?","Text","Control, or with primary outcome.<br>No, Yes")

		# INDIVIDUAL CRITERIA FOR MAKING DIAGNOSIS

			crit<-criteria.data
			crit$id<-substring(100000+as.numeric(crit$study_id),2,6)

			cisco$crit_ecg_myo<-factor(crit$ecg_myocarditis=="Yes",c(F,T),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_ecg_myo<-cisco$crit_ecg_myo[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_ecg_myo",
					paste(
						"Myocarditis criteria:",
						"ECG/Holter/stress test features - Newly abnormal 12 lead ECG and/or Holter and/or stress testing, any of the following:",
						"I to III degree atrioventricular block, or bundle branch block, ST/T wave change (ST elevation or non ST elevation, T wave inversion),",
						"sinus arrest, ventricular tachycardia or fibrillation and asystole, atrial fibrillation, reduced R wave height,",
						"intraventricular conduction delay (widened QRS complex), abnormal Q waves, low voltage, frequent premature beats, supraventricular tachycardia"),
					"Text","No, Yes; not done classified as No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_ecg_myo",
					paste(
						"Myocarditis criteria:",
						"ECG/Holter/stress test features - Newly abnormal 12 lead ECG and/or Holter and/or stress testing, any of the following:",
						"I to III degree atrioventricular block, or bundle branch block, ST/T wave change (ST elevation or non ST elevation, T wave inversion),",
						"sinus arrest, ventricular tachycardia or fibrillation and asystole, atrial fibrillation, reduced R wave height,",
						"intraventricular conduction delay (widened QRS complex), abnormal Q waves, low voltage, frequent premature beats, supraventricular tachycardia"),
					"Text","No, Yes; not done classified as No")

			cisco$crit_ch_pain<-factor(crit$chest_pain,c("No","Yes"),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_ch_pain<-cisco$crit_ch_pain[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_ch_pain",
					paste(
						"Myocarditis criteria:",
						"Acute chest pain, pericarditic, or pseudo-ischaemic?"),
					"Text","No, Yes")
			spec.cec<-
				add.spec(
					spec.cec,"crit_ch_pain",
					paste(
						"Myocarditis criteria:",
						"Acute chest pain, pericarditic, or pseudo-ischaemic?"),
					"Text","No, Yes")

			cisco$crit_t2<-factor(pmax(0,as.numeric(crit$t2_criteria),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_t2<-cisco$crit_t2[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_t2",
					paste(
						"Myocarditis criteria:",
						"T2 criteria (T2 map or T2 ratio)"),
					"Text","No, Yes; missing value recorded as No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_t2",
					paste(
						"Myocarditis criteria:",
						"T2 criteria (T2 map or T2 ratio)"),
					"Text","No, Yes; missing value recorded as No")

			cisco$crit_t1<-factor(pmax(0,as.numeric(crit$t1_criteria),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_t1<-cisco$crit_t1[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_t1",
					paste(
						"Myocarditis criteria:",
						"T1 criteria (T1 map, ECV or LGE)"),
					"Text","No, Yes; missing value recorded as No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_t1",
					paste(
						"Myocarditis criteria:",
						"T1 criteria (T1 map, ECV or LGE)"),
					"Text","No, Yes; missing value recorded as No")

			cisco$crit_llc<-factor(pmax(0,as.numeric(crit$llc_num_met),na.rm=T),0:2,c("0 of 2","1 of 2","2 of 2"))[match(cisco$id,crit$id)]
			cec$crit_llc<-cisco$crit_llc[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_llc",
					paste(
						"Myocarditis criteria:",
						"Modified Lake Louise Criteria"),
					"Text","0 of 2, 1 of 2, 2 of 2; missing value recorded as 0")
			spec.cec<-
				add.spec(
					spec.cec,"crit_llc",
					paste(
						"Myocarditis criteria:",
						"Modified Lake Louise Criteria"),
					"Text","0 of 2, 1 of 2, 2 of 2; missing value recorded as 0")

			cisco$crit_trop<-factor(crit$troponin_elevated=="Yes",c(F,T),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_trop<-cisco$crit_trop[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_trop",
					paste(
						"Myocarditis criteria:",
						"troponin elevated by sex normal reference ranges"),
					"Text","No, Yes; not done coded as No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_trop",
					paste(
						"Myocarditis criteria:",
						"troponin elevated by sex normal reference ranges"),
					"Text","No, Yes; not done coded as No")

			cisco$crit_new_sob<-factor(crit$new_sob,c("No","Yes"),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_new_sob<-cisco$crit_new_sob[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_new_sob",
					paste(
						"Myocarditis criteria:",
						"New-onset (days up to 3 months) or worsening of: dyspnoea at rest or exercise, and/or fatigue, with or without left and/or right heart failure signs"),
					"Text","No, Yes")
			spec.cec<-
				add.spec(
					spec.cec,"crit_new_sob",
					paste(
						"Myocarditis criteria:",
						"New-onset (days up to 3 months) or worsening of: dyspnoea at rest or exercise, and/or fatigue, with or without left and/or right heart failure signs"),
					"Text","No, Yes")

			cisco$crit_chron_sob<-factor(crit$chronic_sob,c("No","Yes"),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_chron_sob<-cisco$crit_chron_sob[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_chron_sob",
					paste(
						"Myocarditis criteria:",
						"Subacute/chronic (&gt;3 months) or worsening of: dyspnoea at rest or exercise, and/or fatigue, with or without left and/or right heart failure signs"),
					"Text","No, Yes")
			spec.cec<-
				add.spec(
					spec.cec,"crit_chron_sob",
					paste(
						"Myocarditis criteria:",
						"Subacute/chronic (&gt;3 months) or worsening of: dyspnoea at rest or exercise, and/or fatigue, with or without left and/or right heart failure signs"),
					"Text","No, Yes")

			cisco$crit_cardiogenic_shock<-factor(crit$cardiogenic_shock,c("No","Yes"),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_cardiogenic_shock<-cisco$crit_cardiogenic_shock[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_cardiogenic_shock",
					paste(
						"Myocarditis criteria:",
						"Unexplained cardiogenic shock"),
					"Text","No, Yes")
			spec.cec<-
				add.spec(
					spec.cec,"crit_cardiogenic_shock",
					paste(
						"Myocarditis criteria:",
						"Unexplained cardiogenic shock"),
					"Text","No, Yes")

			cisco$crit_palp_sync_arrh<-factor(crit$palp_syncope_arrhythmia,c("No","Yes"),c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_palp_sync_arrh<-cisco$crit_palp_sync_arrh[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_palp_sync_arrh",
					paste(
						"Myocarditis criteria:",
						"Palpitation, and/or unexplained arrhythmia symptoms and/or syncope, and/or aborted sudden cardiac death"),
					"Text","No, Yes")
			spec.cec<-
				add.spec(
					spec.cec,"crit_palp_sync_arrh",
					paste(
						"Myocarditis criteria:",
						"Palpitation, and/or unexplained arrhythmia symptoms and/or syncope, and/or aborted sudden cardiac death"),
					"Text","No, Yes")

			cisco$crit_glob_lv_dys<-factor(pmax(0,as.logical(crit$global_lv_dysfunction),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_glob_lv_dys<-cisco$crit_glob_lv_dys[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_glob_lv_dys",
					paste(
						"Myocarditis criteria:",
						"Global LV systolic dysfunction"),
					"Text","No, Yes; missing values coded at No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_glob_lv_dys",
					paste(
						"Myocarditis criteria:",
						"Global LV systolic dysfunction"),
					"Text","No, Yes; missing values coded at No")

			cisco$crit_reg_lv_dys<-factor(pmax(0,as.logical(crit$regional_lv_dysfunction),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_reg_lv_dys<-cisco$crit_reg_lv_dys[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_reg_lv_dys",
					paste(
						"Myocarditis criteria:",
						"Regional LV systolic dysfunction"),
					"Text","No, Yes; missing values coded at No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_reg_lv_dys",
					paste(
						"Myocarditis criteria:",
						"Regional LV systolic dysfunction"),
					"Text","No, Yes; missing values coded at No")

			cisco$crit_peri_ch<-factor(pmax(0,as.logical(crit$pericardial_changes),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_peri_ch<-cisco$crit_peri_ch[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_peri_ch",
					paste(
						"Myocarditis criteria:",
						"Pericardial effusion/thickening?"),
					"Text","No, Yes; missing values coded at No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_peri_ch",
					paste(
						"Myocarditis criteria:",
						"Pericardial effusion/thickening?"),
					"Text","No, Yes; missing values coded at No")

			cisco$crit_new_lv_dys<-factor(pmax(0,as.logical(crit$new_lv_dysfunction),na.rm=T),0:1,c("No","Yes"))[match(cisco$id,crit$id)]
			cec$crit_new_lv_dys<-cisco$crit_new_lv_dys[match(cec$id,cisco$id)]
			spec<-
				add.spec(
					spec,"crit_new_lv_dys",
					paste(
						"Myocarditis criteria:",
						"New or unexplained LV systolic dysfunction?",
						"Derived from global/regional dysfunction variable above with the addition of clinical interpretation to whether it is new or not"),
					"Text","No, Yes; missing values coded at No")
			spec.cec<-
				add.spec(
					spec.cec,"crit_new_lv_dys",
					paste(
						"Myocarditis criteria:",
						"New or unexplained LV systolic dysfunction?",
						"Derived from global/regional dysfunction variable above with the addition of clinical interpretation to whether it is new or not"),
					"Text","No, Yes; missing values coded at No")

			cisco$crit_num_clin<-
				apply(
					cbind(
						cisco$crit_ch_pain=="Yes",
						cisco$crit_new_sob=="Yes",
						cisco$crit_chron_sob=="Yes",
						cisco$crit_cardiogenic_shock=="Yes",
						cisco$crit_palp_sync_arrh=="Yes"),
					1,sum,na.rm=T)
			cec$crit_num_clin<-cisco$crit_num_clin[match(cec$id,cisco$id)]
			cisco$crit_num_clin_cat<-factor(cisco$crit_num_clin)
			cec$crit_num_clin_cat<-cisco$crit_num_clin_cat[match(cec$id,cisco$id)]

			spec<-add.spec(spec,"crit_num_clin","Number of clinical myocarditis criteria","Integer")
			spec<-add.spec(spec,"crit_num_clin_cat","Number of clinical myocarditis criteria, categorised","Text","0, 1-2, 3-4, 5-6, 7+")

			cisco$crit_num_diag<-
				apply(
					cbind(
						cisco$crit_ecg_myo=="Yes",
						cisco$crit_t2=="Yes",
						cisco$crit_t1=="Yes",
						cisco$crit_llc=="1 of 2",
						2*(cisco$crit_llc=="2 of 2"),
						cisco$crit_trop=="Yes",
						cisco$crit_glob_lv_dys=="Yes",
						cisco$crit_reg_lv_dys=="Yes",
						cisco$crit_peri_ch=="Yes",
						cisco$crit_new_lv_dys=="Yes"),
					1,sum,na.rm=T)
			cec$crit_num_diag<-cisco$crit_num_diag[match(cec$id,cisco$id)]

			cisco$crit_num_diag_cat<-
				factor(
					c(1,2,2,3,3,4,4,5)[1+cisco$crit_num_diag],
					1:5,
					c("0","1-2","3-4","5-6","7+"))
			cec$crit_num_diag_cat<-cisco$crit_num_diag_cat[match(cec$id,cisco$id)]

			spec<-add.spec(spec,"crit_num_diag","Number of diagnostic myocarditis criteria","Integer")
			spec<-add.spec(spec,"crit_num_diag_cat","Number of diagnostic myocarditis criteria, categorised","Text","0, 1-2, 3-4, 5-6, 7+")


	# AGE
		cisco$age<-
			as.numeric(priv.data$pd1001[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"age","Age in years","Integer")

		cisco$age_group<-
			cut(cisco$age,c(0,44.9999,54.9999,64.9999,1000),c("&lt;45","45-54","55-64","&ge;65"))
		spec<-add.spec(spec,"age_group","Age groups","Text","&lt;45, 45-54, 55-64, &ge;65")

	# SEX
		cisco$sex<-
			factor(priv.data$pd1002,1:2,c("Male","Female"))[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"sex","Sex","Text","Male, Female")

	# RACE

		cisco$race_pd<-
			factor(
				priv.data$pd1006,1:7,c("Arab","Black","East Asian","South Asian","West Asian","Latin American","White"))[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"race_pd","Race (patient detail question)","Text","'Arab', 'Black', 'East Asian', 'South Asian', 'West Asian', 'Latin American', 'White'")

		cisco$race_j<-
			factor(
				crf1.data$jbs$jbs004,c(1:4,8,5:7,9),c("White","Indian","Pakistani","Bangladeshi","Other Asian","Black Caribbean","Black African","Chinese","Other"))[match(cisco$id,crf1.data$jbs$sno)]
		spec<-add.spec(spec,"race_j","Race (JBS3 question)","Text","'White', 'Indian', 'Pakistani', 'Bangladeshi', 'Black Caribbean', 'Black African', 'Chinese', 'Other Asian', 'Other'")

		cisco$race_3<-
			factor(
				c(3,3,2,2,2,3,1)[as.numeric(priv.data$pd1006)],1:3,c("White","Asian","Other"))[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"race_3","Race (patient detail question, collapsed)","Text","White, Asian, Other")

		cisco$race_2<-
			factor(
				c(2,2,2,2,2,2,1)[as.numeric(priv.data$pd1006)],1:2,c("White","Other"))[match(cisco$id,priv.data$sno)]
		spec<-add.spec(spec,"race_2","Race (patient detail question, binary)","Text","White, Other")

	# SIMD DEPRIVATION
		cisco$simd<-
			factor(as.numeric(simd.data$"simd 2020v2 quintile"[match(cisco$id,simd.data$sno)]),1:5,c("Q1 - Most Deprived","Q2","Q3","Q4","Q5 - Least Deprived"))
		spec<-add.spec(spec,"simd","SIMD Quintile","Text","'Q1 - Most Deprived', 'Q2', 'Q3', 'Q4', 'Q5 - Least Deprived'")

	# TOWNSEND DEPRIVATION
		cisco$townsend<-
			round(as.numeric(crf1.data$har_restricted$townsend_tds[match(cisco$id,crf1.data$har_restricted$sno)]),4)
		spec<-add.spec(spec,"townsend","Townsend score","Numeric","4dp")

	# HEALTHCARE WORKER
		hcw.covid<-
			factor(crf1.data$cov$cov003,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		hcw.control<-
			factor(as.numeric(hcw.data$"hcw (0 - no, 1 - yes)"),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(hcw.data$id),2,6))]
		cisco$hcw<-factor(ifelse(cisco$group=="Control",hcw.control,hcw.covid),1:2,c("Yes","No"))
		spec<-add.spec(spec,"hcw","Healthcare worker","Text","Yes, No")

	# HEIGHT, WEIGHT, BMI
		cisco$height<-
			as.numeric(priv.data$pd1003[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"height","Height in cm","Numeric","1dp")

		cisco$weight<-
			as.numeric(priv.data$pd1004[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"weight","Weight in kg","Numeric","1dp")

		cisco$bmi<-
			round(cisco$weight/((cisco$height/100)^2),4)
		spec<-add.spec(spec,"bmi","BMI in kg/m<sup>2</sup>","Numeric","4dp")

	# PRESENTING CHARACTERISTICS

	# HEART RATE
		cisco$hr<-
			as.numeric(priv.data$pd1011[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"hr","Heart rate in bpm","Integer")

	# SBP
		cisco$sbp<-
			as.numeric(priv.data$pd1010s[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"sbp","SBP in mmHg","Integer")

	# DBP
		cisco$dbp<-
			as.numeric(priv.data$pd1010d[match(cisco$id,priv.data$sno)])
		spec<-add.spec(spec,"dbp","DBP in mmHg","Integer")

	# OXYGEN SATURATION
		cisco$oxysat<-
			as.numeric(crf1.data$chu$chu020[match(cisco$id,crf1.data$chu$sno)])
		spec<-add.spec(spec,"oxysat","Oxygen saturation, %","Integer")

	# RESPIRATORY RATE
		cisco$rr<-
			as.numeric(crf1.data$chu$chu019[match(cisco$id,crf1.data$chu$sno)])
		spec<-add.spec(spec,"rr","Respiratory rate, bpm","Integer")

	# WHO CLINICAL SEVERITY SCORE
		cisco$who_full<-
			factor(
				crf1.data$osc$osc002,
				0:8,
				c(
					"No evidence of infection",
					"No limitation of activities",
					"Limitation of activities",
					"Hospitalized, no oxygen therapy",
					"Oxygen by mask or nasal prongs",
					"Non-invasive ventilation",
					"Mechanical ventilation",
					"Ventilation with organ support",
					"Death"))[match(cisco$id,crf1.data$osc$sno)]
		spec<-add.spec(
			spec,"who_full","WHO clinical severity score, in full","Text",
			paste(
				"'No evidence of infection', 'No limitation of activities', 'Limitation of activities', 'Hospitalized, no oxygen therapy',",
				"'Oxygen by mask or nasal prongs', 'Non-invasive ventilation', 'Mechanical ventilation',",
				"'Ventilation with organ support', 'Death'"))

		cisco$who_short<-
			factor(
				c(NA,NA,NA,1,2,3,4,4,NA)[1+crf1.data$osc$osc002],
				1:4,
				c(
					"Hospitalized, no oxygen therapy",
					"Oxygen by mask or nasal prongs",
					"Non-invasive ventilation",
					"Mechanical ventilation"))[match(cisco$id,crf1.data$osc$sno)]
		spec<-add.spec(
			spec,"who_short","WHO clinical severity score, short version (missing for controls, combining mechanical ventilation groups)","Text",
			"'Hospitalized, no oxygen therapy', 'Oxygen by mask or nasal prongs', 'Non-invasive ventilation', 'Mechanical ventilation'")

	# PCR DIAGNOSIS
		cisco$pcr<-factor(crf1.data$cov$cov023==1,c(T,F),c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		cisco$pcr[cisco$group=="Control"]<-"No"
		spec<-add.spec(spec,"pcr","PCR diagnosis","Text","Yes, No<br>'Not done' classed as 'No'<br>'No' for controls")

	# NOSOCOMIAL (IN HOSPITAL) INFECTION
		cisco$nosocomial<-factor(crf1.data$cov$cov002,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		cisco$nosocomial[cisco$group=="Control"]<-"No"
		spec<-add.spec(spec,"nosocomial","Nosocomial (in hospital) infection","Text","Yes, No<br>'No' for controls")

	# ANTIBODY TEST
		cisco$antibody<-factor(cisco$group=="Control",c(T,F),c("Yes","No"))
		spec<-add.spec(spec,"antibody","Confirmation by antibody test","Text","Yes, No<br>By definition, 'No' for COVID patients, 'Yes' for controls")

	# CHEST XRAY / CT
		cisco$chest<-
			factor(crf1.data$sct$scth08,1:4,c("Typical of COVID-19","Atypical of COVID-19","Unlikely","Normal"))[match(cisco$id,crf1.data$sct$sno)]
		cisco$chest[cisco$group=="Control"]<-NA
		spec<-add.spec(spec,"chest","Chest xray / CT scan","Text","'Typical of COVID-19','Atypical of COVID-19','Unlikely','Normal'<br>Missing if not done, and for controls")

	# COVID-19 TREATMENT

	# OXYGEN
		cisco$oxygen<-
			factor(crf1.data$cov$cov006,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"oxygen","COVID-19 treatment: oxygen","Text","Yes, No<br>Missing for controls")
		
	# NON-INVASIVE RESPIRATORY SUPPORT
		cisco$ni_resp<-
			factor(crf1.data$cov$cov007,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"ni_resp","COVID-19 treatment: non-invasive respiratory support","Text","Yes, No<br>Missing for controls")
		
	# INVASIVE VENTILATION
		cisco$inv_vent<-
			factor(crf1.data$cov$cov008,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"inv_vent","COVID-19 treatment: invasive ventilation","Text","Yes, No<br>Missing for controls")
		
	# INTRAVENOUS INOTROPE
		cisco$iv_ino<-
			factor(crf1.data$cov$cov010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"iv_ino","COVID-19 treatment: IV inotrope","Text","Yes, No<br>Missing for controls")
		
	# ANTIVIRAL
		cisco$antiviral<-
			factor(crf1.data$cov$cov014,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"antiviral","COVID-19 treatment: antiviral","Text","Yes, No<br>Missing for controls")
		
	# STEROID
		cisco$steroid<-
			factor(crf1.data$cov$cov016,1:2,c("Yes","No"))[match(cisco$id,crf1.data$cov$sno)]
		spec<-add.spec(spec,"steroid","COVID-19 treatment: steroid","Text","Yes, No<br>Missing for controls")
	
	# INTENSIVE CARE
		cisco$icu<-
			factor(
				(priv.data$pd1015==7)|(priv.data$pd1016==7)|(priv.data$pd1017==6),
				c(T,F),c("Yes","No"))[match(cisco$id,priv.data$sno)]
		cisco$icu[cisco$group=="Control"]<-NA
		spec<-add.spec(spec,"icu","COVID-19 treatment: ICU","Text","Yes, No<br>Missing for controls")

	# CARDIOVASCULAR HISTORY

	# HYPERTENSION

		cisco$hyperten<-
			factor(crf1.data$har$har019,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
		spec<-
			add.spec(
				spec,"hyperten","History of hypertension","Text",
				"Yes, No<br>Defined as record of taking antihypertensive medication in HAR (ignores possible causes of myocardial injury)")

	# RENAL IMPAIRMENT
		cisco$renal_i<-
			factor(crf1.data$isa$isa003,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
		spec<-
			add.spec(spec,"renal_i","Renal impairment/CKD (based on ISARIC-4c question)","Text","Yes, No")

		cisco$renal_h<-
			factor(crf1.data$har$har012,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
		spec<-
			add.spec(spec,"renal_h","Renal impairment/CKD (based on HAR question)","Text","Yes, No")
		
		cisco$renal_j<-
			factor(crf1.data$jbs$jbs010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$jbs$sno)]
		spec<-
			add.spec(spec,"renal_j","Renal impairment/CKD (based on JBS3 question)","Text","Yes, No")
		
		cisco$renal_c<-
			factor(crf1.data$chu$chu014,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
		spec<-
			add.spec(spec,"renal_c","Renal impairment/CKD (based on Charlson question)","Text","Yes, No")

		cisco$renal<-
			factor(
				apply(cbind(cisco$renal_i=="Yes",cisco$renal_h=="Yes",cisco$renal_j=="Yes",cisco$renal_c=="Yes"),1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T)),
				c(T,F),c("Yes","No"))
		spec<-
			add.spec(spec,"renal","Renal impairment/CKD (based on ISARIC-4c, HAR, JBS3, and Charlson questions)","Text","Yes, No<br>Set to missing ONLY if all parts missing")
		
	# DIABETES
		cisco$diabetes_h<-
			factor(crf1.data$har$har010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
		spec<-
			add.spec(spec,"diabetes","Diabetes (based on HAR question)","Text","Yes, No")
		
		cisco$diabetes_j<-
			factor(crf1.data$jbs$jbs008,1:2,c("Yes","No"))[match(cisco$id,crf1.data$jbs$sno)]
		spec<-
			add.spec(spec,"diabetes","Diabetes (based on JBS3 question)","Text","Yes, No")
		
		cisco$diabetes_c<-
			factor(crf1.data$chu$chu011,1:3,c("None or diet controlled","Uncomplicated","Complicated"))[match(cisco$id,crf1.data$chu$sno)]
		spec<-
			add.spec(
				spec,"diabetes","Diabetes (based on Charlson question)","Text",
				"'None or diet controlled', 'Uncomplicated', 'Complicated'")
		
		cisco$diabetes<-
			factor(
				apply(cbind(cisco$diabetes_h=="Yes",cisco$diabetes_j=="Yes",cisco$diabetes_c!="None or diet controlled"),1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T)),
				c(T,F),c("Yes","No"))
		spec<-
			add.spec(spec,"renal","Diabetes (based on HAR, JBS3, and Charlson questions)","Text","Yes, No<br>Set to missing ONLY if all parts missing")
		
		cisco[
			(cisco$diabetes=="Yes")&
			((cisco$diabetes_h=="No")|(cisco$diabetes_j=="No")|(cisco$diabetes_c=="None or diet controlled")),
			c("id","diabetes","diabetes_h","diabetes_j","diabetes_c")]


	# HYPERCHOLESTEROLAEMIA
		cisco$hyperchol<-
			factor(as.numeric(extra1.data$hypercholesterolaemia),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(extra1.data$'study id'),2,6))]
		spec<-
			add.spec(
				spec,"hyperchol","Hypercholesterolaemia","Text",
				"Yes, No<br>Currently missing - no definition")
		
	# SMOKING
		cisco$smoke_5<-factor(crf1.data$har$har007,1:5,c("Never","Former","Current, &lt;10 per day","Current, &lt;20 per day","Current, &ge;20 per day"))[match(cisco$id,crf1.data$har$sno)]
		spec<-
			add.spec(spec,"smoke_5","Smoking, in 5 categories","Text","'Never', 'Former', 'Current, &lt;10 per day', 'Current, &lt;20 per day', 'Current, &ge;20 per day'")

		cisco$smoke_3<-
			factor(c(1,2,3,3,3)[as.numeric(crf1.data$har$har007)],1:3,c("Never","Former","Current"))[match(cisco$id,crf1.data$har$sno)]
		spec<-
			add.spec(spec,"smoke_3","Smoking, in 3 categories","Text","Never, Former, Current")

	# ANGINA
		cisco$angina<-
			factor(crf1.data$cas$cas010,c(5,1:4),c("No chest pain","Angina (typical)","Angina (atypical)","Non-Anginal","Not available"))[match(cisco$id,crf1.data$cas$sno)]
		spec<-
			add.spec(
				spec,"angina","Angina Diagnosis","Text",
				"'No chest pain', 'Angina (typical)', 'Angina (atypical)', 'Non-Anginal', 'Not available'")

		cisco$ccs<-
			factor(crf1.data$cas$cas020,1:6,c("None","Angina Class I","Angina Class II","Angina Class III","Angina Class IV","Not available"))[match(cisco$id,crf1.data$cas$sno)]
		spec<-
			add.spec(
				spec,"ccs","Canadian Cardiovascular Society Angina Class","Text",
				"'None', 'Angina Class I', 'Angina Class II', 'Angina Class III', 'Angina Class IV', 'Not available'")

		cisco$ccs_bin<-
			factor(c(1,2,2,2,2)[crf1.data$cas$cas020],1:2,c("No Angina","Angina Class I-IV"))[match(cisco$id,crf1.data$cas$sno)]
		spec<-
			add.spec(
				spec,"ccs_bin","CCS Angina Class (Binary)","Text",
				"'No Angina', 'Angina Class I-IV'")

	# MI
		cisco$mi<-
			factor(crf1.data$chu$chu001,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
		spec<-
			add.spec(spec,"mi","MI","Text","Yes, No. Taken from Charlson page of eCRF")
		
	# STROKE/TIA
		cisco$stroke<-
			factor(crf1.data$chu$chu004,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
		spec<-
			add.spec(spec,"stroke","Stroke or TIA","Text","Yes, No. Taken from Charlson page of eCRF")
		
	# PVD
		cisco$pvd<-
			factor(crf1.data$chu$chu003,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
		spec<-
			add.spec(spec,"pvd","PVD","Text","Yes, No. Taken from Charlson page of eCRF")
		
	# CVD
		cvd.har<-apply(crf1.data$har[,c("har006","har013")]==1,1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T))[match(cisco$id,crf1.data$har$sno)]
		cvd.pct<-
			apply(
				crf1.data$pct[,c("pct001","pct002","pct003","pct004","pct005","pct006","pct007","pct008","pct009","pct010","pct011","pct012","pct013")]==1,
				1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T))[match(cisco$id,crf1.data$pct$sno)]
		cvd.chu<-apply(crf1.data$chu[,c("chu001","chu002","chu003","chu004")]==1,1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T))[match(cisco$id,crf1.data$chu$sno)]
		cvd.isa<-crf1.data$isa$isa001[match(cisco$id,crf1.data$isa$sno)]==1
		cisco$cvd<-factor(apply(cbind(cvd.har,cvd.pct,cvd.chu,cvd.isa),1,function(x)if(all(is.na(x))) NA else any(x,na.rm=T)),c(T,F),c("Yes","No"))
		spec<-
			add.spec(spec,"cvd","CVD","Text","Yes, No. Taken from HAR (006, 013), PCT (001-013), CHU (001-004) and ISA (001) pages of eCRF")

	# RISK SCORES

	# ISARIC-4C

		isaric<-
			function(
				age,		# age in years
				sex,		# sex (Male, Female)
				cm_ccd,	# comorbidities: chronic cardiac disease (Yes, No)
				cm_cresp,	# comorbidities: chronic respiratory disease (Yes, No)
				cm_crenal,	# comorbidities: chronic renal disease (Yes, No)
				cm_msld,	# comorbidities: mild-severe liver disease (Yes, No)
				cm_dem,	# comorbidities: dementia (Yes, No)
				cm_cnc,	# comorbidities: chronic neurological conditions (Yes, No)
				cm_ctd,	# comorbidities: connective tissue disease (Yes, No)
				cm_diab,	# comorbidities: diabetes (Yes, No)
				cm_hiv,	# comorbidities: HIV/AIDS (Yes, No)
				cm_mal,	# comorbidities: malignancy (Yes, No)
				cm_obes,	# comorbidities: clinician defined obesity (Yes, No)
				rr,		# respiratory rate /min
				oxysat,	# oxygen saturation in %
				gcs,		# glasgow coma scale
				urea,		# urea in mmol/l
				crp,		# crp in mg/l
				...)		# filler fields to allow use of do.call
			{
				cm_count<-
					apply(data.frame(cm_ccd,cm_cresp,cm_crenal,cm_msld,cm_dem,cm_cnc,cm_ctd,cm_diab,cm_hiv,cm_mal,cm_obes)=="Yes",1,sum)

				score<-
					c(0,2,4,6,7)[as.numeric(cut(age,c(0,49.5,59.5,69.5,79.5,1000)))]+
					(sex=="Male")+
					pmin(2,cm_count)+
					pmax(0,c(0,1,2)[as.numeric(cut(rr,c(0,19.5,29.5,1000)))],na.rm=T)+
					pmax(0,2*(oxysat<92),na.rm=T)+
					pmax(0,2*(gcs<15),na.rm=T)+
					pmax(0,c(0,1,3)[as.numeric(cut(urea,c(0,6.9999,13.9999,1000)))],na.rm=T)+
					pmax(0,c(0,1,2)[as.numeric(cut(crp,c(0,49.9999,99.9999,1000)))],na.rm=T)
			}

		# DEFINE ADDITIONAL VARIABLES REQUIRED

		# COMORBIDITIES

			cisco$cm_ccd<-factor(crf1.data$isa$isa001,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_ccd","ISARIC-4c comorbidity: chronic cardiac disease","Text","Yes, No")

			cisco$cm_cresp<-factor(crf1.data$isa$isa002,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_cresp","ISARIC-4c comorbidity: chronic respiratory disease","Text","Yes, No")

			cisco$cm_crenal<-cisco$renal_i
			spec<-add.spec(spec,"cm_crenal","ISARIC-4c comorbidity: chronic renal disease","Text","Yes, No")

			cisco$cm_msld<-factor(crf1.data$isa$isa004,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_msld","ISARIC-4c comorbidity: mild-severe liver disease","Text","Yes, No")

			cisco$cm_dem<-factor(crf1.data$chu$chu005,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
			spec<-add.spec(spec,"cm_dem","ISARIC-4c comorbidity: dementia ","Text","Yes, No<br>Taken from Charlson page")
		
			cisco$cm_cnc<-factor(crf1.data$isa$isa006,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_cnc","ISARIC-4c comorbidity: chronic neurological conditions","Text","Yes, No")

			cisco$cm_ctd<-factor(crf1.data$isa$isa007,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_ctd","ISARIC-4c comorbidity: connective tissue disease","Text","Yes, No")

			temp<-sort(unique(crf1.data$isa$sno,crf1.data$chu$sno))
			temp2<-temp[crf1.data$isa$isa007[match(temp,crf1.data$isa$sno)]!=crf1.data$chu$chu007[match(temp,crf1.data$chu$sno)]]

			cisco$cm_diab<-factor(crf1.data$har$har010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"cm_diab","ISARIC-4c comorbidity: diabetes","Text","Yes, No<br>Taken for HAR page")

			cisco$cm_hiv<-factor(crf1.data$chu$chu018,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_hiv","ISARIC-4c comorbidity: HIV/AIDS","Text","Yes, No<br>Taken for Charlson page")

			cisco$cm_mal<-factor(crf1.data$isa$isa010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_mal","ISARIC-4c comorbidity: malignancy","Text","Yes, No")

			cisco$cm_obes<-factor(crf1.data$isa$isa011,1:2,c("Yes","No"))[match(cisco$id,crf1.data$isa$sno)]
			spec<-add.spec(spec,"cm_mal","ISARIC-4c comorbidity: clinician defined obesity","Text","Yes, No")

		# GLASGOW COMA SCALE

			cisco$gcs<-as.numeric(crf1.data$chu$chu021[match(cisco$id,crf1.data$chu$sno)])
			spec<-add.spec(spec,"gcs","Glasgow Coma Scale","Integer")

		cisco[cisco$id=="01001",]

	# STANDARD CARE BLOODS

		# FUNCTION TO REPLACE "<" AND ">" VALUES
		# ALSO REPLACE ZEROES WITH HALF LOWEST POSITIVE VALUE

			lab.numeric<-function(x)
			{
				# STRIP WHITE SPACE
					x<-gsub(" ","",x)
				# CONVERT TO NUMERIC
					temp<-as.numeric(x)

				# REPLACE ZEROES WITH HALF LOWEST POSITIVE VALUE
					if(any(temp==0,na.rm=T))
					{
						temp.change<-!is.na(temp)&(temp==0)
						temp[temp.change]<-0.5*min(temp[!is.na(temp)&(temp>0)])
					}

				# IF MISSING PATTERN IS THE SAME, THEN OK TO USE
					if(all(is.na(x)==is.na(temp))) return(temp)

				# IF MISSING PATTERN IS DIFFERENT, FIRST REPLACE ANY ">X" WITH X
					if(any(substring(x,1,1)==">",na.rm=T))
					{
						x.change<-!is.na(x)&(substring(x,1,1)==">")
						temp[x.change]<-as.numeric(gsub(">","",x))[x.change]
					}

				# THEN REPLACE ANY "<X" WITH X/2
					if(any(substring(x,1,1)=="<",na.rm=T))
					{
						x.change<-!is.na(x)&(substring(x,1,1)=="<")
						temp[x.change]<-as.numeric(gsub("<","",x))[x.change]/2
					}

				temp
			}

		# ALBUMIN
			cisco$albumin<-lab.numeric(crf1.data$bld$bld021[match(cisco$id,crf1.data$bld$sno)])
			spec<-add.spec(spec,"albumin","Albumin during index admission, in g/l","Numeric","1dp")

		# UREA
			cisco$urea<-lab.numeric(crf1.data$bld$bld010[match(cisco$id,crf1.data$bld$sno)])
			spec<-add.spec(spec,"urea","Urea during index admission, in mmol/l","Numeric","1dp")

		# TOTAL CHOLESTEROL
			cisco$chol_h<-lab.numeric(crf1.data$har$har016)[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"chol_h","Total cholesterol during index admission (measured in HAR), in mmol/l","Numeric","1dp")

			cisco$chol_b<-lab.numeric(crf1.data$bld$bld043)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"chol_b","Total cholesterol during index admission (measured in bloods), in mmol/l","Numeric","1dp")

			cisco$chol<-ifelse(is.na(cisco$chol_b),cisco$chol_h,cisco$chol_b)
			spec<-add.spec(spec,"chol","Total cholesterol during index admission (use bloods if available, otherwise use HAR), in mmol/l","Numeric","1dp")

		# HDL CHOLESTEROL
			cisco$hdl_h<-lab.numeric(crf1.data$har$har017)[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"hdl_h","HDL cholesterol during index admission (measured in HAR), in mmol/l","Numeric","1dp")

			cisco$hdl_b<-lab.numeric(crf1.data$bld$bld044)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"hdl_b","HDL cholesterol during index admission (measured in bloods), in mmol/l","Numeric","1dp")

			cisco$hdl<-ifelse(is.na(cisco$hdl_b),cisco$hdl_h,cisco$hdl_b)
			spec<-add.spec(spec,"hdl","HDL cholesterol during index admission (use bloods if available, otherwise use HAR), in mmol/l","Numeric","1dp")

		# HAEMOGLOBIN
			cisco$hb<-lab.numeric(crf1.data$bld$bld002)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"hb","Haemoglobin during index admission, g/dl","Integer")

		# PLATELET COUNT
			cisco$platelet<-lab.numeric(crf1.data$bld$bld004)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"platelet","Platelet count during index admission, 10<sup>9</sup>/l","Integer")

		# WCC
			cisco$wcc<-lab.numeric(crf1.data$bld$bld003)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"wcc","WCC during index admission, 10<sup>9</sup>/l","Numeric","2dp")

		# LYMPHOCYTE COUNT
			cisco$lymph<-lab.numeric(crf1.data$bld$bld006)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"lymph","Lymphocyte count during index admission, 10<sup>9</sup>/l","Numeric","1dp")

		# PEAK D-DIMER
			cisco$ddimer_hi<-lab.numeric(crf1.data$bld$bld037)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"ddimer_hi","Peak D-Dimer during index admission, ng/ml","Integer")

		# PEAK HBA1C
			cisco$hba1c_hi<-lab.numeric(crf1.data$bld$bld041)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"hba1c_hi","Peak HbA1c during index admission, mmol/mol","Integer")

		# INITIAL CREATININE
			cisco$creat_init<-lab.numeric(crf1.data$bld$bld011)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"creat_init","Initial creatinine during index admission, &mu;mol/l","Integer")

			cisco$egfr_init<-egfr.fun(cisco$creat_init,cisco$age,cisco$sex=="Female",cisco$race_pd=="Black")
			spec<-add.spec(spec,"egfr_init","Initial eGFR during index admission, ml/min/1.73m<sup>2</sup>","Numeric","2dp")

			cisco$rendys_init<-factor(cisco$egfr_init<60,c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"rendys_init","Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup> during index admission","Text","Yes, No")

		# PEAK CREATININE
			cisco$creat_hi<-lab.numeric(crf1.data$bld$bld013)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"creat_hi","Peak creatinine during index admission, &mu;mol/l","Integer")

			cisco$egfr_lo<-egfr.fun(cisco$creat_hi,cisco$age,cisco$sex=="Female",cisco$race_pd=="Black")
			spec<-add.spec(spec,"egfr_lo","Minimum eGFR during index admission, ml/min/1.73m<sup>2</sup>","Numeric","2dp")

			cisco$rendys_lo<-factor(cisco$egfr_lo<60,c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"rendys_lo","Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup> during index admission","Text","Yes, No")

		# PEAK FERRITIN
			cisco$ferritin_hi<-lab.numeric(crf1.data$bld$bld039)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"ferritin_hi","Peak ferritin during index admission, &mu;g/l","Integer")

		# INITIAL TROPONIN I
			cisco$tni_init<-lab.numeric(crf1.data$bld$bld029[match(cisco$id,crf1.data$bld$sno)])
			spec<-add.spec(spec,"tni_init","Initial Troponin I in ng/l","Numeric","1dp")

		# PEAK TROPONIN I
			cisco$tni_hi<-lab.numeric(crf1.data$bld$bld045)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"tni_hi","Peak Troponin I during index admission, ng/l","Integer")

		# PEAK FIBRINOGEN
			cisco$fib_hi<-lab.numeric(crf1.data$bld$bld035)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"fib_hi","Peak fibrinogen during index admission, g/l","Numeric","2dp")

		# CRP
			cisco$crp_init<-lab.numeric(crf1.data$bld$bld023[match(cisco$id,crf1.data$bld$sno)])
			spec<-add.spec(spec,"crp_init","Initial CRP in mg/l","Integer")
			cisco$crp_init_cat<-crp.cat(cisco$crp_init)
			spec<-add.spec(spec,"crp_init_cat","Initial CRP in mg/l","Text","'&lt;5 mg/l', '&ge;5, &lt;10 mg/l', '&ge;10 mg/l'")

		# PEAK CRP
			cisco$crp<-cisco$crp_hi<-lab.numeric(crf1.data$bld$bld023)[match(cisco$id,crf1.data$bld$sno)]
			spec<-add.spec(spec,"crp_hi","Peak CRP during index admission, mg/l","Integer")
			spec<-add.spec(spec,"crp","CRP for ISARIC calculations - use peak CRP during index admission, mg/l","Integer")
			cisco$crp_hi_cat<-crp.cat(cisco$crp_hi)
			spec<-add.spec(spec,"crp_hi_cat","Peak CRP during index admission, mg/l","Text","'&lt;5 mg/l', '&ge;5, &lt;10 mg/l', '&ge;10 mg/l'")

		# PEAK SODIUM
			cisco$sodium_hi<-lab.numeric(extra1.data$'peak sodium')[match(cisco$id,substring(100000+as.numeric(extra1.data$'study id'),2,6))]
			spec<-add.spec(spec,"sodium_hi","Peak sodium during index admission, mmol/l","Integer")

		# INITIAL MCV
			rdw.data$id<-substring(100000+as.numeric(rdw.data$"study id"),2,6)

			cisco$mcv_init<-lab.numeric(rdw.data$"mcv admission"[match(cisco$id,rdw.data$id)])
			spec<-add.spec(spec,"mcv_init","Initial MCV in fl","Numeric","1dp")
			
		# INITIAL RDW
			cisco$rdw_init<-lab.numeric(rdw.data$"admission rdw"[match(cisco$id,rdw.data$id)])
			spec<-add.spec(spec,"rdw_init","Initial RDW in %","Numeric","2dp")
			
		# PEAK MCV
			cisco$mcv_hi<-lab.numeric(rdw.data$"peak mcv"[match(cisco$id,rdw.data$id)])
			spec<-add.spec(spec,"mcv_hi","Peak MCV during index admission, fl","Numeric","1dp")
			
		# RDW AT PEAK MCV
			cisco$rdw_mcv_hi<-lab.numeric(rdw.data$"peak mcv rdw"[match(cisco$id,rdw.data$id)])
			spec<-add.spec(spec,"rdw_mcv_hi","RDW at peak MCV during index admission, %","Numeric","2dp")
			

	# ISARIC-4C SCORE

		cisco$isaric<-do.call(isaric,cisco)
		spec<-add.spec(spec,"isaric","ISARIC-4c Mortality Score","Integer","See https://isaric4c.net/risk/v2/")

	# ISARIC-4C MORTALITY RISK

		isaric.risks<-
			matrix(
				c(
					0,0,
					1,0.3,
					2,0.8,
					3,2.3,
					4,4.8,
					5,7.5,
					6,7.8,
					7,11.7,
					8,14.4,
					9,19.2,
					10,22.9,
					11,26.9,
					12,32.9,
					13,40.1,
					14,44.6,
					15,51.6,
					16,59.1,
					17,66.1,
					18,75.8,
					19,77.4,
					20,82.9,
					21,87.5),
				nc=2,byrow=T)

		cisco$isaric_risk<-isaric.risks[1+cisco$isaric,2]
		spec<-add.spec(spec,"isaric_risk","ISARIC-4c Mortality Risk, in %","Numeric","1dp")

	# QRISK3

		# ADDITIONAL VARIABLES REQUIRED

		# AF

			cisco$af<-factor(crf1.data$har$har013,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"af","Atrial Fibrillation","Text","Yes, No")

		# RA

			cisco$ra<-factor(crf1.data$har$har011,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"ra","Rheumatoid Arthritis","Text","Yes, No")

		# FAMILY HISTORY OF HEART ATTACK

			cisco$fh<-factor(crf1.data$har$har014,1:2,c("Yes","No"))[match(cisco$id,crf1.data$har$sno)]
			spec<-add.spec(spec,"fh","Family history of heart attack","Text","Yes, No")

		temp<-
			data.frame(
				id=cisco$id,
				gender=as.numeric(cisco$sex=="Female"),
				age=cisco$age,
				atrial_fibrillation=as.numeric(cisco$af=="Yes"),
				atypical_antipsy=0,
				regular_steroid_tablets=0,
				erectile_disfunction=0,
				migraine=0,
				rheumatoid_arthritis=as.numeric(cisco$ra=="Yes"),
				chronic_kidney_disease=as.numeric(cisco$renal=="Yes"),
				severe_mental_illness=0,
				systemic_lupus_erythematosis=0,
				blood_pressure_treatment=as.numeric(cisco$hyperten=="Yes"),
				diabetes1=0,
				diabetes2=as.numeric(cisco$diabetes=="Yes"),
				weight=cisco$weight,
				height=cisco$height,
				ethiniciy=as.numeric(cisco$race_j),
				heart_attack_relative=as.numeric(cisco$fh=="Yes"),
				cholesterol_HDL_ratio=cisco$chol/cisco$hdl,
				systolic_blood_pressure=cisco$sbp,
				std_systolic_blood_pressure=0,
				smoke=as.numeric(cisco$smoke_5),
				townsend=cisco$townsend)

		temp<-temp[apply(!is.na(temp),1,all)&(temp$age>=25)&(temp$age<=84),]

		temp2<-
			QRISK3_2017(
				data=temp,
				patid="id",
				gender="gender",
				age="age",
				atrial_fibrillation="atrial_fibrillation",
				atypical_antipsy="atypical_antipsy",
				regular_steroid_tablets="regular_steroid_tablets",
				erectile_disfunction="erectile_disfunction",
				migraine="migraine",
				rheumatoid_arthritis="rheumatoid_arthritis",
				chronic_kidney_disease="chronic_kidney_disease",
				severe_mental_illness="severe_mental_illness",
				systemic_lupus_erythematosis="systemic_lupus_erythematosis",
				blood_pressure_treatment="blood_pressure_treatment",
				diabetes1="diabetes1",
				diabetes2="diabetes2",
				weight="weight",
				height="height",
				ethiniciy="ethiniciy",
				heart_attack_relative="heart_attack_relative",
				cholesterol_HDL_ratio="cholesterol_HDL_ratio",
				systolic_blood_pressure="systolic_blood_pressure",
				std_systolic_blood_pressure="std_systolic_blood_pressure",
				smoke="smoke",
				townsend="townsend")

		cisco$qrisk3<-round(temp2$QRISK3_2017,4)[match(cisco$id,temp2$id)]
		spec<-
			add.spec(
				spec,"qrisk3","QRisk3 predicted 10-year CVD risk","Numeric",
				paste(
					"4dp",
					"Uses 'QRISK3' package in R (v0.3.0).",
					paste(
						"Binary factors not available in database assumed to be absent.",
						"Applies to atypical antipsychotics, steroids, erectile dysfunction, migraine, severe mental illness, SLE, type I diabetes, SBP SD.",
						"If any other variable is missing, then predicted risk is missing."),
					sep="<br>"))

	# CHARLSON SCORE

		charlson<-
			function(age,mi,chf,pvd,stroke,dem,copd,conn,pep,mld,diab.unc,diab.comp,hemi,renal,malig,msld,metas,hiv)
			{
				as.numeric(cut(age,c(0,49.5,59.5,69.5,79.5,1000)))-1+
				mi+chf+pvd+stroke+dem+copd+conn+pep+mld+diab.unc+
				2*(diab.comp+hemi+renal+malig)+
				3*msld+
				6*(metas+hiv)
			}

		# NEW VARIABLES REQUIRED

			# CHF
				cisco$chf<-factor(crf1.data$chu$chu002,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"chf","Chronic HF","Text","Yes, No. Taken from Charlson page of eCRF")

			# COPD
				cisco$copd<-factor(crf1.data$chu$chu006,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"copd","COPD","Text","Yes, No. Taken from Charlson page of eCRF")

			# PEPTIC ULCER
				cisco$pep<-factor(crf1.data$chu$chu008,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"pep","Peptic ulcer","Text","Yes, No. Taken from Charlson page of eCRF")

			# LIVER DISEASE
				cisco$liver<-factor(crf1.data$chu$chu010,3:1,c("Moderate/Severe","Mild","None"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"liver","Liver disease","Text","Yes, No. Taken from Charlson page of eCRF")

			# HEMIPLEGIA
				cisco$hemi<-factor(crf1.data$chu$chu012,1:2,c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"hemi","Hemiplegia","Text","Yes, No. Taken from Charlson page of eCRF")

			# MALIGNANCY
				cisco$malig<-
					factor(
						(crf1.data$chu$chu015==2)|(crf1.data$chu$chu016==1)|(crf1.data$chu$chu017==1),
						c(T,F),c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"malig","Any (non-metastatic) malignancy, including leukaemia or lymphoma","Text","Yes, No. Taken from Charlson page of eCRF")

			# METASTATIC SOLID TUMOUR
				cisco$metas<-
					factor(crf1.data$chu$chu015==3,c(T,F),c("Yes","No"))[match(cisco$id,crf1.data$chu$sno)]
				spec<-add.spec(spec,"metas","Metastatic solid tumour","Text","Yes, No. Taken from Charlson page of eCRF")

		
		cisco$charlson<-
			charlson(
				cisco$age,
				cisco$mi=="Yes",
				cisco$chf=="Yes",
				cisco$pvd=="Yes",
				cisco$stroke=="Yes",
				cisco$cm_dem=="Yes",
				cisco$copd=="Yes",
				cisco$cm_ctd=="Yes",
				cisco$pep=="Yes",
				cisco$liver=="Mild",
				cisco$diabetes_c=="Uncomplicated",
				cisco$diabetes_c=="Complicated",
				cisco$hemi=="Yes",
				cisco$renal=="Yes",
				cisco$malig=="Yes",
				cisco$liver=="Moderate/Severe",
				cisco$metas=="Yes",
				cisco$cm_hiv=="Yes")
		spec<-
			add.spec(
				spec,"charlson","Charlson Score","Integer",
				"Use rheumatoid arthritis in place of rheumatic disease")

	# PRE-EXISTING MAINTENANCE MEDICATION

		# ASPIRIN
			cisco$aspirin<-factor(crf1.data$pct$pct001,1:2,c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"aspirin","Pre-existing aspirin","Text","Yes, No")

		# STATIN
			cisco$statin<-factor(crf1.data$pct$pct007,1:2,c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"statin","Pre-existing statin","Text","Yes, No")

		# BETA BLOCKER
			cisco$bb<-factor(crf1.data$pct$pct010,1:2,c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"bb","Pre-existing beta blocker","Text","Yes, No")

		# ACEi
			cisco$ace<-factor(crf1.data$pct$pct005,1:2,c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"ace","Pre-existing ACE inhibitor","Text","Yes, No")

		# ARB
			cisco$arb<-factor(crf1.data$pct$pct006,1:2,c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"arb","Pre-existing ARB","Text","Yes, No")

		# ORAL ANTICOAGULATION
			cisco$oac<-factor((crf1.data$pct$pct012==1)|(crf1.data$pct$pct013==1),c(T,F),c("Yes","No"))[match(cisco$id,crf1.data$pct$sno)]
			spec<-add.spec(spec,"oac","Pre-existing oral anticoagulation","Text","Yes, No. Defined as use of warfarin or NOAC")

	# ECG DATA

		# names(ecg.data)

		# LINKING VARIABLES
			ecg.data$id<-substring(100000+as.numeric(ecg.data$"ecrf id"),2,6)

		# MYOPERICARDITIS CRITERIA
			temp<-factor(as.numeric(ecg.data$"meets myocarditis criteria with new changes (0 = no, 1 = yes)"),1:0,c("Yes","No"))
			cisco$myoperi0<-temp[match(paste(cisco$id,"Admission"),paste(ecg.data$id,ecg.data$visit))]
			cisco$myoperi1<-temp[match(paste(cisco$id,1),paste(ecg.data$id,ecg.data$visit))]
			cisco$myoperi2<-temp[match(paste(cisco$id,2),paste(ecg.data$id,ecg.data$visit))]
			cisco$myoperi2[cisco$group=="Control"]<-cisco$myoperi1[cisco$group=="Control"]<-cisco$myoperi0[cisco$group=="Control"]<-"No"
			spec<-add.spec(spec,"myoperi0","Meets myocarditis criteria at admission","Text","Yes, No<br>Assumed 'No' for controls")
			spec<-add.spec(spec,"myoperi1","Meets myocarditis criteria at enrolment","Text","Yes, No<br>Assumed 'No' for controls")
			spec<-add.spec(spec,"myoperi2","Meets myocarditis criteria at 28-60 days post-discharge","Text","Yes, No<br>Assumed 'No' for controls")

		# PREMATURE ATRIAL CONRACTION
			temp<-factor(is.element(ecg.data$"rhythm (see arrhythmia list tab for key)",c(5,8)),c(T,F),c("Yes","No"))
			cisco$premac0<-temp[match(paste(cisco$id,"Admission"),paste(ecg.data$id,ecg.data$visit))]
			cisco$premac1<-temp[match(paste(cisco$id,1),paste(ecg.data$id,ecg.data$visit))]
			cisco$premac2<-temp[match(paste(cisco$id,2),paste(ecg.data$id,ecg.data$visit))]
			cisco$premac2[cisco$group=="Control"]<-cisco$premac1[cisco$group=="Control"]<-cisco$premac0[cisco$group=="Control"]<-"No"
			spec<-add.spec(spec,"premac0","Premature atrial contraction at Admission","Text","Yes, No")
			spec<-add.spec(spec,"premac1","Premature atrial contraction at enrolment","Text","Yes, No")
			spec<-add.spec(spec,"premac2","Premature atrial contraction at 28-60 days post-discharge","Text","Yes, No")

		# PREMATURE VENTRICULAR CONRACTION
			temp<-factor(is.element(ecg.data$"rhythm (see arrhythmia list tab for key)",c(4)),c(T,F),c("Yes","No"))
			cisco$premvc0<-temp[match(paste(cisco$id,"Admission"),paste(ecg.data$id,ecg.data$visit))]
			cisco$premvc1<-temp[match(paste(cisco$id,1),paste(ecg.data$id,ecg.data$visit))]
			cisco$premvc2<-temp[match(paste(cisco$id,2),paste(ecg.data$id,ecg.data$visit))]
			cisco$premvc2[cisco$group=="Control"]<-cisco$premvc1[cisco$group=="Control"]<-cisco$premvc0[cisco$group=="Control"]<-"No"
			spec<-add.spec(spec,"premvc0","Premature ventricular contraction at Admission","Text","Yes, No")
			spec<-add.spec(spec,"premvc1","Premature ventricular contraction at enrolment","Text","Yes, No")
			spec<-add.spec(spec,"premvc2","Premature ventricular contraction at 28-60 days post-discharge","Text","Yes, No")

		# ATRIAL FIBRILLATION OR FLUTTER
			temp<-
				factor(
					ifelse(
						is.na(as.numeric(ecg.data$"rhythm (see arrhythmia list tab for key)")),NA,
						is.element(as.numeric(ecg.data$"rhythm (see arrhythmia list tab for key)"),c(2,3))),c(T,F),c("Yes","No"))
			cisco$aff0<-temp[match(paste(cisco$id,"Admission"),paste(ecg.data$id,ecg.data$visit))]
			cisco$aff1<-temp[match(paste(cisco$id,1),paste(ecg.data$id,ecg.data$visit))]
			cisco$aff2<-temp[match(paste(cisco$id,2),paste(ecg.data$id,ecg.data$visit))]
			cisco$aff2[cisco$group=="Control"]<-cisco$aff1[cisco$group=="Control"]<-cisco$aff0[cisco$group=="Control"]<-"No"
			spec<-add.spec(spec,"aff0","Atrial fibrillation or flutter at Admission","Text","Yes, No")
			spec<-add.spec(spec,"aff1","Atrial fibrillation or flutter at enrolment","Text","Yes, No")
			spec<-add.spec(spec,"aff2","Atrial fibrillation or flutter at 28-60 days post-discharge","Text","Yes, No")

	# CHEST CT DATA

		# names(chestct.data)

		# LINKING VARIABLES
			chestct.data$id<-substring(100000+as.numeric(chestct.data$"study id"),2,6)

		# ATELECTASIS
			cisco$atelectasis<-factor(as.numeric(chestct.data$"atelectasis "),1:0,c("Yes","No"))[match(cisco$id,chestct.data$id)]
			spec<-add.spec(spec,"atelectasis","Atelectasis at 28-60 days post-discharge","Text","Yes, No")

		# RETICULATION AND/OR ARCHITECTURAL DISTORTION
			cisco$reticulation<-factor(as.numeric(chestct.data$"reticular ad"),1:0,c("Yes","No"))[match(cisco$id,chestct.data$id)]
			spec<-add.spec(spec,"reticulation","Reticulation and/or architectural distortion at 28-60 days post-discharge","Text","Yes, No")

		# GROUND GLASS OPACITY
			cisco$ggopacity<-factor(as.numeric(chestct.data$"ground glass opacification"),1:0,c("Yes","No"))[match(cisco$id,chestct.data$id)]
			spec<-add.spec(spec,"ggopacity","Ground glass opacity at 28-60 days post-discharge","Text","Yes, No")

		# PULMONARY ARTERIAL THROMBUS
			cisco$pathrombus<-factor(as.numeric(chestct.data$"pulmonary embolism"),1:0,c("Yes","No"))[match(cisco$id,chestct.data$id)]
			spec<-add.spec(spec,"pathrombus","Pulmonary arterial thrombus at 28-60 days post-discharge","Text","Yes, No")

		# VISUAL ESTIMATE OF PERCENTAGE OF TOTAL LUNG AREA ABNORMAL 
			cisco$percabnlung<-as.numeric(chestct.data$"visual estimate abnormal lung %")[match(cisco$id,chestct.data$id)]
			spec<-add.spec(spec,"abnlung","Visual estimate of percentage of total lung area abnormal at 28-60 days post-discharge","Numeric","1dp")

		# VISUAL ESTIMATE OF PERCENTAGE OF TOTAL LUNG AREA ABNORMAL 
			cisco$abnlung<-cut(cisco$percabnlung,c(-1,19.9,49.9,1000),c("&lt;20%","&ge;20, &lt;50%","&ge;50%"))
			spec<-add.spec(spec,"abnlung","Visual estimate of percentage of total lung area abnormal at 28-60 days post-discharge","Text","'&lt;20%', '&ge;20, &lt;50%', '&ge;50%'")

	# PCI & CABG DATA

		# PCI
			cisco$pci<-factor(as.numeric(extra1.data$'previous pci'),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(extra1.data$'study id'),2,6))]
			spec<-add.spec(spec,"pci","PCI","Text","Yes, No")

		# CABG
			cisco$cabg<-factor(as.numeric(extra1.data$'previous cabg'),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(extra1.data$'study id'),2,6))]
			spec<-add.spec(spec,"cabg","CABG","Text","Yes, No")

	# CTCA DATA

		# LINKING VARIABLES
			ctca.data$id<-substring(100000+as.numeric(ctca.data$study_id),2,6)

		# LVEDV
			cisco$lvedv_ctca<-ctca.data$lv_edv[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"lvedv_ctca","CTCA LVEDV at 28-60 days post-discharge, ml","Integer")

		# LV MASS
			cisco$lvmass_ctca<-ctca.data$lv_mass[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"lvmass_ctca","CTCA LV mass at 28-60 days post-discharge, g","Integer")

		# AGATSTON SCORE
			cisco$agatston<-ctca.data$agatston_score[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"agatston","Agatston score at 28-60 days post-discharge","Integer")

		# MESA PERCENTILE
			cisco$mesa<-ctca.data$mesa_percentile[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"mesa","MESA percentile at 28-60 days post-discharge","Integer")

		# CAD-RADS SCORE
			cisco$cadrads<-factor(ctca.data$cad.rads,0:5,paste("Level",0:5))[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"cadrads","CAD-RADS score at 28-60 days post-discharge","Text","Level 0, ..., Level 5")

		# OBSTRUCTIVE DISEASE
			cisco$ocad<-factor(ctca.data$obs_cor_dis,1:0,c("Yes","No"))[match(cisco$id,ctca.data$id)]
			spec<-add.spec(spec,"ocad","Obstructive CAD at 28-60 days post-discharge","Text","Yes, No")

		# FFR-CT - NEED RAW DATA

		# CORRECT FFR VALUE RECORDED IN LESION COLUMN
			ctca.use1<-ctca.raw1
			ctca.use2<-ctca.raw2

		# LINKING VARIABLE
			link.id<-
				c(
					substring(100000+as.numeric(ctca.use1$"study id"),2,6),
					substring(100000+as.numeric(ctca.use2$"study id"),2,6))

			for(i in grep("lesion",names(ctca.use1)))
			{
				x<-as.numeric(ctca.use1[,i])
				if(any(!is.na(x)&(x>0)&(x<1)))
					ctca.use1[!is.na(x)&(x>0)&(x<1),i-1]<-ctca.use1[!is.na(x)&(x>0)&(x<1),i]
			}
			for(i in grep("lesion",names(ctca.use2)))
			{
				x<-as.numeric(ctca.use2[,i])
				if(any(!is.na(x)&(x>0)&(x<1)))
					ctca.use2[!is.na(x)&(x>0)&(x<1),i-1]<-ctca.use2[!is.na(x)&(x>0)&(x<1),i]
			}

		# IF HEARTFLOW NOT AVAILABLE, SET ALL FFR VALUES TO MISSING - ONLY DO THIS FOR COVID PATIENTS - ERROR IN CONTROL DATA (COLUMNS MISALIGNED)
			for(i in grep("lesion",names(ctca.use1)))
			{
				x<-as.numeric(ctca.use1[,grep("heartflow",names(ctca.use1))])
				if(any(!is.na(x)&(x>1)))
					ctca.use1[!is.na(x)&(x>1),i-1]<-NA
			}

		# FUNCTIONS TO CALCULATE MEAN, MEDIAN, AND MINIMUM FFR
		# ANY ZEROES, SET TO MISSING

			meanffr<-function(x)
			{
				x<-apply(x,2,as.numeric)
				x<-apply(x,2,function(x)ifelse(is.na(x)|(x==0),NA,x))
				apply(x,1,function(x)if(all(is.na(x))) NA else mean(x,na.rm=T))
			}
			medianffr<-function(x)
			{
				x<-apply(x,2,as.numeric)
				x<-apply(x,2,function(x)ifelse(is.na(x)|(x==0),NA,x))
				apply(x,1,function(x)if(all(is.na(x))) NA else median(x,na.rm=T))
			}
			minffr<-function(x)
			{
				x<-apply(x,2,as.numeric)
				x<-apply(x,2,function(x)ifelse(is.na(x)|(x==0),NA,x))
				apply(x,1,function(x)if(all(is.na(x))) NA else min(x,na.rm=T))
			}

			# LAD
				temp1<-
					rbind(
						ctca.use1[,c("plad","mlad","dlad","d1","d1 - additional","d2","additional d2")],
						ctca.use2[,c("plad","mlad","dlad","d1","d1 - additional","d2","additional d2")])

				cisco$meanffr_lad<-round(meanffr(temp1),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"meanffr_lad","Mean LAD FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric",
						paste(
							"4dp.",
							"Derived from individual FFR measures of each segment.",
							"Where '?lesion' column has a value between 0 and 1, assume this is the FFR measure for that segment.",
							"FFR values of zero set to missing.",
							"If 'heartflow' column is >1, then set all FFR measures to missing for that patient.",
							"If all FFR measures for vessel are missing, then set mean/median etc. to missing, otherwise calculate summary score."))

				cisco$medianffr_lad<-round(medianffr(temp1),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"medianffr_lad","Median LAD FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$minffr_lad<-round(minffr(temp1),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"minffr_lad","Minimum LAD FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$ffr_lad<-factor(cisco$minffr_lad<=0.80,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"ffr_lad","Minimum LAD FFR<sub>CT</sub> &le;0.8 at 28-60 days post-discharge","Text","Yes, No")

			# CIRCUMFLEX
				temp2<-
					rbind(
						ctca.use1[,
							c(
								"pcx","intermediate/anterolateral artery","obtuse marginal1","obtuse marginal 2","distal cx",
								"left posterolateral","left posterolateral a","left posterolateral b","posterior descending (left dominant only)")],
						ctca.use2[,
							c(
								"pcx","intermediate/anterolateral artery","obtuse marginal1","obtuse marginal 2","distal cx",
								"left posterolateral","left posterolateral a","left posterolateral b","posterior descending (left dominant only)")])

				cisco$meanffr_cx<-round(meanffr(temp2),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"meanffr_cx","Mean Circumflex FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$medianffr_cx<-round(medianffr(temp2),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"medianffr_cx","Median Circumflex FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$minffr_cx<-round(minffr(temp2),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"minffr_cx","Minimum Circumflex FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$ffr_cx<-factor(cisco$minffr_cx<=0.80,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"ffr_cx","Minimum Circumflex FFR<sub>CT</sub> &le;0.8 at 28-60 days post-discharge","Text","Yes, No")

			# RCA
				temp3<-
					rbind(
						ctca.use1[,
							c(
								"prca","mrca","drca","pda",
								"posterolateral branch from rca","posterolateral branch a from rca","posterolateral branch from rca b","posterolateral branch from rca c")],
						ctca.use2[,
							c(
								"prca","mrca","drca","pda",
								"posterolateral branch from rca","posterolateral branch a from rca","posterolateral branch from rca b","posterolateral branch from rca c")])

				cisco$meanffr_rca<-round(meanffr(temp3),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"meanffr_rca","Mean RCA FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$medianffr_rca<-round(medianffr(temp3),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"medianffr_rca","Median RCA FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$minffr_rca<-round(minffr(temp3),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"minffr_rca","Minimum RCA FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$ffr_rca<-factor(cisco$minffr_rca<=0.80,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"ffr_rca","Minimum RCA FFR<sub>CT</sub> &le;0.8 at 28-60 days post-discharge","Text","Yes, No")

			# PATIENT-LEVEL
				temp<-cbind(temp1,temp2,temp3)

				cisco$meanffr<-round(meanffr(temp),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"meanffr","Mean patient-level FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$medianffr<-round(medianffr(temp),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"medianffr","Median patient-level FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$minffr<-round(minffr(temp),4)[match(cisco$id,link.id)]
				spec<-
					add.spec(
						spec,"minffr","Minimum patient-level FFR<sub>CT</sub> at 28-60 days post-discharge","Numeric","4dp. See above ('meanffr_lad') for assumptions.")

				cisco$ffr<-factor(cisco$minffr<=0.80,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"ffr","Minimum patient-level FFR<sub>CT</sub> &le;0.8 at 28-60 days post-discharge","Text","Yes, No")

	# CTCA - RAW DATA FOR CADRADS

		# CAD-RADS SCORE
			temp<-c(ctca.use1$"cad-rads",ctca.use2$"cad-rads")
			temp<-gsub("STENTS","",temp)
			temp<-gsub("Stents","",temp)
			temp<-gsub("S","",temp)
			temp<-gsub("G","",temp)
			temp<-gsub("/","",temp)
			temp<-as.numeric(temp)
			temp_cadrads_raw<-factor(temp[match(cisco$id,link.id)],0:5,paste("Level",0:5))

		# WHERE DERIVED DATA IS MISSING, USE RAW DATA VALUE
			cisco$cadrads[is.na(cisco$cadrads)]<-temp_cadrads_raw[is.na(cisco$cadrads)]

	# CMR DATA

		# LINKING VARIABLES
			cmr.data$id<-substring(100000+as.numeric(cmr.data$"study id"),2,6)

		# LVEDV INDEX
			cisco$lvedvi_cmr<-round(as.numeric(cmr.data$"lvedvi")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"lvedvi_cmr","CMR LVEDV index at 28-60 days post-discharge, ml/m<sup>2</sup>","Numeric","3dp")

		# LVESV INDEX
			cisco$lvesvi_cmr<-round(as.numeric(cmr.data$"lvesvi")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"lvesvi_cmr","CMR LVESV index at 28-60 days post-discharge, ml/m<sup>2</sup>","Numeric","3dp")

		# LVEF
			cisco$lvef_cmr<-round(as.numeric(cmr.data$"lvef")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"lvef_cmr","CMR LVEF at 28-60 days post-discharge, %","Numeric","3dp")

		# LVEF REDUCED
			cisco$redlvef_cmr<-
				factor(
					ifelse(cisco$sex=="Male",cisco$lvef<48,cisco$lvef<51),
					c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"redlvef_cmr","Reduced CMR LVEF at 28-60 days post-discharge","Text","Yes, No. <48% for males, <51% for females")

		# LV MASS
			cisco$lvm_cmr<-round((as.numeric(cmr.data$"lv mass d")+as.numeric(cmr.data$"lv mass s"))[match(cisco$id,cmr.data$id)]/2,4)
			spec<-add.spec(spec,"lvm_cmr","CMR LV mass at 28-60 days post-discharge, %","Numeric","4dp")

		# LV GLS
			cisco$lvgls_cmr<-round(as.numeric(cmr.data$"lv gls")[match(cisco$id,cmr.data$id)],2)
			spec<-add.spec(spec,"lvgls_cmr","CMR LV GLS at 28-60 days post-discharge, %","Numeric","2dp")

		# LV GCS
			cisco$lvgcs_cmr<-round(as.numeric(cmr.data$"lv gcs")[match(cisco$id,cmr.data$id)],2)
			spec<-add.spec(spec,"lvgcs_cmr","CMR LV GCS at 28-60 days post-discharge, %","Numeric","2dp")

		# LV GRS
			cisco$lvgrs_cmr<-round(as.numeric(cmr.data$"lv grs")[match(cisco$id,cmr.data$id)],2)
			spec<-add.spec(spec,"lvgrs_cmr","CMR LV GRS at 28-60 days post-discharge, %","Numeric","2dp")

		# RVEDV INDEX
			cisco$rvedvi_cmr<-round(as.numeric(cmr.data$"rv edvi")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"rvedvi_cmr","CMR RVEDV index at 28-60 days post-discharge, ml/m<sup>2</sup>","Numeric","3dp")

		# RVESV INDEX
			cisco$rvesvi_cmr<-round(as.numeric(cmr.data$"rv esvi")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"rvesvi_cmr","CMR RVESV index at 28-60 days post-discharge, ml/m<sup>2</sup>","Numeric","3dp")

		# RVEF
			cisco$rvef_cmr<-round(as.numeric(cmr.data$"rvef")[match(cisco$id,cmr.data$id)],3)
			if(any(!is.na(cisco$rvef_cmr)&(cisco$rvef_cmr>100)))
			{
				cisco$rvef_cmr<-ifelse(!is.na(cisco$rvef_cmr)&(cisco$rvef_cmr>100),cisco$rvef_cmr-100,cisco$rvef_cmr)
				spec<-add.spec(spec,"rvef_cmr","CMR RVEF at 28-60 days post-discharge, %","Numeric","3dp, value &gt;100% corrected by subtracting 100")
			} else spec<-add.spec(spec,"rvef_cmr","CMR RVEF at 28-60 days post-discharge, %","Numeric","3dp")


		# RVEF REDUCED
			cisco$redrvef_cmr<-
				factor(
					ifelse(cisco$sex=="Male",cisco$rvef<45,cisco$rvef<47),
					c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"redrvef_cmr","Reduced CMR RVEF at 28-60 days post-discharge","Text","Yes, No. <45% for males, <47% for females")

		# RV GLS
			cisco$rvgls_cmr<-round(as.numeric(cmr.data$"rv gls")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"rvgls_cmr","CMR RV GLS at 28-60 days post-discharge, %","Numeric","3dp")

		# PERICARDIAL THICKENING
			cisco$pcthick_cmr<-factor(as.numeric(cmr.data$"pericardial thickening"),1:0,c("Yes","No"))[match(cisco$id,cmr.data$id)]
			spec<-add.spec(spec,"pcthick_cmr","Pericardial thickening at 28-60 days post-discharge","Text","Yes, No")

		# PERICARDIAL EFFUSION
			cisco$pceff_cmr<-factor(as.numeric(cmr.data$"pericardial effusion"),1:0,c("Yes","No"))[match(cisco$id,cmr.data$id)]
			spec<-add.spec(spec,"pceff_cmr","Pericardial effusion at 28-60 days post-discharge","Text","Yes, No")

		# RA AREA
			cisco$raa_cmr<-round(as.numeric(cmr.data$"raa")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"raa_cmr","Right atrial area at 28-60 days post-discharge, cm<sup>2</sup>","Numeric","3dp")

		# LA AREA
			cisco$laa_cmr<-round(as.numeric(cmr.data$"laa")[match(cisco$id,cmr.data$id)],3)
			spec<-add.spec(spec,"laa_cmr","Left atrial area at 28-60 days post-discharge, cm<sup>2</sup>","Numeric","3dp")

	# MULTI PARAMETRIC MYOCARDIAL MAPPING

	# T1

		# names(t1.data)

		# LINKING VARIABLES
			t1.data$id<-substring(100000+as.numeric(t1.data$"study id"),2,6)

		# ABNORMAL GLOBAL T1
			cisco$abnglob_t1<-factor(as.numeric(t1.data$"t1_global")>1233,c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			spec<-add.spec(spec,"abnglob_t1","Abnormal global T1 at 28-60 days post-discharge","Text","Yes, No")

		# ABNORMAL GLOBAL ECV T1
			cisco$abnglobecv_t1<-factor(as.numeric(t1.data$"global ecv")>27.4,c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			spec<-add.spec(spec,"abnglobecv_t1","Abnormal global ECV (T1) at 28-60 days post-discharge","Text","Yes, No")

		# ANY LGE
			cisco$any_lge<-factor(t1.data$"lge",c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			spec<-add.spec(spec,"any_lge","Any LGE at 28-60 days post-discharge","Text","Yes, No")

		# ISCHAEMIC DISTRIBUTION
			cisco$isch_dist<-factor(apply(t1.data[,paste("lge_aetiology",1:16,sep="_")],1,function(x)any(as.numeric(x)==2)),c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			cisco$isch_dist[is.na(cisco$any_lge)]<-NA
			spec<-add.spec(spec,"isch_dist","Ischaemic distribution (LGE aetiology) at 28-60 days post-discharge","Text","Yes, No. If any_lge is missing, then set to missing")

		# NON-ISCHAEMIC DISTRIBUTION
			cisco$nonisch_dist<-factor(apply(t1.data[,paste("lge_aetiology",1:16,sep="_")],1,function(x)any(as.numeric(x)==1)),c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			cisco$nonisch_dist[is.na(cisco$any_lge)]<-NA
			spec<-add.spec(spec,"nonisch_dist","Non-ischaemic distribution (LGE aetiology) at 28-60 days post-discharge","Text","Yes, No. If any_lge is missing, then set to missing")

		# MIXED DISTRIBUTION
			cisco$mixed_dist<-factor(apply(t1.data[,paste("lge_aetiology",1:16,sep="_")],1,function(x)any(as.numeric(x)==3)),c(T,F),c("Yes","No"))[match(cisco$id,t1.data$id)]
			cisco$mixed_dist[is.na(cisco$any_lge)]<-NA
			spec<-add.spec(spec,"mixed_dist","Mixed distribution (LGE aetiology) at 28-60 days post-discharge","Text","Yes, No. If any_lge is missing, then set to missing")

		# LAKE LOUISE CRITERIA
			cisco$lake_louise<-factor(as.numeric(t1.data$"llc (2/2)"),2:0,c("Definite","Probable","None"))[match(cisco$id,t1.data$id)]
			cisco$lake_louise[cisco$group=="Control"]<-"None"
			spec<-add.spec(spec,"lake_louise","Lake Louise criteria at 28-60 days post-discharge","Text","'Definite', 'Probable', 'None'<br>Controls assigned value 'None'")
			
	# T2

		# names(t2.data)

		# LINKING VARIABLES
			t2.data$id<-substring(100000+as.numeric(t2.data$"study id"),2,6)

		# ABNORMAL GLOBAL T2
			cisco$abnglob_t2<-factor(as.numeric(t2.data$"global t2")>44,c(T,F),c("Yes","No"))[match(cisco$id,t2.data$id)]
			spec<-add.spec(spec,"abnglob_t2","Abnormal global T2 at 28-60 days post-discharge","Text","Yes, No")

		# T2 RATIO
			cisco$t2_ratio<-round(as.numeric(t2.data$"t2 ratio (myo/ser ant)")[match(cisco$id,t2.data$id)],4)
			spec<-add.spec(spec,"t2_ratio","T2 ratio (myocardium/serratus anterior) at 28-60 days post-discharge","Numeric","4dp")

	# LGE CLASSIFICATION
		lge.data$id<-substring(100000+as.numeric(lge.data$"study id"),2,6)

		cisco$lge_class<-
			factor(
				c(0,1,2,3,4,5,5)[1+as.numeric(lge.data$"lge ch class")],
				c(2,1,3,4,5,0),
				c("Myocardial Infarction","Myocarditis","Mixed","Micro-emboli","Other/Minor","None"))[match(cisco$id,lge.data$id)]
		spec<-add.spec(spec,"lge_class","LGE Classification at 28-60 days post-discharge","Text","'Myocardial Infarction', 'Myocarditis', 'Mixed', 'Micro-emboli', 'Other/Minor', 'None'")

	# CORE BLOODS - BIOMARKERS

		# names(blood.data)

		# LINKING VARIABLES
			blood.data$id<-
				paste(
					substring(blood.data$ciscopatientid,7,8),
					substring(blood.data$ciscopatientid,10,12),
					sep="")

			blood.data$visit<-
				ifelse(nchar(blood.data$ciscopatientid)<14,1,2)

		# INSERT V1 DATA AT V2 FOR THE CONTROLS
			temp<-blood.data[is.element(blood.data$id,cisco$id[cisco$group=="Control"]),]
			temp$visit<-2
			blood.data<-rbind(blood.data,temp)

		# CRP
			cisco$crp_lab_1<-lab.numeric(blood.data$"crp (mg/l)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$crp_lab_2<-lab.numeric(blood.data$"crp (mg/l)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"crp_lab_1","Core lab CRP at enrolment, mg/l","Numeric","2dp")
			spec<-add.spec(spec,"crp_lab_2","Core lab CRP at 28-60 days post-discharge (at enrolment for controls), mg/l","Numeric","2dp")

			cisco$crp_lab_cat_1<-crp.cat(cisco$crp_lab_1)
			spec<-add.spec(spec,"crp_lab_cat_1","Core lab CRP at enrolment, mg/l","Text","'&lt;5 mg/l', '&ge;5, &lt;10 mg/l', '&ge;10 mg/l'")
			cisco$crp_lab_cat_2<-crp.cat(cisco$crp_lab_2)
			spec<-add.spec(spec,"crp_lab_cat_2","Core lab CRP at 28-60 days post-discharge (at enrolment for controls), mg/l","Text","'&lt;5 mg/l', '&ge;5, &lt;10 mg/l', '&ge;10 mg/l'")

		# hsTnI
			cisco$hstni_lab_1<-lab.numeric(blood.data$"hstni (pg/ml)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$hstni_lab_2<-lab.numeric(blood.data$"hstni (pg/ml)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"hstni_lab_1","Core lab hsTnI at enrolment, pg/ml","Numeric","1dp")
			spec<-add.spec(spec,"hstni_lab_2","Core lab hsTnI at 28-60 days post-discharge (at enrolment for controls), pg/ml","Numeric","1dp")

		# NT proBNP
			cisco$ntprobnp_lab_1<-lab.numeric(blood.data$"nt-probnp (pg/ml)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$ntprobnp_lab_2<-lab.numeric(blood.data$"nt-probnp (pg/ml)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"ntprobnp_lab_1","Core lab NT pro BNP at enrolment, pg/ml","Numeric","1dp")
			spec<-add.spec(spec,"ntprobnp_lab_2","Core lab NT pro BNP at 28-60 days post-discharge (at enrolment for controls), pg/ml","Numeric","1dp")

		# FERRITIN
			cisco$ferritin_lab_1<-lab.numeric(blood.data$"ferritin (ug/l)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$ferritin_lab_2<-lab.numeric(blood.data$"ferritin (ug/l)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"ferritin_lab_1","Core lab ferritin at enrolment, ug/l","Numeric","1dp")
			spec<-add.spec(spec,"ferritin_lab_2","Core lab ferritin at 28-60 days post-discharge (at enrolment for controls), ug/l","Numeric","1dp")

		# CHOLESTEROL
			cisco$chol_lab_1<-lab.numeric(blood.data$"cholesterol (mmol/l)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$chol_lab_2<-lab.numeric(blood.data$"cholesterol (mmol/l)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"chol_lab_1","Core lab total cholesterol at enrolment, mmol/l","Numeric","2dp")
			spec<-add.spec(spec,"chol_lab_2","Core lab total cholesterol at 28-60 days post-discharge (at enrolment for controls), mmol/l","Numeric","2dp")

		# HDL CHOLESTEROL
			cisco$hdl_lab_1<-lab.numeric(blood.data$"hdl(mmol/l)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$hdl_lab_2<-lab.numeric(blood.data$"hdl(mmol/l)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"hdl_lab_1","Core lab HDL cholesterol at enrolment, mmol/l","Numeric","2dp")
			spec<-add.spec(spec,"hdl_lab_2","Core lab HDL cholesterol at 28-60 days post-discharge (at enrolment for controls), mmol/l","Numeric","2dp")

		# TRIGLYCERIDES
			cisco$trig_lab_1<-lab.numeric(blood.data$"triglycerides (mmol/l)")[match(paste(cisco$id,1),paste(blood.data$id,blood.data$visit))]
			cisco$trig_lab_2<-lab.numeric(blood.data$"triglycerides (mmol/l)")[match(paste(cisco$id,2),paste(blood.data$id,blood.data$visit))]
			spec<-add.spec(spec,"trig_lab_1","Core lab triglycerides at enrolment, mmol/l","Numeric","2dp")
			spec<-add.spec(spec,"trig_lab_2","Core lab triglycerides at 28-60 days post-discharge (at enrolment for controls), mmol/l","Numeric","2dp")

	# EXTRA BIOMARKERS

		# LINKING VARIABLES
			blood3.data$id<-
				paste(
					substring(blood3.data$ciscopatientid,7,8),
					substring(blood3.data$ciscopatientid,10,12),
					sep="")
			blood3.data$visit<-
				ifelse(nchar(blood3.data$ciscopatientid)<14,1,2)

			blood4.data$id<-
				paste(
					substring(blood4.data$ciscopatientid,7,8),
					substring(blood4.data$ciscopatientid,10,12),
					sep="")
			blood4.data$visit<-
				ifelse(nchar(blood4.data$ciscopatientid)<14,1,2)

			blood5.data$id<-
				paste(
					substring(blood5.data$ciscopatientid,7,8),
					substring(blood5.data$ciscopatientid,10,12),
					sep="")
			blood5.data$visit<-
				ifelse(nchar(blood5.data$ciscopatientid)<14,1,2)

		# DATASETS 3 AND 4 ARE COMPLEMENTARY - DIFFERENT VARIABLES - MERGE AS DATASET 6
			blood6.data<-merge(blood3.data,blood4.data)

		# ADD GROUP VARIABLE
			blood5.data$group<-cisco$group[match(blood5.data$id,cisco$id)]
			blood6.data$group<-cisco$group[match(blood6.data$id,cisco$id)]

		# CONVERT BOTH DATASETS TO SAME FORMAT

			temp1<-
				data.frame(
					blood6.data[,c("id","visit","barcode","group")],
					icam1=lab.numeric(blood6.data$"icam-1 (ng/ml)"),
					vcam1=lab.numeric(blood6.data$"vcam-1 (ng/ml)"),
					endothelin1=lab.numeric(blood6.data$"endothelin-1 (pg/ml)"),
					il6=lab.numeric(blood6.data$"il-6 (pg/ml)"),
					st2=lab.numeric(blood6.data$"st2 (ng/ml)"),
					pselectin=lab.numeric(blood6.data$"p-selectin (ng/ml)"),
					ldh=lab.numeric(blood6.data$"ldh (u/l)"),
					haptoglobin=lab.numeric(blood6.data$"haptoglobin (g/l)"),
					total_bili=lab.numeric(blood6.data$"total bilirubin (umol/l)"),
					direct_bili=lab.numeric(blood6.data$"direct bilirubin (umol/l)"),
					indirect_bili=lab.numeric(blood6.data$"indirect bilirubin (umol/l)"))

			temp2<-
				data.frame(
					blood5.data[,c("id","visit","barcode","group")],
					icam1=lab.numeric(blood5.data$"icam(ng/ml)"),
					vcam1=lab.numeric(blood5.data$"vcam (ng/ml)"),
					endothelin1=lab.numeric(blood5.data$"et-1 (pg/ml)"),
					il6=lab.numeric(blood5.data$"il-6 (pg/ml)"),
					st2=lab.numeric(blood5.data$"st2 (ng/ml)"),
					pselectin=rep(NA,nrow(blood5.data)),
					ldh=lab.numeric(blood5.data$"ldh (u/l)"),
					haptoglobin=lab.numeric(blood5.data$"haptoglobin (g/l)"),
					total_bili=lab.numeric(blood5.data$"total bilirubin (umol/l)"),
					direct_bili=lab.numeric(blood5.data$"direct bilirubin (umol/l)"),
					indirect_bili=lab.numeric(blood5.data$"indirect bilirubin (umol/l)"))

		# USE DATA FROM temp1 IF AVAILABLE, OTHERWISE USE DATA FROM temp2
		# DO IT BY VISIT

			# INSERT V1 DATA AT V2 FOR THE CONTROLS
				temp<-temp1[is.element(temp1$id,cisco$id[cisco$group=="Control"]),]
				temp$visit<-2
				temp1<-rbind(temp1,temp)

				temp<-temp2[is.element(temp2$id,cisco$id[cisco$group=="Control"]),]
				temp$visit<-2
				temp2<-rbind(temp2,temp)

			# VISIT 1
				temp1.1<-temp1[match(paste(cisco$id,1),paste(temp1$id,temp1$visit)),]
				temp1.2<-temp1[match(paste(cisco$id,2),paste(temp1$id,temp1$visit)),]
				temp2.1<-temp2[match(paste(cisco$id,1),paste(temp2$id,temp2$visit)),]
				temp2.2<-temp2[match(paste(cisco$id,2),paste(temp2$id,temp2$visit)),]

			# COMBINE
				tempAll.1<-
					data.frame(
						id=cisco$id,
						icam1=ifelse(is.na(temp1.1$icam1),temp2.1$icam1,temp1.1$icam1),
						vcam1=ifelse(is.na(temp1.1$vcam1),temp2.1$vcam1,temp1.1$vcam1),
						endothelin1=ifelse(is.na(temp1.1$endothelin1),temp2.1$endothelin1,temp1.1$endothelin1),
						il6=ifelse(is.na(temp1.1$il6),temp2.1$il6,temp1.1$il6),
						st2=ifelse(is.na(temp1.1$st2),temp2.1$st2,temp1.1$st2),
						pselectin=ifelse(is.na(temp1.1$pselectin),temp2.1$pselectin,temp1.1$pselectin),
						ldh=ifelse(is.na(temp1.1$ldh),temp2.1$ldh,temp1.1$ldh),
						haptoglobin=ifelse(is.na(temp1.1$haptoglobin),temp2.1$haptoglobin,temp1.1$haptoglobin),
						total_bili=ifelse(is.na(temp1.1$total_bili),temp2.1$total_bili,temp1.1$total_bili),
						direct_bili=ifelse(is.na(temp1.1$direct_bili),temp2.1$direct_bili,temp1.1$direct_bili))
				tempAll.2<-
					data.frame(
						id=cisco$id,
						icam1=ifelse(is.na(temp1.2$icam1),temp2.2$icam1,temp1.2$icam1),
						vcam1=ifelse(is.na(temp1.2$vcam1),temp2.2$vcam1,temp1.2$vcam1),
						endothelin1=ifelse(is.na(temp1.2$endothelin1),temp2.2$endothelin1,temp1.2$endothelin1),
						il6=ifelse(is.na(temp1.2$il6),temp2.2$il6,temp1.2$il6),
						st2=ifelse(is.na(temp1.2$st2),temp2.2$st2,temp1.2$st2),
						pselectin=ifelse(is.na(temp1.2$pselectin),temp2.2$pselectin,temp1.2$pselectin),
						ldh=ifelse(is.na(temp1.2$ldh),temp2.2$ldh,temp1.2$ldh),
						haptoglobin=ifelse(is.na(temp1.2$haptoglobin),temp2.2$haptoglobin,temp1.2$haptoglobin),
						total_bili=ifelse(is.na(temp1.2$total_bili),temp2.2$total_bili,temp1.2$total_bili),
						direct_bili=ifelse(is.na(temp1.2$direct_bili),temp2.2$direct_bili,temp1.2$direct_bili))

		# ICAM-1
			cisco$icam1_lab_1<-tempAll.1$icam1
			cisco$icam1_lab_2<-tempAll.2$icam1
			spec<-add.spec(spec,"icam1_lab_1","Core lab ICAM-1 at enrolment, ng/ml","Integer")
			spec<-add.spec(spec,"icam1_lab_2","Core lab ICAM-1 at 28-60 days post-discharge (at enrolment for controls), ng/ml","Integer")

		# VCAM-1
			cisco$vcam1_lab_1<-tempAll.1$vcam1
			cisco$vcam1_lab_2<-tempAll.2$vcam1
			spec<-add.spec(spec,"vcam1_lab_1","Core lab VCAM-1 at enrolment, ng/ml","Integer")
			spec<-add.spec(spec,"vcam1_lab_2","Core lab VCAM-1 at 28-60 days post-discharge (at enrolment for controls), ng/ml","Integer")

		# ENDOTHELIN-1
			cisco$endothelin1_lab_1<-tempAll.1$endothelin1
			cisco$endothelin1_lab_2<-tempAll.2$endothelin1
			spec<-add.spec(spec,"endothelin1_lab_1","Core lab Endothelin-1 at enrolment, pg/ml","Numeric","2dp")
			spec<-add.spec(spec,"endothelin1_lab_2","Core lab Endothelin-1 at 28-60 days post-discharge (at enrolment for controls), pg/ml","Numeric","2dp")

		# IL-6
			cisco$il6_lab_1<-tempAll.1$il6
			cisco$il6_lab_2<-tempAll.2$il6
			spec<-add.spec(spec,"il6_lab_1","Core lab IL-6 at enrolment, pg/ml","Numeric","2dp")
			spec<-add.spec(spec,"il6_lab_2","Core lab IL-6 at 28-60 days post-discharge (at enrolment for controls), pg/ml","Numeric","2dp")

		# ST2
			cisco$st2_lab_1<-tempAll.1$st2
			cisco$st2_lab_2<-tempAll.2$st2
			spec<-add.spec(spec,"st2_lab_1","Core lab ST2 at enrolment, ng/ml","Numeric","1dp")
			spec<-add.spec(spec,"st2_lab_2","Core lab ST2 at 28-60 days post-discharge (at enrolment for controls), ng/ml","Numeric","1dp")

		# P-SELECTIN
			cisco$pselectin_lab_1<-tempAll.1$pselectin
			cisco$pselectin_lab_2<-tempAll.2$pselectin
			spec<-add.spec(spec,"pselectin_lab_1","Core lab p-selectin at enrolment, ng/ml","Integer")
			spec<-add.spec(spec,"pselectin_lab_2","Core lab p-selectin at 28-60 days post-discharge (at enrolment for controls), ng/ml","Integer")

		# LDH
			cisco$ldh_lab_1<-tempAll.1$ldh
			cisco$ldh_lab_2<-tempAll.2$ldh
			spec<-add.spec(spec,"ldh_lab_1","Core lab LDH at enrolment, U/l","Integer")
			spec<-add.spec(spec,"ldh_lab_2","Core lab LDH at 28-60 days post-discharge (at enrolment for controls), U/l","Integer")

		# HAPTOGLOBIN
			cisco$haptoglobin_lab_1<-tempAll.1$haptoglobin
			cisco$haptoglobin_lab_2<-tempAll.2$haptoglobin
			spec<-add.spec(spec,"haptoglobin_lab_1","Core lab haptoglobin at enrolment, g/l","Numeric","2dp")
			spec<-add.spec(spec,"haptoglobin_lab_2","Core lab haptoglobin at 28-60 days post-discharge (at enrolment for controls), g/l","Numeric","2dp")

		# TOTAL BILIRUBIN
			cisco$total_bili_lab_1<-tempAll.1$total_bili
			cisco$total_bili_lab_2<-tempAll.2$total_bili
			spec<-add.spec(spec,"total_bili_lab_1","Core lab total bilirubin at enrolment, &mu;mol/l","Numeric","2dp")
			spec<-add.spec(spec,"total_bili_lab_2","Core lab total bilirubin at 28-60 days post-discharge (at enrolment for controls), &mu;mol/l","Numeric","2dp")

		# DIRECT BILIRUBIN
			cisco$direct_bili_lab_1<-tempAll.1$direct_bili
			cisco$direct_bili_lab_2<-tempAll.2$direct_bili
			spec<-add.spec(spec,"direct_bili_lab_1","Core lab direct bilirubin at enrolment, &mu;mol/l","Numeric","2dp")
			spec<-add.spec(spec,"direct_bili_lab_2","Core lab direct bilirubin at 28-60 days post-discharge (at enrolment for controls), &mu;mol/l","Numeric","2dp")

		# INDIRECT BILIRUBIN
		# CALCULATE BY SUBTRACTION (TOTAL-DIRECT), WITH ANY NEGATIVE VALUES SET TO ZERO
			cisco$indirect_bili_lab_1<-pmax(0,tempAll.1$total_bili-tempAll.1$direct_bili)
			cisco$indirect_bili_lab_2<-pmax(0,tempAll.2$total_bili-tempAll.2$direct_bili)
			spec<-
				add.spec(
					spec,"direct_bili_lab_1","Core lab indirect bilirubin at enrolment, &mu;mol/l","Numeric",
					"2dp; calculated as total - direct bilirubin, with negative values set to zero")
			spec<-
				add.spec(
					spec,"direct_bili_lab_2","Core lab indirect bilirubin at 28-60 days post-discharge (at enrolment for controls), &mu;mol/l","Numeric",
					"2dp; calculated as total - direct bilirubin, with negative values set to zero")

	# CREATININE

		# LINKING VARIABLES
			creat.data$id<-
				paste(
					substring(creat.data$"cisco-id",7,8),
					substring(creat.data$"cisco-id",10,12),
					sep="")

			creat.data$visit<-
				ifelse(nchar(creat.data$"cisco-id")<14,1,2)

		# INSERT V1 DATA AT V2 FOR THE CONTROLS
			temp<-creat.data[is.element(creat.data$id,cisco$id[cisco$group=="Control"]),]
			temp$visit<-2
			creat.data<-rbind(creat.data,temp)

		# CREATININE
			cisco$creat_lab_1<-lab.numeric(creat.data$"creatinine (?mol/l)")[match(paste(cisco$id,1),paste(creat.data$id,creat.data$visit))]
			cisco$creat_lab_2<-lab.numeric(creat.data$"creatinine (?mol/l)")[match(paste(cisco$id,2),paste(creat.data$id,creat.data$visit))]
			spec<-add.spec(spec,"creat_lab_1","Core lab creatinine at enrolment, &mu;mol/l","Integer")
			spec<-add.spec(spec,"creat_lab_2","Core lab creatinine at 28-60 days post-discharge (at enrolment for controls), &mu;mol/l","Integer")

		# eGFR

			cisco$egfr_lab_1<-egfr.fun(cisco$creat_lab_1,cisco$age,cisco$sex=="Female",cisco$race_pd=="Black")
			cisco$egfr_lab_2<-egfr.fun(cisco$creat_lab_2,cisco$age,cisco$sex=="Female",cisco$race_pd=="Black")
			spec<-add.spec(spec,"egfr_lab_1","Core lab eGFR at enrolment, ml/min/1.73m<sup>2</sup>","Numeric","2dp")
			spec<-add.spec(spec,"egfr_lab_2","Core lab eGFR at 28-60 days post-discharge (at enrolment for controls), ml/min/1.73m<sup>2</sup>","Numeric","2dp")
	
			cisco$rendys_lab_1<-factor(cisco$egfr_lab_1<60,c(T,F),c("Yes","No"))
			cisco$rendys_lab_2<-factor(cisco$egfr_lab_2<60,c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"rendys_lab_1","Core lab eGFR &lt;60 ml/min/1.73m<sup>2</sup> at enrolment","Text","Yes, No")
			spec<-add.spec(spec,"rendys_lab_2","Core lab eGFR &lt;60 ml/min/1.73m<sup>2</sup> at 28-60 days post-discharge (at enrolment for controls)","Text","Yes, No")
	
			

	# CORE BLOODS - COAGULATION

		# CREATE 2 DATASETS, ALL IN SAME FORMAT

			temp1<-
				data.frame(
					id=substring(100000+as.numeric(coag.data4$"study #"),2,6),
					labno=as.numeric(substring(coag.data4$"lab #",6,12)),
					visit=ifelse(coag.data4$visit=="visit 2",2,1),
					pt=lab.numeric(coag.data4$"pt (9-13s)"),
					ptratio=lab.numeric(coag.data4$"pt ratio"),
					aptt=lab.numeric(coag.data4$"aptt (27-36s)"),
					apttratio=lab.numeric(coag.data4$"aptt ratio (0#9-1#1)"),
					tct=lab.numeric(coag.data4$"tct (11-15s)"),
					tctratio=lab.numeric(coag.data4$"tct ratio"),
					ddimer=lab.numeric(coag.data4$"d-dimer (0-230ng/ml)"),
					fib=lab.numeric(coag.data4$"fibrinogen (1#7-4g/l)"),
					factorviii=lab.numeric(coag.data4$"factor viii (58-152 iu/dl)"),
					antithr=lab.numeric(coag.data4$"antithrombin (82-123 iu/dl)"),
					prots=lab.numeric(coag.data4$"protein s (75-137 iu/dl)"),
					protc=lab.numeric(coag.data4$"protein c (71-146 iu/dl)"),
					vwf_gp1br=lab.numeric(coag.data4$"vwf:gp1br (52-172 iu/dl)"),
					vwf_ag=lab.numeric(coag.data4$"vwf:ag (51-170 iu/dl)"),
					comments=rep("",nrow(coag.data4)),
					stringsAsFactors=F)

			temp3<-
				data.frame(
					id=temp2$id[match(coag.data6$"lab number",coag.data5$"lab no")],
					visit=ifelse(coag.data6$"visit number"=="Visit 2",2,1),
					labno=as.numeric(coag.data6$"lab number"),
					pt=lab.numeric(coag.data6$"pt"),
					ptratio=lab.numeric(rep(NA,nrow(coag.data6))),
					aptt=lab.numeric(coag.data6$"aptt"),
					apttratio=lab.numeric(rep(NA,nrow(coag.data6))),
					tct=lab.numeric(coag.data6$"tct"),
					tctratio=lab.numeric(rep(NA,nrow(coag.data6))),
					ddimer=lab.numeric(coag.data6$"d-dimer"),
					fib=lab.numeric(coag.data6$"fib"),
					factorviii=lab.numeric(coag.data6$"f#viii"),
					antithr=lab.numeric(coag.data6$"antithrombin act#"),
					prots=lab.numeric(coag.data6$"free protein s"),
					protc=lab.numeric(coag.data6$"protein c  act#"),
					vwf_gp1br=lab.numeric(coag.data6$"vwf:gp1br"),
					vwf_ag=lab.numeric(coag.data6$"vwf:ag"),
					comments=coag.data6$comments,
					stringsAsFactors=F)

			temp1<-temp1[order(temp1$id,temp1$visit),]
			temp3<-temp3[order(temp3$id,temp3$visit),]

			temp1$group<-cisco$group[match(temp1$id,cisco$id)]
			temp3$group<-cisco$group[match(temp3$id,cisco$id)]

		# 4 RECORDS IN BOTH DATASETS - ALL MISSING IN temp3 - USE temp1 DATA

			tempAll<-rbind(temp1,temp3[!is.element(paste(temp3$id,temp3$visit),paste(temp1$id,temp1$visit)),])

		# INSERT V1 DATA AT V2 FOR THE CONTROLS
			temp<-tempAll[tempAll$group=="Control",]
			temp$visit<-2
			tempAll<-rbind(tempAll,temp)

		# PROTHROMBIN TIME
			cisco$pt_lab_1<-tempAll$pt[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$pt_lab_2<-tempAll$pt[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"pt_lab_1","Core lab prothrombin time at enrolment, s","Integer")
			spec<-add.spec(spec,"pt_lab_2","Core lab prothrombin time at 28-60 days post-discharge, s","Integer")

		# PT RATIO
			cisco$ptratio_lab_1<-tempAll$ptratio[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$ptratio_lab_2<-tempAll$ptratio[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"ptratio_lab_1","Core lab PT ratio at enrolment","Numeric","1dp")
			spec<-add.spec(spec,"ptratio_lab_2","Core lab PT ratio at 28-60 days post-discharge","Numeric","1dp")

		# APTT
			cisco$aptt_lab_1<-tempAll$aptt[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$aptt_lab_2<-tempAll$aptt[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"aptt_lab_1","Core lab APTT at enrolment, s","Integer")
			spec<-add.spec(spec,"aptt_lab_2","Core lab APTT at 28-60 days post-discharge, s","Integer")

		# APTT RATIO
			cisco$apttratio_lab_1<-tempAll$apttratio[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$apttratio_lab_2<-tempAll$apttratio[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"apttratio_lab_1","Core lab APTT ratio at enrolment","Numeric","1dp")
			spec<-add.spec(spec,"apttratio_lab_2","Core lab APTT ratio at 28-60 days post-discharge","Numeric","1dp")

		# TCT
			cisco$tct_lab_1<-tempAll$tct[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$tct_lab_2<-tempAll$tct[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"tct_lab_1","Core lab TCT at enrolment, s","Integer")
			spec<-add.spec(spec,"tct_lab_2","Core lab TCT at 28-60 days post-discharge, s","Integer")

		# TCT RATIO
			cisco$tctratio_lab_1<-tempAll$tctratio[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$tctratio_lab_2<-tempAll$tctratio[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"tctratio_lab_1","Core lab TCT ratio at enrolment","Numeric","1dp")
			spec<-add.spec(spec,"tctratio_lab_2","Core lab TCT ratio at 28-60 days post-discharge","Numeric","1dp")

		# D-DIMER
			cisco$ddimer_lab_1<-tempAll$ddimer[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$ddimer_lab_2<-tempAll$ddimer[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"ddimer_lab_1","Core lab D-Dimer at enrolment, ng/ml","Integer")
			spec<-add.spec(spec,"ddimer_lab_2","Core lab D-Dimer at 28-60 days post-discharge, ng/ml","Integer")

		# FIBRINOGEN
			cisco$fib_lab_1<-tempAll$fib[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$fib_lab_2<-tempAll$fib[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"fib_lab_1","Core lab fibrinogen at enrolment, g/l","Numeric","1dp")
			spec<-add.spec(spec,"fib_lab_2","Core lab fibrinogen at 28-60 days post-discharge, g/l","Numeric","1dp")

		# FACTOR VIII
			cisco$factorviii_lab_1<-tempAll$factorviii[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$factorviii_lab_2<-tempAll$factorviii[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"factorviii_lab_1","Core lab Factor VIII at enrolment, IU/dl","Numeric","1dp")
			spec<-add.spec(spec,"factorviii_lab_2","Core lab Factor VIII at 28-60 days post-discharge, IU/dl","Numeric","1dp")

		# ANTITHROMBIN
			cisco$antithr_lab_1<-tempAll$antithr[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$antithr_lab_2<-tempAll$antithr[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"antithr_lab_1","Core lab antithrombin at enrolment, IU/dl","Integer")
			spec<-add.spec(spec,"antithr_lab_2","Core lab antithrombin at 28-60 days post-discharge, IU/dl","Integer")

		# PROTEIN S
			cisco$prots_lab_1<-tempAll$prots[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$prots_lab_2<-tempAll$prots[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"prots_lab_1","Core lab protein S at enrolment, IU/dl","Integer")
			spec<-add.spec(spec,"prots_lab_2","Core lab protein S at 28-60 days post-discharge, IU/dl","Integer")

		# PROTEIN C
			cisco$protc_lab_1<-tempAll$protc[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$protc_lab_2<-tempAll$protc[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"protc_lab_1","Core lab protein C at enrolment, IU/dl","Integer")
			spec<-add.spec(spec,"protc_lab_2","Core lab protein C at 28-60 days post-discharge, IU/dl","Integer")

		# VWF:GP1bR
			cisco$vwf_gp1br_lab_1<-tempAll$vwf_gp1br[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$vwf_gp1br_lab_2<-tempAll$vwf_gp1br[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"vwf_gp1br_lab_1","Core lab VWF:GP1bR at enrolment, IU/dl","Integer")
			spec<-add.spec(spec,"vwf_gp1br_lab_2","Core lab VWF:GP1bR at 28-60 days post-discharge, IU/dl","Integer")

		# VWF:Ag
			cisco$vwf_ag_lab_1<-tempAll$vwf_ag[match(paste(cisco$id,1),paste(tempAll$id,tempAll$visit))]
			cisco$vwf_ag_lab_2<-tempAll$vwf_ag[match(paste(cisco$id,2),paste(tempAll$id,tempAll$visit))]
			spec<-add.spec(spec,"vwf_ag_lab_1","Core lab VWF:Ag at enrolment, IU/dl","Integer")
			spec<-add.spec(spec,"vwf_ag_lab_2","Core lab VWF:Ag at 28-60 days post-discharge, IU/dl","Integer")

	# URINE BIMARKERS

		# ACR
			cisco$acr_1<-lab.numeric(crf1.data$uri$uri001[match(cisco$id,crf1.data$uri$sno)])
			cisco$acr_2<-ifelse(cisco$group=="Control",cisco$acr_1,lab.numeric(crf2.data$uri$uri001[match(cisco$id,crf2.data$uri$sno)]))

		# SET LARGE NEGATIVE VALUE TO MISSING
			spec<-add.spec(spec,"acr_1","Albumin:creatinine ratio at enrolment","Numeric","2dp")
			spec<-add.spec(spec,"acr_2","Albumin:creatinine ratio at 28-60 days post-discharge (at enrolment for controls)","Numeric","2dp")

	# EQ5D
		cisco$eq5d_hu_1<-eq5d(apply(crf1.data$eqd[,paste("eqd00",2:6,sep="")],1,paste,collapse=""),type="VT",version="5L",country="England",ignore.invalid=T)[match(cisco$id,crf1.data$eqd$sno)]
		cisco$eq5d_hu_2<-
			ifelse(
				cisco$group=="Control",cisco$eq5d_hu_1,
				eq5d(apply(crf2.data$eqd[,paste("eqd00",2:6,sep="")],1,paste,collapse=""),type="VT",version="5L",country="England",ignore.invalid=T)[match(cisco$id,crf2.data$eqd$sno)])
		spec<-add.spec(spec,"eq5d_hu_1","EQ-5D Health Utility at enrolment","Numeric","3dp. Uses 'eq5d' paskage, type='VT', version='5L', country='England'")
		spec<-add.spec(spec,"eq5d_hu_2","EQ-5D Health Utility at 28-60 days post-discharge","Numeric","3dp. Uses 'eq5d' paskage, type='VT', version='5L', country='England'")
		
		cisco$eq5d_vas_1<-crf1.data$eqd$eqd007[match(cisco$id,crf1.data$eqd$sno)]
		cisco$eq5d_vas_2<-
			ifelse(
				cisco$group=="Control",cisco$eq5d_vas_1,
				crf2.data$eqd$eqd007[match(cisco$id,crf2.data$eqd$sno)])
		spec<-add.spec(spec,"eq5d_vas_1","EQ-5D VAS at enrolment","Integer")
		spec<-add.spec(spec,"eq5d_vas_2","EQ-5D VAS at 28-60 days post-discharge","Integer")

	# BRIEF ILLNESS PERCEPTION QUESTIONNAIRE
		cisco$bip_1<-
			bip.fun(
				crf1.data$bip$bip002-1,crf1.data$bip$bip003-1,crf1.data$bip$bip004-1,crf1.data$bip$bip005-1,
				crf1.data$bip$bip006-1,crf1.data$bip$bip007-1,crf1.data$bip$bip008-1,crf1.data$bip$bip009-1)[match(cisco$id,crf1.data$bip$sno)]
		cisco$bip_2<-
			ifelse(
				cisco$group=="Control",cisco$bip_1,
				bip.fun(
					crf2.data$bip$bip002-1,crf2.data$bip$bip003-1,crf2.data$bip$bip004-1,crf2.data$bip$bip005-1,
					crf2.data$bip$bip006-1,crf2.data$bip$bip007-1,crf2.data$bip$bip008-1,crf2.data$bip$bip009-1)[match(cisco$id,crf2.data$bip$sno)])
		spec<-add.spec(spec,"bip_1","Brief illness perception questionnaire score at enrolment","Integer","Impute missing items if at least 50% answered")
		spec<-add.spec(spec,"bip_2","Brief illness perception questionnaire score at 28-60 days post-discharge (at enrolment for controls)","Integer","Impute missing items if at least 50% answered")

	# PHQ4
		cisco$phq4_anx_1<-(crf1.data$phq$phq003+crf1.data$phq$phq004-2)[match(cisco$id,crf1.data$phq$sno)]
		cisco$phq4_dep_1<-(crf1.data$phq$phq005+crf1.data$phq$phq006-2)[match(cisco$id,crf1.data$phq$sno)]
		cisco$phq4_1<-cisco$phq4_anx_1+cisco$phq4_dep_1
		cisco$phq4_anx_2<-
			ifelse(
				cisco$group=="Control",cisco$phq4_anx_1,
				(crf2.data$phq$phq003+crf2.data$phq$phq004-2)[match(cisco$id,crf2.data$phq$sno)])
		cisco$phq4_dep_2<-
			ifelse(
				cisco$group=="Control",cisco$phq4_dep_1,
				(crf2.data$phq$phq005+crf2.data$phq$phq006-2)[match(cisco$id,crf2.data$phq$sno)])
		cisco$phq4_2<-cisco$phq4_anx_2+cisco$phq4_dep_2
		spec<-add.spec(spec,"phq4_anx_1","PHQ-4 Anxiety score at enrolment","Integer","Any missing item, then missing score")
		spec<-add.spec(spec,"phq4_dep_1","PHQ-4 Depression score at enrolment","Integer","Any missing item, then missing score")
		spec<-add.spec(spec,"phq4_1","PHQ-4 Total score at enrolment","Integer","Any missing item, then missing score")
		spec<-add.spec(spec,"phq4_anx_2","PHQ-4 Anxiety score at 28-60 days post-discharge (at enrolment for controls)","Integer","Any missing item, then missing score")
		spec<-add.spec(spec,"phq4_dep_2","PHQ-4 Depression score at 28-60 days post-discharge (at enrolment for controls)","Integer","Any missing item, then missing score")
		spec<-add.spec(spec,"phq4_2","PHQ-4 Total score at 28-60 days post-discharge (at enrolment for controls)","Integer","Any missing item, then missing score")

	# IPAQ
		cisco$ipaq_cat_1<-with(crf1.data$ipq,ipaq.short.fun(ipq001,ipq002,ipq003,ipq005,ipq006,ipq007,ipq008,ipq009,ipq010,ipq011,ipq012,sno))$ipaq_cat[match(cisco$id,crf1.data$ipq$sno)]
		cisco$ipaq_met_1<-with(crf1.data$ipq,ipaq.short.fun(ipq001,ipq002,ipq003,ipq005,ipq006,ipq007,ipq008,ipq009,ipq010,ipq011,ipq012,sno))$met[match(cisco$id,crf1.data$ipq$sno)]
		cisco$ipaq_cat_2<-with(crf2.data$ipq,ipaq.short.fun(ipq001,ipq002,ipq003,ipq005,ipq006,ipq007,ipq008,ipq009,ipq010,ipq011,ipq012,sno))$ipaq_cat[match(cisco$id,crf2.data$ipq$sno)]
		cisco$ipaq_cat_2[cisco$group=="Control"]<-cisco$ipaq_cat_1[cisco$group=="Control"]
		cisco$ipaq_met_2<-
			ifelse(
				cisco$group=="Control",cisco$ipaq_met_1,
				with(crf2.data$ipq,ipaq.short.fun(ipq001,ipq002,ipq003,ipq005,ipq006,ipq007,ipq008,ipq009,ipq010,ipq011,ipq012,sno))$met[match(cisco$id,crf2.data$ipq$sno)])

		spec<-add.spec(spec,"ipaq_cat_1","IPAQ Activity Category at enrolment","Text","Low, Moderate, High")
		spec<-add.spec(spec,"ipaq_met_1","IPAQ MET Minutes at enrolment","Integer")
		spec<-add.spec(spec,"ipaq_cat_2","IPAQ Activity Category at 28-60 days post-discharge (at enrolment for controls)","Text","Low, Moderate, High")
		spec<-add.spec(spec,"ipaq_met_2","IPAQ MET Minutes at 28-60 days post-discharge (at enrolment for controls)","Integer")

	# DUKE ACTIVITY STATUS INDEX
		cisco$dasi_score_1<-with(crf1.data$dai,dasi.fun(dai001,dai002,dai003,dai004,dai005,dai006,dai007,dai008,dai009,dai010,dai011,dai012))$dasi_score[match(cisco$id,crf1.data$dai$sno)]
		cisco$dasi_vo2max_1<-with(crf1.data$dai,dasi.fun(dai001,dai002,dai003,dai004,dai005,dai006,dai007,dai008,dai009,dai010,dai011,dai012))$dasi_vo2max[match(cisco$id,crf1.data$dai$sno)]
		cisco$dasi_score_2<-
			ifelse(
				cisco$group=="Control",cisco$dasi_score_1,
				with(crf2.data$dai,dasi.fun(dai001,dai002,dai003,dai004,dai005,dai006,dai007,dai008,dai009,dai010,dai011,dai012))$dasi_score[match(cisco$id,crf2.data$dai$sno)])
		cisco$dasi_vo2max_2<-
			ifelse(
				cisco$group=="Control",cisco$dasi_vo2max_1,
				with(crf2.data$dai,dasi.fun(dai001,dai002,dai003,dai004,dai005,dai006,dai007,dai008,dai009,dai010,dai011,dai012))$dasi_vo2max[match(cisco$id,crf2.data$dai$sno)])

		spec<-add.spec(spec,"dasi_score_1","DASI Score at enrolment","Numeric","2dp")
		spec<-add.spec(spec,"dasi_vo2max_1","DASI Estimated VO<sub>2</sub> max at enrolment","Numeric","4dp")
		spec<-add.spec(spec,"dasi_score_2","DASI Score at 28-60 days post-discharge (at enrolment for controls)","Numeric","2dp")
		spec<-add.spec(spec,"dasi_vo2max_2","DASI Estimated VO<sub>2</sub> max at 28-60 days post-discharge (at enrolment for controls)","Numeric","4dp")

	# RENAL DATA

		renal.data$id<-substring(100000+as.numeric(renal.data$id),2,6)

		# AKI
			cisco$aki<-factor(as.numeric(renal.data$aki),1:0,c("Yes","No"))[match(cisco$id,renal.data$id)]
			spec<-add.spec(spec,"aki","AKI during COVID-19 illness","Text","Yes, No")

		# AVERAGE T1 CORTEX TR550
			cisco$avt1cort_tr550<-round(as.numeric(renal.data$"cort t1 ave tr550")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avt1cort_tr550","Average of T1 cortex value of right and left kidneys using TR550, ms","Numeric","2dp")

		# AVERAGE T1 MEDULLA TR550
			cisco$avt1med_tr550<-round(as.numeric(renal.data$"med t1 ave tr550")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avt1med_tr550","Average of T1 medulla value of right and left kidneys using TR550, ms","Numeric","2dp")

		# AVERAGE T1 CORTEX TR1000
			cisco$avt1cort_tr1000<-round(as.numeric(renal.data$"cort  t1 ave tr1000")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avt1cort_tr1000","Average of T1 cortex value of right and left kidneys using TR1000, ms","Numeric","2dp")

		# AVERAGE T1 MEDULLA TR1000
			cisco$avt1med_tr1000<-round(as.numeric(renal.data$"med t1 ave tr1000")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avt1med_tr1000","Average of T1 medulla value of right and left kidneys using TR1000, ms","Numeric","2dp")

		# AVERAGE T1 CORTICOMEDULLARY DIFFERENTIATION TR550
			cisco$avt1diff_tr550<-round(as.numeric(renal.data$"cmd ?t1 ave tr550")[match(cisco$id,renal.data$id)],4)
			spec<-add.spec(spec,"avt1diff_tr550","Average T1 corticomedullary differentiation of left and right kidneys using TR550, ms","Numeric","4dp")

		# AVERAGE T1 CORTICOMEDULLARY DIFFERENTIATION TR1000
			cisco$avt1diff_tr1000<-round(as.numeric(renal.data$"cmd ?t1 ave tr1000")[match(cisco$id,renal.data$id)],4)
			spec<-add.spec(spec,"avt1diff_tr1000","Average T1 corticomedullary differentiation of left and right kidneys using TR1000, ms","Numeric","4dp")

		# AVERAGE T2 CORTEX
			cisco$avt2cort<-round(as.numeric(renal.data$"cortex t2 ave")[match(cisco$id,renal.data$id)],4)
			spec<-add.spec(spec,"avt2cort","Average of T2 cortex value of right and left kidneys, ms","Numeric","4dp")

		# AVERAGE T2 MEDULLA
			cisco$avt2med<-round(as.numeric(renal.data$"med t2 ave")[match(cisco$id,renal.data$id)],4)
			spec<-add.spec(spec,"avt2med","Average of T2 medulla value of right and left kidneys, ms","Numeric","4dp")

		# AVERAGE CORTEX ADC
			cisco$avcortadc<-round(as.numeric(renal.data$"cortex adc ave")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avcortadc","Average of cortex apparent diffusion coefficient values of right and left kidneys, mm<sup>2</sup>/s","Numeric","2dp")

		# AVERAGE MEDULLA ADC
			cisco$avmedadc<-round(as.numeric(renal.data$"med adc ave")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avmedadc","Average of medulla apparent diffusion coefficient values of right and left kidneys, mm<sup>2</sup>/s","Numeric","2dp")

		# AVERAGE VOLUME
			cisco$avvol<-round(as.numeric(renal.data$"vol ave")[match(cisco$id,renal.data$id)],2)
			spec<-add.spec(spec,"avvol","Average volume of right and left kidneys, ml","Numeric","2dp")

	# V3 OUTCOMES
	
		# INDICATOR FOR V3 ANALYSIS
		# EXCLUDE CONTROLS WHO HAVE HAD COVID
			cisco$paper_v3<-
				factor(
					ifelse(as.numeric(vitalstatus.data$covid_status[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))])==3,1,cisco$paper),
					1:2,c("No","Yes"))
			spec<-add.spec(spec,"paper_v3","Included in paper, for visit 3 analysis?","Text","Control who has not had COVID, or with primary outcome.<br>No, Yes")

		# DATE OF FOLLOW-UP

			cisco$v3_date<-
				chron(
					ifelse(
						substring(vitalstatus.data$v3date,1,4)==2020,
						paste(2021,substring(vitalstatus.data$v3date,6,10),sep="-"),
						substring(vitalstatus.data$v3date,1,10)),
					format="y-m-d",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
			spec<-add.spec(spec,"v3_date","Date of visit 3","Date","DD/MM/YY; assume any date in 2020 should be 2021")

		# DEATH

			# ANY DEATH
				cisco$death<-factor(as.numeric(vitalstatus.data$alive),0:1,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
				spec<-add.spec(spec,"death","Death before visit 3","Text","No, Yes")

			# DATE OF DEATH
				cisco$death_date<-chron(vitalstatus.data$deathdate,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
				spec<-add.spec(spec,"death_date","Date of death","Date","DD/MM/YY")

			# CV DEATH
				cisco$death_cv<-factor(pmax(0,as.numeric(vitalstatus.data$cardiovascular),na.rm=T),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
				spec<-add.spec(spec,"death_cv","Cardiovascular death before visit 3","Text","No, Yes")

			# RENAL DEATH
				cisco$death_renal<-factor(pmax(0,as.numeric(vitalstatus.data$renal),na.rm=T),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
				spec<-add.spec(spec,"death_renal","Renal death before visit 3","Text","No, Yes")

			# RESPIRATORY DEATH
				cisco$death_resp<-factor(pmax(0,as.numeric(vitalstatus.data$respiratory),na.rm=T),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(vitalstatus.data$id),2,6))]
				spec<-add.spec(spec,"death_resp","Respiratory death before visit 3","Text","No, Yes")

		# HOSPITALISATIONS

			# CHECK DATES - EXCLUDE IF BEFORE COVID DISCHARGE (OR V1 FOR CONTROLS)
				secondarycare.use<-secondarycare.data
				secondarycare.use$id<-substring(100000+as.numeric(secondarycare.use$id),2,6)
				secondarycare.use$hospitalized<-as.numeric(secondarycare.use$hospitalized)
				secondarycare.use$cv<-as.numeric(secondarycare.use$cv)
				secondarycare.use$renal<-as.numeric(secondarycare.use$renal)
				secondarycare.use$resp<-as.numeric(secondarycare.use$resp)

				secondarycare.use$date.adm<-chron(substring(secondarycare.use$scadmdate,1,10),format="y-m-d",out.format="d/m/y")
				secondarycare.use$date.covid.disch<-cisco$disch_date[match(secondarycare.use$id,cisco$id)]
				secondarycare.use$date.v1<-cisco$v1_date[match(secondarycare.use$id,cisco$id)]
				secondarycare.use$group<-cisco$group[match(secondarycare.use$id,cisco$id)]

				secondarycare.use<-secondarycare.use[order(as.numeric(secondarycare.use$id),secondarycare.use$date.adm),]

			# ANY HOSPITALISATION - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_hosp_fu<-table(factor(secondarycare.use$id[secondarycare.use$hospitalized==1],cisco$id))
				spec<-add.spec(spec,"n_hosp_fu","Number of hospitalisations before visit 3","Integer")

				cisco$any_hosp_fu<-factor(cisco$n_hosp_fu>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_hosp_fu","Any hospitalisation before visit 3","Text","No, Yes")

				cisco$first_hosp_fu_date<-secondarycare.use$date.adm[match(paste(cisco$id,1),paste(secondarycare.use$id,secondarycare.use$hospitalized))]
				spec<-add.spec(spec,"first_hosp_fu_date","Date of first hospitalisation before visit 3","Date","DD/MM/YY")

			# CV HOSPITALISATION - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_cv_hosp_fu<-table(factor(secondarycare.use$id[(secondarycare.use$hospitalized==1)&(secondarycare.use$cv==1)],cisco$id))
				spec<-add.spec(spec,"n_cv_hosp_fu","Number of CV hospitalisations before visit 3","Integer")

				cisco$any_cv_hosp_fu<-factor(cisco$n_cv_hosp_fu>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_cv_hosp_fu","Any CV hospitalisation before visit 3","Text","No, Yes")

				cisco$first_cv_hosp_fu_date<-secondarycare.use$date.adm[match(paste(cisco$id,1,1),paste(secondarycare.use$id,secondarycare.use$hospitalized,secondarycare.use$cv))]
				spec<-add.spec(spec,"first_cv_hosp_fu_date","Date of first CV hospitalisation before visit 3","Date","DD/MM/YY")

			# RENAL HOSPITALISATION - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_renal_hosp_fu<-table(factor(secondarycare.use$id[(secondarycare.use$hospitalized==1)&(secondarycare.use$renal==1)],cisco$id))
				spec<-add.spec(spec,"n_renal_hosp_fu","Number of renal hospitalisations before visit 3","Integer")

				cisco$any_renal_hosp_fu<-factor(cisco$n_renal_hosp_fu>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_renal_hosp_fu","Any renal hospitalisation before visit 3","Text","No, Yes")

				cisco$first_renal_hosp_fu_date<-secondarycare.use$date.adm[match(paste(cisco$id,1,1),paste(secondarycare.use$id,secondarycare.use$hospitalized,secondarycare.use$renal))]
				spec<-add.spec(spec,"first_renal_hosp_fu_date","Date of first renal hospitalisation before visit 3","Date","DD/MM/YY")

			# RESPIRATORY HOSPITALISATION - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_resp_hosp_fu<-table(factor(secondarycare.use$id[(secondarycare.use$hospitalized==1)&(secondarycare.use$resp==1)],cisco$id))
				spec<-add.spec(spec,"n_resp_hosp_fu","Number of respiratory hospitalisations before visit 3","Integer")

				cisco$any_resp_hosp_fu<-factor(cisco$n_resp_hosp_fu>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_resp_hosp_fu","Any respiratory hospitalisation before visit 3","Text","No, Yes")

				cisco$first_resp_hosp_fu_date<-secondarycare.use$date.adm[match(paste(cisco$id,1,1),paste(secondarycare.use$id,secondarycare.use$hospitalized,secondarycare.use$resp))]
				spec<-add.spec(spec,"first_resp_hosp_fu_date","Date of first respiratory hospitalisation before visit 3","Date","DD/MM/YY")

		# DEATH OR HOSPITALISATION

			cisco$death_hosp<-factor((cisco$death=="Yes")|(cisco$any_hosp_fu=="Yes"),c(T,F),c("Yes","No"))
			spec<-add.spec(spec,"death_hosp","Death or hospitalisation before visit 3","Text","No, Yes")

			cisco$death_hosp_date<-pmin(cisco$death_date,cisco$first_hosp_fu_date,na.rm=T)
			spec<-add.spec(spec,"death_hosp_date","Date of death or first hospitalisation before visit 3","Date","DD/MM/YY")

		# CARDIOVASCULAR OUTCOMES

			# MI
				cisco$mi_v3<-factor(as.numeric(events.data$mi_acs),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"mi_v3","MI before visit 3","Text","No, Yes")

				cisco$mi_v3_date<-chron(events.data$mi_acs_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"mi_v3_date","Date of first MI before visit 3","Date","DD/MM/YY")

			# PCI
				cisco$pci_v3<-factor(as.numeric(events.data$pci),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pci_v3","PCI before visit 3","Text","No, Yes")

				cisco$pci_v3_date<-chron(events.data$pci_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pci_v3_date","Date of first PCI before visit 3","Date","DD/MM/YY")

			# CABG
				cisco$cabg_v3<-factor(as.numeric(events.data$cabg),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"cabg_v3","CABG before visit 3","Text","No, Yes")

				cisco$cabg_v3_date<-
					chron(ifelse(events.data$cabg_date=="",NA,events.data$cabg_date),format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"cabg_v3_date","Date of first CABG before visit 3","Date","DD/MM/YY")

			# CEREBROVASCULAR ACCIDENT
				cisco$cva_v3<-factor(as.numeric(events.data$cva),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"cva_v3","Cerebrovascular accident before visit 3","Text","No, Yes")

				cisco$cva_v3_date<-chron(events.data$cva_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"cva_v3_date","Date of first cerebrovascular accident before visit 3","Date","DD/MM/YY")

			# HEART FAILURE
				cisco$hf_v3<-factor(as.numeric(events.data$hf),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"hf_v3","Heart failure event before visit 3","Text","No, Yes")

				cisco$hf_v3_date<-chron(events.data$hf_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"hf_v3_date","Date of first heart failure event before visit 3","Date","DD/MM/YY")

			# DVT
				cisco$dvt_v3<-factor(as.numeric(events.data$dvt),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"dvt_v3","DVT before visit 3","Text","No, Yes")

				cisco$dvt_v3_date<-chron(events.data$dvt_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"dvt_v3_date","Date of first DVT before visit 3","Date","DD/MM/YY")

			# NEW AF
				cisco$newaf_v3<-factor(as.numeric(events.data$newaf),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"newaf_v3","New AF before visit 3","Text","No, Yes")

				cisco$newaf_v3_date<-chron(events.data$newaf_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				cisco$newaf_v3_date<-
					chron(
						ifelse(
							is.na(cisco$newaf_v3_date),
							chron(events.data$newaf_date,format="m/d/y")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))],
							cisco$newaf_v3_date),
						out.format="day mon year")
				spec<-add.spec(spec,"newaf_v3_date","Date of first new AF before visit 3","Date","DD/MM/YY")

			# VETRICULAR TACHYCARDIA OR FIBRILLATION
				cisco$vf_vt_v3<-factor(as.numeric(events.data$vf_vt),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"vf_vt_v3","VT or VF before visit 3","Text","No, Yes")

				cisco$vf_vt_v3_date<-chron(events.data$vf_vt_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"vf_vt_v3_date","Date of first VT or VF before visit 3","Date","DD/MM/YY")

		# RESPIRATORY OUTCOMES

			# PULMONARY FIBROSIS
				cisco$pulfib_v3<-factor(as.numeric(events.data$pulfib),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pulfib_v3","Pulmonary fibrosis before visit 3","Text","No, Yes")

				cisco$pulfib_v3_date<-chron(events.data$pulfib_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pulfib_v3_date","Date of first pulmonary fibrosis event before visit 3","Date","DD/MM/YY")

			# NEW DIAGNOSIS ASTHMA
				cisco$newasthma_v3<-factor(as.numeric(events.data$newasthma),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"newasthma_v3","New diagnosis asthma before visit 3","Text","No, Yes")

				cisco$newasthma_v3_date<-chron(events.data$newasthma_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"newasthma_v3_date","Date of new asthma diagnosis before visit 3","Date","DD/MM/YY")

			# PULMONARY EMBOLISM
				cisco$pte_v3<-factor(as.numeric(events.data$pte),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pte_v3","Pulmonary thromboembolism before visit 3","Text","No, Yes")

				cisco$pte_v3_date<-chron(events.data$pte_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"pte_v3_date","Date of first pulmonary thromboembolism event before visit 3","Date","DD/MM/YY")

			# LONG TERM OXYGEN THERAPY
				cisco$ltot_v3<-factor(as.numeric(events.data$ltot),1:0,c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"ltot_v3","Long term oxygen therapy before visit 3","Text","No, Yes")

				cisco$ltot_v3_date<-chron(events.data$ltot_date,format="d/m/y",out.format="day mon year")[match(cisco$id,substring(100000+as.numeric(events.data$id),2,6))]
				spec<-add.spec(spec,"ltot_v3_date","Date of long term oxygen therapy before visit 3","Date","DD/MM/YY")

		# OUTPATIENTS

			# CHECK DATES - EXCLUDE IF BEFORE COVID DISCHARGE (OR V1 FOR CONTROLS)
				outpatient.use<-outpatient.data
				outpatient.use$id<-substring(100000+as.numeric(outpatient.use$id),2,6)
				outpatient.use$acutecovid<-as.numeric(outpatient.use$acutecovid_symptoms)
				outpatient.use$ongoingcovid<-as.numeric(outpatient.use$ongoing_symptoms)
				outpatient.use$longcovid<-as.numeric(outpatient.use$longcovid_symptoms)

				outpatient.use$date.op<-chron(substring(outpatient.use$opdate,1,10),format="y-m-d",out.format="d/m/y")
				outpatient.use$date.covid.disch<-cisco$disch_date[match(outpatient.use$id,cisco$id)]
				outpatient.use$date.v1<-cisco$v1_date[match(outpatient.use$id,cisco$id)]
				outpatient.use$group<-cisco$group[match(outpatient.use$id,cisco$id)]

				outpatient.use<-outpatient.use[order(as.numeric(outpatient.use$id),outpatient.use$date.op),]

			# ANY REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_op_v3<-table(factor(outpatient.use$id[as.numeric(outpatient.use$noopvisit)==0],cisco$id))
				spec<-add.spec(spec,"n_op_v3","Number of outpatient referrals before visit 3","Integer")

				cisco$any_op_v3<-factor(cisco$n_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_op_v3","Any outpatient referral before visit 3","Text","No, Yes")

				cisco$first_op_v3_date<-outpatient.use$date.op[match(cisco$id,outpatient.use$id)]
				spec<-add.spec(spec,"first_op_v3_date","Date of first outpatient referral before visit 3","Date","DD/MM/YY")

			# ACUTE COVID REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_acute_op_v3<-table(factor(outpatient.use$id[outpatient.use$acutecovid==1],cisco$id))
				spec<-add.spec(spec,"n_acute_op_v3","Number of outpatient referrals relating to acute COVID symptoms before visit 3","Integer")

				cisco$any_acute_op_v3<-factor(cisco$n_acute_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_acute_op_v3","Any outpatient referral relating to acute COVID symptoms before visit 3","Text","No, Yes")

				cisco$first_acute_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,1),paste(outpatient.use$id,outpatient.use$acutecovid))]
				spec<-add.spec(spec,"first_acute_op_v3_date","Date of first outpatient referral relating to acute COVID symptoms before visit 3","Date","DD/MM/YY")

			# ONGOING COVID REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_ongoing_op_v3<-table(factor(outpatient.use$id[outpatient.use$ongoingcovid==1],cisco$id))
				spec<-add.spec(spec,"n_ongoing_op_v3","Number of outpatient referrals relating to ongoing COVID symptoms before visit 3","Integer")

				cisco$any_ongoing_op_v3<-factor(cisco$n_ongoing_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_ongoing_op_v3","Any outpatient referral relating to ongoing COVID symptoms before visit 3","Text","No, Yes")

				cisco$first_ongoing_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,1),paste(outpatient.use$id,outpatient.use$ongoingcovid))]
				spec<-add.spec(spec,"first_ongoing_op_v3_date","Date of first outpatient referral relating to ongoing COVID symptoms before visit 3","Date","DD/MM/YY")

			# LONG COVID REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_long_op_v3<-table(factor(outpatient.use$id[outpatient.use$longcovid==1],cisco$id))
				spec<-add.spec(spec,"n_long_op_v3","Number of outpatient referrals relating to long COVID symptoms before visit 3","Integer")

				cisco$any_long_op_v3<-factor(cisco$n_long_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_long_op_v3","Any outpatient referral relating to long COVID symptoms before visit 3","Text","No, Yes")

				cisco$first_long_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,1),paste(outpatient.use$id,outpatient.use$longcovid))]
				spec<-add.spec(spec,"first_long_op_v3_date","Date of first outpatient referral relating to long COVID symptoms before visit 3","Date","DD/MM/YY")

			# CARDIOLOGY REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_cardio_op_v3<-table(factor(outpatient.use$id[outpatient.use$opspecialty=="Cardiology"],cisco$id))
				spec<-add.spec(spec,"n_cardio_op_v3","Number of cardiology outpatient referrals before visit 3","Integer")

				cisco$any_cardio_op_v3<-factor(cisco$n_cardio_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_cardio_op_v3","Any cardiology outpatient referral before visit 3","Text","No, Yes")

				cisco$first_cardio_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,"Cardiology"),paste(outpatient.use$id,outpatient.use$opspecialty))]
				spec<-add.spec(spec,"first_cardio_op_v3_date","Date of first cardiology outpatient referral before visit 3","Date","DD/MM/YY")

			# RESPIRATORY REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_resp_op_v3<-table(factor(outpatient.use$id[outpatient.use$opspecialty=="Respiratory"],cisco$id))
				spec<-add.spec(spec,"n_resp_op_v3","Number of respiratory outpatient referrals before visit 3","Integer")

				cisco$any_resp_op_v3<-factor(cisco$n_resp_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_resp_op_v3","Any respiratory outpatient referral before visit 3","Text","No, Yes")

				cisco$first_resp_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,"Respiratory"),paste(outpatient.use$id,outpatient.use$opspecialty))]
				spec<-add.spec(spec,"first_resp_op_v3_date","Date of first respiratory outpatient referral before visit 3","Date","DD/MM/YY")

			# PHYSIOTHERAPY REFERRAL - NUMBER, ANY (YES/NO), DATE OF FIRST
				cisco$n_physio_op_v3<-table(factor(outpatient.use$id[outpatient.use$opspecialty=="Physiotherapy"],cisco$id))
				spec<-add.spec(spec,"n_physio_op_v3","Number of physiotherapy outpatient referrals before visit 3","Integer")

				cisco$any_physio_op_v3<-factor(cisco$n_physio_op_v3>0,c(T,F),c("Yes","No"))
				spec<-add.spec(spec,"any_physio_op_v3","Any physiotherapy outpatient referral before visit 3","Text","No, Yes")

				cisco$first_physio_op_v3_date<-outpatient.use$date.op[match(paste(cisco$id,"Physiotherapy"),paste(outpatient.use$id,outpatient.use$opspecialty))]
				spec<-add.spec(spec,"first_physio_op_v3_date","Date of first physiotherapy outpatient referral before visit 3","Date","DD/MM/YY")

		# MEDICATIONS

			# ASPIRIN
				cisco$aspirin_v3<-factor(as.numeric(medications.data$aspirin)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"aspirin_v3","Aspirin at visit 3","Text","No, Yes")

			# P2Y12 INHIBITOR
				cisco$p2y12_v3<-factor(as.numeric(medications.data$clop_tic)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"p2y12_v3","P2Y12 inhibitor at visit 3","Text","No, Yes")

			# CALCIUM CHANNEL BLOCKER
				cisco$ccb_v3<-factor(as.numeric(medications.data$ccb)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"ccb_v3","Calcium channel blocker at visit 3","Text","No, Yes")

			# ACE INHIBITOR
				cisco$acei_v3<-factor(as.numeric(medications.data$acei)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"acei_v3","ACE inhibitor at visit 3","Text","No, Yes")

			# ARB
				cisco$arb_v3<-factor(as.numeric(medications.data$arb)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"arb_v3","ARB at visit 3","Text","No, Yes")

			# SALCUBITRIL/VALSATRTAN
				cisco$salval_v3<-factor(as.numeric(medications.data$entresto)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"salval_v3","Sacubitril/Valsartan at visit 3","Text","No, Yes")

			# STATIN
				cisco$statin_v3<-factor(as.numeric(medications.data$statin)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"statin_v3","Statin at visit 3","Text","No, Yes")

			# MRA
				cisco$mra_v3<-factor(as.numeric(medications.data$epler_spiro)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"mra_v3","Mineralocorticoid receptor antagonist at visit 3","Text","No, Yes")

			# BETA BLOCKER
				cisco$bb_v3<-factor(as.numeric(medications.data$betablocker)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"bb_v3","Beta blocker at visit 3","Text","No, Yes")

			# THIAZIDE
				cisco$thiazide_v3<-factor(as.numeric(medications.data$thiazide)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"thiazide_v3","Thiazide at visit 3","Text","No, Yes")

			# WARFARIN
				cisco$warfarin_v3<-factor(as.numeric(medications.data$warfarin)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"warfarin_v3","Warfarin at visit 3","Text","No, Yes")

			# NOAC
				cisco$noac_v3<-factor(as.numeric(medications.data$noac)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"noac_v3","Novel oral anticoagulant therapy at visit 3","Text","No, Yes")

			# INHALED STEROID
				cisco$inh_steroid_v3<-factor(as.numeric(medications.data$inhsteroid)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"inh_steroid_v3","Inhaled steroid at visit 3","Text","No, Yes")

			# INHALED BRONCHODILATOR
				cisco$inh_broncho_v3<-factor(as.numeric(medications.data$inhbroncho)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"inh_broncho_v3","Inhaled bronchodilator at visit 3","Text","No, Yes")

			# INHALED ANTIMUSCARINIC
				cisco$inh_antimusc_v3<-factor(as.numeric(medications.data$inhantimusc)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"inh_antimusc_v3","Inhaled Anti-muscarinics at visit 3","Text","No, Yes")

			# SGLT2i
				cisco$sglt2i_v3<-factor(as.numeric(medications.data$sglt2i)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"sglt2i_v3","SGLT2i at visit 3","Text","No, Yes")

			# GLP1 AGONIST
				cisco$glp1_v3<-factor(as.numeric(medications.data$glp1)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"glp1_v3","GLP1 agonist at visit 3","Text","No, Yes")

			# INSULIN
				cisco$insulin_v3<-factor(as.numeric(medications.data$insulin)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"insulin_v3","Insulin at visit 3","Text","No, Yes")

			# OTHER ANTIDIABETIC
				cisco$odiab_v3<-factor(as.numeric(medications.data$oth_diab)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"odiab_v3","Other antidiabetic at visit 3","Text","No, Yes")

			# LOOP DIURETIC
				cisco$loop_v3<-factor(as.numeric(medications.data$loop)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"loop_v3","Loop diuretic at visit 3","Text","No, Yes")

			# ORAL STEROID
				cisco$oral_steroid_v3<-factor(as.numeric(medications.data$oralsteroid)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"oral_steroid_v3","Oral steroid at visit 3","Text","No, Yes")

			# ANTIDEPRESSANT
				cisco$antidep_v3<-factor(as.numeric(medications.data$antidepressant)==1,c(T,F),c("Yes","No"))[match(cisco$id,substring(100000+as.numeric(medications.data$id),2,6))]
				spec<-add.spec(spec,"antidep_v3","Antidepressant at visit 3","Text","No, Yes")

	# CHANGE THOSE IN PAPER POPULATION WHO ARE COVID, BUT WITHOUT PCR, INTO CONTROLS, OR JUST REMOVE FROM PAPER

		# IF CROSSING OVER TO CONTROL GROUP, THEN IF COVID PATIENT HAS NO PCR, THEN REASSIGN AS CONTROL
		# USE VISIT 2 DATA WHERE RELEVANT (LABS, PROMS) AS "THE" DATA FOR THESE "CONTROLS"
			old.cisco<-new.cisco<-cisco

			flip<-(cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$pcr=="No")

			if(noncovid.crossover)
			{
				# FLIP TO CONTROL
					new.cisco$group[flip]<-"Control"

				# FOR ALL VARIABLES THAT ARE MISSING FOR ALL CONTROLS, SET TO MISSING
				# EXCEPT VARIABLES RELATING TO VISIT 3
					all.missing<-unlist(lapply(old.cisco[old.cisco$group=="Control",],function(x)sum(!is.na(x))==0))
					all.missing[grep("v3",names(old.cisco))]<-F
					new.cisco[flip,all.missing]<-NA

				# CHANGE VISIT 1 DATE TO VISIT 2 DATE - WILL USE DATA COLLECTED AT VISIT 2 WHERE AVAILABLE
					new.cisco$v1_date[flip]<-old.cisco$v2_date[flip]

				# CHANGE NOSOCOMIAL TO NO
					new.cisco$nosocomial[flip]<-"No"

				# CHANGE ANTIBODY TEST TO YES
					new.cisco$antibody[flip]<-"Yes"

				# CHANGE WHO SEVERITY
					new.cisco$who_full[flip]<-"No evidence of infection"

				# CHANGE ALL ECG MYOPERICARDITIS VALUES TO NO
					new.cisco$myoperi0[flip]<-"No"
					new.cisco$myoperi1[flip]<-"No"
					new.cisco$myoperi2[flip]<-"No"

				# CHANGE ALL ECG PREMATURE ATRIAL CONTRACTION VALUES TO NO
					new.cisco$premac0[flip]<-"No"
					new.cisco$premac1[flip]<-"No"
					new.cisco$premac2[flip]<-"No"

				# CHANGE ALL ECG PREMATURE VENTRICULAR CONTRACTION VALUES TO NO
					new.cisco$premvc0[flip]<-"No"
					new.cisco$premvc1[flip]<-"No"
					new.cisco$premvc2[flip]<-"No"

				# CHANGE ALL ECG ATRIAL FIBRILLATION / FLUTTER VALUES TO NO
					new.cisco$aff0[flip]<-"No"
					new.cisco$aff1[flip]<-"No"
					new.cisco$aff2[flip]<-"No"

				# CHANGE ALL LAB VALUES AT VISIT 1 TO THE SAME AS AT VISIT 2
				# USE V2 VALUE AT V1 IF V2 VALUE AVAILABLE
				# IF V2 VALUE NOT AVAILABLE, USE V1 VALUE
					v2v1<-function(v1,v2)ifelse(is.na(v2),v1,v2)
					v2v1flip<-
						function(x,vname,flip)
						{

							temp1<-ifelse(flip&!is.na(x[,paste(vname,"_2",sep="")]),x[,paste(vname,"_2",sep="")],x[,paste(vname,"_1",sep="")])
							temp2<-ifelse(flip&is.na(x[,paste(vname,"_2",sep="")]),x[,paste(vname,"_1",sep="")],x[,paste(vname,"_2",sep="")])

							if(is.factor(x[,paste(vname,"_2",sep="")]))
							{
								temp1<-factor(temp1,1:length(levels(x[,paste(vname,"_2",sep="")])),levels(x[,paste(vname,"_2",sep="")]))
								temp2<-factor(temp2,1:length(levels(x[,paste(vname,"_2",sep="")])),levels(x[,paste(vname,"_2",sep="")]))
							}

							x[,paste(vname,"_1",sep="")]<-temp1
							x[,paste(vname,"_2",sep="")]<-temp2

							x
						}
					new.cisco<-v2v1flip(new.cisco,"crp_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"crp_lab_cat",flip)
					new.cisco<-v2v1flip(new.cisco,"hstni_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"ntprobnp_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"ferritin_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"chol_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"hdl_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"trig_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"icam1_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"vcam1_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"endothelin1_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"il6_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"st2_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"pselectin_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"ldh_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"haptoglobin_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"total_bili_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"direct_bili_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"indirect_bili_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"creat_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"egfr_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"rendys_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"pt_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"ptratio_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"aptt_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"apttratio_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"tct_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"tctratio_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"ddimer_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"fib_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"factorviii_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"antithr_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"prots_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"protc_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"vwf_gp1br_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"vwf_ag_lab",flip)
					new.cisco<-v2v1flip(new.cisco,"acr",flip)
					new.cisco<-v2v1flip(new.cisco,"eq5d_hu",flip)
					new.cisco<-v2v1flip(new.cisco,"eq5d_vas",flip)
					new.cisco<-v2v1flip(new.cisco,"bip",flip)
					new.cisco<-v2v1flip(new.cisco,"phq4_anx",flip)
					new.cisco<-v2v1flip(new.cisco,"phq4_dep",flip)
					new.cisco<-v2v1flip(new.cisco,"phq4",flip)
					new.cisco<-v2v1flip(new.cisco,"ipaq_cat",flip)
					new.cisco<-v2v1flip(new.cisco,"ipaq_met",flip)
					new.cisco<-v2v1flip(new.cisco,"dasi_score",flip)
					new.cisco<-v2v1flip(new.cisco,"dasi_vo2max",flip)
			}

		# IF NOT CROSSING OVER, REMOVE FROM PAPER
			if(!noncovid.crossover)
				new.cisco$paper<-"No"


	# REMOVE THOSE WHO ARE COVID, BUT WITHOUT PCR, FROM THE STUDY POPULATION

		if(noncovid.remove)
		{
			remove.nopcr<-(new.cisco$pcr=="No")&(new.cisco$group=="COVID-19")
			new.cisco<-new.cisco[!remove.nopcr,]
		}

	# SUBSTITUTE DATA

		cisco<-new.cisco


	# FLAG CEC POPULATIONS
		cec$paper<-cisco$paper[match(cec$id,cisco$id)]
		spec.cec<-add.spec(spec.cec,"paper","Included in paper?","Text","Copy from cisco dataset<br>No, Yes")

		cec$group<-cisco$group[match(cec$id,cisco$id)]
		spec.cec<-add.spec(spec.cec,"group","COVID or control","Text","Control, COVID-19")

	# DROP NON-COVID PATIENTS FROM CEC DATASET
		cec<-cec[cec$group=="COVID-19",]

	# find1

	# EXPORT DATA
		write.csv(cisco,paste(export.path,"/","CISCO19_AnalysisData_cisco.csv",sep=""),row.names=F)
		write.csv(cisco,paste(export.path,"/","CISCO19_AnalysisData_cec.csv",sep=""),row.names=F)
		saveRDS(cisco,paste(export.path,"/","CISCO19_AnalysisData_cisco.RDS",sep=""))
		saveRDS(cisco,paste(export.path,"/","CISCO19_AnalysisData_cec.RDS",sep=""))

# CHECK IN HOSPITAL EVENTS TABLE (GENERATED BY RFs)

	# IN HOPSITAL EVENTS
		temp<-inhospevents.data
		temp$id<-substring(100000+as.numeric(inhospevents.data$study_id),2,6)
		temp$paper<-cisco$paper[match(temp$id,cisco$id)]
		temp$group<-cisco$group[match(temp$id,cisco$id)]

		do.call(
			"rbind",
			lapply(
				temp[,c("ip_death","mi","cva","hf","pte","dvt","af","vt/vf","noncv_sae")],
				function(x,group,paper)
				{
					tab<-table(factor(as.numeric(x),0:1),group,paper)[,2,]
					c(
						paste(tab[2,]," (",my.format(apply(tab,2,function(y)100*y[2]/sum(y)),1),"%)",sep=""),
						p.format(fisher.test(tab)$p.value))
				},
				group=temp$group,paper=temp$paper))

###################################################
#                                                 #
# SMALL ANALYSES IN RESPONSE TO REVIEWER COMMENTS #
#                                                 #
###################################################

	# eGFR vs TROPONIN

		# ADMISSION
			temp<-table(factor(cisco$egfr_init<60,c(F,T),c(">=60","<60")),factor(c(1,1,1,2)[as.numeric(trop.cat(cisco$tni_init,cisco$sex,"All"))],1:2,c("<=99th percetile",">99th percentile")),cisco$group)[,,2]
			temp
			apply(temp,1,function(x) paste(x," (",my.format(100*x/sum(x),1),"%)",sep=""))
			fisher.test(temp)

		# V1
			temp<-table(factor(cisco$egfr_lab_1<60,c(F,T),c(">=60","<60")),factor(c(1,1,1,2)[as.numeric(trop.cat(cisco$hstni_lab_1,cisco$sex,"All"))],1:2,c("<=99th percetile",">99th percentile")),cisco$group)[,,2]
			temp
			apply(temp,1,function(x) paste(x," (",my.format(100*x/sum(x),1),"%)",sep=""))
			fisher.test(temp)

		# V2
			temp<-table(factor(cisco$egfr_lab_2<60,c(F,T),c(">=60","<60")),factor(c(1,1,1,2)[as.numeric(trop.cat(cisco$hstni_lab_2,cisco$sex,"All"))],1:2,c("<=99th percetile",">99th percentile")),cisco$group)[,,2]
			temp
			apply(temp,1,function(x) paste(x," (",my.format(100*x/sum(x),1),"%)",sep=""))
			fisher.test(temp)

			boxplot(
				c(
					split(cisco$tni_init[cisco$group=="COVID-19"],cisco$egfr_init[cisco$group=="COVID-19"]<60),
					split(cisco$hstni_lab_1[cisco$group=="COVID-19"],cisco$egfr_lab_1[cisco$group=="COVID-19"]<60),
					split(cisco$hstni_lab_2[cisco$group=="COVID-19"],cisco$egfr_lab_2[cisco$group=="COVID-19"]<60)),
				log="y",ylab="Troponin I",at=c(1,2,4,5,7,8),names=c(">=60","<60",">=60","<60",">=60","<60"))
			mtext("eGFR",1,at=0,line=1)
			mtext(c("Admission","Enrollment","Scan"),1,at=c(1.5,4.5,7.5),line=2.5)
			mtext(
				c(
					p.format(wilcox.test(tni_init~factor(egfr_init<60),data=cisco,subset=group=="COVID-19")$p.value),
					p.format(wilcox.test(hstni_lab_1~factor(egfr_lab_1<60),data=cisco,subset=group=="COVID-19")$p.value),
					p.format(wilcox.test(hstni_lab_2~factor(egfr_lab_2<60),data=cisco,subset=group=="COVID-19")$p.value)),
				3,at=c(1.5,4.5,7.5),line=1)

			boxplot(
				c(
					split(cisco$tni_init[cisco$group=="COVID-19"],cisco$egfr_init[cisco$group=="COVID-19"]<60),
					split(cisco$hstni_lab_1[cisco$group=="COVID-19"],cisco$egfr_init[cisco$group=="COVID-19"]<60),
					split(cisco$hstni_lab_2[cisco$group=="COVID-19"],cisco$egfr_init[cisco$group=="COVID-19"]<60)),
				log="y",ylab="Troponin I",at=c(1,2,4,5,7,8),names=c(">=60","<60",">=60","<60",">=60","<60"))
			mtext("eGFR",1,at=0,line=1)
			mtext(c("Admission","Enrollment","Scan"),1,at=c(1.5,4.5,7.5),line=2.5)
			mtext(
				c(
					p.format(wilcox.test(tni_init~factor(egfr_init<60),data=cisco,subset=group=="COVID-19")$p.value),
					p.format(wilcox.test(hstni_lab_1~factor(egfr_init<60),data=cisco,subset=group=="COVID-19")$p.value),
					p.format(wilcox.test(hstni_lab_2~factor(egfr_init<60),data=cisco,subset=group=="COVID-19")$p.value)),
				3,at=c(1.5,4.5,7.5),line=1)

	# "ERRORS" IN P-VALUES

		# REVIEWER'S CALCULATIONS
			tab1 <- matrix(c(4, 23, 56, 105), ncol=2)
			chisq.test(tab1)
			fisher.test(tab1)

		# ACTUAL CALCULATIONS
			tab1 <- matrix(c(4, 20, 56, 104), ncol=2)
			chisq.test(tab1)
			fisher.test(tab1)

			tab1 <- matrix(c(1, 22, 21, 132), ncol=2)
			chisq.test(tab1)
			fisher.test(tab1)

	# HBA1C VS PRIMARY

		boxplot(split(cisco$hba1c_hi,cisco$primary),range=0,col=NULL,ylab="HbA1c (mmol/mol)")
		points(
			jitter(rep(1,sum(cisco$primary==levels(cisco$primary)[1],na.rm=T)),amount=0.3),
			cisco$hba1c[!is.na(cisco$primary)&(cisco$primary==levels(cisco$primary)[1])],col="red",pch=16)
		points(
			jitter(rep(2,sum(cisco$primary==levels(cisco$primary)[2],na.rm=T)),amount=0.3),
			cisco$hba1c[!is.na(cisco$primary)&(cisco$primary==levels(cisco$primary)[2])],col="purple",pch=16)
		points(
			jitter(rep(3,sum(cisco$primary==levels(cisco$primary)[3],na.rm=T)),amount=0.3),
			cisco$hba1c[!is.na(cisco$primary)&(cisco$primary==levels(cisco$primary)[3])],col="blue",pch=16)
		points(
			jitter(rep(4,sum(cisco$primary==levels(cisco$primary)[4],na.rm=T)),amount=0.3),
			cisco$hba1c[!is.na(cisco$primary)&(cisco$primary==levels(cisco$primary)[4])],col="green",pch=16)

	# DIABETES DRUGS
		head(ddrugs.data)
		dim(ddrugs.data)
		sort(names(cisco))
		temp<-
			merge(
				ddrugs.data,
				cisco[,c("id","paper","group","diabetes_c","diabetes_h","diabetes_j","diabetes","hba1c_hi")],
				by.x="sno",by.y="id")
		head(temp)
		dim(temp)

		temp<-temp[temp$group=="COVID-19",]

		table(temp$diabetes_c,temp$"diabetes mellitus - charlson score",exclude=NULL)
		table(temp$diabetes_h,temp$"diabetes - heart age risk score",exclude=NULL)
		table(temp$diabetes_j,temp$"diabetes - jbs3 risk score",exclude=NULL)
		table(as.numeric(temp$hba1c)-temp$hba1c_hi,exclude=NULL)
		table(is.na(temp$hba1c),is.na(temp$hba1c_hi))

		table(temp$diabetes,temp$"therapy 1",exclude=NULL)

		temp$nmeds<-
			apply(
				temp[,
					c(
						"sodium/glucose cotransporter 2 inhibitor","biguanide","sulfonylurea","thiazolidinedione",
						"dipeptidyl peptidase 4 inhibitor","glucagon-like-peptide 1 antagonist","insulin")],
				1,function(x) sum(!is.na(x)))

		table(temp$diabetes,temp$"sodium/glucose cotransporter 2 inhibitor",exclude=NULL)
		table(temp$diabetes,temp$"biguanide",exclude=NULL)
		table(temp$diabetes,temp$"sulfonylurea",exclude=NULL)
		table(temp$diabetes,temp$"thiazolidinedione",exclude=NULL)
		table(temp$diabetes,temp$"dipeptidyl peptidase 4 inhibitor",exclude=NULL)
		table(temp$diabetes,temp$"glucagon-like-peptide 1 antagonist",exclude=NULL)
		table(temp$diabetes,temp$"insulin",exclude=NULL)

		table(temp$diabetes,temp$"diet",exclude=NULL)

		table(temp$diabetes,temp$nmeds,exclude=NULL)
		table(temp$diabetes,temp$nmeds,temp$diet,exclude=NULL)

		by(temp$hba1c_hi,temp[,c("nmeds","diabetes")],mean,na.rm=T)

		boxplot(split(temp$hba1c_hi[temp$diabetes=="Yes"],temp$nmeds[temp$diabetes=="Yes"]),xlab="Number of Diabetes Medications",ylab="HbA1c (mmol/mol)")
		boxplot(split(temp$hba1c_hi[(temp$diabetes=="Yes")&(temp$paper=="Yes")],temp$nmeds[(temp$diabetes=="Yes")&(temp$paper=="Yes")]),xlab="Number of Diabetes Medications",ylab="HbA1c (mmol/mol)")

		dim(temp)
		table(temp$paper)
		table(temp$diabetes,is.na(temp$hba1c_hi),temp$paper,exclude=NULL)
		table(temp$diabetes,is.na(temp$hba1c_hi),exclude=NULL)

	# AKI vs HbA1c

		table(cisco$aki,cisco$diabetes,cisco$paper,exclude=NULL)
		table(cisco$aki,cisco$diabetes,cisco$paper)[,,2]
		fisher.test(table(cisco$aki,cisco$diabetes,cisco$paper)[,,2])
		by(cisco$hba1c_hi[cisco$paper=="Yes"],cisco[cisco$paper=="Yes",c("aki","diabetes")],mean,na.rm=T)
		barplot(by(cisco$hba1c_hi[cisco$paper=="Yes"],cisco[cisco$paper=="Yes",c("aki","diabetes")],mean,na.rm=T),beside=T)
		tbox<-boxplot(split(cisco$hba1c_hi[cisco$paper=="Yes"],cisco[cisco$paper=="Yes",c("aki","diabetes")]),axes=F,ylab="HbA1c (mmol/mol)")
		axis(2)
		mtext(c("AKI","No AKI","AKI","No AKI"),1,at=1:4)
		axis(1,1:2,line=1.5,labels=F)
		axis(1,3:4,line=1.5,labels=F)
		mtext(c("Diabetes","No Diabetes"),1,line=2,at=c(1.5,3.5))
		mtext(paste("N=",tbox$n,sep=""),3,at=1:4)

	# RELIABILITY - ICC

		var(as.numeric(cisco$primary[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]))
		var(as.numeric(cec$primary[(cec$paper=="Yes")]))
		var(as.numeric(cisco$primary[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]))/var(as.numeric(cec$primary[(cec$paper=="Yes")]))

		var(unlist(by(as.numeric(cec$primary[(cec$paper=="Yes")]),cec$id[(cec$paper=="Yes")],mean)))
		var(unlist(by(as.numeric(cec$primary[(cec$paper=="Yes")]),cec$id[(cec$paper=="Yes")],mean)))/var(as.numeric(cec$primary[(cec$paper=="Yes")]))

		temp<-as.numeric(cec$primary[(cec$paper=="Yes")])
		temp.mean<-unlist(by(temp,cec$id[(cec$paper=="Yes")],mean))
		temp2.mean<-temp.mean[match(cec$id[(cec$paper=="Yes")],unique(cec$id[(cec$paper=="Yes")]))]
		temp.median<-unlist(by(temp,cec$id[(cec$paper=="Yes")],median))
		temp2.median<-temp.median[match(cec$id[(cec$paper=="Yes")],unique(cec$id[(cec$paper=="Yes")]))]

		var(temp)
		var(temp.mean)
		var(temp2.mean)
		var(temp-temp2.mean)
		var(temp2.mean)/var(temp)

		var(temp)
		var(temp.median)
		var(temp2.median)
		var(temp-temp2.median)
		var(temp2.median)/var(temp)

		icc.dist<-
			unlist(
				lapply(
					as.list(1:10000),
					function(i,primary,id,primvar)
					{
						id<-sample(id)
						var(unlist(by(primary,id,median)))/primvar
					},
					primary=as.numeric(cec$primary[cec$paper=="Yes"]),
					id=cec$id[cec$paper=="Yes"],
					primvar=var(as.numeric(cec$primary[cec$paper=="Yes"]))))
		hist(icc.dist)



	# DURATION OF FOLLOW-UP - SYMPTOM ONSET TO V2

		names(cisco)[grep("date",names(cisco))]
		summary.fun(as.numeric(cisco$v2_date-cisco$onset_date)[(cisco$group=="COVID-19")&(cisco$paper=="Yes")],"@MEAN (@SD), @MEDIAN (@LQ, @UQ), [@MIN, @MAX]",0)
		cisco[!is.na((cisco$v2_date-cisco$onset_date))&(as.numeric(cisco$v2_date-cisco$onset_date)<28),]
		cisco$paper[!is.na((cisco$v2_date-cisco$onset_date))&(as.numeric(cisco$v2_date-cisco$onset_date)<28)]
		cisco[!is.na((cisco$v2_date-cisco$onset_date))&(as.numeric(cisco$v2_date-cisco$disch_date)<28),]
		cisco$paper[!is.na((cisco$v2_date-cisco$onset_date))&(as.numeric(cisco$v2_date-cisco$disch_date)<28)]

	# DURATION OF FOLLOW-UP - DISCHARGE/VISIT 1 TO V3

		temp<-(pmin(cisco$death_date,cisco$v3_date,na.rm=T)-cisco$disch_date)[cisco$group=="COVID-19"]
		quantile(temp,1:3/4)

	# DIFFERENT WAYS OF DEFINING MYOCARDITIS

		temp<-cor.test(
			-as.numeric(cisco$lake_louise[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			as.numeric(cisco$primary[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			method="kendall")
		names(temp)
		cor.test(
			-as.numeric(cisco$lake_louise[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			as.numeric(cisco$primary_bin[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			method="kendall")
		temp<-cor.test(
			-as.numeric(cisco$lake_louise[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			as.numeric(cisco$primary[(cisco$group=="COVID-19")&(cisco$paper=="Yes")]),
			method="spearman")
		names(temp)

	# RADAR CHART
		temp<-rbind(
			100,0,
			do.call(
				"rbind",
				by(
					cbind(
						cisco$crit_ch_pain=="Yes",
						cisco$crit_new_sob=="Yes",
						cisco$crit_chron_sob=="Yes",
						cisco$crit_cardiogenic_shock=="Yes",
						cisco$crit_palp_sync_arrh=="Yes",
						cisco$crit_ecg_myo=="Yes",
						cisco$crit_t2=="Yes",
						cisco$crit_t1=="Yes",
						(as.numeric(cisco$crit_llc)-1)/2,
						cisco$crit_trop=="Yes",
						cisco$crit_glob_lv_dys=="Yes",
						cisco$crit_reg_lv_dys=="Yes",
						cisco$crit_peri_ch=="Yes",
						cisco$crit_new_lv_dys=="Yes"),
					cisco$primary,
					apply,2,function(x)100*mean(x))))
		temp<-as.data.frame(temp)
		names(temp)<-c(paste("C",1:5,sep=""),paste("D",1:9,sep=""))

		pal.use<-"set 1"
		radarchart(
			temp,
			cglty=1,					# Grid line type
			cglcol="gray",				# Grid line color
			pcol=palette.colors(4,pal.use),		# Color for each line
			plwd=2,					# Width for each line
			plty=1,					# Line type for each line
			pfcol=palette.colors(4,pal.use,0.25),
			xlim=c(-2.5,3),ylim=c(-1.5,1.5))
		legend(
			"left",
			levels(cisco$primary),
			lty=1,lwd=2,pch=1,col=palette.colors(4,pal.use),bty="n",
			title="Adjudicated Diagnosis\nof Myocarditis\n")
		legend(
			"right",
			paste(
				c("","",paste("C",1:5,sep=""),"","","",paste("D",1:9,sep="")),
				c(
					"Clinical Criteria","",
					"Chest Pain",
					"New SOB",
					"Chronic SOB",
					"Cardiogenic Shock",
					"Palpitations/Syncope/Arrhythmia",
					"","Diagnostic Criteria","",
					"ECG Myocarditis",
					"T2 Criteria",
					"T1 Criteria",
					"Lake Louise Criteria",
					"Elevated Troponin",
					"Global LV Dysfunction",
					"Regional LV Dysfunction",
					"Pericardial Changes",
					"New LV Dysfunction")),
			bty="n",
			title="Individual Criteria\n")
		


###########
#         #
# OUTPUTS #
#         #
###########

	# START OUTPUT FILES
	# DEFINE FILENAMES AND WRITE FRONT PAGE
	# IF LIVE TREATMENT DATA IS AVAILABLE, PUT TIME STAMP ON OUTPUT FILE NAME
	# IF ONLY BLINDED TREATMENT DATA AVAILABLE, LAPEL OUTPUT FILE NAME AS DUMMY
		datetime<-gsub(" ","_",date())
		datetime<-gsub(":","_",datetime)

		tables.file<-
			paste(
				table.path,
				paste(
					project.name,"_",
					analysis.name,
					"_Tables_",Version,"_",
					gsub("-","",Sys.Date()),"_",
					substring(datetime,12,13),
					substring(datetime,15,16),
					".doc",sep=""),sep="/")
		tables.file
		tables.file.mega<-gsub("Tables_v","Mega_Tables_v",tables.file)
		tables.file.alt<-gsub("Tables_v","Alternative_Tables_v",tables.file)

		expanded.file<-
			paste(
				table.path,
				paste(
					project.name,"_",
					analysis.name,
					"_ExpandedTables_",Version,"_",
					gsub("-","",Sys.Date()),"_",
					substring(datetime,12,13),
					substring(datetime,15,16),
					".doc",sep=""),sep="/")
		tables.file

		spec.file<-
			paste(
				table.path,
				paste(
					project.name,"_",
					analysis.name,
					"_DataSpec_",Version,"_",
					gsub("-","",Sys.Date()),"_",
					substring(datetime,12,13),
					substring(datetime,15,16),
					".doc",sep=""),sep="/")
		spec.file

		ExportTable.HTML(
			as.Table(list(Text=project.name,Style=list("font-size"=16))),
			rbindTable(
				"",
				as.Table(list(Text=project.title,Style=list("font-size"=14))),
				"",
				paste("Statistical Tables for Paper 1"),
				"",
				paste("Based on a snapshot of the study database, created on",snapshot.date),
				"",
				paste("Program run by",stats.name,"on",substring(date(),1,10)),
				"",
				"Robertson Centre for Biostatistics"),
			none.fmt,tables.file,T,F,T)

		ExportTable.HTML(
			as.Table(list(Text=project.name,Style=list("font-size"=16))),
			rbindTable(
				"",
				as.Table(list(Text=project.title,Style=list("font-size"=14))),
				"",
				paste("Mega Tables for Paper 1 - Including Whole Population"),
				"",
				paste("Based on a snapshot of the study database, created on",snapshot.date),
				"",
				paste("Program run by",stats.name,"on",substring(date(),1,10)),
				"",
				"Robertson Centre for Biostatistics"),
			none.fmt,tables.file.mega,T,F,T)

		ExportTable.HTML(
			as.Table(list(Text=project.name,Style=list("font-size"=16))),
			rbindTable(
				"",
				as.Table(list(Text=project.title,Style=list("font-size"=14))),
				"",
				paste("Additional Tables for Paper 1 - Alternative Splits"),
				"",
				paste("Based on a snapshot of the study database, created on",snapshot.date),
				"",
				paste("Program run by",stats.name,"on",substring(date(),1,10)),
				"",
				"Robertson Centre for Biostatistics"),
			none.fmt,tables.file.alt,T,F,T)

		ExportTable.HTML(
			as.Table(list(Text=project.name,Style=list("font-size"=16))),
			rbindTable(
				"",
				as.Table(list(Text=project.title,Style=list("font-size"=14))),
				"",
				paste("Expanded Statistical Tables for Paper 1"),
				"",
				paste("Based on a snapshot of the study database, created on",snapshot.date),
				"",
				paste("Program run by",stats.name,"on",substring(date(),1,10)),
				"",
				"Robertson Centre for Biostatistics"),
			none.fmt,expanded.file,T,F,T)

	# OUTPUT DATA SPECS
		temp<-
			list(
				head=
					rbindTable(
						row.title("Data Spec for CISCO-19<br>'cisco' dataset",Style=list("border-top"="solid windowtext 1pt")),
						cbindTable("Variable<br>Name","Description","Format","Notes")),
				body=
					do.call(
						"rbindTable",
						lapply(spec,cbindVector)))
		ExportTable.HTML(temp$head,temp$body,horiz.fmt,spec.file,T,F,T)

		temp<-
			list(
				head=
					rbindTable(
						row.title("Data Spec for CISCO-19<br>'cec' dataset",Style=list("border-top"="solid windowtext 1pt")),
						cbindTable("Variable<br>Name","Description","Format","Notes")),
				body=
					do.call(
						"rbindTable",
						lapply(spec.cec,cbindVector)))
		ExportTable.HTML(temp$head,temp$body,horiz.fmt,spec.file,F,F,T)

	# TABLE 1

		temp<-
			list(
				head=
					rbindTable(
						table.title(1,"Clinical characteristics of the study population, by control/COVID-19, and by myocarditis status."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1),
						npc.row("Sex",cisco$sex),
						npc.row("Race (Patient Details)",cisco$race_pd),
						npc.row("Race (JBS3)",cisco$race_j),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T),
						npc.row("Healthcare Worker",cisco$hcw,"Yes"),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1),
						msd.row("Oxygen saturation, %",cisco$oxysat,1),
						msd.row("Respiratory rate, /min",cisco$rr,1),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes"),
						npc.row("Nosocomial",cisco$nosocomial,"Yes"),
						npc.row("Antibody test",cisco$antibody,"Yes"),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F),
						npc.row("ICU",cisco$icu,"Yes",p1=F),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes"),
						npc.row("Renal impairment",cisco$renal,"Yes"),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes"),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes"),
						npc.row("Smoking",cisco$smoke_3),
						npc.row("PCI",cisco$pci,"Yes"),
						npc.row("CABG",cisco$cabg,"Yes"),
						npc.row("CCS Angina Class",cisco$ccs_bin),
						npc.row("HF",cisco$chf,"Yes"),
						npc.row("MI",cisco$mi,"Yes"),
						npc.row("Stroke or TIA",cisco$stroke,"Yes"),
						npc.row("PVD",cisco$pvd,"Yes"),
						npc.row("CVD",cisco$cvd,"Yes"),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes"),
						npc.row("Statin",cisco$statin,"Yes"),
						npc.row("Beta-blocker",cisco$bb,"Yes"),
						npc.row("ACE inhibitor",cisco$ace,"Yes"),
						npc.row("ARB",cisco$arb,"Yes"),
						npc.row("Oral anticoagulation",cisco$oac,"Yes"),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes"),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes"),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1),
						msd.row("Albumin, g/l",cisco$albumin,1),
						msd.row("Initial MCV, fl",cisco$mcv_init,1),
						msd.row("Initial RDW, %",cisco$rdw_init,1),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title(1,"Clinical characteristics of the study population, by control/COVID-19, and by myocarditis status."),
						cbindTable(
							"","",
							rbindTable("COVID-19",cbindTable("Without Primary<br>Outcome","With Primary<br>Outcome")),
							"Control","p-value<sup>(a)</sup>","p-value<sup>(b)</sup>",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="No")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,showAll=T),
						npc.row("Sex",cisco$sex,showAll=T),
						npc.row("Race (Patient Details)",cisco$race_pd,showAll=T),
						npc.row("Race (JBS3)",cisco$race_j,showAll=T),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,showAll=T),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,showAll=T),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",showAll=T),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,showAll=T),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,showAll=T),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,showAll=T),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,showAll=T),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,showAll=T),
						msd.row("Respiratory rate, /min",cisco$rr,1,showAll=T),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,showAll=T),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,showAll=T),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",showAll=T),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",showAll=T),
						npc.row("Antibody test",cisco$antibody,"Yes",showAll=T),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,showAll=T),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,showAll=T),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,showAll=T),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,showAll=T),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,showAll=T),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,showAll=T),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,showAll=T),
						npc.row("ICU",cisco$icu,"Yes",p1=F,showAll=T),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",showAll=T),
						npc.row("Renal impairment",cisco$renal,"Yes",showAll=T),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",showAll=T),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",showAll=T),
						npc.row("Smoking",cisco$smoke_3,showAll=T),
						npc.row("PCI",cisco$pci,"Yes",showAll=T),
						npc.row("CABG",cisco$cabg,"Yes",showAll=T),
						npc.row("CCS Angina Class",cisco$ccs_bin,showAll=T),
						npc.row("HF",cisco$chf,"Yes",showAll=T),
						npc.row("MI",cisco$mi,"Yes",showAll=T),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",showAll=T),
						npc.row("PVD",cisco$pvd,"Yes",showAll=T),
						npc.row("CVD",cisco$cvd,"Yes",showAll=T),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,showAll=T),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,showAll=T),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,showAll=T),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,showAll=T),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",showAll=T),
						npc.row("Statin",cisco$statin,"Yes",showAll=T),
						npc.row("Beta-blocker",cisco$bb,"Yes",showAll=T),
						npc.row("ACE inhibitor",cisco$ace,"Yes",showAll=T),
						npc.row("ARB",cisco$arb,"Yes",showAll=T),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",showAll=T),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,showAll=T),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,showAll=T),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,showAll=T),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,showAll=T),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,showAll=T),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,showAll=T),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,showAll=T),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,showAll=T),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",showAll=T),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,showAll=T),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,showAll=T),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",showAll=T),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,showAll=T),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,showAll=T),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,showAll=T),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,showAll=T),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,showAll=T),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,showAll=T),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,showAll=T),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,showAll=T),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,showAll=T),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,showAll=T),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,showAll=T),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,showAll=T),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,showAll=T),
						msd.row("Albumin, g/l",cisco$albumin,1,showAll=T),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,showAll=T),
						msd.row("Initial RDW, %",cisco$rdw_init,1,showAll=T),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,showAll=T),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,showAll=T),

						end.row(
							footnote=
								paste(
									"<sup>(a)</sup>: COVID-19 patients, without primary outcome vs. with primary outcome.",
									"<sup>(b)</sup>: COVID-19 patients with primary outcome vs. controls."))))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.mega,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title(1,"Clinical characteristics of the study population, by control/COVID-19, and by myocarditis status."),
						cbindTable(
							"","","COVID-19","Control","p",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("Age, years",cisco$age,1),
						expnpc.row("Sex",cisco$sex),
						expnpc.row("Race (Patient Details)",cisco$race_pd),
						expnpc.row("Race (JBS3)",cisco$race_j),
						expnpc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000),
						expnpc.row("SIMD Quintile",cisco$simd,simulate.p.value=T,B=10000),
						expnpc.row("Healthcare Worker",cisco$hcw),
						expcont.row("Height, cm",cisco$height,1),
						expcont.row("Weight, kg",cisco$weight,1),
						expcont.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("Heart Rate, bpm",cisco$hr,1),
						expcont.row("Systolic blood pressure, mmHg",cisco$sbp,1),
						expcont.row("Diastolic blood pressure, mmHg",cisco$dbp,1),
						expcont.row("Oxygen saturation, %",cisco$oxysat,1),
						expcont.row("Respiratory rate, /min",cisco$rr,1),
						expnpc.row("WHO Clinical severity score (full version)",cisco$who_full,simulate.p.value=T,B=10000),
						expnpc.row("WHO Clinical severity score (short version)",cisco$who_short,simulate.p.value=T,B=10000,p1=F),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("PCR test",cisco$pcr),
						expnpc.row("Nosocomial",cisco$nosocomial),
						expnpc.row("Antibody test",cisco$antibody),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("",cisco$chest,p1=F),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Oxygen",cisco$oxygen,p1=F),
						expnpc.row("Non-invasive respiratory support",cisco$ni_resp,p1=F),
						expnpc.row("Invasive ventilation",cisco$inv_vent,p1=F),
						expnpc.row("IV inotrope",cisco$iv_ino,p1=F),
						expnpc.row("Antiviral",cisco$antiviral,p1=F),
						expnpc.row("Steroid",cisco$steroid,p1=F),
						expnpc.row("ICU",cisco$icu,p1=F),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Hypertension",cisco$hyperten),
						expnpc.row("Renal impairment",cisco$renal),
						expnpc.row("Diabetes mellitus",cisco$diabetes),
						expnpc.row("Hypercholesterolaemia",cisco$hyperchol),
						expnpc.row("Smoking (3 categories)",cisco$smoke_3),
						expnpc.row("Smoking (5 categories)",cisco$smoke_5),
						expnpc.row("PCI",cisco$pci),
						expnpc.row("CABG",cisco$cabg),
						expnpc.row("CCS Angina Class",cisco$ccs_bin),
						expnpc.row("HF",cisco$chf),
						expnpc.row("MI",cisco$mi),
						expnpc.row("Stroke or TIA",cisco$stroke),
						expnpc.row("PVD",cisco$pvd),
						expnpc.row("CVD",cisco$cvd),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("ISARIC-4c COVID Risk Score",cisco$isaric,2),
						expcont.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1),
						expcont.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1),
						expcont.row("Charlson Comorbidity Index",cisco$charlson,2),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Aspirin",cisco$aspirin),
						expnpc.row("Statin",cisco$statin),
						expnpc.row("Beta-blocker",cisco$bb),
						expnpc.row("ACE inhibitor",cisco$ace),
						expnpc.row("ARB",cisco$arb),
						expnpc.row("Oral anticoagulation",cisco$oac),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("Haemoglobin, g/l",cisco$hb,0),
						expcont.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0),
						expcont.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2),
						expcont.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2),
						expcont.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0),
						expcont.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1),
						expcont.row("Initial creatinine, mmol/l",cisco$creat_init,1),
						expcont.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1),
						expnpc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init),
						expcont.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0),
						expcont.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1),
						expnpc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo),
						expcont.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0),
						expcont.row("Initial hsTn I, ng/l",cisco$tni_init,0),
						expnpc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						expnpc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						expnpc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000),
						expcont.row("Peak hsTn I, ng/l",cisco$tni_hi,1),
						expnpc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						expnpc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						expnpc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000),
						expcont.row("Peak fibrinogen, g/l",cisco$fib_hi,2),
						expcont.row("Peak CRP, mg/l",cisco$crp_hi,0),
						expnpc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000),
						expcont.row("Peak sodium, mmol/l",cisco$sodium_hi,1),
						expcont.row("Albumin, g/l",cisco$albumin,1),
						expcont.row("Initial MCV, fl",cisco$mcv_init,1),
						expcont.row("Initial RDW, %",cisco$rdw_init,1),
						expcont.row("Peak MCV, fl",cisco$mcv_hi,1),
						expcont.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.1","Clinical characteristics of the study population, by control/COVID-19, and by healthcare worker status."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Healthcare Worker",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$hcw))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$hcw=="Yes")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$hcw=="No")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$hcw),
						npc.row("Sex",cisco$sex,primary=cisco$hcw),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$hcw),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$hcw),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$hcw),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$hcw,p2=F),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$hcw),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$hcw),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$hcw),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$hcw),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$hcw),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$hcw),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$hcw),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$hcw),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$hcw),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$hcw),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$hcw),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$hcw),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$hcw),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$hcw),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$hcw),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$hcw),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$hcw),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$hcw),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$hcw),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$hcw),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$hcw),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$hcw),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$hcw),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$hcw),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$hcw),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$hcw),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$hcw),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$hcw),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$hcw),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$hcw),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$hcw),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$hcw),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$hcw),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$hcw),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$hcw),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$hcw),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$hcw),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$hcw),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$hcw),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$hcw),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$hcw),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$hcw),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$hcw),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$hcw),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$hcw),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$hcw),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$hcw),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$hcw),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$hcw),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$hcw),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$hcw),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$hcw),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$hcw),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$hcw),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$hcw),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$hcw),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$hcw),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$hcw),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$hcw),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$hcw),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$hcw),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$hcw),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$hcw),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$hcw),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$hcw),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$hcw),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.2","Clinical characteristics of the study population, by control/COVID-19, and by age."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Age",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$age_group))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="&lt;45")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="45-54")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="55-64")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="&ge;65")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$age_group,p2=F),
						npc.row("Sex",cisco$sex,primary=cisco$age_group),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$age_group),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$age_group),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$age_group),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$age_group),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$age_group),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$age_group),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$age_group),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$age_group),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$age_group),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$age_group),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$age_group),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$age_group),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$age_group),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$age_group),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$age_group),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$age_group),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$age_group),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$age_group),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$age_group),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$age_group),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$age_group),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$age_group),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$age_group),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$age_group),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$age_group),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$age_group),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$age_group),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$age_group),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$age_group),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$age_group),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$age_group),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$age_group),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$age_group),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$age_group),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$age_group),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$age_group),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$age_group),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$age_group),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$age_group),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$age_group),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$age_group),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$age_group),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$age_group),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$age_group),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$age_group),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$age_group),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$age_group),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$age_group),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$age_group),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$age_group),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$age_group),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$age_group),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$age_group),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$age_group),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$age_group),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$age_group),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$age_group),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$age_group),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$age_group),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$age_group),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$age_group),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$age_group),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$age_group),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$age_group),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$age_group),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$age_group),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$age_group),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$age_group),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$age_group),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$age_group),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.3","Clinical characteristics of the study population, by control/COVID-19, and by sex."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Sex",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$sex))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$sex=="Male")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$sex=="Female")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$sex),
						npc.row("Sex",cisco$sex,primary=cisco$sex,p2=F),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$sex),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$sex),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$sex),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$sex),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$sex),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$sex),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$sex),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$sex),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$sex),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$sex),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$sex),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$sex),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$sex),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$sex),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$sex),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$sex),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$sex),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$sex),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$sex),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$sex),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$sex),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$sex),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$sex),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$sex),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$sex),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$sex),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$sex),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$sex),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$sex),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$sex),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$sex),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$sex),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$sex),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$sex),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$sex),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$sex),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$sex),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$sex),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$sex),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$sex),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$sex),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$sex),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$sex),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$sex),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$sex),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$sex),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$sex),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$sex),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$sex),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$sex),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$sex),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$sex),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$sex),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$sex),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$sex),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$sex),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$sex),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$sex),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$sex),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$sex),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$sex),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$sex),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$sex),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$sex),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$sex),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$sex),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$sex),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$sex),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$sex),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$sex),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$sex),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$sex),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.4","Clinical characteristics of the study population, by control/COVID-19, and by SIMD."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="SIMD",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$simd))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q1 - Most Deprived")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q2")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q3")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q4")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q5 - Least Deprived")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$simd),
						npc.row("Sex",cisco$sex,primary=cisco$simd),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$simd),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$simd),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$simd,p2=F),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$simd),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$simd),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$simd),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$simd),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$simd),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$simd),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$simd),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$simd),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$simd),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$simd),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$simd),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$simd,simulate.p.value=T,B=10000),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$simd),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$simd),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$simd),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$simd),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$simd),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$simd),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$simd),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$simd),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$simd),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$simd),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$simd),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$simd),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$simd),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$simd),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$simd),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$simd),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$simd),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$simd),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$simd),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$simd),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$simd),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$simd),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$simd),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$simd),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$simd),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$simd),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$simd),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$simd),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$simd),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$simd),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$simd),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$simd),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$simd),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$simd),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$simd),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$simd),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$simd),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$simd),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$simd),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$simd),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$simd),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$simd),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$simd),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$simd),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$simd),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$simd),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$simd),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$simd),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$simd),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$simd),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$simd),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$simd),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$simd),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$simd),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$simd),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.5","Clinical characteristics of the study population, by control/COVID-19, and by Lake Louise Criteria."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Lake Louise Criteria",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$lake_louise))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="Definite")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="Probable")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="None")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$lake_louise),
						npc.row("Sex",cisco$sex,primary=cisco$lake_louise),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$lake_louise),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$lake_louise),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$lake_louise),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$lake_louise),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$lake_louise),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$lake_louise),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$lake_louise),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$lake_louise),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$lake_louise),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$lake_louise),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$lake_louise),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$lake_louise),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$lake_louise),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$lake_louise),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$lake_louise),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$lake_louise),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$lake_louise),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$lake_louise),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$lake_louise),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$lake_louise),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$lake_louise),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$lake_louise),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$lake_louise),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$lake_louise),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$lake_louise),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$lake_louise),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$lake_louise),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$lake_louise),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$lake_louise),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$lake_louise),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$lake_louise),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$lake_louise),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$lake_louise),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$lake_louise),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$lake_louise),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$lake_louise),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$lake_louise),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$lake_louise),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$lake_louise),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$lake_louise),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$lake_louise),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$lake_louise),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$lake_louise),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$lake_louise),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$lake_louise),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$lake_louise),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$lake_louise),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$lake_louise),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$lake_louise),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$lake_louise),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$lake_louise),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$lake_louise),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$lake_louise),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$lake_louise),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$lake_louise),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$lake_louise),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$lake_louise),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$lake_louise),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$lake_louise),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$lake_louise),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$lake_louise),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$lake_louise),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$lake_louise),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("1.6","Clinical characteristics of the study population, by control/COVID-19, and by binary classification of primary outcome."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Primary outcome: at least probable myocarditis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary_bin))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary_bin=="No")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary_bin=="Yes")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Baseline Demographics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Age, years",cisco$age,1,primary=cisco$primary_bin),
						npc.row("Sex",cisco$sex,primary=cisco$primary_bin),
						npc.row("Race (Patient Details)",cisco$race_pd,primary=cisco$primary_bin),
						npc.row("Race (JBS3)",cisco$race_j,primary=cisco$primary_bin),
						npc.row("Race (Collapsed)",cisco$race_3,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Most deprived SIMD quintile",factor(cisco$simd=="Q1 - Most Deprived"),T,primary=cisco$primary_bin),
						npc.row("Healthcare Worker",cisco$hcw,"Yes",primary=cisco$primary_bin),
						msd.row("BMI, kg/m<sup>2</sup>",cisco$bmi,1,primary=cisco$primary_bin),

						row.title(italic("Presenting Characteristics"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Heart Rate, bpm",cisco$hr,1,primary=cisco$primary_bin),
						msd.row("Systolic blood pressure, mmHg",cisco$sbp,1,primary=cisco$primary_bin),
						msd.row("Diastolic blood pressure, mmHg",cisco$dbp,1,primary=cisco$primary_bin),
						msd.row("Oxygen saturation, %",cisco$oxysat,1,primary=cisco$primary_bin),
						msd.row("Respiratory rate, /min",cisco$rr,1,primary=cisco$primary_bin),
						npc.row("WHO Clinical severity score (in full)",cisco$who_full,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("WHO Clinical severity score",cisco$who_short,simulate.p.value=T,B=10000,p1=F,primary=cisco$primary_bin),

						row.title(italic("COVID-19 diagnosis"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("PCR test",cisco$pcr,"Yes",primary=cisco$primary_bin),
						npc.row("Nosocomial",cisco$nosocomial,"Yes",primary=cisco$primary_bin),
						npc.row("Antibody test",cisco$antibody,"Yes",primary=cisco$primary_bin),

						row.title(italic("Chest x-ray / CT scan"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("",cisco$chest,p1=F,primary=cisco$primary_bin),

						row.title(italic("COVID-19 treatment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Oxygen",cisco$oxygen,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("Non-invasive respiratory support",cisco$ni_resp,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("Invasive ventilation",cisco$inv_vent,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("IV inotrope",cisco$iv_ino,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("Antiviral",cisco$antiviral,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("Steroid",cisco$steroid,"Yes",p1=F,primary=cisco$primary_bin),
						npc.row("ICU",cisco$icu,"Yes",p1=F,primary=cisco$primary_bin),

						row.title(italic("Cardiovascular History"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Hypertension",cisco$hyperten,"Yes",primary=cisco$primary_bin),
						npc.row("Renal impairment",cisco$renal,"Yes",primary=cisco$primary_bin),
						npc.row("Diabetes mellitus",cisco$diabetes,"Yes",primary=cisco$primary_bin),
						npc.row("Hypercholesterolaemia",cisco$hyperchol,"Yes",primary=cisco$primary_bin),
						npc.row("Smoking",cisco$smoke_3,primary=cisco$primary_bin),
						npc.row("PCI",cisco$pci,"Yes",primary=cisco$primary_bin),
						npc.row("CABG",cisco$cabg,"Yes",primary=cisco$primary_bin),
						npc.row("CCS Angina Class",cisco$ccs_bin,primary=cisco$primary_bin),
						npc.row("HF",cisco$chf,"Yes",primary=cisco$primary_bin),
						npc.row("MI",cisco$mi,"Yes",primary=cisco$primary_bin),
						npc.row("Stroke or TIA",cisco$stroke,"Yes",primary=cisco$primary_bin),
						npc.row("PVD",cisco$pvd,"Yes",primary=cisco$primary_bin),
						npc.row("CVD",cisco$cvd,"Yes",primary=cisco$primary_bin),

						row.title(italic("Risk Scores"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("ISARIC-4c Mortality Score",cisco$isaric,2,primary=cisco$primary_bin),
						msd.row("ISARIC-4c Mortality Risk, in %",cisco$isaric_risk,1,primary=cisco$primary_bin),
						msd.row("QRisk3 10y CVD Risk, in %",cisco$qrisk3,1,primary=cisco$primary_bin),
						msd.row("Charlson Comorbidity Index",cisco$charlson,2,primary=cisco$primary_bin),

						row.title(italic("Pre-existing maintenance medication"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Aspirin",cisco$aspirin,"Yes",primary=cisco$primary_bin),
						npc.row("Statin",cisco$statin,"Yes",primary=cisco$primary_bin),
						npc.row("Beta-blocker",cisco$bb,"Yes",primary=cisco$primary_bin),
						npc.row("ACE inhibitor",cisco$ace,"Yes",primary=cisco$primary_bin),
						npc.row("ARB",cisco$arb,"Yes",primary=cisco$primary_bin),
						npc.row("Oral anticoagulation",cisco$oac,"Yes",primary=cisco$primary_bin),

						row.title(italic("Blood results, index admission"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("Haemoglobin, g/l",cisco$hb,0,primary=cisco$primary_bin),
						msd.row("Platelet count, 10<sup>9</sup>/l",cisco$platelet,0,primary=cisco$primary_bin),
						msd.row("White cell count, 10<sup>9</sup>/l",cisco$wcc,2,primary=cisco$primary_bin),
						msd.row("Lymphocyte count, 10<sup>9</sup>/l",cisco$lymph,2,primary=cisco$primary_bin),
						msd.row("Peak D-Dimer, ng/ml",cisco$ddimer_hi,0,primary=cisco$primary_bin),
						msd.row("Peak HbA1c, mmol/mol",cisco$hba1c_hi,1,primary=cisco$primary_bin),
						msd.row("Initial creatinine, mmol/l",cisco$creat_init,1,primary=cisco$primary_bin),
						msd.row("Initial eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_init,1,primary=cisco$primary_bin),
						npc.row("Initial eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_init,"Yes",primary=cisco$primary_bin),
						msd.row("Peak creatinine, &mu;mol/l",cisco$creat_hi,0,primary=cisco$primary_bin),
						msd.row("Minimum eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lo,1,primary=cisco$primary_bin),
						npc.row("Minimum eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lo,"Yes",primary=cisco$primary_bin),
						miqr.row("Peak ferritin, &mu;g/l",cisco$ferritin_hi,0,primary=cisco$primary_bin),
						miqr.row("Initial hsTn I, ng/l",cisco$tni_init,0,primary=cisco$primary_bin),
						npc.row("Initial hsTn I, males",trop.cat(cisco$tni_init,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Initial hsTn I, females",trop.cat(cisco$tni_init,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Initial hsTn I",trop.cat(cisco$tni_init,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("Peak hsTn I, ng/l",cisco$tni_hi,1,primary=cisco$primary_bin),
						npc.row("Peak hsTn I, males",trop.cat(cisco$tni_hi,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Peak hsTn I, females",trop.cat(cisco$tni_hi,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Peak hsTn I",trop.cat(cisco$tni_hi,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						msd.row("Peak fibrinogen, g/l",cisco$fib_hi,2,primary=cisco$primary_bin),
						miqr.row("Peak CRP, mg/l",cisco$crp_hi,0,primary=cisco$primary_bin),
						npc.row("Peak CRP, mg/l",cisco$crp_hi_cat,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						msd.row("Peak sodium, mmol/l",cisco$sodium_hi,1,primary=cisco$primary_bin),
						msd.row("Albumin, g/l",cisco$albumin,1,primary=cisco$primary_bin),
						msd.row("Initial MCV, fl",cisco$mcv_init,1,primary=cisco$primary_bin),
						msd.row("Initial RDW, %",cisco$rdw_init,1,primary=cisco$primary_bin),
						msd.row("Peak MCV, fl",cisco$mcv_hi,1,primary=cisco$primary_bin),
						msd.row("RDW at peak MCV, %",cisco$rdw_mcv_hi,1,primary=cisco$primary_bin),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

	# TABLE 2

		temp<-
			list(
				head=
					rbindTable(
						table.title(2,"Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data]."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes"),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes"),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes"),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes"),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes"),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes"),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes"),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes"),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes"),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes"),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes"),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes"),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes"),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes"),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes"),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes"),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0),
						msd.row("LV mass, g",cisco$lvmass_ctca,0),
						msd.row("Agatston score",cisco$agatston,0),
						msd.row("MESA percentile",cisco$mesa,1),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000),
						npc.row("Obstructive CAD",cisco$ocad,"Yes"),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes"),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes"),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes"),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes"),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1),
						msd.row("LVEF, %",cisco$lvef_cmr,1),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male"),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female"),
						msd.row("LV mass, g",cisco$lvm_cmr,1),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1),
						msd.row("RVEF, %",cisco$rvef_cmr,1),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male"),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female"),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes"),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes"),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes"),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes"),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes"),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes"),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes"),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes"),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes"),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes"),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1),
						msd.row("PT ratio",cisco$ptratio_lab_1,2),
						msd.row("APTT, s",cisco$aptt_lab_1,1),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2),
						msd.row("TCT, s",cisco$tct_lab_1,1),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0),
						msd.row("Protein S",cisco$prots_lab_1,1),
						msd.row("Protein C",cisco$protc_lab_1,1),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes"),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1),
						msd.row("PT ratio",cisco$ptratio_lab_2,2),
						msd.row("APTT, s",cisco$aptt_lab_2,1),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2),
						msd.row("TCT, s",cisco$tct_lab_2,1),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0),
						msd.row("Protein S",cisco$prots_lab_2,1),
						msd.row("Protein C",cisco$protc_lab_2,1),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title(2,"Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data]."),
						cbindTable(
							"","",
							rbindTable("COVID-19",cbindTable("Without Primary<br>Outcome","With Primary<br>Outcome")),
							"Control","p-value<sup>(a)</sup>","p-value<sup>(b)</sup>",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="No")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",showAll=T),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",showAll=T),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",showAll=T),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",showAll=T),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",showAll=T),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",showAll=T),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",showAll=T),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",showAll=T),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",showAll=T),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",showAll=T),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",showAll=T),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",showAll=T),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",showAll=T),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",showAll=T),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",showAll=T),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",showAll=T),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,showAll=T),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,showAll=T),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,showAll=T),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,showAll=T),
						msd.row("Agatston score",cisco$agatston,0,showAll=T),
						msd.row("MESA percentile",cisco$mesa,1,showAll=T),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,showAll=T),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",showAll=T),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,showAll=T),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,showAll=T),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,showAll=T),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",showAll=T),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,showAll=T),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,showAll=T),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,showAll=T),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",showAll=T),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,showAll=T),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,showAll=T),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,showAll=T),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",showAll=T),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,showAll=T),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,showAll=T),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,showAll=T),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",showAll=T),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,showAll=T),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,showAll=T),
						msd.row("LVEF, %",cisco$lvef_cmr,1,showAll=T),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",showAll=T),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",showAll=T),
						msd.row("LV mass, g",cisco$lvm_cmr,1,showAll=T),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,showAll=T),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,showAll=T),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,showAll=T),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,showAll=T),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,showAll=T),
						msd.row("RVEF, %",cisco$rvef_cmr,1,showAll=T),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",showAll=T),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",showAll=T),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,showAll=T),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",showAll=T),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",showAll=T),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,showAll=T),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",showAll=T),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",showAll=T),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",showAll=T),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",showAll=T),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",showAll=T),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",showAll=T),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",showAll=T),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,showAll=T),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,showAll=T),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,showAll=T),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,showAll=T),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,showAll=T),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,showAll=T),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,showAll=T),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,showAll=T),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,showAll=T),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,showAll=T),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,showAll=T),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,showAll=T),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,showAll=T),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,showAll=T),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,showAll=T),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,showAll=T),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,showAll=T),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,showAll=T),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,showAll=T),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,showAll=T),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,showAll=T),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,showAll=T),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,showAll=T),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,showAll=T),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",showAll=T),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,showAll=T),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,showAll=T),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,showAll=T),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,showAll=T),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,showAll=T),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,showAll=T),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,showAll=T),
						msd.row("APTT, s",cisco$aptt_lab_1,1,showAll=T),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,showAll=T),
						msd.row("TCT, s",cisco$tct_lab_1,1,showAll=T),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,showAll=T),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,showAll=T),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,showAll=T),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,showAll=T),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,showAll=T),
						msd.row("Protein S",cisco$prots_lab_1,1,showAll=T),
						msd.row("Protein C",cisco$protc_lab_1,1,showAll=T),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,showAll=T),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,showAll=T),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,showAll=T),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,showAll=T),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,showAll=T),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,showAll=T),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,showAll=T),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,showAll=T),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,showAll=T),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,showAll=T),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,showAll=T),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,showAll=T),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,showAll=T),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,showAll=T),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,showAll=T),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,showAll=T),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,showAll=T),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,showAll=T),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,showAll=T),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,showAll=T),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,showAll=T),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,showAll=T),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",showAll=T),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,showAll=T),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,showAll=T),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,showAll=T),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,showAll=T),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,showAll=T),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,showAll=T),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,showAll=T),
						msd.row("APTT, s",cisco$aptt_lab_2,1,showAll=T),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,showAll=T),
						msd.row("TCT, s",cisco$tct_lab_2,1,showAll=T),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,showAll=T),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,showAll=T),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,showAll=T),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,showAll=T),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,showAll=T),
						msd.row("Protein S",cisco$prots_lab_2,1,showAll=T),
						msd.row("Protein C",cisco$protc_lab_2,1,showAll=T),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,showAll=T),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,showAll=T),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,showAll=T),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,showAll=T),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.mega,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title(2,"Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data]."),
						cbindTable(
							"","","COVID-19","Control","p",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Myopericarditis criteria",cisco$myoperi0),
						expnpc.row("Premature atrial contraction",cisco$premac0),
						expnpc.row("Premature ventricular contraction",cisco$premvc0),
						expnpc.row("Atrial fibrillation or flutter",cisco$aff0),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Myopericarditis criteria",cisco$myoperi1),
						expnpc.row("Premature atrial contraction",cisco$premac1),
						expnpc.row("Premature ventricular contraction",cisco$premvc1),
						expnpc.row("Atrial fibrillation or flutter",cisco$aff1),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Myopericarditis criteria",cisco$myoperi2),
						expnpc.row("Premature atrial contraction",cisco$premac2),
						expnpc.row("Premature ventricular contraction",cisco$premvc2),
						expnpc.row("Atrial fibrillation or flutter",cisco$aff2),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Atelectasis",cisco$atelectasis),
						expnpc.row("Reticulation and/or architectural distortion",cisco$reticulation),
						expnpc.row("Ground glass opacity",cisco$ggopacity),
						expnpc.row("Pulmonary arterial thrombus",cisco$pathrombus),
						expcont.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1),
						expnpc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("LVEDV, ml",cisco$lvedv_ctca,0),
						expcont.row("LV mass, g",cisco$lvmass_ctca,0),
						expcont.row("Agatston score",cisco$agatston,0),
						expcont.row("MESA percentile",cisco$mesa,1),
						expnpc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000),
						expnpc.row("Obstructive CAD",cisco$ocad),
						expcont.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2),
						expcont.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2),
						expcont.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2),
						expnpc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad),
						expcont.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2),
						expcont.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2),
						expcont.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2),
						expnpc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx),
						expcont.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2),
						expcont.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2),
						expcont.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2),
						expnpc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca),
						expcont.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2),
						expcont.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2),
						expcont.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2),
						expnpc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						expcont.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1),
						expcont.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1),
						expcont.row("LVEF, %",cisco$lvef_cmr,1),
						expnpc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,subset=cisco$sex=="Male"),
						expnpc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,subset=cisco$sex=="Female"),
						expcont.row("LV mass, g",cisco$lvm_cmr,1),
						expcont.row("LV GLS, %",cisco$lvgls_cmr,1),
						expcont.row("LV GCS, %",cisco$lvgcs_cmr,1),
						expcont.row("LV GRS, %",cisco$lvgrs_cmr,1),
						expcont.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1),
						expcont.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1),
						expcont.row("RVEF, %",cisco$rvef_cmr,1),
						expnpc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,subset=cisco$sex=="Male"),
						expnpc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,subset=cisco$sex=="Female"),
						expcont.row("RV GLS, %",cisco$rvgls_cmr,1),

						row.title(italic("Multi-parametric myocardial mapping"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1),
						expnpc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2),
						expcont.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2),
						expnpc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1),

						row.title(italic("Late gadolinium enhancement"),Style=list("border-top"="solid windowtext 1pt")),
						expnpc.row("Any LGE",cisco$any_lge),
						expnpc.row("Ischaemic distribution",cisco$isch_dist),
						expnpc.row("Non-ischaemic distribution",cisco$nonisch_dist),
						expnpc.row("Mixed distribution",cisco$mixed_dist),
						expnpc.row("Pericardial thickening",cisco$pcthick_cmr),
						expnpc.row("Pericardial effusion",cisco$pceff_cmr),
						expcont.row("Right atrial area (cm<sup>2</sup>)",cisco$raa_cmr,2),
						expcont.row("Left atrial area (cm<sup>2</sup>)",cisco$laa_cmr,2),
						expnpc.row("Myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,simulate.p.value=T,B=10000),
						expnpc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						expcont.row("C-reactive protein, mg/l",cisco$crp_lab_1,1),
						expnpc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000),
						expcont.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0),
						expnpc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						expnpc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						expnpc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000),
						expcont.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0),
						expnpc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000),
						expcont.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0),
						expcont.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2),
						expcont.row("Triglycerides, mmol/l",cisco$trig_lab_1,2),
						expcont.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2),
						expcont.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0),
						expcont.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0),
						expcont.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2),
						expcont.row("IL-6, pg/ml",cisco$il6_lab_1,2),
						expcont.row("ST2, ng/ml",cisco$st2_lab_1,1),
						expcont.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0),
						expcont.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1),
						expcont.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1),
						expnpc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1),
						expcont.row("LDH, U/l",cisco$ldh_lab_1,0),
						expcont.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2),
						expcont.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2),
						expcont.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2),
						expcont.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2),

						expcont.row("Prothrombin Time, s",cisco$pt_lab_1,1),
						expcont.row("PT ratio",cisco$ptratio_lab_1,2),
						expcont.row("APTT, s",cisco$aptt_lab_1,1),
						expcont.row("APTT ratio",cisco$apttratio_lab_1,2),
						expcont.row("TCT, s",cisco$tct_lab_1,1),
						expcont.row("TCT ratio",cisco$tctratio_lab_1,2),
						expcont.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0),
						expcont.row("Fibrinogen, g/l",cisco$fib_lab_1,2),
						expcont.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0),
						expcont.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0),
						expcont.row("Protein S",cisco$prots_lab_1,1),
						expcont.row("Protein C",cisco$protc_lab_1,1),
						expcont.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0),
						expcont.row("VWF: Ag",cisco$vwf_ag_lab_1,0),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						expcont.row("C-reactive protein, mg/l",cisco$crp_lab_2,1),
						expnpc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000),
						expcont.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0),
						expnpc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000),
						expnpc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000),
						expnpc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000),
						expcont.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0),
						expnpc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000),
						expcont.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0),
						expcont.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2),
						expcont.row("Triglycerides, mmol/l",cisco$trig_lab_2,2),
						expcont.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2),
						expcont.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0),
						expcont.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0),
						expcont.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2),
						expcont.row("IL-6, pg/ml",cisco$il6_lab_2,2),
						expcont.row("ST2, ng/ml",cisco$st2_lab_2,1),
						expcont.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0),
						expcont.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1),
						expcont.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1),
						expnpc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2),
						expcont.row("LDH, U/l",cisco$ldh_lab_2,0),
						expcont.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2),
						expcont.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2),
						expcont.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2),
						expcont.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2),

						expcont.row("Prothrombin Time, s",cisco$pt_lab_2,1),
						expcont.row("PT ratio",cisco$ptratio_lab_2,2),
						expcont.row("APTT, s",cisco$aptt_lab_2,1),
						expcont.row("APTT ratio",cisco$apttratio_lab_2,2),
						expcont.row("TCT, s",cisco$tct_lab_2,1),
						expcont.row("TCT ratio",cisco$tctratio_lab_2,2),
						expcont.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0),
						expcont.row("Fibrinogen, g/l",cisco$fib_lab_2,2),
						expcont.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0),
						expcont.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0),
						expcont.row("Protein S",cisco$prots_lab_2,1),
						expcont.row("Protein C",cisco$protc_lab_2,1),
						expcont.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0),
						expcont.row("VWF: Ag",cisco$vwf_ag_lab_2,0),

						row.title(italic("Urine biomarkers")),
						expcont.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2),
						expcont.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.1","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by healthcare worker status."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Healthcare Worker",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$hcw))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$hcw=="Yes")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$hcw=="No")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$hcw),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$hcw),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$hcw),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$hcw),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$hcw),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$hcw),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$hcw),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$hcw),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$hcw),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$hcw),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$hcw),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$hcw),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$hcw),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$hcw),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$hcw),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$hcw),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$hcw),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,primary=cisco$hcw),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$hcw),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$hcw),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$hcw),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$hcw),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$hcw),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$hcw),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$hcw),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$hcw),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$hcw),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$hcw),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$hcw),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$hcw),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$hcw),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$hcw),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$hcw),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$hcw),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$hcw),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$hcw),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$hcw),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$hcw),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$hcw),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$hcw),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$hcw),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$hcw),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$hcw),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$hcw),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$hcw),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$hcw),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$hcw),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$hcw),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$hcw),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$hcw),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$hcw),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$hcw),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$hcw),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$hcw),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$hcw),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$hcw),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$hcw),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$hcw),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$hcw),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$hcw),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$hcw),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$hcw),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$hcw),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$hcw),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$hcw),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$hcw),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$hcw),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$hcw),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$hcw),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$hcw),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$hcw),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$hcw),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$hcw),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$hcw),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$hcw),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$hcw),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$hcw),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$hcw),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$hcw),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$hcw),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$hcw),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$hcw),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$hcw),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$hcw),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$hcw),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$hcw),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$hcw),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$hcw),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$hcw),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$hcw),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$hcw),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$hcw),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$hcw),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$hcw),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$hcw),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$hcw),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$hcw),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$hcw),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$hcw),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$hcw),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$hcw),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$hcw),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$hcw),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$hcw),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$hcw),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$hcw),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$hcw),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$hcw),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$hcw),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$hcw),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$hcw),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$hcw),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$hcw),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$hcw),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$hcw),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$hcw),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$hcw),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$hcw),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$hcw),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$hcw),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$hcw),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$hcw),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$hcw),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$hcw),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$hcw),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$hcw),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$hcw),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$hcw),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$hcw),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$hcw),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$hcw),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$hcw),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$hcw),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$hcw),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$hcw),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$hcw),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$hcw),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$hcw),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$hcw),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$hcw),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$hcw),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$hcw),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.2","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by age."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Age",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$age_group))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="&lt;45")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="45-54")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="55-64")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$age_group=="&ge;65")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$age_group),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$age_group),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$age_group),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$age_group),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$age_group),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$age_group),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$age_group),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$age_group),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$age_group),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$age_group),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$age_group),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$age_group),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$age_group),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$age_group),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$age_group),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$age_group),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$age_group),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,primary=cisco$age_group),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$age_group),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$age_group),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$age_group),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$age_group),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$age_group),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$age_group),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$age_group),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$age_group),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$age_group),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$age_group),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$age_group),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$age_group),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$age_group),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$age_group),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$age_group),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$age_group),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$age_group),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$age_group),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$age_group),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$age_group),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$age_group),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$age_group),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$age_group),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$age_group),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$age_group),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$age_group),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$age_group),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$age_group),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$age_group),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$age_group),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$age_group),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$age_group),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$age_group),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$age_group),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$age_group),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$age_group),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$age_group),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$age_group),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$age_group),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$age_group),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$age_group),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$age_group),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$age_group),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$age_group),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$age_group),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$age_group),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$age_group),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$age_group),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$age_group),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$age_group),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$age_group),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$age_group),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$age_group),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$age_group),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$age_group),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$age_group),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$age_group),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$age_group),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$age_group),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$age_group),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$age_group),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$age_group),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$age_group),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$age_group),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$age_group),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$age_group),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$age_group),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$age_group),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$age_group),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$age_group),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$age_group),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$age_group),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$age_group),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$age_group),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$age_group),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$age_group),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$age_group),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$age_group),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$age_group),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$age_group),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$age_group),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$age_group),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$age_group),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$age_group),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$age_group),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$age_group),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$age_group),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$age_group),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$age_group),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$age_group),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$age_group),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$age_group),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$age_group),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$age_group),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$age_group),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$age_group),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$age_group),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$age_group),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$age_group),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$age_group),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$age_group),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$age_group),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$age_group),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$age_group),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$age_group),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$age_group),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$age_group),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$age_group),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$age_group),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$age_group),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$age_group),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$age_group),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$age_group),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$age_group),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$age_group),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$age_group),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$age_group),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$age_group),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$age_group),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$age_group),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$age_group),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$age_group),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$age_group),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$age_group),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.3","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by sex."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Sex",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$sex))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$sex=="Male")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$sex=="Female")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$sex),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$sex),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$sex),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$sex),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$sex),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$sex),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$sex),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$sex),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$sex),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$sex),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$sex),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$sex),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$sex),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$sex),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$sex),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$sex),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$sex),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,primary=cisco$sex),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$sex),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$sex),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$sex),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$sex),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$sex),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$sex),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$sex),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$sex),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$sex),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$sex),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$sex),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$sex),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$sex),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$sex),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$sex),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$sex),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$sex),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$sex),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$sex),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$sex),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$sex),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$sex),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$sex),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$sex),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$sex),
						npc.row("LVEF reduced (males<48%, females<51%)",cisco$redlvef_cmr,"Yes",primary=cisco$sex),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$sex),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$sex),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$sex),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$sex),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$sex),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$sex),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$sex),
						npc.row("RVEF reduced (males<45%, females<47%)",cisco$redrvef_cmr,"Yes",primary=cisco$sex),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$sex),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$sex),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$sex),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$sex),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$sex),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$sex),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$sex),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$sex),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$sex),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$sex),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$sex),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$sex),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$sex),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$sex),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$sex),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$sex),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$sex),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$sex),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$sex),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$sex),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$sex),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$sex),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$sex),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$sex),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$sex),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$sex),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$sex),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$sex),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$sex),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$sex),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$sex),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$sex),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$sex),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$sex),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$sex),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$sex),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$sex),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$sex),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$sex),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$sex),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$sex),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$sex),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$sex),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$sex),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$sex),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$sex),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$sex),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$sex),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$sex),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$sex),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$sex),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$sex),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$sex),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$sex),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$sex),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$sex),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$sex),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$sex),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$sex),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$sex),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$sex),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$sex),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$sex),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$sex),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$sex),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$sex),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$sex),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$sex),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$sex),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$sex),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$sex),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$sex),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$sex),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$sex),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$sex),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$sex),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$sex),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$sex),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$sex),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$sex),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$sex),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$sex),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$sex),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$sex),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$sex),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$sex),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$sex),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$sex),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.4","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by SIMD."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="SIMD",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$simd))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q1 - Most Deprived")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q2")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q3")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q4")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$simd=="Q5 - Least Deprived")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$simd),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$simd),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$simd),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$simd),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$simd),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$simd),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$simd),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$simd),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$simd),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$simd),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$simd),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$simd),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$simd),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$simd),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$simd),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$simd),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$simd),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,simulate.p=T,B=10000,primary=cisco$simd),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$simd),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$simd),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$simd),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$simd),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$simd),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$simd),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$simd),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$simd),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$simd),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$simd),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$simd),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$simd),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$simd),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$simd),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$simd),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$simd),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$simd),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$simd),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$simd),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$simd),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$simd),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$simd),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$simd),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$simd),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$simd),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$simd),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$simd),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$simd),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$simd),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$simd),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$simd),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$simd),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$simd),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$simd),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$simd),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$simd),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$simd),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$simd),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$simd),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$simd),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$simd),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$simd),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$simd),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$simd),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$simd),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$simd),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$simd),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$simd),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$simd),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$simd),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$simd),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$simd),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$simd),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$simd),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$simd),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$simd),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$simd),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$simd),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$simd),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$simd),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$simd),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$simd),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$simd),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$simd),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$simd),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$simd),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$simd),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$simd),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$simd),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$simd),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$simd),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$simd),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$simd),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$simd),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$simd),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$simd),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$simd),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$simd),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$simd),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$simd),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$simd),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$simd),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$simd),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$simd),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$simd),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$simd),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$simd),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$simd),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$simd),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$simd),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$simd),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$simd),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$simd),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$simd),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$simd),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$simd),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$simd),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$simd),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$simd),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$simd),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$simd),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$simd),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$simd),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$simd),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$simd),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$simd),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$simd),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$simd),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$simd),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$simd),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$simd),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$simd),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$simd),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$simd),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$simd),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$simd),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$simd),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$simd),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$simd),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$simd),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$simd),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$simd),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$simd),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$simd),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.5","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by Lake Louise Criteria."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Lake Louise Criteria",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$lake_louise))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="Definite")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="Probable")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$lake_louise=="None")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$lake_louise),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$lake_louise),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$lake_louise),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$lake_louise),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$lake_louise),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$lake_louise),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$lake_louise),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$lake_louise),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$lake_louise),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$lake_louise),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$lake_louise),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$lake_louise),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$lake_louise),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$lake_louise),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$lake_louise),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$lake_louise),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$lake_louise),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,primary=cisco$lake_louise),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$lake_louise),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$lake_louise),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$lake_louise),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$lake_louise),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$lake_louise),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$lake_louise),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$lake_louise),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$lake_louise),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$lake_louise),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$lake_louise),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$lake_louise),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$lake_louise),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$lake_louise),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$lake_louise),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$lake_louise),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$lake_louise),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$lake_louise),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$lake_louise),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$lake_louise),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$lake_louise),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$lake_louise),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$lake_louise),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$lake_louise),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$lake_louise),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$lake_louise),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$lake_louise),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$lake_louise),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$lake_louise),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$lake_louise),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$lake_louise),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$lake_louise),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$lake_louise),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$lake_louise),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$lake_louise),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$lake_louise),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$lake_louise),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$lake_louise),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$lake_louise),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$lake_louise),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$lake_louise),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$lake_louise),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$lake_louise),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$lake_louise),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$lake_louise),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$lake_louise),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$lake_louise),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$lake_louise),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$lake_louise),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$lake_louise,p2=F),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$lake_louise),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$lake_louise),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$lake_louise),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$lake_louise),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$lake_louise),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$lake_louise),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$lake_louise),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$lake_louise),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$lake_louise),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$lake_louise),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$lake_louise),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$lake_louise),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$lake_louise),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$lake_louise),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$lake_louise),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$lake_louise),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$lake_louise),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$lake_louise),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$lake_louise),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$lake_louise),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$lake_louise),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$lake_louise),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$lake_louise),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$lake_louise),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$lake_louise),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$lake_louise),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$lake_louise),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$lake_louise),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$lake_louise),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$lake_louise),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$lake_louise),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$lake_louise),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$lake_louise),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$lake_louise),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$lake_louise),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$lake_louise),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$lake_louise),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$lake_louise),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$lake_louise),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$lake_louise),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$lake_louise),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$lake_louise),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$lake_louise),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$lake_louise),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$lake_louise),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$lake_louise),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$lake_louise),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$lake_louise),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$lake_louise),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$lake_louise),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$lake_louise),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$lake_louise),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$lake_louise),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$lake_louise),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$lake_louise),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$lake_louise),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$lake_louise),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$lake_louise),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$lake_louise),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$lake_louise),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$lake_louise),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$lake_louise),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$lake_louise),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$lake_louise),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$lake_louise),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$lake_louise),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$lake_louise),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$lake_louise),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$lake_louise),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$lake_louise),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$lake_louise),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$lake_louise),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("2.6","Cardiovascular phenotyping including ECG, CTCA, FFRCT, and MRI, and biomarker results [all core lab data], by Lake Louise Criteria."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Primary outcome: at least probable myocarditis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary_bin))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary_bin=="No")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary_bin=="Yes")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("ECG"),Style=list("border-top"="solid windowtext 1pt")),

						row.title(italic("Admission"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi0,"Yes",primary=cisco$primary_bin),
						npc.row("Premature atrial contraction",cisco$premac0,"Yes",primary=cisco$primary_bin),
						npc.row("Premature ventricular contraction",cisco$premvc0,"Yes",primary=cisco$primary_bin),
						npc.row("Atrial fibrillation or flutter",cisco$aff0,"Yes",primary=cisco$primary_bin),

						row.title(italic("Enrolment"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi1,"Yes",primary=cisco$primary_bin),
						npc.row("Premature atrial contraction",cisco$premac1,"Yes",primary=cisco$primary_bin),
						npc.row("Premature ventricular contraction",cisco$premvc1,"Yes",primary=cisco$primary_bin),
						npc.row("Atrial fibrillation or flutter",cisco$aff1,"Yes",primary=cisco$primary_bin),

						row.title(italic("28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Myopericarditis criteria",cisco$myoperi2,"Yes",primary=cisco$primary_bin),
						npc.row("Premature atrial contraction",cisco$premac2,"Yes",primary=cisco$primary_bin),
						npc.row("Premature ventricular contraction",cisco$premvc2,"Yes",primary=cisco$primary_bin),
						npc.row("Atrial fibrillation or flutter",cisco$aff2,"Yes",primary=cisco$primary_bin),

						row.title(bold("CT Chest 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						npc.row("Atelectasis",cisco$atelectasis,"Yes",primary=cisco$primary_bin),
						npc.row("Reticulation and/or architectural distortion",cisco$reticulation,"Yes",primary=cisco$primary_bin),
						npc.row("Ground glass opacity",cisco$ggopacity,"Yes",primary=cisco$primary_bin),
						npc.row("Pulmonary arterial thrombus",cisco$pathrombus,"Yes",primary=cisco$primary_bin),
						msd.row("Visual estimate of percentage of total lung area abnormal",cisco$percabnlung,1,primary=cisco$primary_bin),
						npc.row("Visual estimate of percentage of total lung area abnormal",cisco$abnlung,primary=cisco$primary_bin),

						row.title(bold("CT coronary angiogram 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV, ml",cisco$lvedv_ctca,0,primary=cisco$primary_bin),
						msd.row("LV mass, g",cisco$lvmass_ctca,0,primary=cisco$primary_bin),
						msd.row("Agatston score",cisco$agatston,0,primary=cisco$primary_bin),
						msd.row("MESA percentile",cisco$mesa,1,primary=cisco$primary_bin),
						npc.row("CAD-RADS score",cisco$cadrads,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("Obstructive CAD",cisco$ocad,"Yes",primary=cisco$primary_bin),
						msd.row("Mean LAD FFR<sub>CT</sub>",cisco$meanffr_lad,2,primary=cisco$primary_bin),
						msd.row("Median LAD FFR<sub>CT</sub>",cisco$medianffr_lad,2,primary=cisco$primary_bin),
						msd.row("Minimum LAD FFR<sub>CT</sub>",cisco$minffr_lad,2,primary=cisco$primary_bin),
						npc.row("Minimum LAD FFR<sub>CT</sub> &le;0.8",cisco$ffr_lad,"Yes",primary=cisco$primary_bin),
						msd.row("Mean Circumflex FFR<sub>CT</sub>",cisco$meanffr_cx,2,primary=cisco$primary_bin),
						msd.row("Median Circumflex FFR<sub>CT</sub>",cisco$medianffr_cx,2,primary=cisco$primary_bin),
						msd.row("Minimum Circumflex FFR<sub>CT</sub>",cisco$minffr_cx,2,primary=cisco$primary_bin),
						npc.row("Minimum Circumflex FFR<sub>CT</sub> &le;0.8",cisco$ffr_cx,"Yes",primary=cisco$primary_bin),
						msd.row("Mean RCA FFR<sub>CT</sub>",cisco$meanffr_rca,2,primary=cisco$primary_bin),
						msd.row("Median RCA FFR<sub>CT</sub>",cisco$medianffr_rca,2,primary=cisco$primary_bin),
						msd.row("Minimum RCA FFR<sub>CT</sub>",cisco$minffr_rca,2,primary=cisco$primary_bin),
						npc.row("Minimum RCA FFR<sub>CT</sub> &lt;0.8",cisco$ffr_rca,"Yes",primary=cisco$primary_bin),
						msd.row("Mean patient-level FFR<sub>CT</sub>",cisco$meanffr,2,primary=cisco$primary_bin),
						msd.row("Median patient-level FFR<sub>CT</sub>",cisco$medianffr,2,primary=cisco$primary_bin),
						msd.row("Minimum patient-level FFR<sub>CT</sub>",cisco$minffr,2,primary=cisco$primary_bin),
						npc.row("Minimum patient-level FFR<sub>CT</sub> &lt;0.8",cisco$ffr,"Yes",primary=cisco$primary_bin),

						row.title(bold("CMR 28 – 60 days post-discharge"),Style=list("border-top"="solid windowtext 1pt")),
						msd.row("LVEDV indexed, ml/m<sup>2</sup>",cisco$lvedvi_cmr,1,primary=cisco$primary_bin),
						msd.row("LVESV indexed, ml/m<sup>2</sup>",cisco$lvesvi_cmr,1,primary=cisco$primary_bin),
						msd.row("LVEF, %",cisco$lvef_cmr,1,primary=cisco$primary_bin),
						npc.row("LVEF reduced (males) <48%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$primary_bin),
						npc.row("LVEF reduced (females) <51%",cisco$redlvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$primary_bin),
						msd.row("LV mass, g",cisco$lvm_cmr,1,primary=cisco$primary_bin),
						msd.row("LV GLS, %",cisco$lvgls_cmr,1,primary=cisco$primary_bin),
						msd.row("LV GCS, %",cisco$lvgcs_cmr,1,primary=cisco$primary_bin),
						msd.row("LV GRS, %",cisco$lvgrs_cmr,1,primary=cisco$primary_bin),
						msd.row("RVEDV indexed, ml/m<sup>2</sup>",cisco$rvedvi_cmr,1,primary=cisco$primary_bin),
						msd.row("RVESV indexed, ml/m<sup>2</sup>",cisco$rvesvi_cmr,1,primary=cisco$primary_bin),
						msd.row("RVEF, %",cisco$rvef_cmr,1,primary=cisco$primary_bin),
						npc.row("RVEF reduced (males) <45%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Male",primary=cisco$primary_bin),
						npc.row("RVEF reduced (females) <47%",cisco$redrvef_cmr,"Yes",subset=cisco$sex=="Female",primary=cisco$primary_bin),
						msd.row("RV GLS, %",cisco$rvgls_cmr,1,primary=cisco$primary_bin),

						row.title(italic("Multi-parametric myocardial mapping")),
						npc.row("Abnormal global T1 (&gt;1233ms by MOLLI)",cisco$abnglob_t1,"Yes",primary=cisco$primary_bin),
						npc.row("Abnormal global T2 (&gt;44ms)",cisco$abnglob_t2,"Yes",primary=cisco$primary_bin),
						msd.row("T2 ratio (myocardium/serratus anterior)",cisco$t2_ratio,2,primary=cisco$primary_bin),
						npc.row("Abnormal global ECV (&gt;27.4%)",cisco$abnglobecv_t1,"Yes",primary=cisco$primary_bin),

						row.title(italic("Late gadolinium enhancement")),
						npc.row("Any LGE",cisco$any_lge,"Yes",primary=cisco$primary_bin),
						npc.row("Ischaemic distribution",cisco$isch_dist,"Yes",primary=cisco$primary_bin),
						npc.row("Non-ischaemic distribution",cisco$nonisch_dist,"Yes",primary=cisco$primary_bin),
						npc.row("Mixed distribution",cisco$mixed_dist,"Yes",primary=cisco$primary_bin),
						npc.row("Pericardial thickening",cisco$pcthick_cmr,"Yes",primary=cisco$primary_bin),
						npc.row("Pericardial effusion",cisco$pceff_cmr,"Yes",primary=cisco$primary_bin),
						msd.row("Right atrial area, cm<sup>2</sup>",cisco$raa_cmr,2,primary=cisco$primary_bin),
						msd.row("Left atrial area, cm<sup>2</sup>",cisco$laa_cmr,2,primary=cisco$primary_bin),
						npc.row("Probable myocardial inflammation<br>Definite myocardial inflammation (Lake Louise Criteria)",cisco$lake_louise,c("Probable","Definite"),simulate.p=T,B=10000,primary=cisco$primary_bin),
						npc.row("LGE Classification",cisco$lge_class,simulate.p=T,B=10000,primary=cisco$primary_bin),

						row.title(bold("Blood biomarkers"),Style=list("border-top"="solid windowtext 1pt")),
						row.title(italic("Core lab (enrolment)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_1,1,primary=cisco$primary_bin),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_1,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_1,0,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_1,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_1,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_1,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_1,0,primary=cisco$primary_bin),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_1),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_1,0,primary=cisco$primary_bin),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_1,2,primary=cisco$primary_bin),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_1,2,primary=cisco$primary_bin),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_1,2,primary=cisco$primary_bin),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_1,0,primary=cisco$primary_bin),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_1,0,primary=cisco$primary_bin),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_1,2,primary=cisco$primary_bin),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_1,2,primary=cisco$primary_bin),
						miqr.row("ST2, ng/ml",cisco$st2_lab_1,1,primary=cisco$primary_bin),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_1,0,primary=cisco$primary_bin),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_1,1,primary=cisco$primary_bin),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_1,1,primary=cisco$primary_bin),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_1,"Yes",primary=cisco$primary_bin),
						miqr.row("LDH, U/l",cisco$ldh_lab_1,0,primary=cisco$primary_bin),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_1,2,primary=cisco$primary_bin),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_1,2,primary=cisco$primary_bin),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_1,2,primary=cisco$primary_bin),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_1,2,primary=cisco$primary_bin),

						msd.row("Prothrombin Time, s",cisco$pt_lab_1,1,primary=cisco$primary_bin),
						msd.row("PT ratio",cisco$ptratio_lab_1,2,primary=cisco$primary_bin),
						msd.row("APTT, s",cisco$aptt_lab_1,1,primary=cisco$primary_bin),
						msd.row("APTT ratio",cisco$apttratio_lab_1,2,primary=cisco$primary_bin),
						msd.row("TCT, s",cisco$tct_lab_1,1,primary=cisco$primary_bin),
						msd.row("TCT ratio",cisco$tctratio_lab_1,2,primary=cisco$primary_bin),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_1,0,primary=cisco$primary_bin),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_1,2,primary=cisco$primary_bin),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_1,0,primary=cisco$primary_bin),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_1,0,primary=cisco$primary_bin),
						msd.row("Protein S",cisco$prots_lab_1,1,primary=cisco$primary_bin),
						msd.row("Protein C",cisco$protc_lab_1,1,primary=cisco$primary_bin),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_1,0,primary=cisco$primary_bin),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_1,0,primary=cisco$primary_bin),

						row.title(italic("Core lab (28 – 60 days post-discharge)")),
						miqr.row("C-reactive protein, mg/l",cisco$crp_lab_2,1,primary=cisco$primary_bin),
						npc.row("C-reactive protein, mg/l",cisco$crp_lab_cat_2,simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("High sensitivity troponin I, ng/l",cisco$hstni_lab_2,0,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I, males",trop.cat(cisco$hstni_lab_2,cisco$sex,"Male"),subset=cisco$sex=="Male",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I, females",trop.cat(cisco$hstni_lab_2,cisco$sex,"Female"),subset=cisco$sex=="Female",simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						npc.row("High sensitivity troponin I",trop.cat(cisco$hstni_lab_2,cisco$sex,"All"),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("NTproBNP, ng/l",cisco$ntprobnp_lab_2,0,primary=cisco$primary_bin),
						npc.row("NTproBNP",bnp.cat(cisco$ntprobnp_lab_2),simulate.p.value=T,B=10000,primary=cisco$primary_bin),
						miqr.row("Ferritin, &mu;g/l",cisco$ferritin_lab_2,0,primary=cisco$primary_bin),
						msd.row("Total Cholesterol, mmol/l",cisco$chol_lab_2,2,primary=cisco$primary_bin),
						msd.row("Triglycerides, mmol/l",cisco$trig_lab_2,2,primary=cisco$primary_bin),
						msd.row("HDL Cholesterol, mmol/l",cisco$hdl_lab_2,2,primary=cisco$primary_bin),
						miqr.row("ICAM-1, ng/ml",cisco$icam1_lab_2,0,primary=cisco$primary_bin),
						miqr.row("VCAM-1, ng/ml",cisco$vcam1_lab_2,0,primary=cisco$primary_bin),
						miqr.row("Endothelin-1, pg/ml",cisco$endothelin1_lab_2,2,primary=cisco$primary_bin),
						miqr.row("IL-6, pg/ml",cisco$il6_lab_2,2,primary=cisco$primary_bin),
						miqr.row("ST2, ng/ml",cisco$st2_lab_2,1,primary=cisco$primary_bin),
						miqr.row("p-selectin, ng/ml",cisco$pselectin_lab_2,0,primary=cisco$primary_bin),
						miqr.row("Creatinine, &mu;mol/l",cisco$creat_lab_2,1,primary=cisco$primary_bin),
						miqr.row("eGFR, ml/min/1.73m<sup>2</sup>",cisco$egfr_lab_2,1,primary=cisco$primary_bin),
						npc.row("eGFR &lt;60 ml/min/1.73m<sup>2</sup>",cisco$rendys_lab_2,"Yes",primary=cisco$primary_bin),
						miqr.row("LDH, U/l",cisco$ldh_lab_2,0,primary=cisco$primary_bin),
						miqr.row("Haptoglobin, g/l",cisco$haptoglobin_lab_2,2,primary=cisco$primary_bin),
						miqr.row("Total bilirubin, &mu;mol/l",cisco$total_bili_lab_2,2,primary=cisco$primary_bin),
						miqr.row("Direct bilirubin, &mu;mol/l",cisco$direct_bili_lab_2,2,primary=cisco$primary_bin),
						miqr.row("Indirect bilirubin, &mu;mol/l",cisco$indirect_bili_lab_2,2,primary=cisco$primary_bin),

						msd.row("Prothrombin Time, s",cisco$pt_lab_2,1,primary=cisco$primary_bin),
						msd.row("PT ratio",cisco$ptratio_lab_2,2,primary=cisco$primary_bin),
						msd.row("APTT, s",cisco$aptt_lab_2,1,primary=cisco$primary_bin),
						msd.row("APTT ratio",cisco$apttratio_lab_2,2,primary=cisco$primary_bin),
						msd.row("TCT, s",cisco$tct_lab_2,1,primary=cisco$primary_bin),
						msd.row("TCT ratio",cisco$tctratio_lab_2,2,primary=cisco$primary_bin),
						msd.row("D-Dimer, ng/ml",cisco$ddimer_lab_2,0,primary=cisco$primary_bin),
						msd.row("Fibrinogen, g/l",cisco$fib_lab_2,2,primary=cisco$primary_bin),
						msd.row("Factor VIII, IU/dl",cisco$factorviii_lab_2,0,primary=cisco$primary_bin),
						msd.row("Antithrombin, IU/dl",cisco$antithr_lab_2,0,primary=cisco$primary_bin),
						msd.row("Protein S",cisco$prots_lab_2,1,primary=cisco$primary_bin),
						msd.row("Protein C",cisco$protc_lab_2,1,primary=cisco$primary_bin),
						msd.row("VWF: GP1bR",cisco$vwf_gp1br_lab_2,0,primary=cisco$primary_bin),
						msd.row("VWF: Ag",cisco$vwf_ag_lab_2,0,primary=cisco$primary_bin),

						row.title(italic("Urine biomarkers")),
						msd.row("Albumin:creatinine ratio, enrolment",cisco$acr_1,2,primary=cisco$primary_bin),
						msd.row("Albumin:creatinine ratio, 28 – 60 days post-discharge",cisco$acr_2,2,primary=cisco$primary_bin),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file.alt,F,F,T)





	# TABLE 3

		temp<-
			list(
				head=
					rbindTable(
						table.title(3,"Secondary outcomes."),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						npc.row("Myocardial injury",cisco$sec_myoinj,"Yes",showCont=F),

						row.title(italic("Cause of Myocardial Inflammation (endotype)")),
						npc.row("SARS-COV-2 Myocarditis",cisco$sec_sars,showCont=F,simulate.p.value=T,B=10000),
						npc.row("Acute Stress Cardiomyopathy",cisco$sec_asc,showCont=F,simulate.p.value=T,B=10000),
						npc.row("Ischaemia/Impaired Perfusion as a Stressor of Inflammation",cisco$sec_isch,showCont=F,simulate.p.value=T,B=10000),
						npc.row("Infective Myopercarditis (Non-Covid Infection)",cisco$sec_noncov,showCont=F,simulate.p.value=T,B=10000),
						npc.row("Drug Induced (Toxic) Myocardial Inflammation",cisco$sec_drug,showCont=F,simulate.p.value=T,B=10000),
						npc.row("Idiopathic Myocardial and/or Pericardial Inflammation",cisco$sec_idio,showCont=F,simulate.p.value=T,B=10000),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)


		temp<-
			list(
				head=
					rbindTable(
						table.title(3,"Secondary outcomes. Consensus patient-level data, split by consensus primary diagnosis"),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						expnpc.row("Myocardial injury",cisco$sec_myoinj,showCont=F),

						row.title(italic("Cause of Myocardial Inflammation (endotype)")),
						expnpc.row("SARS-COV-2 Myocarditis",cisco$sec_sars,showCont=F,simulate.p.value=T,B=10000),
						expnpc.row("Acute Stress Cardiomyopathy",cisco$sec_asc,showCont=F,simulate.p.value=T,B=10000),
						expnpc.row("Ischaemia/Impaired Perfusion as a Stressor of Inflammation",cisco$sec_isch,showCont=F,simulate.p.value=T,B=10000),
						expnpc.row("Infective Myopercarditis (Non-Covid Infection)",cisco$sec_noncov,showCont=F,simulate.p.value=T,B=10000),
						expnpc.row("Drug Induced (Toxic) Myocardial Inflammation",cisco$sec_drug,showCont=F,simulate.p.value=T,B=10000),
						expnpc.row("Idiopathic Myocardial and/or Pericardial Inflammation",cisco$sec_idio,showCont=F,simulate.p.value=T,B=10000),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)


		temp<-
			list(
				head=
					rbindTable(
						table.title("3b","Primary and Secondary outcomes. Reviewer-level data, split by reviewer-level primary diagnosis."),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Reviewer Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Not")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Primary outcome")),
						expnpc.row("Primary outcome",cec$primary,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary,simulate.p.value=T,B=10000),
						expnpc.row("Certainty of diagnosis",cec$certainty,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary,simulate.p.value=T,B=10000),

						row.title(italic("Secondary outcomes")),
						expnpc.row("Myocardial injury",cec$sec_myoinj,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Chronicity",cec$sec_myoinjchron,subset=cec$sec_myoinj=="Yes",showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("ACS",cec$sec_acs,subset=cec$sec_myoinj=="Yes",showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary),

						row.title(italic("Cause of Myocardial Inflammation (endotype)")),
						expnpc.row("SARS-COV-2 Myocarditis",cec$sec_sars,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Acute Stress Cardiomyopathy",cec$sec_asc,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Ischaemia/Impaired Perfusion as a Stressor of Inflammation",cec$sec_isch,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Infective Myopercarditis (Non-Covid Infection)",cec$sec_noncov,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Drug Induced (Toxic) Myocardial Inflammation",cec$sec_drug,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),
						expnpc.row("Idiopathic Myocardial and/or Pericardial Inflammation",cec$sec_idio,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)


		temp<-
			list(
				head=
					rbindTable(
						table.title("3c","Primary and Secondary outcomes. Reviewer-level data, split by consensus primary diagnosis."),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cec$paper=="Yes")),")",sep=""),
							"",
							paste("(N=",sum((cec$paper=="Yes")&(cec$primary_consensus=="Not")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$primary_consensus=="Unlikely")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$primary_consensus=="Probably")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$primary_consensus=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Primary outcome")),
						expnpc.row("Primary outcome",cec$primary,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary_consensus,simulate.p.value=T,B=10000),
						expnpc.row("Certainty of diagnosis",cec$certainty,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary_consensus,simulate.p.value=T,B=10000),

						row.title(italic("Secondary outcomes")),
						expnpc.row("Myocardial injury",cec$sec_myoinj,showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Chronicity",cec$sec_myoinjchron,subset=cec$sec_myoinj=="Yes",showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("ACS",cec$sec_acs,subset=cec$sec_myoinj=="Yes",showCont=F,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),

						row.title(italic("Cause of Myocardial Inflammation (endotype)")),
						expnpc.row("SARS-COV-2 Myocarditis",cec$sec_sars,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Acute Stress Cardiomyopathy",cec$sec_asc,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Ischaemia/Impaired Perfusion as a Stressor of Inflammation",cec$sec_isch,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Infective Myopercarditis (Non-Covid Infection)",cec$sec_noncov,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Drug Induced (Toxic) Myocardial Inflammation",cec$sec_drug,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),
						expnpc.row("Idiopathic Myocardial and/or Pericardial Inflammation",cec$sec_idio,showCont=F,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary_consensus),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("3d","Primary Outcome: Myocarditis criteria."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Clinical criteria")),
						npc.row("Chest Pain",cisco$crit_ch_pain,"Yes"),
						npc.row("New SOB",cisco$crit_new_sob,"Yes"),
						npc.row("Chronic SOB",cisco$crit_chron_sob,"Yes"),
						npc.row("Cardiogenic Shock",cisco$crit_cardiogenic_shock,"Yes"),
						npc.row("Palpitations/Syncope/Arrhythmia",cisco$crit_palp_sync_arrh,"Yes"),
						npc.row("Number of clinical criteria",cisco$crit_num_clin_cat,simulate.p.value=T,B=10000),

						row.title(italic("Diagnostic criteria")),
						npc.row("ECG Myocarditis",cisco$crit_ecg_myo,"Yes"),
						npc.row("T2 Criteria",cisco$crit_t2,"Yes"),
						npc.row("T1 Criteria",cisco$crit_t1,"Yes"),
						npc.row("Lake Louise Criteria",cisco$crit_llc,levels(cisco$crit_llc)[-1],simulate.p.value=T,B=10000,showlevel=T),
						npc.row("Elevated Troponin",cisco$crit_trop,"Yes"),
						npc.row("Global LV Dysfunction",cisco$crit_glob_lv_dys,"Yes"),
						npc.row("Regional LV Dysfunction",cisco$crit_reg_lv_dys,"Yes"),
						npc.row("Pericardial Changes",cisco$crit_peri_ch,"Yes"),
						npc.row("New LV Dysfunction",cisco$crit_new_lv_dys,"Yes"),
						npc.row("Number of diagnostic criteria",cisco$crit_num_diag_cat,simulate.p.value=T,B=10000),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("3e","Primary Outcome: Myocarditis criteria. Split by reviewer-level diagnosis"),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cec$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Not")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cec$paper=="Yes")&(cec$group=="COVID-19")&(cec$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Clinical criteria")),
						npc.row("Chest Pain",cec$crit_ch_pain,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("New SOB",cec$crit_new_sob,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Chronic SOB",cec$crit_chron_sob,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Cardiogenic Shock",cec$crit_cardiogenic_shock,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Palpitations/Syncope/Arrhythmia",cec$crit_palp_sync_arrh,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Number of clinical criteria",cec$crit_num_clin_cat,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),

						row.title(italic("Diagnostic criteria")),
						npc.row("ECG Myocarditis",cec$crit_ecg_myo,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("T2 Criteria",cec$crit_t2,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("T1 Criteria",cec$crit_t1,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Lake Louise Criteria",cec$crit_llc,levels(cec$crit_llc)[-1],simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F,showlevel=T),
						npc.row("Elevated Troponin",cec$crit_trop,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Global LV Dysfunction",cec$crit_glob_lv_dys,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Regional LV Dysfunction",cec$crit_reg_lv_dys,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Pericardial Changes",cec$crit_peri_ch,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("New LV Dysfunction",cec$crit_new_lv_dys,"Yes",paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),
						npc.row("Number of diagnostic criteria",cec$crit_num_diag_cat,simulate.p.value=T,B=10000,paper=cec$paper,group=cec$group,primary=cec$primary,showCont=F),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)

	# TABLE 4
		yesno<-function(x)factor(x=="Yes",c(F,T),c("No","Yes"))
		z<-function(x)(x-mean(x,na.rm=T))/sd(x,na.rm=T)

		fit.uv.age<-glm(primary_bin~I(age/10),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.sex<-glm(primary_bin~sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.eth<-glm(primary_bin~race_2,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.simd<-glm(primary_bin~simd,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.hcw<-glm(primary_bin~yesno(hcw),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.bmi<-glm(primary_bin~I(bmi/5),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.uv.hyperten<-glm(primary_bin~yesno(hyperten),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.ckd<-glm(primary_bin~yesno(renal),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.diabetes<-glm(primary_bin~yesno(diabetes),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.hyperchol<-glm(primary_bin~yesno(hyperchol),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.smoke<-glm(primary_bin~smoke_3,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.cvd<-glm(primary_bin~yesno(cvd),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.qrisk3<-glm(primary_bin~I(qrisk3/10),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.uv.char<-glm(primary_bin~charlson,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.isaric<-glm(primary_bin~I(isaric_risk/10),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.who<-glm(primary_bin~who_short,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.aki<-glm(primary_bin~yesno(aki),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.uv.hb<-glm(primary_bin~z(hb),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.platelet<-glm(primary_bin~z(log(platelet)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.wcc<-glm(primary_bin~z(log(wcc)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.lymph<-glm(primary_bin~z(log(lymph)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.ddimer<-glm(primary_bin~z(log(ddimer_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.fib<-glm(primary_bin~z(fib_hi),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.hba1c<-glm(primary_bin~z(log(hba1c_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.creat<-glm(primary_bin~z(log(creat_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.ferritin<-glm(primary_bin~z(log(ferritin_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.tni<-glm(primary_bin~z(log(tni_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.uv.crp<-glm(primary_bin~z(log(crp_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.as.age<-glm(primary_bin~I(age/10)+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.sex<-glm(primary_bin~sex+age,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.eth<-glm(primary_bin~race_2+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.simd<-glm(primary_bin~simd+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.hcw<-glm(primary_bin~yesno(hcw)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.bmi<-glm(primary_bin~I(bmi/5)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.as.hyperten<-glm(primary_bin~yesno(hyperten)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.ckd<-glm(primary_bin~yesno(renal)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.diabetes<-glm(primary_bin~yesno(diabetes)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.hyperchol<-glm(primary_bin~yesno(hyperchol)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.smoke<-glm(primary_bin~smoke_3+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.cvd<-glm(primary_bin~yesno(cvd)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.qrisk3<-glm(primary_bin~I(qrisk3/10)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.as.char<-glm(primary_bin~charlson+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.isaric<-glm(primary_bin~I(isaric_risk/10)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.who<-glm(primary_bin~who_short+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.aki<-glm(primary_bin~yesno(aki)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.as.hb<-glm(primary_bin~z(hb)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.platelet<-glm(primary_bin~z(log(platelet))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.wcc<-glm(primary_bin~z(log(wcc))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.lymph<-glm(primary_bin~z(log(lymph))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.ddimer<-glm(primary_bin~z(log(ddimer_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.fib<-glm(primary_bin~z(fib_hi)+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.hba1c<-glm(primary_bin~z(log(hba1c_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.creat<-glm(primary_bin~z(log(creat_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.ferritin<-glm(primary_bin~z(log(ferritin_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.tni<-glm(primary_bin~z(log(tni_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		fit.as.crp<-glm(primary_bin~z(log(crp_hi))+age+sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))

		fit.mv<-glm(primary_bin~I(age/10)+sex+yesno(hcw)+yesno(aki)+z(log(hba1c_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19"))
		summary(fit.mv)

		summary(glm(primary_bin~sex+yesno(hcw)+yesno(aki)+z(log(hba1c_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))
		summary(glm(primary_bin~sex+yesno(hcw)+yesno(aki),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))
		summary(glm(primary_bin~sex+yesno(hcw),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))
		summary(glm(primary_bin~sex+yesno(aki),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))
		summary(glm(primary_bin~sex+z(log(hba1c_hi)),data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))
		summary(glm(primary_bin~sex,data=cisco,family=binomial,subset=(paper=="Yes")&(group=="COVID-19")))

		temp<-
			list(
				head=
					rbindTable(
						table.title(
							"4",
							paste(
								"Univariable and multivariable associates of probably/very likely adjudicated myocarditis (primary outcome) including",
								"demographic characteristics (A), cardiovascular history (B), severity of COVID-19 (C), and biomarkers (D).")),
						cbindTable(
							"",
							rbindTable("Univariate",cbindTable("Odds Ratio (95% CI)","p-value")),
							"",
							rbindTable("Age/Sex-adjusted",cbindTable("Odds Ratio (95% CI)","p-value")),
							"",
							rbindTable("Multivariable",cbindTable("Odds Ratio (95% CI)","p-value")))),
				body=
					rbindTable(
						row.title(italic("Demographics")),
						cbindTable(row.title("Age (per 10 years)"),lm.summ(fit.uv.age,2,exp,2),"",lm.summ(fit.as.age,2,exp,2),"",lm.summ(fit.mv,2,exp,2)),
						cbindTable(row.title("Sex (Female vs. Male)"),lm.summ(fit.uv.sex,2,exp,2),"",lm.summ(fit.as.sex,2,exp,2),"",lm.summ(fit.mv,3,exp,2)),
						cbindTable(row.title("Ethnicity (Other vs. White)"),lm.summ(fit.uv.eth,2,exp,2),"",lm.summ(fit.as.eth,2,exp,2),"","",""),
						cbindTable(row.title("SIMD (Quintile 2 vs. Most Deprived)"),lm.summ(fit.uv.simd,2,exp,2),"",lm.summ(fit.as.simd,2,exp,2),"","",""),
						cbindTable(row.title("SIMD (Quintile 3 vs. Most Deprived)"),lm.summ(fit.uv.simd,3,exp,2),"",lm.summ(fit.as.simd,3,exp,2),"","",""),
						cbindTable(row.title("SIMD (Quintile 4 vs. Most Deprived)"),lm.summ(fit.uv.simd,4,exp,2),"",lm.summ(fit.as.simd,4,exp,2),"","",""),
						cbindTable(row.title("SIMD (Quintile 5 vs. Most Deprived)"),lm.summ(fit.uv.simd,5,exp,2),"",lm.summ(fit.as.simd,5,exp,2),"","",""),
						cbindTable(row.title("Healthcare Worker (Yes vs. No)"),lm.summ(fit.uv.hcw,2,exp,2),"",lm.summ(fit.as.hcw,2,exp,2),"",lm.summ(fit.mv,4,exp,2)),
						cbindTable(row.title("Body Mass Index (per 5 kg/m<sup>2</sup>)"),lm.summ(fit.uv.bmi,2,exp,2),"",lm.summ(fit.as.bmi,2,exp,2),"","",""),

						row.title(italic("Cardiovascular history")),
						cbindTable(row.title("Hypertension (Yes vs. No)"),lm.summ(fit.uv.hyperten,2,exp,2),"",lm.summ(fit.as.hyperten,2,exp,2),"","",""),
						cbindTable(row.title("Chronic kidney disease (Yes vs. No)"),lm.summ(fit.uv.ckd,2,exp,2),"",lm.summ(fit.as.ckd,2,exp,2),"","",""),
						cbindTable(row.title("Diabetes (Yes vs. No)"),lm.summ(fit.uv.diabetes,2,exp,2),"",lm.summ(fit.as.diabetes,2,exp,2),"","",""),
						cbindTable(row.title("Hypercholesterolemia (Yes vs. No)"),lm.summ(fit.uv.hyperchol,2,exp,2),"",lm.summ(fit.as.hyperchol,2,exp,2),"","",""),
						cbindTable(row.title("Smoking (Former vs. Never)"),lm.summ(fit.uv.smoke,2,exp,2),"",lm.summ(fit.as.smoke,2,exp,2),"","",""),
						cbindTable(row.title("Smoking (Current vs. Never)"),lm.summ(fit.uv.smoke,3,exp,2),"",lm.summ(fit.as.smoke,3,exp,2),"","",""),
						cbindTable(row.title("History of Cardiovascular Disease (Yes vs. No)"),lm.summ(fit.uv.cvd,2,exp,2),"",lm.summ(fit.as.cvd,2,exp,2),"","",""),
						cbindTable(row.title("Q-Risk 3 10-year cardiovascular risk (per 10%)"),lm.summ(fit.uv.qrisk3,2,exp,2),"",lm.summ(fit.as.qrisk3,2,exp,2),"","",""),
	
						row.title(italic("Medical history")),
						cbindTable(row.title("Charlson Comorbidity Index (per point)"),lm.summ(fit.uv.char,2,exp,2),"",lm.summ(fit.as.char,2,exp,2),"","",""),
						cbindTable(row.title("ISARIC-4c in-hospital mortality risk (per 10%)"),lm.summ(fit.uv.isaric,2,exp,2),"",lm.summ(fit.as.isaric,2,exp,2),"","",""),
						cbindTable(row.title("WHO Score (oxygen therapy vs. hospitalized, no oxygen)"),lm.summ(fit.uv.who,2,exp,2),"",lm.summ(fit.as.who,2,exp,2),"","",""),
						cbindTable(row.title("WHO Score (non-invasive ventilation vs. hospitalized, no oxygen)"),lm.summ(fit.uv.who,3,exp,2),"",lm.summ(fit.as.who,3,exp,2),"","",""),
						cbindTable(row.title("WHO Score (invasive ventilation vs. hospitalized, no oxygen)"),lm.summ(fit.uv.who,4,exp,2),"",lm.summ(fit.as.who,4,exp,2),"","",""),
						cbindTable(row.title("Acute kidney injury (Yes vs. No)"),lm.summ(fit.uv.aki,2,exp,2),"",lm.summ(fit.as.aki,2,exp,2),"",lm.summ(fit.mv,5,exp,2)),

						row.title(italic("Biomarkers (standard care)")),
						cbindTable(row.title("Hemoglobin (per SD)"),lm.summ(fit.uv.hb,2,exp,2),"",lm.summ(fit.as.hb,2,exp,2),"","",""),
						cbindTable(row.title("Platelet count (per SD, log scale)"),lm.summ(fit.uv.platelet,2,exp,2),"",lm.summ(fit.as.platelet,2,exp,2),"","",""),
						cbindTable(row.title("Peak white cell count (per SD, log scale)"),lm.summ(fit.uv.wcc,2,exp,2),"",lm.summ(fit.as.wcc,2,exp,2),"","",""),
						cbindTable(row.title("Lowest lymphocyte count (per SD, log scale)"),lm.summ(fit.uv.lymph,2,exp,2),"",lm.summ(fit.as.lymph,2,exp,2),"","",""),
						cbindTable(row.title("Peak d-dimer (per SD, log scale)"),lm.summ(fit.uv.ddimer,2,exp,2),"",lm.summ(fit.as.ddimer,2,exp,2),"","",""),
						cbindTable(row.title("Peak fibrinogen (per SD)"),lm.summ(fit.uv.fib,2,exp,2),"",lm.summ(fit.as.fib,2,exp,2),"","",""),
						cbindTable(row.title("Peak HbA1c (per SD, log scale)"),lm.summ(fit.uv.hba1c,2,exp,2),"",lm.summ(fit.as.hba1c,2,exp,2),"",lm.summ(fit.mv,6,exp,2)),
						cbindTable(row.title("Peak creatinine (per SD, log scale)"),lm.summ(fit.uv.creat,2,exp,2),"",lm.summ(fit.as.creat,2,exp,2),"","",""),
						cbindTable(row.title("Peak ferritin (per SD, log scale)"),lm.summ(fit.uv.ferritin,2,exp,2),"",lm.summ(fit.as.ferritin,2,exp,2),"","",""),
						cbindTable(row.title("Peak high sensitivity troponin I (per SD, log scale)"),lm.summ(fit.uv.tni,2,exp,2),"",lm.summ(fit.as.tni,2,exp,2),"","",""),
						cbindTable(row.title("Peak C-reactive protein (per SD, log scale)"),lm.summ(fit.uv.crp,2,exp,2),"",lm.summ(fit.as.crp,2,exp,2),"","",""),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)

	# TABLE 5

		temp<-
			list(
				head=
					rbindTable(
						table.title(5,"Health status, illness perception, anxiety and depression, and physical function."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Health status")),
						msd.row("EQ-5D Heath Utility Score at enrolment",cisco$eq5d_hu_1,2),
						msd.row("EQ-5D Heath Utility Score at 28-60 days post-discharge",cisco$eq5d_hu_2,2),
						msd.row("EQ-5D VAS at enrolment",cisco$eq5d_vas_1,2),
						msd.row("EQ-5D VAS at 28-60 days post-discharge",cisco$eq5d_vas_2,2),

						row.title(italic("Illness perception")),
						msd.row("Brief Illness Perception Questionnaire Score at enrolment",cisco$bip_1,1),
						msd.row("Brief Illness Perception Questionnaire Score at 28-60 days post-discharge",cisco$bip_2,1),

						row.title(italic("Anxiety and depression")),
						msd.row("PHQ-4 anxiety score at enrolment",cisco$phq4_anx_1,2),
						msd.row("PHQ-4 anxiety score at 28-60 days post-discharge",cisco$phq4_anx_2,2),
						msd.row("PHQ-4 depression score at enrolment",cisco$phq4_dep_1,2),
						msd.row("PHQ-4 depression score at 28-60 days post-discharge",cisco$phq4_dep_2,2),
						msd.row("PHQ-4 total score at enrolment",cisco$phq4_1,2),
						msd.row("PHQ-4 total score at 28-60 days post-discharge",cisco$phq4_2,2),

						row.title(italic("Physical function")),
						npc.row("IPAQ Category at enrolment",cisco$ipaq_cat_1),
						npc.row("IPAQ Category at 28-60 days post-discharge",cisco$ipaq_cat_2),
						msd.row("Duke Activity Status Index at enrolment",cisco$dasi_score_1,1),
						msd.row("Duke Activity Status Index at 28-60 days post-discharge",cisco$dasi_score_2,1),
						msd.row("DASI VO<sub>2</sub> max estimate at enrolment",cisco$dasi_vo2max_1,1),
						msd.row("DASI VO<sub>2</sub> max estimate at 28-60 days post-discharge",cisco$dasi_vo2max_2,1),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)


		temp<-
			list(
				head=
					rbindTable(
						table.title(5,"Health status, illness perception, anxiety and depression, and physical function."),
						cbindTable(
							"","","COVID-19","Control","p",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Health status")),
						expcont.row("EQ-5D Heath Utility Score at enrolment",cisco$eq5d_hu_1,2),
						expcont.row("EQ-5D Heath Utility Score at 28-60 days post-discharge",cisco$eq5d_hu_2,2),
						expcont.row("EQ-5D VAS at enrolment",cisco$eq5d_vas_1,1),
						expcont.row("EQ-5D VAS at 28-60 days post-discharge",cisco$eq5d_vas_2,1),

						row.title(italic("Illness perception")),
						expcont.row("Brief Illness Perception Questionnaire Score at enrolment",cisco$bip_1,1),
						expcont.row("Brief Illness Perception Questionnaire Score at 28-60 days post-discharge",cisco$bip_2,1),

						row.title(italic("Anxiety and depression")),
						expcont.row("PHQ-4 anxiety score at enrolment",cisco$phq4_anx_1,2),
						expcont.row("PHQ-4 anxiety score at 28-60 days post-discharge",cisco$phq4_anx_2,2),
						expcont.row("PHQ-4 depression score at enrolment",cisco$phq4_dep_1,2),
						expcont.row("PHQ-4 depression score at 28-60 days post-discharge",cisco$phq4_dep_2,2),
						expcont.row("PHQ-4 total score at enrolment",cisco$phq4_1,2),
						expcont.row("PHQ-4 total score at 28-60 days post-discharge",cisco$phq4_2,2),

						row.title(italic("Physical function")),
						expnpc.row("IPAQ Category at enrolment",cisco$ipaq_cat_1),
						expnpc.row("IPAQ Category at 28-60 days post-discharge",cisco$ipaq_cat_2),
						expcont.row("Duke Activity Status Index at enrolment",cisco$dasi_score_1,1),
						expcont.row("Duke Activity Status Index at 28-60 days post-discharge",cisco$dasi_score_2,1),
						expcont.row("DASI VO<sub>2</sub> max estimate at enrolment",cisco$dasi_vo2max_1,1),
						expcont.row("DASI VO<sub>2</sub> max estimate at 28-60 days post-discharge",cisco$dasi_vo2max_2,1),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)


	# SUPPLEMENTARY TABLE 1

		temp<-
			list(
				head=
					rbindTable(
						table.title("S1","Times (days) between Symptom Onset, Diagnosis, Admission, Discharge and Study Visits."),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						miqr.row("Symptom Onset to Diagnosis",as.numeric(cisco$cdiag_date-cisco$onset_date),0,showCont=F),
						miqr.row("Symptom Onset to Admission",as.numeric(cisco$adm_date-cisco$onset_date),0,showCont=F),
						miqr.row("Admission to Discharge",as.numeric(cisco$disch_date-cisco$adm_date),0,showCont=F),
						miqr.row("Symptom Onset to Visit 1",as.numeric(cisco$v1_date-cisco$onset_date),0,showCont=F),
						miqr.row("Diagnosis to Visit 1",as.numeric(cisco$v1_date-cisco$cdiag_date),0,showCont=F),
						miqr.row("Symptom Onset to Visit 2",as.numeric(cisco$v2_date-cisco$onset_date),0,showCont=F),
						miqr.row("Diagnosis to Visit 2",as.numeric(cisco$v2_date-cisco$cdiag_date),0,showCont=F),
						miqr.row("Discharge to Visit 2",as.numeric(cisco$v2_date-cisco$disch_date),0,showCont=F),
						miqr.row("Visit 1 to Visit 2",as.numeric(cisco$v2_date-cisco$v1_date),0,showCont=F),
						npc.row("Hospitalised (Overnight Stay)",cisco$hosp,"Yes",showCont=F,simulate.p.value=T,B=10000),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)


		temp<-
			list(
				head=
					rbindTable(
						table.title("S1","Times (days) between Symptom Onset, Diagnosis, Admission, Discharge and Study Visits."),
						cbindTable(
							"","","All COVID-19","",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						expcont.row("Symptom Onset to Diagnosis",as.numeric(cisco$cdiag_date-cisco$onset_date),0,showCont=F),
						expcont.row("Symptom Onset to Admission",as.numeric(cisco$adm_date-cisco$onset_date),0,showCont=F),
						expcont.row("Admission to Discharge",as.numeric(cisco$disch_date-cisco$adm_date),0,showCont=F),
						expcont.row("Symptom Onset to Visit 1",as.numeric(cisco$v1_date-cisco$onset_date),0,showCont=F),
						expcont.row("Diagnosis to Visit 1",as.numeric(cisco$v1_date-cisco$cdiag_date),0,showCont=F),
						expcont.row("Symptom Onset to Visit 2",as.numeric(cisco$v2_date-cisco$onset_date),0,showCont=F),
						expcont.row("Diagnosis to Visit 2",as.numeric(cisco$v2_date-cisco$cdiag_date),0,showCont=F),
						expcont.row("Discharge to Visit 2",as.numeric(cisco$v2_date-cisco$disch_date),0,showCont=F),
						expcont.row("Visit 1 to Visit 2",as.numeric(cisco$v2_date-cisco$v1_date),0,showCont=F),
						expnpc.row("Hospitalised (Overnight Stay)",cisco$hosp,showCont=F,simulate.p.value=T,B=10000),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)


	# SUPPLEMENTARY TABLE 2

		temp<-
			list(
				head=
					rbindTable(
						table.title("S2","Renal Imaging and Clinical Results."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("Renal Imaging")),
						msd.row("Average of T1 cortex value<br>of right and left kidneys using TR550 (ms)",cisco$avt1cort_tr550,0),
						msd.row("Average of T1 medulla value<br>of right and left kidneys using TR550 (ms)",cisco$avt1med_tr550,0),
						msd.row("Average of T1 cortex value<br>of right and left kidneys using TR1000 (ms)",cisco$avt1cort_tr1000,0),
						msd.row("Average of T1 medulla value<br>of right and left kidneys using TR1000 (ms)",cisco$avt1med_tr1000,0),
						msd.row("Average T1 corticomedullary differentiation<br>of right and left kidneys using TR550 (ms)",cisco$avt1diff_tr550,2),
						msd.row("Average T1 corticomedullary differentiation<br>of right and left kidneys using TR1000 (ms)",cisco$avt1diff_tr1000,2),
						msd.row("Average of T2 cortex value<br>of right and left kidneys (ms)",cisco$avt2cort,1),
						msd.row("Average of T2 medulla value<br>of right and left kidneys (ms)",cisco$avt2med,1),
						msd.row("Average of cortex apparent diffusion coefficient values<br>of right and left kidneys (mm<sup>2</sup>/s)",cisco$avcortadc,0),
						msd.row("Average of medulla apparent diffusion coefficient values<br>of right and left kidneys (mm<sup>2</sup>/s)",cisco$avmedadc,0),
						msd.row("Average volume<br>of right and left kidneys (ml)",cisco$avvol,0),

						row.title(bold("Clinical Question")),
						npc.row("Acute kidney injury (AKI) during covid-19 illness",cisco$aki,"Yes",p1=F),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("S2","Renal Imaging and Clinical Results."),
						cbindTable(
							"","","COVID-19","Control","p-value",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="Control")),")",sep=""),
							"",
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(bold("Renal Imaging")),
						expcont.row("Average of T1 cortex value<br>of right and left kidneys using TR550 (ms)",cisco$avt1cort_tr550,0),
						expcont.row("Average of T1 medulla value<br>of right and left kidneys using TR550 (ms)",cisco$avt1med_tr550,0),
						expcont.row("Average of T1 cortex value<br>of right and left kidneys using TR1000 (ms)",cisco$avt1cort_tr1000,0),
						expcont.row("Average of T1 medulla value<br>of right and left kidneys using TR1000 (ms)",cisco$avt1med_tr1000,0),
						expcont.row("Average T1 corticomedullary differentiation<br>of right and left kidneys using TR550 (ms)",cisco$avt1diff_tr550,2),
						expcont.row("Average T1 corticomedullary differentiation<br>of right and left kidneys using TR1000 (ms)",cisco$avt1diff_tr1000,2),
						expcont.row("Average of T2 cortex value<br>of right and left kidneys (ms)",cisco$avt2cort,1),
						expcont.row("Average of T2 medulla value<br>of right and left kidneys (ms)",cisco$avt2med,1),
						expcont.row("Average of cortex apparent diffusion coefficient values<br>of right and left kidneys (mm<sup>2</sup>/s)",cisco$avcortadc,0),
						expcont.row("Average of medulla apparent diffusion coefficient values<br>of right and left kidneys (mm<sup>2</sup>/s)",cisco$avmedadc,0),
						expcont.row("Average volume<br>of right and left kidneys (ml)",cisco$avvol,0),

						row.title(bold("Clinical Question")),
						expnpc.row("Acute kidney injury (AKI) during covid-19 illness",cisco$aki,p1=F),

						end.row()))
		ExportTable.HTML(temp$head,temp$body,table.fmt,expanded.file,F,F,T)


	# SUPPLEMENTARY TABLE 3

		temp<-
			list(
				head=
					rbindTable(
						table.title("S3","Clinical outcomes. p-values from log rank test of time to first event."),
						cbindTable(
							"","",
							rbindTable("COVID-19",cbindTable("Without Primary<br>Outcome","With Primary<br>Outcome")),
							"Control","p-value<sup>(a)</sup>","p-value<sup>(b)</sup>",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper_v3=="No")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="Control")),")",sep=""),
							"","",
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Duration of Follow-up")),
						miqr.row(
							"Days to Visit 3 or death",
							pmin(
								as.numeric(cisco$v3_date-ifelse(cisco$group=="COVID-19",cisco$disch_date,cisco$v1_date)),
								as.numeric(cisco$death_date-ifelse(cisco$group=="COVID-19",cisco$disch_date,cisco$v1_date)),
								na.rm=T),
							0,showAll=T),

						row.title(italic("Outcomes")),
						npc.lr.row(
							"Death or Hospitalization (Any Cause)",cisco$death_hosp,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_hosp_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Death (Any Cause)",cisco$death,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Cardiovascular Death",cisco$death_cv,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Renal Death",cisco$death_renal,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T),
						npc.lr.row(
							"Respiratory Death",cisco$death_resp,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T),
						npc.lr.row(
							"Hospitalization (Any Cause)",cisco$any_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Cardiovascular Hospitalization",cisco$any_cv_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_cv_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Renal Hospitalization",cisco$any_renal_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_renal_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Respiratory Hospitalization",cisco$any_resp_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_resp_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),

						row.title(italic("Cardiovascular Outcomes")),
						npc.lr.row(
							"Myocardial infarction",cisco$mi_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$mi_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Percutaneous Coronary Intervention",cisco$pci_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pci_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Coronary Artery Bypass Grafting",cisco$cabg_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$cabg_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T),
						npc.lr.row(
							"Cerebrovascular accident",cisco$cva_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$cva_v3_date,cisco$death_date,paper=cisco$paper_v3,p2=F,showAll=T),
						npc.lr.row(
							"Heart Failure",cisco$hf_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$hf_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Deep vein thrombosis",cisco$dvt_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$dvt_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T),
						npc.lr.row(
							"New atrial fibrillation",cisco$newaf_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$newaf_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Ventricular tachycardia or fibrillation",cisco$vf_vt_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$vf_vt_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T),

						row.title(italic("Respiratory Outcomes")),
						npc.lr.row(
							"Pulmonary fibrosis",cisco$pulfib_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pulfib_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"New diagnosis asthma",cisco$newasthma_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$newasthma_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Pulmonary embolism",cisco$pte_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pte_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Long-term oxygen therapy",cisco$ltot_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$ltot_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),

						row.title(italic("Secondary Care (Outpatients)")),
						npc.lr.row(
							"Any Outpatient Referral",cisco$any_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Acute COVID-19 (&lt;28 days)",cisco$any_acute_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_acute_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Ongoing COVID-19 (28-84 days)",cisco$any_ongoing_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_ongoing_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Long COVID-19 (&gt;84 days)",cisco$any_long_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_long_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Cardiology",cisco$any_cardio_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_cardio_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Respiratory",cisco$any_resp_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_resp_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),
						npc.lr.row(
							"Physiotherapy",cisco$any_physio_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_physio_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T),

						row.title(italic("Medications")),
						npc.row("Aspirin",cisco$aspirin_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("P2Y12 inhibitor",cisco$p2y12_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Calcium channel blocker",cisco$ccb_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("ACE inhibitor",cisco$acei_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("ARB",cisco$arb_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Sacubitril/Valsartan",cisco$salval_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Statin",cisco$statin_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Mineralocorticoid receptor antagonist",cisco$mra_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Beta Blocker",cisco$bb_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Thiazide",cisco$thiazide_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Warfarin",cisco$warfarin_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Novel oral anticoagulant therapy",cisco$noac_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Inhaled Steroid",cisco$inh_steroid_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Inhaled Bronchodilator",cisco$inh_broncho_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Inhaled Anti-muscarinics",cisco$inh_antimusc_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("SGLT2i",cisco$sglt2i_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("GLP-1 agonists",cisco$glp1_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Insulin",cisco$insulin_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Other anti-diabetic",cisco$odiab_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Loop diuretic",cisco$loop_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Oral Steroid",cisco$oral_steroid_v3,"Yes",paper=cisco$paper_v3,showAll=T),
						npc.row("Antidepressant",cisco$antidep_v3,"Yes",paper=cisco$paper_v3,showAll=T),

						end.row(
							footnote=
								paste(
									"<sup>(a)</sup>: COVID-19 patients, without primary outcome vs. with primary outcome.",
									"<sup>(b)</sup>: COVID-19 patients with primary outcome vs. controls."))))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)

		temp<-
			list(
				head=
					rbindTable(
						table.title("S3 (Censored)","Clinical outcomes (censored at 12 months). p-values from log rank test of time to first event."),
						cbindTable(
							"","",
							rbindTable("COVID-19",cbindTable("Without Primary<br>Outcome","With Primary<br>Outcome")),
							"Control","p-value<sup>(a)</sup>","p-value<sup>(b)</sup>",
							rbindTable(list(Text="Adjudicated Myocarditis Diagnosis",Style=list("border-bottom"="solid windowtext 1pt")),cbindVector(levels(cisco$primary))),
							"p-value"),
						cbindTable(
							"","",
							paste("(N=",sum((cisco$paper_v3=="No")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="Control")),")",sep=""),
							"","",
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Not")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Unlikely")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Probably")),")",sep=""),
							paste("(N=",sum((cisco$paper_v3=="Yes")&(cisco$group=="COVID-19")&(cisco$primary=="Very")),")",sep=""),
							"")),
				body=
					rbindTable(
						row.title(italic("Duration of Follow-up")),
						miqr.row(
							"Days to Visit 3, death, or 1 year",
							pmin(
								as.numeric(cisco$v3_date-ifelse(cisco$group=="COVID-19",cisco$disch_date,cisco$v1_date)),
								as.numeric(cisco$death_date-ifelse(cisco$group=="COVID-19",cisco$disch_date,cisco$v1_date)),
								365,
								na.rm=T),
							0,showAll=T),

						row.title(italic("Outcomes")),
						npc.lr.row(
							"Death or Hospitalization (Any Cause)",cisco$death_hosp,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_hosp_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Death (Any Cause)",cisco$death,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Cardiovascular Death",cisco$death_cv,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Renal Death",cisco$death_renal,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T,Cens=365),
						npc.lr.row(
							"Respiratory Death",cisco$death_resp,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$death_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T,Cens=365),
						npc.lr.row(
							"Hospitalization (Any Cause)",cisco$any_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Cardiovascular Hospitalization",cisco$any_cv_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_cv_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Renal Hospitalization",cisco$any_renal_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_renal_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Respiratory Hospitalization",cisco$any_resp_hosp_fu,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_resp_hosp_fu_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),

						row.title(italic("Cardiovascular Outcomes")),
						npc.lr.row(
							"Myocardial infarction",cisco$mi_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$mi_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Percutaneous Coronary Intervention",cisco$pci_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pci_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Coronary Artery Bypass Grafting",cisco$cabg_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$cabg_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T,Cens=365),
						npc.lr.row(
							"Cerebrovascular accident",cisco$cva_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$cva_v3_date,cisco$death_date,paper=cisco$paper_v3,p2=F,showAll=T,Cens=365),
						npc.lr.row(
							"Heart Failure",cisco$hf_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$hf_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Deep vein thrombosis",cisco$dvt_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$dvt_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T,Cens=365),
						npc.lr.row(
							"New atrial fibrillation",cisco$newaf_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$newaf_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Ventricular tachycardia or fibrillation",cisco$vf_vt_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$vf_vt_v3_date,cisco$death_date,paper=cisco$paper_v3,p1=F,p2=F,showAll=T,Cens=365),

						row.title(italic("Respiratory Outcomes")),
						npc.lr.row(
							"Pulmonary fibrosis",cisco$pulfib_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pulfib_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"New diagnosis asthma",cisco$newasthma_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$newasthma_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Pulmonary embolism",cisco$pte_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$pte_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Long-term oxygen therapy",cisco$ltot_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$ltot_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),

						row.title(italic("Secondary Care (Outpatients)")),
						npc.lr.row(
							"Any Outpatient Referral",cisco$any_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Acute COVID-19 (&lt;28 days)",cisco$any_acute_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_acute_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Ongoing COVID-19 (28-84 days)",cisco$any_ongoing_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_ongoing_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Long COVID-19 (&gt;84 days)",cisco$any_long_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_long_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Cardiology",cisco$any_cardio_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_cardio_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Respiratory",cisco$any_resp_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_resp_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),
						npc.lr.row(
							"Physiotherapy",cisco$any_physio_op_v3,"Yes",
							cisco$disch_date,cisco$v1_date,cisco$v3_date,cisco$first_physio_op_v3_date,cisco$death_date,paper=cisco$paper_v3,showAll=T,Cens=365),

						end.row(
							footnote=
								paste(
									"<sup>(a)</sup>: COVID-19 patients, without primary outcome vs. with primary outcome.",
									"<sup>(b)</sup>: COVID-19 patients with primary outcome vs. controls."))))
		ExportTable.HTML(temp$head,temp$body,table.fmt,tables.file,F,F,T)


