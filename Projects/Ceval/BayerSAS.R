
# Preprecess -------------------------------------------------------------

# data eff1;
# 	set ads.adeff(where=(ANATYPE='原始数据'));
# 	AVISITN=1;output;
# 	if CGMTM le 1440 then do;AVISITN=2;output;end;
# 		else if CGMTM le 7200 then do;AVISITN=3;output;end;
# 		else if CGMTM le 14400 then do;AVISITN=4;output;end;
# 		else if CGMTM le 21600 then do;AVISITN=5;output;end;
# run;

# data eff2;
# 	set eff1;
# 	stvar=YSIRES;index=1;output;
# 	stvar=CGMRES;index=2;output;
# run;

# Statistics --------------------------------------------------------------

# /*计算定量指标*/
# proc means data=st n nmiss mean std median q1 q3 min max;
# 	by AVISITN index ;
# 	class CGMLOCN;
# 	var stvar;
# 	ods output Summary=_mean;
# run;
# data mean;
# 	set _mean;
# 	length nnmiss meandsd median q13 minmax $200;
# 	nnmiss=cats(stvar_N);/*,'(',stvar_NMiss,')');*/
# 	meandsd=cats(put(stvar_Mean,16.2),'(',put(stvar_StdDev,16.3),')');
# 	median=cats(put(stvar_Median,16.2));
# 	q13=cats(put(stvar_Q1,16.2),',',put(stvar_Q3,16.2));
# 	minmax=cats(put(stvar_Min,16.1),',',put(stvar_Max,16.1));
# 	label nnmiss='N' meandsd='Mean(SD)' median='Median' q13='Q1,Q3' minmax='Min,Max';
# run;

# MARD-MAD ----------------------------------------------------------------

# proc sql;
# 	create table _eff as select distinct AVISITN,CGMLOCN,mean(ARD) as MMARD,mean(AD) as MMAD
# 		from eff1 group by AVISITN,CGMLOCN;
# quit;
  
# data _eff_1;
# 	set _eff(where=(CGMLOCN=1));
# 	rename MMARD=ABMMARD;
# 	keep AVISITN MMARD;
# run;
 
# data _eff;
# 	merge _eff _eff_1;
# 	by AVISITN;
# 	if CGMLOCN ne 1 then CHG=MMARD-ABMMARD;
# run;

# data _eff2;
# 	set _eff;
# 	length stvar $200;
# 	stvar=cats(put(MMARD,16.1),'%');index=1;output;
# 	stvar=ifc(^missing(CHG),cats(put(CHG,16.1),'%'),'');index=2;output;
# 	stvar=cats(put(MMAD,16.2));index=3;output;
# run;

# CI(T-test) --------------------------------------------------------------

# data eff3;
# 	set eff1;
# 	stvar=ARD;index=1;output;
# 	stvar=AD;index=3;output;
# run;
# proc sort data= eff3;by AVISITN index CGMLOCN;run;

# proc ttest data=eff3 ;
# 	by AVISITN index CGMLOCN;
# 	var stvar;
# 	ods output ConfLimits=cl;
# run;

# data cl2;
# 	set cl;
# 	length stvar $200;
# 	if index=1 then stvar=cats(put(LowerCLMean,16.1),'%,',put(UpperCLMean,16.1),'%');
# 		else stvar=cats(put(LowerCLMean,16.2),',',put(UpperCLMean,16.2));
# run;

# Adjust Formats ----------------------------------------------------------

# proc transpose data=mean out=rst1 prefix=col;
# 	by AVISITN index;
# 	id CGMLOCN;
# 	var nnmiss meandsd median q13 minmax;
# run;

# data _final1;
# 	length var para col1 col2 col3 $200;
# 	set rst2(in=a) rst3(in=b);
# 	by AVISITN index;
# 	if a then do;
# 		if index=1 then para='MARD\super 1\nosupersub';
# 		if index=2 then para='MARD差值\super 2\nosupersub';
# 		if index=3 then para='MAD(mM)\super 3\nosupersub';
# 	end;
# 	else do;	
# 		if index=1 then para='95% CI';
# 		if index=3 then para='95% CI';
# 	end;
# 	var='';
# 	output;
# 	if  para='MARD差值\super 2\nosupersub' then do;
# 		call missing(var,para,col1,col2,col3);output;
# 	end;
# 	keep var--col3 index AVISITN;
 
# data _final1_2;
# 	set _final1;
# 	index=3;
# 	output;
# 	if para='MAD(mM)\super 3\nosupersub' then do;
# 		para='MAD(mg/dL)';
# 		col1=strip(put(input(col1,best.)*18,8.1));
# 		col2=strip(put(input(col2,best.)*18,8.1));
# 		col3=strip(put(input(col3,best.)*18,8.1));
# 		output;
# 	end;
# run;

# data final;
# 	length visit $200;
# 	set _final1_2 rst1(in=a);
# 	by AVISITN index;
# 	if a then do;
# 		if index=1 then var='YSI(mg/dL)';
# 		if index=2 then var='CGM(mg/dL)';
# 		para=_LABEL_;
# 	end;
# 	if ^first.index then call missing(var);

# 	array vst{5} $200 ('合计','第1天访视','第5天访视','第10天访视','第15天访视');
# 	if first.avisitn then do;
# 		do i=1 to 5;
# 			if avisitn=i then visit=vst(i);
# 		end;
# 	end;
# 	keep visit var--col3;
# 	output;
# 	if last.index then do;call missing(visit,var,para,col1,col2,col3);output;end;
# run;