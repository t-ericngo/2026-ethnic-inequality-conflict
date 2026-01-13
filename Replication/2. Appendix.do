
cd "C:\Users\anhtu\OneDrive\[Eric Ngo_UIowa]\[Research Projects]\Ngo 2024 Ethnic Inequality_Diversion\Research Paper - Analysis Project"


*import delimited "C:\Users\anhtu\OneDrive\[Eric Ngo_UIowa]\[Research Projects]\Ngo 2024 Ethnic Inequality_Diversion\Research Paper - Data\Building Data_Ngo\DataCreation\ngo2024ethnic_final.csv", clear 
*(encoding automatically selected: ISO-8859-1)
*(236 vars, 163,276 obs)


* use "ngo2024ethnic_final.dta", clear // this one is NOT xtset, use "ngo2024ethnic_final_xtset.dta" for clarity
*use "ngo2024ethnic_final.dta", clear

use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data
xtset

//merge m:1 gwcode1 year using epr_bgi_measures.dta, nogen

sort dirdyadid year

save ngo2024ethnic_final_xtset2.dta, replace

browse if cowmidinit==1 & l.lexclpop>0.25 & l.munr>10 
browse if ccode1==551 & cowmidinit==1

***** Notable cases
*** Morocco (600) in 1960s
// High exclpop (0.4) + Political tensions
// MID initiation vs. Algeria (1962) --> Sand War (1963)
browse if ccode1==600

*** Iraq (645) in 1960s & perhaps in 1991 as well
// Moderate exclpop (0.17) + Political tensions
// MID initiation vs. Kuwait (1961) 
browse if ccode1==645




****************************************************
*****       Appendix. Descriptive Stats        *****
****************************************************
help dtable

logit cowmidinit l.lexclpop l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)


***** Descriptive Stats Table
dtable i.cowmidinit lexclpop munr i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell if e(sample), varlab continuous(, statistics(count min max mean sd p1 p25 median p75 p99)) factor(, statistics(fvfrequency fvpercent)) //export(tableA1_final.xlsx) replace


* match labels from original poster
collect label levels result ///
    count "Obs" fvfrequency "Freq." fvpercent "Percentage" min "Min" max "Max" mean "Mean" sd "Std.Dev." p1 "P1" p25 "P25" median "P50(Med)" p75 "P75" p99 "P99" ///
    , modify

* show variable names instead of labels
collect style header var, level(value)

* split statistics into their own columns
collect layout (var) (result[count fvfrequency fvpercent min max mean sd p1 p25 median p75 p99])

* export to MS Word document
collect export tableA1_final.docx, replace


table (cowmidinit lexclpop munr ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell), stat(min max mean sd )





***** Correlation Matrix
correlate cowmidinit lexclpop ethfrac1 if e(sample)




****************************************************
*****    Appendix. REs for Domestic Unrest     *****
****************************************************

*** B1: 2-way Interaction ***
xtlogit cowmidinit c.l.lexclpop##c.l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store B1_2way_lmunr

* Coefplot B1
coefplot, drop(_cons) order(l.exclpop lnmunr c.exclpop#c.lnmunr ethnicfrac apwr adep sglo othdisp polity1 polity2 dist cwpceyrs pceyrs2 pceyrs3) ///
	coeflabels(l.exclpop = "% Excluded Pop." lnmunr = "Mass Unrest (log & lag)" c.l.exclpop#c.lnmunr = "Excluded Pop*Mass Unrest" ethnicfrac = "Ethnic Frac." apwr = "Relative Capabilities (Side A)" adep = "Trade Dependence (Side A)" sglo = "Alliance Similarity" othdisp = "Other Dispute" polity1 = "Polity Score (Side A)" polity2 = "Polity Score (Side B)" dist = "Distance" cwpceyrs = "Peace Yrs" pceyrs2 = "Peace Yrs^2" pceyrs3 = "Peace Yrs^3") ///
	xlabel(-1.5(0.5)1) xline(0) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick)) msymbol(circle) msize(medsmall)  

*  Interaction plot B1
set scheme cleanplots
summarize exclpop lnmunr, detail
summarize l.lexclpop l.munr, detail
hist munr
/*
graph box lnmunr
pctile pct_lnmunr = lnmunr, nq(100) genp(percent_lnmunr)
list percent_lnmunr pct_lnmunr in 1/100
*/
/*
pctile pct_exclpop = exclpop, nq(100) genp(percent_exclpop)
list percent_exclpop pct_exclpop in 1/100
*/
set level 90
summarize l.munr, detail
margins, at(c.l.lexclpop = (0(0.2)0.8) c.l.munr = (0 15)) // lnmunr{0 = 0-55th percentile; 1.61 = 85th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
_marg_save, saving(final_B3_2way_int, replace)

use final_B3_2way_int, clear
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	title("Figure B2A. Interaction Effects of Excluded Ethnic Pop. & Mass Unrest on MID Initiation") ///
	xtitle("% Excluded Ethnic Pop.") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(1) position(6))


	
* Interaction marginsplot dydx (Marginal Effects) B1
margins, dydx(l.lexclpop) at(l.munr=(0(5)35))
_marg_save, saving(final_B3_2way_dydx_lexclpop, replace)

use final_B3_2way_dydx_lexclpop, clear
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) ylabel(0(0.005)0.04) title("Figure B2B. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Increasing Mass Unrest") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l.munr) at(l.lexclpop=(0(0.2)0.8))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure A2C. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Based on Increasing Ethnic Political Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 

summarize exclpop lnmunr, detail


*** Model B2: excluded pop & ongoing rivalry & domestic turmoil (Significant) ***
xtlogit cowmidinit c.l.lexclpop##c.l.munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store B2_3way_rivalnew

summarize munr_ma5 lnmunr_ma5 lnmunr_ma3, detail


* Marginsplot: Interactive effect B4
margins, at(c.l.lexclpop = (0(0.2)0.8) c.l.munr = (0 9) kinexist = (0 1))
marginsplot, recastci(rarea) ciopt(color(gray%20)) ///
	title("Figure 5A. Interactive Effects of Ethnic Inequality, Mass Unrest, & Kin Ties on Conflict Initiation") ///
	xtitle("% Excluded Ethnic Population") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(2) position(6))
	
* Interaction marginsplot dydx (Marginal Effects) B4
set scheme cleanplots
set level 95
margins, dydx(l.lexclpop) at(l.munr=(0(5)35) ongoingrivalry_update = (0 1))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure 5B. Avg. Marginal Effect of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Mass Unrest & Kin Ties") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Lagged Mass Unrest (log)") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))

margins, dydx(l.munr) at(l.lexclpop=(0(0.2)0.8) ongoingrivalry_update = (0 1))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure 5C. Avg. Marginal Effect of Mass Unrest on MID Initiation") subtitle("Based on Ethnic Pol. Inequality & Kin Ties") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("% Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))

margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8) l.munr=(0 15))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure 5D. Avg. Marginal Effect of Kin Ties on MID Initiation") subtitle("Based on Mass Unrest & Ethnic Pol. Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("% Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))


** 3-way visualization B2
margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_B4_3way_rivalnew_dydx, replace)

use final_B4_3way_rivalnew_dydx, clear
twoway contour _margin _at1 _at2, ccut(0.012(.004)0.072)  ///
	title("Figure B3. Marginal Effects of Ongoing Rivalry on MID Initiation (REs)") ///
	subtitle("Based on Ethnic Pol. Inequality & Mass Unrest") ///
	xtitle("Mass Unrest") ///
	ytitle("Share of Excluded Ethnic Pop.") ///
	ztitle(Effect of Ongoing Rivalry) ///
	xlabel(0(5)35) ylabel(0(0.1)0.8) ///
	crule(linear) scolor(white) ecolor(gs1) ///
	text(0.22 6 "0.02 - 0.024", place(e) size(vsmall)) ///
	text(0.1 16 "0.016 - 0.02", place(e) size(vsmall)) ///
	text(0.05 27 "0.012 - 0.016", place(e) size(vsmall)) ///
	text(0.42 6 "0.024 - 0.028", place(e) size(vsmall)) ///
	text(0.5 11 "0.028 - 0.032", place(e) size(vsmall)) ///
	text(0.57 15.5 "0.032 - 0.036", place(e) size(vsmall)) ///
	text(0.62 18.2 "0.036 - 0.04", place(e) size(vsmall))
	





	
*** Table B1 
outreg2 [B1_2way_lmunr B2_3way_rivalnew] using tableB2.doc, label e(all) replace dec(3)





**_____________________________________________

****************************************************
*****       Appendix. Rare Events Logit        *****
****************************************************
use ngo2024ethnic_final_xtset2, clear

help relogit  // Program and method by King & Zeng (1999)
help firthlogit  // Program by Heinze & Schemper (2002); method by Firth (1993)

gen l_lexclpop = l.lexclpop
gen l_munr = l.munr


*** Model C1: Ineq*Mass Unrest ***
// gen l_lexclpopXl_munr = l_lexclpop*l_munr
// relogit cowmidinit l_lexclpop l_munr l_lexclpopXl_munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, cluster(dirdyadid)
// estimates store C3A_2way_lmunr

firthlogit cowmidinit c.l_lexclpop##c.l_munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3
estimates store C1B_2way_lmunr

* Coefplot C1
coefplot, drop(_cons) order(l.exclpop lnmunr c.exclpop#c.lnmunr ethnicfrac apwr adep sglo othdisp polity1 polity2 dist cwpceyrs pceyrs2 pceyrs3) ///
	coeflabels(l.exclpop = "% Excluded Pop." lnmunr = "Mass Unrest (log & lag)" c.l.exclpop#c.lnmunr = "Excluded Pop*Mass Unrest" ethnicfrac = "Ethnic Frac." apwr = "Relative Capabilities (Side A)" adep = "Trade Dependence (Side A)" sglo = "Alliance Similarity" othdisp = "Other Dispute" polity1 = "Polity Score (Side A)" polity2 = "Polity Score (Side B)" dist = "Distance" cwpceyrs = "Peace Yrs" pceyrs2 = "Peace Yrs^2" pceyrs3 = "Peace Yrs^3") ///
	xlabel(-1.5(0.5)1) xline(0) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick)) msymbol(circle) msize(medsmall)  

*  Interaction plot C1
set scheme cleanplots
summarize exclpop lnmunr, detail
summarize l.lexclpop l.munr, detail
hist munr
/*
graph box lnmunr
pctile pct_lnmunr = lnmunr, nq(100) genp(percent_lnmunr)
list percent_lnmunr pct_lnmunr in 1/100
*/
/*
pctile pct_exclpop = exclpop, nq(100) genp(percent_exclpop)
list percent_exclpop pct_exclpop in 1/100
*/
set level 90
summarize l.munr, detail
margins, at(l_lexclpop = (0(0.2)0.8) l_munr = (0 15)) expression(invlogit(predict(xb))) // lnmunr{0 = 0-55th percentile; 1.61 = 85th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	title("Figure C1. Interaction Effects of Excluded Ethnic Pop. & Mass Unrest on MID Initiation") ///
	subtitle("Penalized MLE (Firth Estimator)") ///
	xtitle("% Excluded Ethnic Pop.") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(1) position(6))


	
* Interaction marginsplot dydx (Marginal Effects) C1
margins, dydx(l_lexclpop) at(l_munr=(0(5)35)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure C1A. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Penalized MLE (Firth Estimator)") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l_munr) at(l_lexclpop=(0(0.2)0.8)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure C1B. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Penalized MLE (Firth Estimator)") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 




*** Model C2: excluded pop & ongoing rivalry & domestic turmoil (Significant) ***
gen l_munrXrivalry = l_munr*ongoingrivalry_update
gen l_lexclpopXl_munrXrivalry = l_lexclpop*l_munr*ongoingrivalry_update

// relogit cowmidinit l_lexclpop l_munr ongoingrivalry_update l_lexclpopXl_munr l_lexclpopXrivalry l_munrXrivalry l_lexclpopXl_munrXrivalry ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, cluster(dirdyadid)
// estimates store C4A_3way_rivalnew


firthlogit cowmidinit c.l_lexclpop##c.l_munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3
estimates store C2B_3way_rivalnew

summarize l.munr munr_ma5, detail


* Marginsplot: Interactive effect C2
margins, at(c.l_lexclpop = (0(0.2)0.8) c.l_munr = (0 15) ongoingrivalry_update = (0 1)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) ciopt(color(gray%20)) ///
	title("Interactive Effects of Ethnic Inequality, Mass Unrest, & Kin Ties on Conflict Initiation") ///
	xtitle("% Excluded Ethnic Population") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(2) position(6))
	
* Interaction marginsplot dydx (Marginal Effects) C2
set scheme cleanplots
set level 90

margins, dydx(ongoingrivalry_update) at(l_lexclpop=(0(0.2)0.8) l_munr=(0 15)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure C2A. Avg. Marginal Effect of Rivalry on MID Initiation") subtitle("Penalized MLE (Firth Estimator)") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("% Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))

margins, dydx(l_lexclpop) at(l_munr=(0(5)35) ongoingrivalry_update = (0 1)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure C2B. Avg. Marginal Effect of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Mass Unrest & Kin Ties") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Lagged Mass Unrest (log)") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))

margins, dydx(l_munr) at(l_lexclpop=(0(0.2)0.8) ongoingrivalry_update = (0 1)) expression(invlogit(predict(xb)))
marginsplot, recastci(rarea) plotopt(color(red%75)) plot2opt(color(blue%75)) ciopt(color(gray%20)) yline(0) title("Figure C2C. Avg. Marginal Effect of Mass Unrest on MID Initiation") subtitle("Based on Ethnic Pol. Inequality & Kin Ties") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("% Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) legend(rows(1) position(6))



** 3-way visualization C2
margins, dydx(ongoingrivalry_update) at(l_lexclpop=(0(0.2)0.8) l_munr=(0(5)35)) expression(invlogit(predict(xb)))
_marg_save, saving(final_C2_3way_dydx_contour, replace)

use final_C2_3way_dydx_contour, clear
twoway contour _margin _at1 _at2, ccut(0.012(.003)0.045)  ///
	title("Figure C2. Marginal Effects of Ongoing Rivalry on MID Initiation") ///
	subtitle("Penalized MLE (Firth Estimator)") ///
	xtitle("Mass Unrest") ///
	ytitle("Share of Excluded Ethnic Pop.") ///
	ztitle(Effect of Ongoing Rivalry) ///
	xlabel(0(5)35) ylabel(0(0.1)0.8) ///
	crule(linear) scolor(white) ecolor(gs1) 
	

	

*** Inflation: 3-way Interaction (Rivalry & EGIP1-EXCL2) (Baseline: 0)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

firthlogit cowmidinit c.l_lexclpop##c.l_munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3
estimates store C3A_3way_egip1excl2_base0


*** Baseline 0, Cat 1
margins, dydx(1.orival_egip1excl2) at(l_lexclpop=(0(0.2)0.8) l_munr=(0(5)35)) expression(invlogit(predict(xb)))
_marg_save, saving(final_C3A_3way_regip1excl2_dydx_base0cat1, replace)

use final_C3A_3way_regip1excl2_dydx_base0cat1, clear
twoway contour _margin _at1 _at2, ccut(0.013 0.015(.0025)0.035)  /// 
	title("Figure C3A. Marginal Effects of Other Rivals (Who Do Not Marginalize State A's EGIPs)", size(medsmall)) ///
	subtitle("Compared to Non-Rivals") ///
	subtitle("Penalized MLE (Firth Estimator)") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Other Rivals (A's EGIPs Not Excluded)") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0 0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 


*** Baseline 0, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)

margins, dydx(2.orival_egip1excl2) at(l_lexclpop=(0(0.2)0.8) l_munr=(0(5)35)) expression(invlogit(predict(xb)))
_marg_save, saving(final_C3B_3way_regip1excl2_dydx_base0cat2, replace)

use final_C3B_3way_regip1excl2_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(0(0.05)0.1 0.2(0.1)0.9 0.95)  /// 
	title("Figure C3B. Marginal Effects of Rivals Who Marginalize State A's EGIP", size(medsmall)) ///
	subtitle("Penalized MLE (Firth Estimator)") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivals Who Marginalize State A's EGIP") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2)

	
*** Inflation: 3-way Interaction (Rivalry EGIP1-EXCL2) (Baseline: 1)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

*Baseline 1, Cat 2
firthlogit cowmidinit c.l_lexclpop##c.l_munr##ib(1).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3
estimates store C3B_3way_egip1excl2_base1

margins, dydx(2.orival_egip1excl2) at(l_lexclpop=(0(0.2)0.8) l_munr=(0(5)35)) expression(invlogit(predict(xb)))
_marg_save, saving(final_C3C_3way_regip1excl2_dydx_base0cat2, replace)

use final_C3C_3way_regip1excl2_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(-0.015 0(0.05)0.1 0.2(.1)0.9)  /// 
	title("Figure E3C. Marginal Effects of Rivalry Who Marginalizes State A's EGIPs", size(medsmall)) ///
	subtitle("Compared to Other Rivals (State A's EGIP Not Excluded)") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rival Who Marginalizes State A's EGIPs") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2)


	


***** Table C *****
outreg2 [C1A_noint C1B_noint C2A_2way_rivalnew C2B_2way_rivalnew C3A_2way_lmunr C3B_2way_lmunr C4A_3way_rivalnew C4B_3way_rivalnew] using tableC1.doc, label e(all) replace dec(3)

esttab C1B_2way_lmunr C2B_3way_rivalnew C3A_3way_egip1excl2_base0 C3B_3way_egip1excl2_base1 using tableC1.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll r2_p chi2) /// 
	nobaselevels ///
	title("Table C1. Robustness Test: Rare Events Logit (Firth Estimator))") ///
	mtitles("Model C1" "Model C2" "Model C3A (Baseline: Non-Rival)" "Model C3B (Baseline: Other Rivals)") ///
	order(l_lexclpop l_munr c.l_lexclpop#c.l_munr 1.ongoingrivalry_update 1.ongoingrivalry_update#c.l_lexclpop 1.ongoingrivalry_update#c.l_munr 1.ongoingrivalry_update#c.l_lexclpop#c.l_munr 0.orival_egip1excl2 1.orival_egip1excl2 2.orival_egip1excl2 0.orival_egip1excl2#c.l_lexclpop 0.orival_egip1excl2#c.l_munr 1.orival_egip1excl2#c.l_lexclpop 1.orival_egip1excl2#c.l_munr 2.orival_egip1excl2#c.l_lexclpop 2.orival_egip1excl2#c.l_munr 0.orival_egip1excl2#c.l_lexclpop#c.l_munr 1.orival_egip1excl2#c.l_lexclpop#c.l_munr 2.orival_egip1excl2#c.l_lexclpop#c.l_munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(l_lexclpop "Ethnic Exclusion" l_munr "Mass Unrest" c.l_lexclpop#c.l_munr "Ethnic Exclusion * Mass Unrest" 1.ongoingrivalry_update "Rivalry" 1.ongoingrivalry_update#c.l_lexclpop "Rivalry * Ethnic Exclusion" 1.ongoingrivalry_update#c.l_munr "Rivalry * Mass Unrest" 1.ongoingrivalry_update#c.l_lexclpop#c.l_munr "Rivalry * Exclusion * Unrest" 0.orival_egip1excl2 "Non-Rival" 1.orival_egip1excl2 "Rival (Other)" 2.orival_egip1excl2 "Rival (EGIPA Marginalized)" 0.orival_egip1excl2#c.l_lexclpop "Non-Rival * Exclusion"0.orival_egip1excl2#c.l_munr "Non-Rival * Unrest" 1.orival_egip1excl2#c.l_lexclpop "Rival (Other) * Exclusion" 1.orival_egip1excl2#c.l_munr "Rival (Other) * Unrest" 2.orival_egip1excl2#c.l_lexclpop "Rival (EGIPA Marginalized) * Exclusion" 2.orival_egip1excl2#c.l_munr "Rival (EGIPA Marginalized) * Unrest" 0.orival_egip1excl2#c.l_lexclpop#c.l_munr "Non-Rival * Exclusion * Unrest" 1.orival_egip1excl2#c.l_lexclpop#c.l_munr "Rival (Other) * Exclusion * Unrest" 2.orival_egip1excl2#c.l_lexclpop#c.l_munr "Rival (EGIPA Marginalized) * Exclusion * Unrest" ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" polity21 "Polity Score A" polity22 "Polity Score B" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3" _cons "Constant") ///
	lines ///
	replace










	   
*************************************************************
*****      Appendix. Joint Dem (Instead of Polity)      *****
*************************************************************

use ngo2024ethnic_final_xtset2, clear

*** Inflation: 2-Way Interaction
//logit cowmidinit c.l.lexclpop##c.l.d.inflation_cpi2010 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3 if e(sample), vce(cluster dirdyadid)

logit cowmidinit c.l.lexclpop##c.l.munr ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store D1_2way_lmunr


* Interaction marginsplot dydx (Marginal Effects)
margins, dydx(l.lexclpop) at(l.munr=(0(5)35))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure D1A. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Increasing Mass Unrest") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l.munr) at(l.lexclpop=(0(0.2)0.8))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure D1B. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Based on Increasing Ethnic Exclusion") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 



*** Inflation: 3-way Interaction (Rivalry)

use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store D2_3way_rival

margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_D2_3way_rival_egip1excl2_dydx, replace)

use final_D2_3way_rival_egip1excl2_dydx, clear
twoway contour _margin _at1 _at2, ccut(0.01(.0025)0.035)  ///
	title("Figure D2. Marginal Effects of Rivalry on MID Initiation") ///
	subtitle("Compared to Non-Rival Target") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivalry") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
	
*** Inflation: 3-way Interaction (Rivalry & EGIP1-EXCL2) (Baseline: 0)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store D3A_3way_egip1excl2_base0

*** Baseline 0, Cat 1
margins, dydx(1.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_D3A_3way_regip1excl2_dydx_base0cat1, replace)

use final_D3A_3way_regip1excl2_dydx_base0cat1, clear
twoway contour _margin _at1 _at2, ccut(0.01(0.002)0.026)  /// 
	title("Figure D3A. Marginal Effects of Other Rivals (Who Do Not Marginalize State A's EGIPs)", size(medsmall)) ///
	subtitle("Compared to Non-Rivals") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Other Rivals (A's EGIPs Not Excluded)") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 

*** Baseline 0, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)

margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_D3B_3way_regip1excl2_dydx_base0cat2, replace)

use final_D3B_3way_regip1excl2_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(0(0.05)0.1 0.2(0.1)0.9 0.95)  /// 
	title("Figure D3B. Marginal Effects of Rivals Who Marginalize State A's EGIP", size(medsmall)) ///
	subtitle("Compared to Non-Rivals") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivals Who Marginalize State A's EGIP") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
*** Inflation: 3-way Interaction (Rivalry EGIP1-EXCL2) (Baseline: 1)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

*Baseline 1, Cat 2
logit cowmidinit c.l.lexclpop##c.l.munr##ib(1).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store D3B_3way_egip1excl2_base1

margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_D3C_3way_regip1excl2_dydx_base0cat2, replace)

use final_D3C_3way_regip1excl2_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(-0.01 0 0.025 0.1 0.2(.1)0.9 0.94)  /// 
	title("Figure D3C. Marginal Effects of Rivalry Who Marginalizes State A's EGIPs", size(medsmall)) ///
	subtitle("Compared to Other Rivals (State A's EGIP Not Excluded)") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rival Who Marginalizes State A's EGIPs") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2)

	
	

***** Table D *****
esttab D1_2way_lmunr D2_3way_rival D3A_3way_egip1excl2_base0 D3B_3way_egip1excl2_base1 using tableD1.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll r2_p chi2) /// 
	nobaselevels ///
	title("Table D1. Robustness Test: Joint Democracy (Instead of Polity Scores for Each State)") ///
	mtitles("Model D1" "Model D2" "Model D3A (Baseline: Non-Rival)" "Model D3B (Baseline: Other Rivals)") ///
	order(L.lexclpop L.munr cL.lexclpop#cL.munr 1.ongoingrivalry_update 1.ongoingrivalry_update#cL.lexclpop 1.ongoingrivalry_update#cL.munr 1.ongoingrivalry_update#cL.lexclpop#cL.munr 0.orival_egip1excl2 1.orival_egip1excl2 2.orival_egip1excl2 0.orival_egip1excl2#cL.lexclpop 0.orival_egip1excl2#cL.munr 1.orival_egip1excl2#cL.lexclpop 1.orival_egip1excl2#cL.munr 2.orival_egip1excl2#cL.lexclpop 2.orival_egip1excl2#cL.munr 0.orival_egip1excl2#cL.lexclpop#cL.munr 1.orival_egip1excl2#cL.lexclpop#cL.munr 2.orival_egip1excl2#cL.lexclpop#cL.munr ethfrac1 relpow_a tradevol srsvaa jointdem mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(L.lexclpop "Ethnic Exclusion" L.munr "Mass Unrest" cL.lexclpop#cL.munr "Ethnic Exclusion * Mass Unrest" 1.ongoingrivalry_update "Rivalry" 1.ongoingrivalry_update#cL.lexclpop "Rivalry * Ethnic Exclusion" 1.ongoingrivalry_update#cL.munr "Rivalry * Mass Unrest" 1.ongoingrivalry_update#cL.lexclpop#cL.munr "Rivalry * Exclusion * Unrest" 0.orival_egip1excl2 "Non-Rival" 1.orival_egip1excl2 "Rival (Other)" 2.orival_egip1excl2 "Rival (EGIPA Marginalized)" 0.orival_egip1excl2#cL.lexclpop "Non-Rival * Exclusion"0.orival_egip1excl2#cL.munr "Non-Rival * Unrest" 1.orival_egip1excl2#cL.lexclpop "Rival (Other) * Exclusion" 1.orival_egip1excl2#cL.munr "Rival (Other) * Unrest" 2.orival_egip1excl2#cL.lexclpop "Rival (EGIPA Marginalized) * Exclusion" 2.orival_egip1excl2#cL.munr "Rival (EGIPA Marginalized) * Unrest" 0.orival_egip1excl2#cL.lexclpop#cL.munr "Non-Rival * Exclusion * Unrest" 1.orival_egip1excl2#cL.lexclpop#cL.munr "Rival (Other) * Exclusion * Unrest" 2.orival_egip1excl2#cL.lexclpop#cL.munr "Rival (EGIPA Marginalized) * Exclusion * Unrest" ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" jointdem "Joint Democracy" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3" _cons "Constant") ///
	lines ///
	replace

	   
	   
   
   


*************************************************************
***** Appendix. Domestic Crisis (Inflation, Pct Change) *****
*************************************************************
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data
 

set scheme cleanplots
// *** Creating annual percent change in inflation
// generate cpi_pctchange = ((inflation_cpi2010 - l.inflation_cpi2010)/l.inflation_cpi2010)
// 
//
// save ngo2024ethnic_final_xtset2.dta, replace

 
summ l.inflation_cpi2010 l.d.inflation_cpi2010 l.inflation_cpi2010_rate l.cpi_pctchange l2. cpi_pctchange, detail
summ l.munr, detail
histogram cpi_pctchange if cpi_pctchange>5
twoway scatter l.inflation_cpi2010 year, xlab(1945(5)2015)
help missing
misstable summarize inflation_cpi2010 if year > 1959


quietly: logit cowmidinit c.l.lexclpop##l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid) // Simply running this to get the sample


*** Inflation: 2-Way Interaction
//logit cowmidinit c.l.lexclpop##c.l.d.inflation_cpi2010 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3 if e(sample), vce(cluster dirdyadid)

logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store E1_2way_lmunr


* Predicted Prob Plot
margins, at(c.l.lexclpop = (0(0.2)0.8) c.l.cpi_pctchange = (-0.1 50 )) // lnmunr{0 = 0-55th percentile; 1.61 = 85th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	title("Figure E1. Interaction Effects of Ethnic Exclusion & Inflation on MID Initiation") ///
	xtitle("Share of Excluded Ethnic Pop.") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(1) position(6))


* Interaction marginsplot dydx (Marginal Effects)
margins, dydx(l.lexclpop) at(l.cpi_pctchange=(-0.1 0(0.5)2.5))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure E1A. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Increasing Inflation") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("CPI (Percent Change)") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l.cpi_pctchange) at(l.lexclpop=(0(0.2)0.8))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure E1B. Marginal Effects of Inflation on MID Initiation") subtitle("Based on Increasing Ethnic Exclusion") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 



*** Inflation: 3-way Interaction (Rivalry)

** cpi (pct change)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store E2_3way_rival

margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8) l.cpi_pctchange=(-0.1 0(0.5)2.5))
_marg_save, saving(final_E2_3way_rival_egip1excl2_cpipct_dydx, replace)

use final_E2_3way_rival_egip1excl2_cpipct_dydx, clear
twoway contour _margin _at1 _at2, ccut(0.008 0.01(.002)0.02)  ///
	title("Figure E2. Marginal Effects of Rivalry on MID Initiation") ///
	subtitle("Compared to Non-Rival Target") ///
	xtitle("CPI (Percent Change)") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivalry") ///
	ylabel(0(0.1)0.8) ///
	xlabel(-0.1 0(0.5)2.5) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
	
*** Inflation: 3-way Interaction (Rivalry & EGIP1-EXCL2) (Baseline: 0)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store E3A_3way_egip1excl2_base0

*** Baseline 0, Cat 1
margins, dydx(1.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.cpi_pctchange=(-0.1 0(0.5)2.5))
_marg_save, saving(final_E3A_3way_regip1excl2_cpipct_dydx_base0cat1, replace)

use final_E3A_3way_regip1excl2_cpipct_dydx_base0cat1, clear
twoway contour _margin _at1 _at2, ccut(0.006(.002)0.014 0.016)  /// 
	title("Figure E3A. Marginal Effects of Other Rivals (Who Do Not Marginalize State A's EGIPs)", size(medsmall)) ///
	subtitle("Compared to Non-Rivals") ///
	xtitle("CPI (Percent Change)") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Other Rivals (A's EGIPs Not Excluded)") ///
	ylabel(0(0.1)0.8) ///
	xlabel(-0.1 0(0.5)2.5) ///
	crule(linear) scolor(white) ecolor(gs2) 

*** Baseline 0, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)

margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.cpi_pctchange=(-0.1 0(0.5)2.5))
_marg_save, saving(final_E3B_3way_regip1excl2_cpipct_dydx_base0cat2, replace)

use final_E3B_3way_regip1excl2_cpipct_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(0(0.025)0.1 0.15(0.05)0.4)  /// 
	title("Figure E3B. Marginal Effects of Rivals Who Marginalize State A's EGIP", size(medsmall)) ///
	subtitle("Compared to Non-Rivals") ///
	xtitle("CPI (Percent Change)") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivals Who Marginalize State A's EGIP") ///
	ylabel(0(0.1)0.8) ///
	xlabel(-0.1 0(0.5)2.5) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
*** Inflation: 3-way Interaction (Rivalry EGIP1-EXCL2) (Baseline: 1)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

*Baseline 1, Cat 2
logit cowmidinit c.l.lexclpop##c.l.cpi_pctchange##ib(1).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store E3B_3way_egip1excl2_base1

margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.2)0.8) l.cpi_pctchange=(-0.1 0(0.5)2.5))
_marg_save, saving(final_E3C_3way_regip1excl2_cpipct_dydx_base0cat2, replace)

use final_E3C_3way_regip1excl2_cpipct_dydx_base0cat2, clear
twoway contour _margin _at1 _at2, ccut(0(0.02)0.06 0.1(.05)0.4)  /// 
	title("Figure E3C. Marginal Effects of Rivalry Who Marginalizes State A's EGIPs", size(medsmall)) ///
	subtitle("Compared to Other Rivals (State A's EGIP Not Excluded)") ///
	xtitle("CPI (Percent Change)") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rival Who Marginalizes State A's EGIPs") ///
	ylabel(0(0.1)0.8) ///
	xlabel(-0.1 0(0.5)2.5) ///
	crule(linear) scolor(white) ecolor(gs2)

	
*** Table E1 ***
help esttab
esttab E1_2way_lmunr E2_3way_rival E3A_3way_egip1excl2_base0 E3B_3way_egip1excl2_base1 using tableE1.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll r2_p chi2) /// 
	nobaselevels ///
	title("Table E1. Interaction of Ethnic Exclusion, Inflation, and Rival-Ethnic Combination ") ///
	mtitles("Model E1" "Model E2" "Model E3A (Baseline: Non-Rival)" "Model E3B (Baseline: Other Rivals)") ///
	order(L.lexclpop L.cpi_pctchange cL.lexclpop#cL.cpi_pctchange 1.ongoingrivalry_update 1.ongoingrivalry_update#cL.lexclpop 1.ongoingrivalry_update#cL.cpi_pctchange 1.ongoingrivalry_update#cL.lexclpop#cL.cpi_pctchange 0.orival_egip1excl2 1.orival_egip1excl2 2.orival_egip1excl2 0.orival_egip1excl2#cL.lexclpop 0.orival_egip1excl2#cL.cpi_pctchange 1.orival_egip1excl2#cL.lexclpop 1.orival_egip1excl2#cL.cpi_pctchange 2.orival_egip1excl2#cL.lexclpop 2.orival_egip1excl2#cL.cpi_pctchange 0.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange 1.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange 2.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(L.lexclpop "Ethnic Exclusion" L.cpi_pctchange "Inflation" cL.lexclpop#cL.cpi_pctchange "Ethnic Exclusion * Inflation" 1.ongoingrivalry_update "Rivalry" 1.ongoingrivalry_update#cL.lexclpop "Rivalry * Ethnic Exclusion" 1.ongoingrivalry_update#cL.cpi_pctchange "Rivalry * Inflation" 1.ongoingrivalry_update#cL.lexclpop#cL.cpi_pctchange "Rivalry * Exclusion * Inflation" 0.orival_egip1excl2 "Non-Rival" 1.orival_egip1excl2 "Rival (Other)" 2.orival_egip1excl2 "Rival (EGIPA Marginalized)" 0.orival_egip1excl2#cL.lexclpop "Non-Rival * Exclusion"0.orival_egip1excl2#cL.cpi_pctchange "Non-Rival * Inflation" 1.orival_egip1excl2#cL.lexclpop "Rival (Other) * Exclusion" 1.orival_egip1excl2#cL.cpi_pctchange "Rival (Other) * Inflation" 2.orival_egip1excl2#cL.lexclpop "Rival (EGIPA Marginalized) * Exclusion" 2.orival_egip1excl2#cL.cpi_pctchange "Rival (EGIPA Marginalized) * Inflation" 0.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange "Non-Rival * Exclusion * Inflation" 1.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange "Rival (Other) * Exclusion * Inflation" 2.orival_egip1excl2#cL.lexclpop#cL.cpi_pctchange "Rival (EGIPA Marginalized) * Exclusion * Inflation" ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" polity21 "Polity Score A" polity22 "Polity Score B" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3") ///
	lines ///
	replace



	

	
*************************************************************
***** Appendix. Econ Inequality (GDP Output by Group)   *****
*****     (Replacing Political Inequality by Group)     *****
*************************************************************

use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable
sort dirdyadid year

summ l.ggini_gdp2 l.ggini_nl2 l.gtheil_gdp2 l.gtheil_nl2, detail

*** 2-way Interaction ***
logit cowmidinit c.l.ggini_nl2##c.l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store F1_2way_lmunr

set level 90
set scheme cleanplots
summarize l.ggini_nl2 ggini_gdp2 gtheil_gdp2 l.munr, detail
margins, at(c.l.ggini_nl2 = (0(0.2)0.6) c.l.munr = (0 15)) // lnmunr{0 = 0-55th percentile; 1.61 = 85th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	title("Figure F1A. Interaction Effects of Horizontal Economic Inequality & Mass Unrest on MID Initiation") ///
	xtitle("Group Gini Index") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(1) position(6))

	
* Interaction marginsplot dydx (Marginal Effects) A3
margins, dydx(l.ggini_nl2) at(l.munr=(0(5)35))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure F1A. Marginal Effects of Horizontal Economic Inequality on MID Initiation") subtitle("Based on Mass Unrest") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l.munr) at(l.ggini_nl2=(0(0.1)0.6))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure F1B. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Based on Horizontal Economic Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Group Gini Index") note("Note: 90% Confidence Interval", size(vsmall)) 
	

	

*** 3-way Interaction (Rivalry) ***
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & Rival_EthnicComp data

logit cowmidinit c.l.ggini_nl2##c.l.munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store F2_3way_rival

margins, dydx(ongoingrivalry_update) at(l.ggini_nl2=(0(0.1)0.6) l.munr=(0(5)35))
_marg_save, saving(final_F2_3way_rival_egip1excl2_ggininl_dydx, replace)

use final_F2_3way_rival_egip1excl2_ggininl_dydx, clear
twoway contour _margin _at1 _at2, ccut(-0.005 0(.01)0.07)  ///
	title("Figure F2. Marginal Effects of Rivalry on MID Initiation") ///
	subtitle("Compared to Non-Rival Target") ///
	xtitle("Mass Unrest") ///
	ytitle("Group Gini (Nightlight Data)") ///
	ztitle("Effect of Rivalry") ///
	ylabel(0(0.1)0.6) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 


	
	
	
	
********************************************************
*****  Appendix. EconIneq*MUNR*Rivalry-EGIP1EXCL2  *****
*****                3-way interaction             *****
********************************************************
***** 
// * Rival & Ethnic Composition Combined - EGIP1 Excluded in T (3 categories)
// tab  ongoingrivalry_update egip1_excl2_dum
//
// generate orival_egip1 = 0
// replace orival_egip1 = 1 if ongoingrivalry_update==1 & egip1_excl2_dum == 0
// replace orival_egip1 = 2 if ongoingrivalry_update==1 & egip1_excl2_dum == 1
// tab  ongoingrivalry_update orival_egip1
// tab orival_egip1 egip1_excl2_dum 


set scheme cleanplots

*** 3-way Interaction: EGIP1EXCL2 (Baseline: 0, Cat 1)

** Baseline 0, Cat 1
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable
sort dirdyadid year
logit cowmidinit c.l.ggini_nl2##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store F3A_3way_egip1excl2_base0


* Interaction Plot
summarize ggini_gdp2, detail
summarize ggini_nl2, detail
correlate ggini_nl2 ggini_gdp2 lexclpop
margins, at(c.l.ggini_nl2 = (0(0.15)0.6) orival_egip1excl2 = (1 2) c.l.munr = (0 12))  // munr{0 = 0-55th percentile; 11 = 95th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
margins, at(c.l.ggini_nl2 = (0(0.15)0.6) c.l.munr = (0 12))  // munr{0 = 0-55th percentile; 11 = 95th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
marginsplot
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	plot3opt(msymbol(square)) ci3opt(color(gray%20)) ///
	title("Interaction Effects of Econ HI & Rivalry & Ethnic Kin on MID Initiation") ///
	xtitle("Group Gini Index") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(2) position(6))


margins, dydx(1.orival_egip1excl2) at(l.ggini_nl2=(0(0.1)0.6) l.munr=(0(5)35))
_marg_save, saving(final_F3A_3way_regip1excl2_base0cat1_dydx, replace)

use final_F3A_3way_regip1excl2_base0cat1_dydx, clear
twoway contour _margin _at1 _at2, ccut(0.005(.001)0.013)  ///
	title("Figure F3A. Marginal Effects of Other Rivals (Who Do Not Marginalize State A's EGIPs)", size(medsmall)) ///
	subtitle("Compared to Non-Rival Targets") ///
	xtitle("Mass Unrest") ///
	ytitle("Group Gini (Nightlight Data))") ///
	ztitle("Effect of Other Rivals (A's EGIPs Not Excluded)") ///
	ylabel(0(0.1)0.6) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
	
	
** Baseline 0, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable
sort dirdyadid year
quietly: logit cowmidinit c.l.ggini_nl2##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)


* Interaction Plot
summarize ggini_gdp2, detail
summarize ggini_nl2, detail
correlate ggini_nl2 ggini_gdp2 lexclpop
margins, at(c.l.ggini_nl2 = (0(0.15)0.6) orival_egip1excl2 = (1 2) c.l.munr = (0 12))  // munr{0 = 0-55th percentile; 11 = 95th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
margins, at(c.l.ggini_nl2 = (0(0.15)0.6) c.l.munr = (0 12))  // munr{0 = 0-55th percentile; 11 = 95th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
marginsplot
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
	plot3opt(msymbol(square)) ci3opt(color(gray%20)) ///
	title("Interaction Effects of Econ HI & Rivalry & Ethnic Kin on MID Initiation") ///
	xtitle("Group Gini Index") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(2) position(6))


margins, dydx(2.orival_egip1excl2) at(l.ggini_nl2=(0(0.1)0.6) l.munr=(0(5)35))
_marg_save, saving(final_F3B_3way_regip1excl2_base0cat2_dydx, replace)

use final_F3B_3way_regip1excl2_base0cat2_dydx, clear
twoway contour _margin _at1 _at2, ccut(-0.025 0(.1)0.9 0.98)  ///
	title("Figure F3B. Marginal Effects of Rivals Who Marginalize State A's EGIPs", size(medsmall)) ///
	subtitle("Compared to Non-Rival Target") ///
	xtitle("Mass Unrest") ///
	ytitle("Group Gini Index") ///
	ztitle("Effect of Rival Who Marginalize State A's EGIPs") ///
	ylabel(0(0.1)0.6) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 
	
	
	
	
*** 3-way Interaction: EGIP1_EXCL2 (Baseline: 1, Cat 2)
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable

logit cowmidinit c.l.ggini_nl2##c.l.munr##ib(1).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store F3B_3way_egip1excl2_base1

* ME Plot
margins, dydx(2.orival_egip1excl2) at(l.ggini_nl2=(0(0.1)0.8) l.munr=(0 15))
marginsplot, recastci(rarea) plot1opt(msymbol(circle)) plot2opt(msymbol(diamond)) ciopt(color(gray%15)) yline(0) ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Group Gini Index") note("Note: 90% Confidence Interval", size(vsmall)) 


* ME Plot: Contour
margins, dydx(2.orival_egip1excl2) at(l.ggini_nl2=(0(0.1)0.6) l.munr=(0(5)35))
_marg_save, saving(final_F3C_3way_regip1excl2_base1cat2_dydx, replace)

use final_F3C_3way_regip1excl2_base1cat2_dydx, clear
twoway contour _margin _at1 _at2, ccut(-0.025 0 0.05 0.1(0.1)0.9 0.98)  ///
	title("Figure F3C. Marginal Effects of Rivals Who Marginalize State A's EGIPs", size(medsmall)) ///
	subtitle("Compared to Other Rival Target (A's EGIPs Not Excluded)") ///
	xtitle("Mass Unrest") ///
	ytitle("Group Gini Index") ///
	ztitle("Effect of Rival w. Excluded Home State's EGIP") ///
	ylabel(0(0.1)0.6) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 

	
	
	
*** Table F1 ***
help esttab
esttab F1_2way_lmunr F2_3way_rival F3A_3way_egip1excl2_base0 F3B_3way_egip1excl2_base1 using tableF1.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll r2_p chi2) /// 
	nobaselevels ///
	title("Table F1. Interaction of Horizontal Inequality (Economic), Inflation, and Rival-Ethnic Combination ") ///
	mtitles("Model F1" "Model F2" "Model F3A (Baseline: Non-Rival)" "Model F3B (Baseline: Other Rivals)") ///
	order(L.ggini_nl2 L.munr cL.ggini_nl2#cL.munr 1.ongoingrivalry_update 1.ongoingrivalry_update#cL.ggini_nl2 1.ongoingrivalry_update#cL.munr 1.ongoingrivalry_update#cL.ggini_nl2#cL.munr 0.orival_egip1excl2 1.orival_egip1excl2 2.orival_egip1excl2 0.orival_egip1excl2#cL.ggini_nl2 0.orival_egip1excl2#cL.munr 1.orival_egip1excl2#cL.ggini_nl2 1.orival_egip1excl2#cL.munr 2.orival_egip1excl2#cL.ggini_nl2 2.orival_egip1excl2#cL.munr 0.orival_egip1excl2#cL.ggini_nl2#cL.munr 1.orival_egip1excl2#cL.ggini_nl2#cL.munr 2.orival_egip1excl2#cL.ggini_nl2#cL.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(L.ggini_nl2 "Group Gini" L.munr "Mass Unrest" cL.ggini_nl2#cL.munr "Group Gini * Mass Unrest" 1.ongoingrivalry_update "Rivalry" 1.ongoingrivalry_update#cL.ggini_nl2 "Rivalry * Group Gini" 1.ongoingrivalry_update#cL.munr "Rivalry * Mass Unrest" 1.ongoingrivalry_update#cL.ggini_nl2#cL.munr "Rivalry * GGini * Unrest" 0.orival_egip1excl2 "Non-Rival" 1.orival_egip1excl2 "Rival (Other)" 2.orival_egip1excl2 "Rival (EGIPA Marginalized)" 0.orival_egip1excl2#cL.ggini_nl2 "Non-Rival * GGini"0.orival_egip1excl2#cL.munr "Non-Rival  *Unrest" 1.orival_egip1excl2#cL.ggini_nl2 "Rival (Other) * GGini" 1.orival_egip1excl2#cL.munr "Rival (Other) * Unrest" 2.orival_egip1excl2#cL.ggini_nl2 "Rival (EGIPA Marginalized) * GGini" 2.orival_egip1excl2#cL.munr "Rival (EGIPA Marginalized) * Unrest" 0.orival_egip1excl2#cL.ggini_nl2#cL.munr "Non-Rival * GGini * Unrest" 1.orival_egip1excl2#cL.ggini_nl2#cL.munr "Rival (Other) * GGini * Unrest" 2.orival_egip1excl2#cL.ggini_nl2#cL.munr "Rival (EGIPA Marginalized) * GGini * Unrest" ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" polity21 "Polity Score A" polity22 "Polity Score B" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3" _cons "Constant") ///
	lines ///
	replace


*================================================== END ==============================================
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



// *************** L.MUNR (Differenced) *****************
// use ngo2024ethnic_final_xtset2
//
// ***** 2-way Int
// logit cowmidinit c.l.lexclpop##c.l.d.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
//
// summ l.d.munr, detail
//
// margins, at(c.l.lexclpop = (0(0.2)0.8) c.l.d.munr = (-3 3)) // lnmunr{0 = 0-55th percentile; 1.61 = 85th percentile} | exclpop{0.014 = 25th percentile; 0.11 = 60th percentile; 0.23 = 75th percentile; 0.297 = 85th percentile}
// marginsplot, recastci(rarea) plot1opt(msymbol(circle)) ci1opt(color(gray%20)) plot2opt(msymbol(diamond)) ci2opt(color(gray%20)) ///
// 	title("Figure 2A. Interaction Effects of Ethnic Exclusion & Mass Unrest on MID Initiation") ///
// 	subtitle("Higher Ethnic Exclusion & Mass Unrest Lead to Higher Risk of MID Initiation", size(small)) ///
// 	xtitle("Share of Excluded Ethnic Pop.") ///
// 	ytitle("Pr(MID Initiation)") ///
// 	note("Note: 90% Confidence Interval", size(vsmall)) ///
// 	legend(rows(1) position(6))
//
//
// * Interaction marginsplot dydx (Marginal Effects)
// margins, dydx(l.lexclpop) at(l.d.munr=(-15(3)18))
// marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure 2B. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Increasing Mass Unrest") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 
//
// margins, dydx(l.d.munr) at(l.lexclpop=(0(0.2)0.8))
// marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure 2B. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Based on Increasing Ethnic Political Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 
//
//
// ***** 3-way Int
// logit cowmidinit c.l.lexclpop##c.l.d.munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)






	
*********************************************************************
*****                            Reserve                        *****
***** Models in the original manuscript but removed in revision *****
*********************************************************************

************************ Logit **********************************
*** Model 1: No interaction ***
logit cowmidinit l.lexclpop l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m1_noint

summarize l.exclpop lexclpop, detail


help outreg2
sort dirdyadid year


*** Model 2: excluded pop & ongoingrivalry (Target State is Rival) (Significant)
logit cowmidinit c.l.lexclpop##i.ongoingrivalry l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m2_2way_rival

* Coefplot M2
help coefplot
coefplot, drop(_cons) ///
	 order(L.lexclpop 1.ongoingrivalry 1.ongoingrivalry#cL.lexclpop L.munr ethfrac1 relpow_a tradevol srswba polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	 coeflabels(L.lexclpop = "Share of Excluded Ethnic" 1.ongoingrivalry = "Rivalry" 1.ongoingrivalry#cL.lexclpop = "Ethnic Exclusion * Rivalry" L.munr = "Mass Unrest (t - 1)" ethfrac1 = "Ethnic Fractionalization" relpow_a = "Relative Capabilities (Side A)" tradevol = "Trade Volume (Side A + B)" srswba = "Alliance Similarity" polity21 = "Polity Score (Side A)" polity22 = "Polity Score (Side B)" mindist = "Distance" cowmidspell = "Peace Yrs" cowmidspell2 = "Peace Yrs^2" cowmidspell3 = "Peace Yrs^3") ///
	 xlabel(-1.5(0.5)1.5) xline(0) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick) color(cranberry%50)) msymbol(circle) msize(medsmall)  ///
	 title("Coefficient Plots of M2: Logit Regression of MID Initiation", size(medium))
	 
	 
* Coefplot M2 - Odds Ratio
help coefplot
coefplot, drop(_cons) ///
	 order(L.lexclpop 1.ongoingrivalry 1.ongoingrivalry#cL.lexclpop L.munr ethfrac1 relpow_a tradevol srswba polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	 coeflabels(L.lexclpop = "Share of Excluded Ethnic" 1.ongoingrivalry = "Rivalry" 1.ongoingrivalry#cL.lexclpop = "Ethnic Exclusion * Rivalry" L.munr = "Mass Unrest (t - 1)" ethfrac1 = "Ethnic Fractionalization" relpow_a = "Relative Capabilities (Side A)" tradevol = "Trade Volume (Side A + B)" srswba = "Alliance Similarity" polity21 = "Polity Score (Side A)" polity22 = "Polity Score (Side B)" mindist = "Distance" cowmidspell = "Peace Yrs" cowmidspell2 = "Peace Yrs^2" cowmidspell3 = "Peace Yrs^3") ///
	 xlabel(0(0.5)5.5) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick) color(eltblue%65)) msymbol(circle) msize(medsmall) mcolor(eltblue%100) ///
	 eform xtitle("Odds Ratio") xline(1)

* summarize distributions of exclpop & lnmunr for subset of Targets w/ Ethnic kin
summarize lexclpop l.munr if ongoingrivalry==1, detail
summarize l.munr, detail

* Marginsplot: No Interactive effect M2
margins, at(c.l.lexclpop = (0(0.2)0.8) ongoingrivalry = (0 1) l.munr = 0)
marginsplot, recastci(rarea) ciopt(color(gray%20)) ///
	title("Figure 1A. Interactive Effects of Ethnic Inequality and Rivalry on Conflict Initiation") ///
	xtitle("Share of Excluded Ethnic Population") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	legend(rows(1) position(6))
	
* Interaction marginsplot dydx (Marginal Effects) M2
set scheme cleanplots

margins, dydx(l.lexclpop) at(ongoingrivalry = (0 1))
marginsplot, recast(bar) plotopt(color(red%35)) ciopt(color(red%75)) yline(0) title("Figure 2A. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Kin Exist in Target") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 


margins, dydx(ongoingrivalry) at(l.lexclpop=(0(0.2)0.8) l.munr=0)
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure 1B. Marginal Effect of Ongoing Rivalry on MID Initiation") subtitle("Based on Ethnic Pol. Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Share of Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) ylabel(0(0.005)0.03)



*** Table 1:
help estout
help outreg2

/* Does NOT Work
estout m1_noiv m2_noint m3_int m4_kin m5_nokin using table1.doc, replace cells(b(fmt(4) star) se(par fmt(4))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N r2_p ll aic bic) varlabels(exclpop "% Excluded Pop." lnmunr "Mass Unrest (log & lag)" c.exclpop#c.lnmunr "Excluded Pop*Mass Unrest" ethnicfrac "Ethnic Fractionalization" apwr "Relative Capabilities (Side A)" adep "Trade Dependence (Side A)" sglo "Alliance Similarity" othdisp "Other Dispute" polity1 "Polity Score A" polity2 "Polity Score B" dist "Distance" cwpceyrs "Peace Yrs" pceyrs2 "Peace Yrs^2" pceyrs3 "Peace Yrs^3")
*/

outreg2 [m1_noint m2_2way_rival] using table1_rival.doc, label e(all) replace dec(3)

help label
	

***********************************************************
***** Reserve Appendix. REs for Inequality & Target   *****
***********************************************************


*** Model B1: No interaction ***
xtlogit cowmidinit l.lexclpop l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store B1_noint_re

summarize l.exclpop lexclpop, detail

* Coefplot B1
help coefplot
coefplot, drop(_cons) ///
	 order(l.lexclpop lnmunr ethnicfrac apwr adep sglo othdisp polity1 polity2 dist cwpceyrs pceyrs2 pceyrs3) ///
	 coeflabels(exclpop = "% Excluded Pop." lnmunr = "Mass Unrest (log & lag)" ethnicfrac = "Ethnic Frac." apwr = "Relative Capabilities (Side A)" adep = "Trade Dependence (Side A)" sglo = "Alliance Similarity" othdisp = "Other Dispute" polity1 = "Polity Score (Side A)" polity2 = "Polity Score (Side B)" dist = "Distance" cwpceyrs = "Peace Yrs" pceyrs2 = "Peace Yrs^2" pceyrs3 = "Peace Yrs^3") ///
	 xlabel(-1.5(0.5)1) xline(0) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick)) msymbol(circle) msize(medsmall)  
help outreg2



*** Model B2: excluded pop & ongoing rivalry
xtlogit cowmidinit c.l.lexclpop##i.ongoingrivalry_update l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store B2_2way_rivalnew_re

excl2_sum

* summarize distributions of exclpop & lnmunr for subset of Targets w/ Ethnic kin
summarize lexclpop lnmunr if kinexist==1, detail

* Marginsplot: No Interactive effect B2
margins, at(c.l.lexclpop = (0(0.2)0.8) ongoingrivalry_update = (0 1))
_marg_save, saving(final_B2_2way_int, replace)

use final_B2_2way_int, clear
marginsplot, recastci(rarea) ciopt(color(gray%20)) ///
	title("Figure B1A. Interactive Effects of Ethnic Inequality and Rivalry on MID Initiation") ///
	xtitle("% Excluded Ethnic Population") ///
	ytitle("Pr(MID Initiation)") ///
	note("Note: 90% Confidence Interval", size(vsmall)) ///
	ylabel(0(0.01)0.05) ///
	legend(rows(1) position(6))
	
* Interaction marginsplot dydx (Marginal Effects) A2
set scheme cleanplots

margins, dydx(l.lexclpop) at(ongoingrivalry_update = (0 1))
marginsplot, recast(bar) plotopt(color(red%35)) ciopt(color(red%75)) yline(0) title("Figure 2A. Marginal Effects of Ethnic Pol. Inequality on MID Initiation") subtitle("Based on Kin Exist in Target") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 


margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8))
_marg_save, saving(final_B2_2way_dydx_rival, replace)

use final_B2_2way_dydx, clear
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) ylabel(0(0.01)0.04) title("Figure B1B. Marginal Effect of Rivalry on MID Initiation") subtitle("Based on Ethnic Pol. Inequality") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Pop.") note("Note: 90% Confidence Interval", size(vsmall)) 


outreg2 [B1_noint_re B2_2way_rivalnew_re] using tableB1.doc, label e(all) replace dec(3)


	
	
	
	