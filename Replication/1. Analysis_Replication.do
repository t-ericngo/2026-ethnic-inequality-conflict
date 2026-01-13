cd "set-working-directory-here"

use ngo2024ethnic_final_xtset2.dta, clear 

*** xtset the data
xtset dirdyadid year
xtset // Check to see the data is xtset (i.e, set for panel/cross-national time-series analysis)

sort ccode1 ccode2 year



*** Key Variables (all at the dyad-year level)
label variable cowmidinit "MID Initiation" 
// MID (conflict) initiation of a state in a year; binary (1 = year, 0 = no); Source: Correlates of War (COW) project

label variable lexclpop "Proportion of Excluded Ethnic" 
// Ethnic Political Inequality, measured by a state's population proportion of ethnic groups being excluded from the state's executive government 

label variable munr "Mass Unrest" 
// Domestic turmoil, measured by number of mass unrest (large protests, mass demonstrations, riots, etc.) in a year

label variable ongoingrivalry_update "Ongoing Rivalry" // Whether a (potential) target state in the dyad is an international rival of the home state in a given year; binary (1 = yes, 0 = no)
 
label variable ethfrac1 "Eth. Frac." 
// Level of ethnic fractionalization (capturing level of ethnic diversity) in a state in a given year

label variable relpow_a "Side A's Relative Capabilites"  
// Share of home state (state A) military capabilities in the total dyad's military power

label variable tradevol "Dyad's Trade Volume" 
// Total dyad's trade value in a year ($ millions)

label variable srsvaa "FP Similarity" 
// Foreign policy similarity of 2 states in the dyad

label variable polity21 "Polity Score A" 
// Level of demmocracy (political openness & competition & institutional strength) of state A 

label variable polity22 "Polity Score B" 
// Level of demmocracy (political openness & competition & institutional strength) of state B 

label variable mindist "Distance" 
// Minimum distance between the borders of the 2 states (A and B) in the dyad

label variable cowmidspell "Peace Years" 
// Number of peace years (years w/o conflict) between 2 states in the dyad

label variable cowmidspell2 "Peace Years^2" 
// Number of peace years (squared) between 2 states in the dyad

label variable cowmidspell3 "Peace Years^3" 
// Number of peace years (cubic) between 2 states in the dyad

label variable kinexist "Kin Exist in Target" 
// Whether an ethnic kin exist in 2 states

label define rival_egip1excl2 0 "Non-Rival" 1 "Rival, No EGIP(A)-EGE(B)" 2 "Rival, EGIP(A)-EGE(B)"
label values orival_egip1excl2 rival_egip1excl2
label variable orival_egip1excl2 "Rival & Kin Ethnic Type"
// Variable orival_egip1excl2 represents a combination of a target state's (state B) rivalry status AND power position of ethnic kin with state A
// 0 = State B Not a Rival (w/ state A)
// 1 = State B a Rival (w/ state A), BUT no ethnic kinship
// 2 = State B in a Rival (w/ state A), AND has an ethnic group in power in state A (EGIP(A)) that is an excluded ethnic group in state B (EGE(B))
	// E.g., China (A) vs. Vietnam (B). Vietnam is a rival to China AND has Chinese ethnic group (kin with state A) who is excluded from the executive government 



********************************************************************
**                          MAIN ANALYSIS                         **
********************************************************************
set level 90    // Set confidence interval to 90%
set scheme cleanplots
help outreg2

********************************************************************
** Table 1. Logistic Regression of MID (dyad-level clustered SEs) **
********************************************************************
use ngo2024ethnic_final_xtset2

*** Model 1: 2-way Interaction ***
logit cowmidinit c.l.lexclpop##c.l.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m1_2way_lmunr

* Coefplot M1
coefplot, drop(_cons) order(l.exclpop lnmunr c.exclpop#c.lnmunr ethnicfrac apwr adep sglo othdisp polity1 polity2 dist cwpceyrs pceyrs2 pceyrs3) ///
	coeflabels(l.exclpop = "% Excluded Pop." lnmunr = "Mass Unrest (log & lag)" c.l.exclpop#c.lnmunr = "Excluded Pop*Mass Unrest" ethnicfrac = "Ethnic Frac." apwr = "Relative Capabilities (Side A)" adep = "Trade Dependence (Side A)" sglo = "Alliance Similarity" othdisp = "Other Dispute" polity1 = "Polity Score (Side A)" polity2 = "Polity Score (Side B)" dist = "Distance" cwpceyrs = "Peace Yrs" pceyrs2 = "Peace Yrs^2" pceyrs3 = "Peace Yrs^3") ///
	xlabel(-1.5(0.5)1) xline(0) levels(95 90) ciopts(recast(rcap) lwidth(medthick thick)) msymbol(circle) msize(medsmall)  

	
* Interaction marginsplot dydx (Marginal Effects) M1
margins, dydx(l.lexclpop) at(l.munr=(0(5)35))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure 1A. Marginal Effects of Ethnic Exclusion on MID Initiation") subtitle("Based on Increasing Mass Unrest") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Mass Unrest") note("Note: 90% Confidence Interval", size(vsmall)) 

margins, dydx(l.munr) at(l.lexclpop=(0(0.2)0.8))
marginsplot, recastci(rarea) plotopt(color(red%85)) ciopt(color(red%35)) yline(0) title("Figure 1B. Marginal Effects of Mass Unrest on MID Initiation") subtitle("Based on Increasing Ethnic Exclusion") ytitle("Change in Effect on Pr(MID Initiation)", size(medsmall)) xtitle("Excluded Ethnic Population") note("Note: 90% Confidence Interval", size(vsmall)) 

summarize exclpop lnmunr, detail


*** Model 2: 3-way Interaction: excluded pop. & ongoing rivalry target & domestic turmoil - Target State Being Ongoing Rival 
*** Ethnic Inequality and Mass Unrest's Effect on Conflict/MID Initiation When The Target State is an International Rival

use ngo2024ethnic_final_xtset2, clear

logit cowmidinit c.l.lexclpop##c.l.munr##i.ongoingrivalry_update ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m2_3way_rivalry


** 3-way visualization M2: Contour Plot
margins, dydx(ongoingrivalry_update) at(l.lexclpop=(0(0.2)0.8) l.munr=(0(5)35))
_marg_save, saving(final_m2_3way_rivalnew_dydx2, replace)

use final_m2_3way_rivalnew_dydx2, clear
twoway contour _margin _at1 _at2, ccut(0.0135 0.015(.0025)0.045) ///
	title("Figure 2. Marginal Effects of Rivalry on MID Initiation", size(medlarge)) ///
	subtitle("Based on Ethnic Exclusion & Mass Unrest") ///
	xtitle("Mass Unrest") ///
	ytitle("Share of Excluded Pop.") ///
	ztitle("Effect of Ongoing Rivalry") ///
	xlabel(0(5)35) ylabel(0(0.1)0.8) ///
	crule(linear) scolor(white) ecolor(gs2) ///
	text(0.07 0.5 "0.02 - 0.0225", place(e) size(vsmall)) ///
	text(0.71 0.5 "0.015 - 0.0175", place(e) size(vsmall)) ///
	text(0.07 25 "0.015 - 0.0175", place(e) size(vsmall)) ///
	text(0.2 7 "0.0175 - 0.02", place(e) size(vsmall)) ///
	text(0.355 12.5 "0.02 - 0.0225", place(e) size(vsmall)) ///
	text(0.49 18 "0.0225 - 0.025", place(e) size(vsmall))


*** Table 1 ***
outreg2 [m1_2way_lmunr m2_3way_rivalry] using table1_final.doc, label e(all) replace dec(3)

esttab m1_2way_lmunr m1_2way_lmunr using table1_final.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll aic bic) /// 
	nobaselevels ///
	title("Table 1. Logistic Regression of MID Initiation") ///
	mtitles("M1" "M2") ///
	order(L.lexclpop L.munr 1.ongoingrivalry_update cL.lexclpop##cL.munr 1.ongoingrivalry_update##cL.lexclpop 1.ongoingrivalry_update##cL.munr 1.ongoingrivalry_update##cL.lexclpop##cL.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(L.lexclpop "Ethnic Exclusion" L.munr "Mass Unrest" 1.ongoingrivalry_update "Rival"  ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" polity21 "Polity Score A" polity22 "Polity Score B" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3") ///
	lines ///
	replace





********************************************************
**   Table 2. Logistic Regression of MID Initiation   **
** Given Ethnic Kinship Power Status in Rival Targets **
********************************************************

*** Model 3A: EGIP1EXCL2 (Baseline: 0, No Rival) ***

** Baseline 0, Cat 1
use ngo2024ethnic_final_xtset2.dta, clear

logit cowmidinit c.l.lexclpop##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m3A_3way_egip1excl2_base0

margins, dydx(1.orival_egip1excl2) at(l.lexclpop=(0(0.1)0.8) l.munr=(0(5)35))
_marg_save, saving(final_m3A_3way_rivalnew_egip1notexcl2_dydx, replace)

use final_m3A_3way_rivalnew_egip1notexcl2_dydx, clear
twoway contour _margin _at1 _at2, ccut( 0.013 0.015(0.0025)0.035)  ///
	title("Figure 3A. Marginal Effects of Other Rivals (Who Do Not Marginalize State A's EGIPs)", size(medium)) ///
	subtitle("Compared to Non-Rival Targets") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Other Rivals") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 




** Baseline 0, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable
logit cowmidinit c.l.lexclpop##c.l.munr##ib(0).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)

* ME Plot: Contour
margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.1)0.8) l.munr=(0(5)35))
_marg_save, saving(final_m3B_3way_rivalnew_egip1excl2_dydx, replace)

use final_m3B_3way_rivalnew_egip1excl2_dydx, clear
twoway contour _margin _at1 _at2, ccut(0 0.05 0.1(.1)0.9 0.95 )  ///
	title("Figure 3B. Marginal Effects of Rivals Who Marginalize State A's EGIPs", size(medlarge)) ///
	subtitle("Compared to Non-Rival Targets") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivals Who Exclude Home State's EGIP") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 




*** Model 3B: EGIP1_EXCL2 vs. EGIP1_NotEXCL2 (Baseline: 1, Other Rivals)

** Baseline 1, Cat 2
use ngo2024ethnic_final_xtset2.dta, clear // ngo2024ethnic_final_xtset has NO Excluded Ethnic Kin data & inflation data & rival_ethniccomp variable

logit cowmidinit c.l.lexclpop##c.l.munr##ib(1).orival_egip1excl2 ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3, vce(cluster dirdyadid)
estimates store m3B_3way_egip1excl2_base1

* ME Plot: Contour
margins, dydx(2.orival_egip1excl2) at(l.lexclpop=(0(0.1)0.8) l.munr=(0(5)35))
_marg_save, saving(final_m3C_3way_rivalnew_egip1excl2_bl1_dydx, replace)

use final_m3C_3way_rivalnew_egip1excl2_bl1_dydx, clear
twoway contour _margin _at1 _at2, ccut(-0.017 0 0.05 0.1(0.1)0.9 0.93)  ///
	title("Figure 3C. Marginal Effects of Rivals Who Marginalize State A's EGIPs", size(medlarge)) ///
	subtitle("Compared to Other Rivals (State A's EGIPs Not Marginalized)") ///
	xtitle("Mass Unrest") ///
	ytitle("Proportion of Excluded Pop.") ///
	ztitle("Effect of Rivals Who Exclude State A's EGIP") ///
	ylabel(0(0.1)0.8) ///
	xlabel(0(5)35) ///
	crule(linear) scolor(white) ecolor(gs2) 



*** Table 2 ***
label define rival_egip1excl2 0 "Non-Rival" 1 "Rival, No EGIP(A)-EGE(B)" 2 "Rival, EGIP(A)-EGE(B)"

label values orival_egip1excl2 rival_egip1excl2


outreg2 [m3A_3way_egip1excl2_base0 m3B_3way_egip1excl2_base1] using table2_final.doc, label e(all) replace dec(3)


esttab m3A_3way_egip1excl2_base0 m3B_3way_egip1excl2_base1 using table2_final.rtf, ///
	cells(b(fmt(3) star) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N ll aic bic) /// 
	nobaselevels ///
	title("Table 2. 3-Way Interaction of Ethnic Exclusion, Mass Unrest, and Rival-Ethnic Combination ") ///
	mtitles("M3A (Baseline: Non-Rival)" "M3B (Baseline: Other Rivals)") ///
	order(L.lexclpop L.munr cL.lexclpop#cL.munr 0.orival_egip1excl2 1.orival_egip1excl2 2.orival_egip1excl2 0.orival_egip1excl2#cL.lexclpop 0.orival_egip1excl2#cL.munr 1.orival_egip1excl2#cL.lexclpop 1.orival_egip1excl2#cL.munr 2.orival_egip1excl2#cL.lexclpop 2.orival_egip1excl2#cL.munr 0.orival_egip1excl2#cL.lexclpop#cL.munr 1.orival_egip1excl2#cL.lexclpop#cL.munr 2.orival_egip1excl2#cL.lexclpop#cL.munr ethfrac1 relpow_a tradevol srsvaa polity21 polity22 mindist cowmidspell cowmidspell2 cowmidspell3) ///
	coeflabels(L.lexclpop "Ethnic Exclusion" L.munr "Mass Unrest" cL.lexclpop#cL.munr "Ethnic Exclusion * Mass Unrest" 0.orival_egip1excl2 "Non-Rival" 1.orival_egip1excl2 "Rival (Other)" 2.orival_egip1excl2 "Rival (EGIPA Marginalized)" 0.orival_egip1excl2#cL.lexclpop "Non-Rival * Exclusion"0.orival_egip1excl2#cL.munr "Non-Rival  *Unrest" 1.orival_egip1excl2#cL.lexclpop "Rival (Other) * Exclusion" 1.orival_egip1excl2#cL.munr "Rival (Other) * Unrest" 2.orival_egip1excl2#cL.lexclpop "Rival (EGIPA Marginalized) * Exclusion" 2.orival_egip1excl2#cL.munr "Rival (EGIPA Marginalized) * Unrest" 0.orival_egip1excl2#cL.lexclpop#cL.munr "Non-Rival * Exclusion * Unrest" 1.orival_egip1excl2#cL.lexclpop#cL.munr "Rival (Other) * Exclusion * Unrest" 2.orival_egip1excl2#cL.lexclpop#cL.munr "Rival (EGIPA Marginalized) * Exclusion * Unrest" ethfrac1 "Eth. Frac." relpow_a "Side A's Relative Capabilities" tradevol "Dyad's Trade Volume" srsvaa "FP Similarity" polity21 "Polity Score A" polity22 "Polity Score B" mindist "Minimum Distance" cowmidspell "Peace Years" cowmidspell2 "Peace Years^2" cowmidspell3 "Peace Years^3") ///
	lines ///
	replace

