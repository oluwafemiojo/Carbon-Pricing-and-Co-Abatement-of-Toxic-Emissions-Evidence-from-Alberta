*Note: All estimations are done on STATA 15/MP.

clear all
set more off
set matsize 11000

cd "G:\My Drive\Research\9. Under review\RGGI and TRI_Travis\Draft\Replication Package_v2"

log using "Results.log", replace

gl rggiXy= "rggiXy2000 rggiXy2001 rggiXy2002 rggiXy2003 rggiXy2004 rggiXy2005 rggiXy2006 rggiXy2007 rggiXy2009 rggiXy2010 rggiXy2011 rggiXy2012 rggiXy2013 rggiXy2014 rggiXy2015 rggiXy2016 rggiXy2017 rggiXy2018 rggiXy2019"
gl absorbvar="frsid st year"
gl clustervar="frsid"
gl control="ffuse rps_pct coalgas lgdp"
gl treatyear=2008

use "EI Data Coal.dta", clear

********************************************************************************
*Table 1: 
********************************************************************************

foreach depvar in emission releases emission_metal1 releases_metal1 emission_metal0 releases_metal0{

*Before 2009:
sum lsum`depvar' if rggi==1&year<2009
sum lsum`depvar' if rggi==0&year<2009

*After 2009:
sum lsum`depvar' if rggi==1&year>=2009
sum lsum`depvar' if rggi==0&year>=2009
}

********************************************************************************
*Figure 2: The order of the foreach loop follows the order of Figure 2
********************************************************************************

foreach depvar in lsumemission lsumreleases lsumemission_metal1 lsumreleases_metal1 lsumemission_metal0 lsumreleases_metal0{
	reghdfe `depvar' $rggiXy $control, absorb($absorbvar) vce(cluster $clustervar)
	g x = 0
	g y = 0
	g y_hi = 0
	g y_lo = 0
	
	loc obs 1
	forvalues n=2000/2019{
		replace x=`n' in `obs'
		if `n' != $treatyear {
			replace y    = _b[rggiXy`n']                   in `obs'
			replace y_hi = _b[rggiXy`n']+1.96*_se[rggiXy`n'] in `obs'
			replace y_lo = _b[rggiXy`n']-1.96*_se[rggiXy`n'] in `obs'
		}
		
		if `n' == $treatyear {
			replace y    = .                 in `obs'
			replace y_hi = . in `obs'
			replace y_lo = . in `obs'
		}
		
		loc obs = `obs' + 1
	}
		
	*Graphs:
	tw (scatter y x in 1/20,lcolor(gs3) mcolor(gs3)) ///
			(rspike y_hi y_lo x in 1/20, lcolor(gs3) lstyle(solid)), ///
			xtit("") ytit("") ///
			yline(0, lcolor(gs10)) ///
			xline($treatyear, lcolor(gs10))  ///
			xlabel(2000(2)2019, grid glcolor(gs14) glwidth(thin)) ///
			ylabel(-3(1)2, grid glcolor(gs14) glwidth(thin)) ///
			legend(off) xsize(8) graphr(color(white)) 
	
	drop x y y_hi y_lo
}

********************************************************************************
*Table 2: The order of the foreach loop is the same as the order of the columns in table 2
********************************************************************************

foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table 3: The order of the foreach loop is the same as the order of the columns in table 3
********************************************************************************

foreach depvar in lsumemission lsumemission_lma1 lsumemission_lma0 lsumreleases lsumreleases_lma1 lsumreleases_lma0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table 4: The order of the foreach loop is the same as the order of the columns in table 4
********************************************************************************
foreach depvar in lsumemissionCARC lsumemission_metal1CARC lsumemission_metal0CARC lsumreleasesCARC lsumreleases_metal1CARC lsumreleases_metal0CARC{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table 5: The order of the foreach loop is the same as the order of the columns in table 5
********************************************************************************
preserve
use "EI Data All.dta", clear

foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases  lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone##dutility rggi##treattwo##dutility $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone#1.dutility]+_b[1.rggi#1.treattwo#1.dutility]) (Begin: (exp(_b[1.rggi#1.treatone#1.dutility])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo#1.dutility])-1)*100) (Total: (exp(_b[1.rggi#1.treatone#1.dutility]+_b[1.rggi#1.treattwo#1.dutility])-1)*100),post  
}

restore

********************************************************************************
*Table 6: The order of the foreach loop is the same as the order of the columns in table 6
********************************************************************************
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control if noneic==1& st!="CA", absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Figure A-1:
********************************************************************************

preserve
bys year rggi: egen mean_emission=mean(lsumemission)
bys year rggi: egen mean_releases=mean(lsumreleases)

bys year rggi: egen mean_emission_metal1=mean(lsumemission_metal1)
bys year rggi: egen mean_releases_metal1=mean(lsumreleases_metal1)

bys year rggi: egen mean_emission_metal0=mean(lsumemission_metal0)
bys year rggi: egen mean_releases_metal0=mean(lsumreleases_metal0)

keep year rggi mean_emission mean_releases mean_emission_metal1 mean_releases_metal1 mean_emission_metal0 mean_releases_metal0
duplicates drop 

label var mean_emission "Total"
label var mean_releases "Total"
label var mean_emission_metal1 "Metal"
label var mean_releases_metal1 "Metal"
label var mean_emission_metal0 "Non-metal"
label var mean_releases_metal0 "Non-metal"

foreach i in 0 1{
	foreach var in emission releases{
		twoway (line mean_`var' year if rggi==`i', lcolor("black")) ///
			(line mean_`var'_metal1 year if rggi==`i', lcolor("black")  lpattern("longdash")) ///
			(line mean_`var'_metal0 year if rggi==`i', lcolor("black")  lpattern("dot")),  ///
			xtit("") ytit("")  xlabel(2000(2)2019, grid glcolor(gs14) glwidth(thin))  ylabel(1(1)4.5, grid glcolor(gs14) glwidth(thin)) ///
			xsize(8) graphr(color(white)) legend(row(1) lpattern(blank))
	}
}
restore

********************************************************************************
*Table B1: The order of the foreach loop is the same as the order of the columns in table B1
********************************************************************************
foreach depvar in lsumemissionCAA lsumemission_metal1CAA lsumemission_metal0CAA lsumreleasesCAA lsumreleases_metal1CAA lsumreleases_metal0CAA{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table B2: The order of the foreach loop is the same as the order of the columns in table B2
********************************************************************************
foreach depvar in lsumemissionCARC0 lsumemission_metal1CARC0 lsumemission_metal0CARC0 lsumreleasesCARC0 lsumreleases_metal1CARC0 lsumreleases_metal0CARC0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table B3: The order of the foreach loop is the same as the order of the columns in table B3
********************************************************************************
foreach depvar in lsumemissionPBT lsumemission_metal1PBT lsumemission_metal0PBT lsumreleasesPBT lsumreleases_metal1PBT lsumreleases_metal0PBT{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table B4: The order of the foreach loop is the same as the order of the columns in table B4
********************************************************************************
foreach depvar in lsumemissionPBT0 lsumemission_metal1PBT0 lsumemission_metal0PBT0 lsumreleasesPBT0 lsumreleases_metal1PBT0 lsumreleases_metal0PBT0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table B5: The order of the foreach loop is the same as the order of the columns in table B5
********************************************************************************
foreach depvar in lsumemissionOLD lsumemission_metal1OLD lsumemission_metal0OLD lsumreleasesOLD lsumreleases_metal1OLD lsumreleases_metal0OLD{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table C1: The order of the foreach loop is the same as the order of the columns in table C1
********************************************************************************
local leaker pjmnonrggi

foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo `leaker'##treatone `leaker'##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffectRGGI: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (CombinedEffectLeaker: _b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo]) ///
				  (BeginRGGI: (exp(_b[1.rggi#1.treatone])-1)*100) (LowerRGGI: (exp(_b[1.rggi#1.treattwo])-1)*100) (TotalRGGI: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100) ///
				  (BeginLeaker: (exp(_b[1.`leaker'#1.treatone])-1)*100) (LowerLeaker: (exp(_b[1.`leaker'#1.treattwo])-1)*100) (TotalLeaker: (exp(_b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo])-1)*100) ///
					,post  
}
	
********************************************************************************
*Table C2: The order of the foreach loop is the same as the order of the columns in table C2
********************************************************************************
local leaker paoh

foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo `leaker'##treatone `leaker'##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffectRGGI: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (CombinedEffectLeaker: _b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo]) ///
				  (BeginRGGI: (exp(_b[1.rggi#1.treatone])-1)*100) (LowerRGGI: (exp(_b[1.rggi#1.treattwo])-1)*100) (TotalRGGI: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100) ///
				  (BeginLeaker: (exp(_b[1.`leaker'#1.treatone])-1)*100) (LowerLeaker: (exp(_b[1.`leaker'#1.treattwo])-1)*100) (TotalLeaker: (exp(_b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo])-1)*100) ///
					,post  
}

********************************************************************************
*Table D1: The order of the foreach loop is the same as the order of the columns in table D1
********************************************************************************
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control if noneic==1, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Appendix E (Estimation for all utilities)
********************************************************************************
use "EI Data All Utilities.dta", clear

********************************************************************************
*Table E1:
********************************************************************************

foreach depvar in emission releases emission_metal1 releases_metal1 emission_metal0 releases_metal0{

*Before 2009:
sum lsum`depvar' if rggi==1&year<2009
sum lsum`depvar' if rggi==0&year<2009

*After 2009:
sum lsum`depvar' if rggi==1&year>=2009
sum lsum`depvar' if rggi==0&year>=2009
}

********************************************************************************
*Table E2
********************************************************************************

foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  

}

********************************************************************************
*Table E3
********************************************************************************
local leaker pjmnonrggi
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo `leaker'##treatone `leaker'##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffectRGGI: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (CombinedEffectLeaker: _b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo]) ///
				  (BeginRGGI: (exp(_b[1.rggi#1.treatone])-1)*100) (LowerRGGI: (exp(_b[1.rggi#1.treattwo])-1)*100) (TotalRGGI: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100) ///
				  (BeginLeaker: (exp(_b[1.`leaker'#1.treatone])-1)*100) (LowerLeaker: (exp(_b[1.`leaker'#1.treattwo])-1)*100) (TotalLeaker: (exp(_b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo])-1)*100) ///
					,post  
}


********************************************************************************
*Tables E4
********************************************************************************
local leaker paoh
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
	reghdfe `depvar' rggi##treatone rggi##treattwo `leaker'##treatone `leaker'##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffectRGGI: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (CombinedEffectLeaker: _b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo]) ///
				  (BeginRGGI: (exp(_b[1.rggi#1.treatone])-1)*100) (LowerRGGI: (exp(_b[1.rggi#1.treattwo])-1)*100) (TotalRGGI: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100) ///
				  (BeginLeaker: (exp(_b[1.`leaker'#1.treatone])-1)*100) (LowerLeaker: (exp(_b[1.`leaker'#1.treattwo])-1)*100) (TotalLeaker: (exp(_b[1.`leaker'#1.treatone]+_b[1.`leaker'#1.treattwo])-1)*100) ///
					,post  
}

********************************************************************************
*Table E5
********************************************************************************
foreach depvar in lsumemissionCAA lsumemission_metal1CAA lsumemission_metal0CAA lsumreleasesCAA lsumreleases_metal1CAA lsumreleases_metal0CAA{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}


********************************************************************************
*Table E6
********************************************************************************
foreach depvar in lsumemissionCARC lsumemission_metal1CARC lsumemission_metal0CARC lsumreleasesCARC lsumreleases_metal1CARC lsumreleases_metal0CARC{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E7
********************************************************************************
foreach depvar in lsumemissionCARC0 lsumemission_metal1CARC0 lsumemission_metal0CARC0 lsumreleasesCARC0 lsumreleases_metal1CARC0 lsumreleases_metal0CARC0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E8
********************************************************************************
foreach depvar in lsumemissionPBT lsumemission_metal1PBT lsumemission_metal0PBT lsumreleasesPBT lsumreleases_metal1PBT lsumreleases_metal0PBT{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E9
********************************************************************************
foreach depvar in lsumemissionPBT0 lsumemission_metal1PBT0 lsumemission_metal0PBT0 lsumreleasesPBT0 lsumreleases_metal1PBT0 lsumreleases_metal0PBT0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E10
********************************************************************************
foreach depvar in lsumemission lsumemission_lma1 lsumemission_lma0 lsumreleases lsumreleases_lma1 lsumreleases_lma0{
	reghdfe `depvar' rggi##treatone rggi##treattwo $control, absorb($absorbvar) vce(cluster $clustervar)
		nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E11
********************************************************************************
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
		reghdfe `depvar' rggi##treatone rggi##treattwo $control if noneic==1, absorb($absorbvar) vce(cluster $clustervar)
			nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

********************************************************************************
*Table E12
********************************************************************************
foreach depvar in lsumemission lsumemission_metal1 lsumemission_metal0 lsumreleases lsumreleases_metal1 lsumreleases_metal0{
		reghdfe `depvar' rggi##treatone rggi##treattwo $control if noneic==1 & st!="CA", absorb($absorbvar) vce(cluster $clustervar)
				nlcom (CombinedEffect: _b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo]) (Begin: (exp(_b[1.rggi#1.treatone])-1)*100) (Lower: (exp(_b[1.rggi#1.treattwo])-1)*100) (Total: (exp(_b[1.rggi#1.treatone]+_b[1.rggi#1.treattwo])-1)*100),post  
}

log close
