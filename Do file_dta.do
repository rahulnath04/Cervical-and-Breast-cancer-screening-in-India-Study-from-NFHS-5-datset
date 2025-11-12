// Use NFHS-5 IR dataset

keep if v481 == 1 

egen br_cer = rowtotal(sb79 sb80)


gen v445_new = v445/100
label variable v445_new "BMI (New)"
*Classification: Bmi
recode v445_new (min/18.5 = 0 "Thin")(18.5/25 = 1 "Normal")(25/max=3 "Overweight or Obesity")(9998 . = . ), gen(bmi)

recode v012 (15/29 = .)(30/39 = 1 "30-39")(40/49 = 2 "40-49"), gen(age)
ta age [aw=v005/100000]
recode v501 (1 = 1 "Married")(0 3/5 = 0 "Not Married"), gen(marriage)


recode v130 (1 = 1 "Hindu")(2=2 "Muslim")(3=3 "Christian")(4/max = 4 "Others"), gen(religion)

recode s116 (1 = 1 "ST")(2=2 "ST")(3=3 "OBC")(4 8 = 4 "Others"), gen(caste)
recode v445_new (min/18.5 = 0 "Underweight")(18.5/25 = 1 "Healthy Weight")(25/max=3 "Overweight or Obesity")(9998 . = . ), gen(bmi)
recode v312 (1/3 16 = 1 "Yes") (0 4/15 17/18 = 0 "No"), gen(horm_contra)
label variable horm_contra "Hormonal contraception"

recode s731d (0 = 0 "Never")(1=1 "Daily")(2/3 = .), gen(fruits)
recode v106 (0=0 "No education")(1=1 "Primary")(2=2 "Secondary")(3=3 "Higher secondary and above"), gen(edu)

// alcohol
ta s720
//tobacco
ta s711
//eats fried food
ta s731h
//reading newspaper/magazine
recode v157 (0 = 0 "No")(1/2 = 1 "Yes"), gen(v157_new)
//Listens to radio 
recode v158 (0 = 0 "No")(1/2 = 1 "Yes"), gen(v158_new)
//Watches television
recode v159 (0 = 0 "No")(1/2 = 1 "Yes"), gen(v159_new)
//Goes to cinema/theatre 
ta s113
egen media_cov = rowtotal(v157_new v158_new v159_new s113)
recode media_cov (0 = 0 "No coverage")(1/4 = 1 "Coverage"), gen(media_cov_new)

gen cr_or_br = (sb79 == 1 | sb80 == 1)
ta cr_or_br

gen cr_and_br = (sb79 == 1 & sb80 == 1)
ta cr_and_br


gen wt = v005/10000



gen residence = (v025 == 1)
label define resid_lbl 0 "Rural" 1 "Urban"
label values residence resid_lbl

// Logistic regression
logistic sb79 v025 age marriage religion caste v190 horm_contra bmi s720 s711 s731h fruits edu v151 media_cov_new [pw=wt]
logistic sb80 v025 age marriage religion caste v190 horm_contra bmi s720 s711 s731h fruits edu v151 media_cov_new [pw=wt]
logistic cr_or_br v025 age marriage religion caste v190 horm_contra bmi s720 s711 s731h fruits edu v151 media_cov_new [pw=wt]
logistic cr_and_br v025 age marriage religion caste v190 horm_contra bmi s720 s711 s731h fruits edu v151 media_cov_new [pw=wt]



// Decomposition Analysis

fairlie sb79 age marriage religion caste v190 horm_contra bmi s720 s711 s731h edu v151 media_cov_new [pw=wt], by(residence) reps(500)

fairlie sb80 age marriage religion caste v190 horm_contra bmi s720 s711 s731h edu v151 media_cov_new [pw=wt], by(residence) reps(500)

// Output: Decomposition analysis 
asdoc fairlie sb79 age marriage religion caste v190 horm_contra bmi s720 s711 s731h edu v151 media_cov_new [pw=wt], by(residence) reps(500) replace save(decomp_output.doc),landscape

asdoc fairlie sb80 age marriage religion caste v190 horm_contra bmi s720 s711 s731h edu v151 media_cov_new [pw=wt], by(residence) reps(500) replace save(decomp_output_1.doc),landscape





// Concentration Index and Concentration Curve
egen rank = rank(v190) // Rank with respect to the contribution the woman is putting forward in a household
gen rank_frac = rank / sum(v005) //percentile of wealth of each woman
conindex sb79 [pw=wt] if residence==1, rankvar(rank_frac) limits(0)
local ci_urb_cerv = string(r(C), "%6.3f")

preserve
keep if residence==1
sort rank_frac
egen tot_w = total(wt)
gen double pop_share = wt / tot_w
gen double cum_pop = sum(pop_share)
gen double health_w = sb79 * wt
egen double tot_health = total(health_w)
replace tot_health = . if tot_health==0
gen double cum_health = sum(health_w) / tot_health

twoway ///
 (line cum_health cum_pop, sort lwidth(medthick) lcolor(blue)) ///
 (function y=x, range(0 1) lpattern(dash) lcolor(gs8)), ///
 title("Urban — Cervical Screening") ///
 ytitle("Cumulative share of screening", size(2.5)) ///
 xtitle("", size(3)) /// no x label on top row
 xlabel(, labsize(2) noticks nolabels) /// hide x-axis labels
 ylabel(0(0.2)1, labsize(2)) ///
 legend(off) ///
 name(urb_cerv, replace)
restore


conindex sb79 [pw=wt] if residence==0, rankvar(rank_frac) limits(0)
local ci_rur_cerv = string(r(C), "%6.3f")

preserve
    keep if residence==0
    sort rank_frac
    egen double tot_w = total(wt)
    gen double pop_share = wt / tot_w
    gen double cum_pop = sum(pop_share)
    gen double health_w = sb79 * wt
    egen double tot_health = total(health_w)
    if tot_health==0 replace tot_health = 1
    gen double cum_health = sum(health_w) / tot_health
	
    twoway ///
    (line cum_health cum_pop, sort lwidth(medthick) lcolor(blue)) ///
    (function y=x, range(0 1) lpattern(dash) lcolor(gs8)), ///
    title("Rural — Cervical Screening") ///
	ytitle("", size(3)) ///
    xtitle("", size(3)) ///
    xlabel(0(0.2)1, labsize(2)) ///
    ylabel(, noticks nolabels) ///
    legend(off) ///
    name(rur_cerv, replace)

restore


conindex sb80 [pw=wt] if residence==1, rankvar(rank_frac) limits(0)
local ci_urb_breast = string(r(C), "%6.3f")

preserve
keep if residence==1
sort rank_frac
egen tot_w = total(wt)
gen double pop_share = wt / tot_w
gen double cum_pop = sum(pop_share)
gen double health_w = sb80 * wt
egen double tot_health = total(health_w)
replace tot_health = . if tot_health==0
gen double cum_health = sum(health_w) / tot_health

twoway ///
 (line cum_health cum_pop, sort lwidth(medthick) lcolor(blue)) ///
 (function y=x, range(0 1) lpattern(dash) lcolor(gs8)), ///
 title("Urban — Breast Screening") ///
 ytitle("Cumulative share of screening", size(2.5)) ///
 xtitle("Cumulative share of population", size(2.5)) ///
 xlabel(0(0.2)1, labsize(2)) ///
 ylabel(0(0.2)1, labsize(2)) ///
 legend(off) ///
 name(urb_breast, replace)
restore

conindex sb80 [pw=wt] if residence==0, rankvar(rank_frac) limits(0)
local ci_rur_breast = string(r(C), "%6.3f")

preserve
keep if residence==0
sort rank_frac
egen tot_w = total(wt)
gen double pop_share = wt / tot_w
gen double cum_pop = sum(pop_share)
gen double health_w = sb80 * wt
egen double tot_health = total(health_w)
replace tot_health = . if tot_health==0
gen double cum_health = sum(health_w) / tot_health

twoway ///
 (line cum_health cum_pop, sort lwidth(medthick) lcolor(blue)) ///
 (function y=x, range(0 1) lpattern(dash) lcolor(gs8)), ///
 title("Rural — Breast Screening") ///
 ytitle("", size(3)) ///
 xtitle("Cumulative share of population", size(3)) ///
 xlabel(0(0.2)1, labsize(2)) ///
 ylabel(, noticks nolabels) ///
 legend(off) ///
 name(rur_breast, replace)
restore

graph combine urb_cerv rur_cerv urb_breast rur_breast, ///
    cols(2) ///
    title("Concentration Curves by Residence and Cancer Type") ///
    subtitle("Dashed = Equality | Solid = Observed") ///
    ycommon xcommon ///
    imargin(2 2 2 2) ///
    iscale(1) ///
    name(conc_final, replace)



