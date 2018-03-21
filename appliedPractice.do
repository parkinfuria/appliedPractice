///Do file

describe
keep SEQN BMXWT BMXHT BMXBMI BMDBMIC BMIWAIST SDDSRVYR SDMVPSU SDMVSTRA RIAGENDR RIDAGEYR RIDAGEMN RIDAGEEX RIDRETH1 INDFMPIR DMDEDUC3 WTINT2YR WTMEC2YR SDMVPSU SDMVSTRA WTDR2D WTDRD1 DR1DRSTZ DRQSDT1 DRQSDT2 DRQSDT3 DRQSDT4 DRQSDT5 DRQSDT6 DR1TKCAL DR1TCARB DR1TPROT DR1TSUGR DR1TFIBE DR1TTFAT DR1TSFAT DR1TMFAT DR1TPFAT DR2TKCAL DR2TCARB DR2TPROT DR2TSUGR DR2TFIBE DR2TTFAT DR2TSFAT DR2TMFAT DR2TPFAT PAD200 PAD320 PAQ605 PAQ620 PAQ650 PAQ665 PAD590 PAQ710 PAD600 PAQ715 WHQ030M WHQ500 WHQ520

describe

///retained 58 variables from the total of 588 variables.
///date: 02/13/2018


//Generating the weight variable and assigning
gen WT10YR=(0.2)*WTDR2D

//Assigning survey weight
svyset [w=WT10YR], psu(SDMVPSU) strata(SDMVSTRA) vce(linearized)

*GENERATE WEIGHT PERCEPTION
gen wt_perception=.
replace wt_perception=0 if WHQ030M==3
replace wt_perception=1 if WHQ030M==2
replace wt_perception=2 if WHQ030M==1
tab wt_perception WHQ030M, m
label var wt_perception "0=just right, 1=too thin, 2=too fat"

/////CODE FOR CALCULATING CHILDREN BMI
findit zanthro

egen zscore = zanthro(BMXBMI,ba,US), xvar(RIDAGEYR) gender(RIAGENDR) gencode(male=1, female=2)

gen p=normal(zscore)
gen bmi_c_child = 1 if p < 0.05
replace bmi_c_child = 2 if (p >= 0.05 & p < 0.85)
replace bmi_c_child = 3 if (p >= 0.85 & p < 0.95)
replace bmi_c_child = 4 if (p >= 0.95 & p != .)
tab bmi_c_child, missing

label var bmi_c_child "1-Underweight, 2-Normal Weight, 3-Overweight, 4-Obese"
tab bmi_c_child if RIDAGEYR>7 & RIDAGEYR<16
///total is 7686

//label define bmichild 1"Underweight" 2"Normal Weight" 3"Overweight" 4"Obese"
//label values bmi_c_child bmichild
codebook bmi_c_child

gen child_bmi=.
replace child_bmi=0 if bmi_c_child==2
replace child_bmi=1 if bmi_c_child==3
replace child_bmi=2 if bmi_c_child==4
tab child_bmi if sample_final==1


//Trying to do about weight
preserve
tab WHQ500, missing
recode WHQ030M (1=0) (2=1) (3=2) (4=3) (7=.) (9=.), gen(try)
tab try, missing
//drop if try==.
//tab try
restore



//How often tried to lose weight
*preserve
*tab WHQ520, missing
*recode WHQ520 (1=0) (2=1) (3=2) (9=.), gen(oftentry)
*tab oftentry, missing
//drop if oftentry==.
//tab oftentry
*restore

//GENDER - no missing values
tab RIAGENDR if sample_final==1

//AGE- no missing values
tab RIDAGEYR if sample_final==1

///race

tab RIDRETH1 if sample_final==1

gen race=.
replace race=0 if RIDRETH1==3 & sample_final==1
replace race=1 if RIDRETH1==4 & sample_final==1
replace race=2 if RIDRETH1==1 & sample_final==1
replace race=3 if (RIDRETH1==2 | RIDRETH1==5) & sample_final==1
tab race
label var race "0=White, 1=Black, 2=Mexican, 3=Other"
***added other hispanics to other section while recoding

tab race if sample_final==1
tab race if RIAGENDR==1
tab race if RIAGENDR==2

///FAMILY PIR

tab INDFMPIR if sample_final==1

///dividing it into 3 categories
//0- <130
//1- 1.30 to 3.49
//2- 350 or more

gen familypir=.
replace familypir=0 if INDFMPIR<1.30
replace familypir=1 if INDFMPIR>1.29 & INDFMPIR<3.50
replace familypir=2 if INDFMPIR>3.49

tab familypir if sample_final==1
tab familypir if sample_final==1 & RIAGENDR==1
tab familypir if sample_final==1 & RIAGENDR==2

//BMI- Percentile in children

/////CODE FOR CALCULATING CHILDREN BMI
findit zanthro

egen zscore = zanthro(BMXBMI,ba,US), xvar(RIDAGEYR) gender(RIAGENDR) gencode(male=1, female=2)

gen p=normal(zscore)
gen bmi_c_child = 1 if p < 0.05
replace bmi_c_child = 2 if (p >= 0.05 & p < 0.85)
replace bmi_c_child = 3 if (p >= 0.85 & p < 0.95)
replace bmi_c_child = 4 if (p >= 0.95 & p != .)
tab bmi_c_child, missing

label var bmi_c_child "1-Underweight, 2-Normal Weight, 3-Overweight, 4-Obese"
tab bmi_c_child if RIDAGEYR>7 & RIDAGEYR<16
///total is 7686

*label define bmichild 1"Underweight" 2"Normal Weight" 3"Overweight" 4"Obese"
*label values bmi_c_child bmichild
*codebook bmi_c_child

gen sample_wtp=.
replace sample_wtp=1 if wt_perception!=.
tab sample_wtp
*n=7297

gen sample_wtp_bmi=.
replace sample_wtp_bmi=1 if sample_wtp==1 & bmi_c_child!=.
tab sample_wtp_bmi
*n=7265

gen sample_final=.
replace sample_final=1 if sample_wtp_bmi==1 & bmi_c_child!=1
tab sample_final
*n=7094

gen bmi_perception=0
replace bmi_perception=1 if (bmi_c_child==1 & wt_perception==2) | (bmi_c_child==2 & wt_perception==3) | (bmi_c_child==3 & wt_perception==1) | (bmi_c_child==4 & wt_perception==1)
replace bmi_perception=. if sample_wtp_bmi==.
tab bmi_perception

gen bmi_p_mismatch=0
replace bmi_p_mismatch=1 if (bmi_c_child==2 & wt_perception==1) | (bmi_c_child==3 & wt_perception==1) | (bmi_c_child==4 & wt_perception==1) | (bmi_c_child==3 & wt_perception==0) | (bmi_c_child==4 & wt_perception==0)
replace bmi_p_mismatch=2 if (bmi_c_child==2 & wt_perception==2) | (bmi_c_child==1 & wt_perception==2)
tab bmi_p_mismatch if sample_final==1

label var bmi_p_mismatch "0-No mismatch, 1-Thought to be thinner, 2-Thought to be heavier"

///age categorical
gen age=.
replace age=1 if (RIDAGEYR==8 | RIDAGEYR==9 | RIDAGEYR==10 | RIDAGEYR==11) & sample_final==1
replace age=2 if (RIDAGEYR==11 | RIDAGEYR==12 | RIDAGEYR==13 | RIDAGEYR==14 | RIDAGEYR==15) & sample_final==1
tab age
label var age "1=8-11 years, 2=12-15 years"

tab age if sample_final==1
tab age if RIAGENDR==1
tab age if RIAGENDR==2

//Average of calories variable
sum DR1TKCAL if sample_final==1
sum DR2TKCAL if sample_final==1
gen avg_cal=(DR1TKCAL+DR2TKCAL)/2
sum avg_cal if sample_final==1
*n=5921

//Average Carbs
sum DR1TCARB
sum DR2TCARB
gen avg_carb=(DR1TCARB+DR2TCARB)/2
sum avg_carb if sample_final==1, d

//Average Fats
sum DR1TTFAT
sum DR2TTFAT
gen avg_fat=(DR1TTFAT+DR2TTFAT)/2
sum avg_fat if sample_final==1, d

//Average Protein
sum DR1TPROT
sum DR2TPROT
gen avg_prot=(DR1TPROT+DR2TPROT)/2
sum avg_prot if sample_final==1, d

//Average Sugar
sum DR1TSUGR
sum DR2TSUGR
gen avg_sugar=(DR1TSUGR+DR2TSUGR)/2
sum avg_sugar if sample_final==1, d

//Average Fiber
sum DR1TFIBE
sum DR2TFIBE
gen avg_fiber=(DR1TFIBE+DR2TFIBE)/2
sum avg_fiber if sample_final==1, d

//Average Saturated Fats
sum DR1TSFAT
sum DR2TSFAT
gen avg_sfat=(DR1TSFAT+DR2TSFAT)/2
sum avg_sfat if sample_final==1, d

//Average Monounsaturated fats
sum DR1TMFAT
sum DR2TMFAT
gen avg_mfat=(DR1TMFAT+DR2TMFAT)/2
sum avg_mfat if sample_final==1, d

//Average Polyunsaturated Fats
sum DR1TPFAT
sum DR2TPFAT
gen avg_pfat=(DR1TPFAT+DR2TPFAT)/2
sum avg_pfat if sample_final==1, d

///Percent Fat, carbs, protein
gen pct_fat = ((avg_fat*9)/avg_cal) *100
sum pct_fat if sample_final==1, d

gen pct_carb= ((avg_carb*4)/avg_cal) *100
sum pct_carb if sample_final==1, d

gen pct_prot= ((avg_prot*4)/avg_cal) *100
sum pct_prot if sample_final==1, d

//Plausible calories
gen plausible_calories= avg_cal 
replace plausible_calories=. if (avg_cal<500)
replace plausible_calories=. if (avg_cal>5000)
sum plausible_calories if sample_final==1
*n=5897

*Sample excluding implausible calorie intakes
gen final_sample_cals=sample_final
replace final_sample_cals=. if plausible_calories==.
tab final_sample_cals

//Physical Activity variables
///Physical Activity vairbales

//combining variables for Vigorous PA
tab PAD200
tab PAQ650

gen vigpa=.
replace vigpa=1 if PAD200==1 | PAQ650==1
replace vigpa=0 if PAD200==2 | PAQ650==2
//replace vigpa=___ if PAD200==3 | PAQ650==3
tab vigpa if sample_final==1

lab var vigpa "0=No, 1=Yes"

//combining variables for Moderate physical activity

tab PAD320
tab PAQ665

gen modpa=.
replace modpa=1 if PAD320==1 | PAQ665==1
replace modpa=0 if PAD320==2 | PAQ665==2
//replace modpa=___ if PAD320==3 | PAQ665==3
tab modpa

label var modpa "0=No, 1=Yes"


//Combining moderate and vigorous PA

gen mvpa=.
replace mvpa=0 if (vigpa==0 | modpa==0)
replace mvpa=1 if (vigpa==1 | modpa==1)
tab mvpa

label var mvpa "0=No, 1=Yes"

tab mvpa if sample_final==1
tab mvpa if sample_final==1 & RIAGENDR==1
tab mvpa if sample_final==1 & RIAGENDR==2

////sedantary time- combining hours of watching tv and on computer

///TV-variables - PAD590 & PAQ710
///computer variables- PAD600 & PAQ715

//categories will be as follows
//0-less than 1 hour==0
///1 hour - upto to 3 hours ==1
///more than 3 hours ==2

gen sedan=.
replace sedan=0 if PAD590==5.40e-79 | PAQ710==5.40e-79 |  PAD600==5.40e-79 | PAQ715==5.40e-79 | PAD590==6 | PAQ710==8 |  PAD600==6 | PAQ715==8 
replace sedan=1 if PAD590==1 |  PAD590==2 |  PAD590==3 |  PAQ710==1 | PAQ710==2 | PAQ710==3 | PAD600==1 | PAD600==2 | PAD600==3 | PAQ715==1 | PAQ715==2 | PAQ715==3 
replace sedan=2 if PAD590==4 | PAD590==5 | PAQ710==4 | PAQ710==5 | PAD600==4 | PAD600==5 | PAQ715==4 | PAQ715==5 

label var sedan "0=None or less than 1 hour, 1=1 to 3 hours, 2=4 hours or more"

tab sedan if sample_final==1

tab sedan if sample_final==1 & RIAGENDR==1
tab sedan if sample_final==1 & RIAGENDR==2

//UNIVARIATE:

//applying weights
svyset [w=WT10YR], psu(SDMVPSU) strata(SDMVSTRA) vce(linearized)


//Age-continuous


svy: mean RIDAGEYR if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean RIDAGEYR if final_sample_cals==1 & RIAGENDR==2
estat sd


//age-categorical
tab age if final_sample_cals==1 & RIAGENDR==1
tab age if final_sample_cals==1 & RIAGENDR==2
svy: tab age if final_sample_cals==1 & RIAGENDR==1
svy: tab age if final_sample_cals==1 & RIAGENDR==2

//race
tab race if final_sample_cals==1 & RIAGENDR==1
tab race if final_sample_cals==1 & RIAGENDR==2
svy: tab race if final_sample_cals==1 & RIAGENDR==1
svy: tab race if final_sample_cals==1 & RIAGENDR==2

//Family PIR
tab familypir if final_sample_cals==1 & RIAGENDR==1
tab familypir if final_sample_cals==1 & RIAGENDR==2
svy: tab familypir if final_sample_cals==1 & RIAGENDR==1
svy: tab familypir if final_sample_cals==1 & RIAGENDR==2

//BMI status
tab bmi_c_child if final_sample_cals==1 & RIAGENDR==1
tab bmi_c_child if final_sample_cals==1 & RIAGENDR==2
svy: tab bmi_c_child if final_sample_cals==1 & RIAGENDR==1
svy: tab bmi_c_child if final_sample_cals==1 & RIAGENDR==2

//Wt perception
tab wt_perception if final_sample_cals==1 & RIAGENDR==1
tab wt_perception if final_sample_cals==1 & RIAGENDR==2
svy: tab wt_perception if final_sample_cals==1 & RIAGENDR==1
svy: tab wt_perception if final_sample_cals==1 & RIAGENDR==2

//BMI-weight perception mismatch
tab bmi_p_mismatch if sample_final==1 & RIAGENDR==1
tab bmi_p_mismatch if sample_final==1 & RIAGENDR==2
svy: tab bmi_p_mismatch if final_sample_cals==1 & RIAGENDR==1
svy: tab bmi_p_mismatch if final_sample_cals==1 & RIAGENDR==2

//Physical activity
tab mvpa if final_sample_cals==1 & RIAGENDR==1
tab mvpa if final_sample_cals==1 & RIAGENDR==2
svy: tab mvpa if final_sample_cals==1 & RIAGENDR==1
svy: tab mvpa if final_sample_cals==1 & RIAGENDR==2

//Sedantary lifestyle
tab sedan if final_sample_cals==1 & RIAGENDR==1
tab sedan if final_sample_cals==1 & RIAGENDR==2
svy: tab sedan if final_sample_cals==1 & RIAGENDR==1
svy: tab sedan if final_sample_cals==1 & RIAGENDR==2

//Calorie Intake
svy: mean avg_cal if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_cal if final_sample_cals==1 & RIAGENDR==2
estat sd

//Protein Intake
svy: mean avg_prot if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_prot if final_sample_cals==1 & RIAGENDR==2
estat sd

//Fat Intake
svy: mean avg_fat if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_fat if final_sample_cals==1 & RIAGENDR==2
estat sd

//Carbohydrate Intake
svy: mean avg_carb if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_carb if final_sample_cals==1 & RIAGENDR==2
estat sd

//Sugar Intake
svy: mean avg_sugar if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_sugar if final_sample_cals==1 & RIAGENDR==2
estat sd

//Fiber Intake
svy: mean avg_fiber if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_fiber if final_sample_cals==1 & RIAGENDR==2
estat sd

//Saturated fat intake
svy: mean avg_sfat if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_sfat if final_sample_cals==1 & RIAGENDR==2
estat sd

//Monounsaturated fat Intake
svy: mean avg_mfat if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_mfat if final_sample_cals==1 & RIAGENDR==2
estat sd

//Polyunsaturated fat Intake
svy: mean avg_pfat if final_sample_cals==1 & RIAGENDR==1
estat sd
svy: mean avg_pfat if final_sample_cals==1 & RIAGENDR==2
estat sd

//Bivariable analysis


svy: 
svy: tab bmi_c_child wt_perception if sample_final==1 & RIAGENDR==1, row
svy: tab bmi_c_child wt_perception if sample_final==1 & RIAGENDR==2, row

//bivariable of calorie intake with weight perception and other covariates
putexcel set "bivariate", modify
svy: regress avg_cal RIDAGEYR if final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.age if final_sample_cals==1 
return list
putexcel A12=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal RIAGENDR if final_sample_cals==1 
return list
putexcel A22=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.race if final_sample_cals==1 
return list
putexcel A32=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.familypir if final_sample_cals==1 
return list
putexcel A42=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.mvpa if final_sample_cals==1 
return list
putexcel A52=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.sedan if final_sample_cals==1 
return list
putexcel A62=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.bmi_c_child if final_sample_cals==1 
return list
putexcel A72=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.wt_perception if final_sample_cals==1 
return list
putexcel A82=matrix(r(table)'), names

putexcel set "bivariate", modify
svy: regress avg_cal i.bmi_p_mismatch if final_sample_cals==1 
return list
putexcel A92=matrix(r(table)'), names

//bivariable of Fat intake with weight perception and other covariates
putexcel set "bivariate-fat", modify
svy: regress avg_fat RIDAGEYR if final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names

svy: regress avg_fat i.age if final_sample_cals==1 
return list
putexcel A12=matrix(r(table)'), names

svy: regress avg_fat RIAGENDR if final_sample_cals==1 
return list
putexcel A22=matrix(r(table)'), names

svy: regress avg_fat i.race if final_sample_cals==1 
return list
putexcel A32=matrix(r(table)'), names

svy: regress avg_fat i.familypir if final_sample_cals==1 
return list
putexcel A42=matrix(r(table)'), names

svy: regress avg_fat i.mvpa if final_sample_cals==1 
return list
putexcel A52=matrix(r(table)'), names

svy: regress avg_fat i.sedan if final_sample_cals==1 
return list
putexcel A62=matrix(r(table)'), names

svy: regress avg_fat i.bmi_c_child if final_sample_cals==1 
return list
putexcel A72=matrix(r(table)'), names

svy: regress avg_fat i.wt_perception if final_sample_cals==1 
return list
putexcel A82=matrix(r(table)'), names

svy: regress avg_fat i.bmi_p_mismatch if final_sample_cals==1 
return list
putexcel A92=matrix(r(table)'), names

//bivariable of Protein intake with weight perception and other covariates
putexcel set "bivariate-protein", modify
svy: regress avg_prot RIDAGEYR if final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names

svy: regress avg_prot i.age if final_sample_cals==1 
return list
putexcel A12=matrix(r(table)'), names

svy: regress avg_prot RIAGENDR if final_sample_cals==1 
return list
putexcel A22=matrix(r(table)'), names

svy: regress avg_prot i.race if final_sample_cals==1 
return list
putexcel A32=matrix(r(table)'), names

svy: regress avg_prot i.familypir if final_sample_cals==1 
return list
putexcel A42=matrix(r(table)'), names

svy: regress avg_prot i.mvpa if final_sample_cals==1 
return list
putexcel A52=matrix(r(table)'), names

svy: regress avg_prot i.sedan if final_sample_cals==1 
return list
putexcel A62=matrix(r(table)'), names

svy: regress avg_prot i.bmi_c_child if final_sample_cals==1 
return list
putexcel A72=matrix(r(table)'), names

svy: regress avg_prot i.wt_perception if final_sample_cals==1 
return list
putexcel A82=matrix(r(table)'), names

svy: regress avg_prot i.bmi_p_mismatch if final_sample_cals==1 
return list
putexcel A92=matrix(r(table)'), names



putexcel set "bivariate", modify
svy: regress avg_cal i.bmi_p_mismatch if final_sample_cals==1
return list
putexcel A82=matrix(r(table)'), names

svy: regress avg_cal bmi_c_child if final_sample_cals==1
svy: proportion wt_perception avg_cal if final_sample_cals==1


svy: mean DR1TKCAL if RIAGENDR==2 & bmi_c_child==4 & wt_perception==0 & sample_final==1 
svy: tab avg_cal bmi_c_child if final_sample_cals==1 & RIAGENDR==1
svy: regress avg_cal wt_perception if final_sample_cals==1 & RIAGENDR==1
svy: regress avg_cal wt_perception if final_sample_cals==1 & RIAGENDR==1
svy: regress avg_cal wt_perception if final_sample_cals==1 & RIAGENDR==1
svy: regress avg_cal wt_perception if final_sample_cals==1 & RIAGENDR==1


//Multilinear regression

svyset [w=WT10YR], psu(SDMVPSU) strata(SDMVSTRA) vce(linearized)

//FOR BOYS in all age groups

regress avg_cal i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_carb i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fat i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_prot i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_sugar i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fibe i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1

putexcel set "multivariable", modify
svy: regress avg_sugar i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names

svy: regress avg_fibe i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
return list
putexcel A24=matrix(r(table)'), names

putexcel set "multivariable", modify
svy: regress avg_fat i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir if RIAGENDR==1 & final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names


///FOR GIRLS in all age groups

svy: regress avg_cal i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_carb i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fat i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_prot i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_sugar i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fibe i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if RIAGENDR==1 & final_sample_cals==1

putexcel set "multivariable", modify
svy: regress avg_cal RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A2=matrix(r(table)'), names

svy: regress avg_carb RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A24=matrix(r(table)'), names


svy: regress avg_fat RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A54=matrix(r(table)'), names


svy: regress avg_prot RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A84=matrix(r(table)'), names


svy: regress avg_sugar RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A114=matrix(r(table)'), names


svy: regress avg_fibe RIDAGEYR i.race i.familypir i.bmi_c_child i.wt_perception i.bmi_p_mismatch if RIAGENDR==2 & final_sample_cals==1
return list
putexcel A134=matrix(r(table)'), names



///Multiple linear regression in Boys aged 8-11 years and NO physical activity.

//generating a new value for subpopulation

svyset [w=WT10YR], psu(SDMVPSU) strata(SDMVSTRA) vce(linearized)

gen boysunder12=.
replace boysunder12=1 if age==1 & RIAGENDR==1 & final_sample_cals==1

tab boysunder12,m 

regress avg_cal i.wt_perception i.bmi_c_child i.race i.familypir if age==1 & RIAGENDR==1 & final_sample_cals==1



svy, subpop(if final_sample_cals==1 & RIAGENDR==1 & age==1): regress avg_cal i.wt_perception i.bmi_c_child i.race i.familypir
regress avg_cal i.wt_perception i.bmi_c_child i.race i.familypir if RIAGENDR==1 & final_sample_cals==1 & age==1


svy: regress avg_cal i.wt_perception i.bmi_c_child i.race i.familypir if boysunder12==1

svy: regress avg_carb i.wt_perception i.bmi_c_child i.race  i.familypir i.bmi_p_mismatch if age==1 & RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fat i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if age==1 RIAGENDR==1 & final_sample_cals==1
svy: regress avg_prot i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if age==1 RIAGENDR==1 & final_sample_cals==1
svy: regress avg_sugar i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if age==1 RIAGENDR==1 & final_sample_cals==1
svy: regress avg_fibe i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.bmi_p_mismatch if age==1 RIAGENDR==1 & final_sample_cals==1

svy: regress avg_cal i.wt_perception i.race i.wt_perception##i.bmi_c_child if RIAGENDR==1 & final_sample_cals==1

svy: regress avg_cal i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir if RIAGENDR==1 & final_sample_cals==1

svy: regress avg_cal i.wt_perception i.bmi_c_child i.race RIDAGEYR i.familypir i.RIAGENDR if age==1 & final_sample_cals==1


regress DR1TKCAL i.wt_perception##i.bmi_c_child i.race RIDAGEYR if RIAGENDR==2 & final_sample_cals==1
testparm i.wt_perception#i.bmi_c_child

putexcel set "bivariate", modify
svy: mean avg_mfat if sample_final==1 & RIAGENDR==1
estat sd
putexcel A1 = ("Name of Variable") C1=("Beta Coefficient") D1=("95% Confidence Interval") E1=("P-value")
putexcel A2 = matrix() B2=matrix(mean) C3=matrix(Std. Dev) D2=matrix(95% Cnnfidence Interval)


reg variable1 variable2
return list
putexcel B2=matrix(r(table)'), names

putexcel set "thesis_table", modify
svy: tab mvpa if sample_final==1 & RIAGENDR==1
svy: tab mvpa if sample_final==1 & RIAGENDR==2
putexcel A70=matrix(r(table)'), names



*analysis for coming to the final sample size*

tab wt_perception
//n=7297
tab bmi_c_child if wt_perception!=.
//n=7,265 (32)
tab bmi_c_child if wt_perception!=. & bmi_c_child!=1
//n=7,094..(171)....removed 203 from 7297, the underweight observations as it would be a very small size in future analysis.
sum avg_cal if sample_wtp_bmi==1 & bmi_c_child!=1
//n=5,921 (excluded 1173)
tab avg_cal if (avg_cal<=500) & sample_wtp_bmi==1 & bmi_c_child!=1
// n=12
sum avg_cal if (avg_cal>=5000) & sample_wtp_bmi==1 & bmi_c_child!=1
/// n=12
//total of 5,897
tab RIAGENDR if final_sample_cals==1 
//boys=2,956
//girls=2,941

//multiple linear regression


