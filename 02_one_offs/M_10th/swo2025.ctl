#C_2025_SWO
#C file created using an r4ss function
#C file write time: 2025-07-08  11:33:25
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
3 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
8 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	 2	1	0	#_recr_dist_pattern1
1	 2	2	0	#_recr_dist_pattern2
1	 5	1	0	#_recr_dist_pattern3
1	 5	2	0	#_recr_dist_pattern4
1	 8	1	0	#_recr_dist_pattern5
1	 8	2	0	#_recr_dist_pattern6
1	11	1	0	#_recr_dist_pattern7
1	11	2	0	#_recr_dist_pattern8
#
8 #_N_movement_definitions goes here if N_areas > 1
0 #_first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_move definition for seas, morph, source, dest, age1, age2
1	1	1	2	1	20	#_moveDef1
2	1	1	2	1	20	#_moveDef2
3	1	1	2	1	20	#_moveDef3
4	1	1	2	1	20	#_moveDef4
1	1	2	1	1	20	#_moveDef5
2	1	2	1	1	20	#_moveDef6
3	1	2	1	1	20	#_moveDef7
4	1	2	1	1	20	#_moveDef8
1 #_Nblock_Patterns
1 #_blocks_per_pattern
#_begin and end years of blocks
1952 2023
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
0 0 0 0 0 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
2 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen
7 #_reference age for Lorenzen M; later read 1P per Sex x G Morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.25 #_Age(post-settlement)_for_L1;linear growth below this
15 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
# Age Maturity or Age fecundity:
#_Age_0	Age_1	Age_2	Age_3	Age_4	Age_5	Age_6	Age_7	Age_8	Age_9	Age_10	Age_11	Age_12	Age_13	Age_14	Age_15	Age_16	Age_17	Age_18	Age_19
0.00135852	0.0060598	0.026597	0.109097	0.354344	0.71095	0.916827	0.98016	0.995504	0.998993	0.999775	0.99995	0.999989	0.999998	0.999999	1	1	1	1	1	#_Age_Maturity1
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
 0.05	0.45	   0.172544	        0.3	 99	0	 -3	0	0	0	0	0	0	0	#_NatM_p_1_Fem_GP_1                 
   30	 150	     41.683	         30	 99	0	  4	0	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1                
  130	 300	    244.063	        160	 99	0	  4	0	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1                
 0.01	 0.5	   0.244668	   0.190777	 99	0	  4	0	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1                
 0.01	 0.5	      0.148	        0.1	 99	0	 -3	0	0	0	0	0	0	0	#_CV_young_Fem_GP_1                 
 0.01	 0.5	      0.074	       0.15	 99	0	 -3	0	0	0	0	0	0	0	#_CV_old_Fem_GP_1                   
   -3	   3	6.56553e-06	6.56553e-06	 99	0	 -3	0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1                  
   -3	   4	    3.13951	    3.13951	 99	0	 -3	0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1                  
   15	  30	         25	         25	 99	0	 -3	0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1                   
   -3	   3	       -0.2	       -0.2	 99	0	 -3	0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1                
   -3	   3	          1	          1	 99	0	 -3	0	0	0	0	0	0	0	#_Eggs_alpha_Fem_GP_1               
   -3	   4	          0	          0	 99	0	 -3	0	0	0	0	0	0	0	#_Eggs_beta_Fem_GP_1                
   -1	   1	   0.107187	        0.3	 99	0	 -3	0	0	0	0	0	0	0	#_NatM_p_1_Mal_GP_1                 
   -1	   1	          0	          0	 99	0	 -4	0	0	0	0	0	0	0	#_L_at_Amin_Mal_GP_1                
   -1	   1	  -0.221605	          0	 99	0	 -4	0	0	0	0	0	0	0	#_L_at_Amax_Mal_GP_1                
   -1	   1	   0.570508	          0	 99	0	 -4	0	0	0	0	0	0	0	#_VonBert_K_Mal_GP_1                
   -1	   1	   0.156079	          0	 99	0	 -3	0	0	0	0	0	0	0	#_CV_young_Mal_GP_1                 
   -1	   1	 -0.0555699	          0	 99	0	 -3	0	0	0	0	0	0	0	#_CV_old_Mal_GP_1                   
   -3	   3	8.94239e-06	8.94239e-06	 99	0	 -3	0	0	0	0	0	0	0	#_Wtlen_1_Mal_GP_1                  
   -3	   4	    3.06302	    3.06302	 99	0	 -3	0	0	0	0	0	0	0	#_Wtlen_2_Mal_GP_1                  
   -4	   4	       -0.5	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_2      
   -4	   4	       -0.5	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_2_month_2      
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_5      
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_2_month_5      
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_8      
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_2_month_8      
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_11     
   -4	   4	      -1000	          0	 99	0	 -2	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_2_month_11     
  0.1	  10	          1	          1	  1	0	 -1	0	0	0	0	0	0	0	#_CohortGrowDev                     
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_1_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_1_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_2_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_2_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_3_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_3_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_4_GP_1_from_1_to_2
   -5	   4	   -2.59027	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_4_GP_1_from_1_to_2
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_1_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_1_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_2_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_2_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_3_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_3_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_A_seas_4_GP_1_from_2_to_1
   -5	   4	   -3.68888	          0	 99	0	 -3	0	0	0	0	0	0	0	#_MoveParm_B_seas_4_GP_1_from_2_to_1
1e-06	0.99	        0.5	        0.5	0.5	0	-99	0	0	0	0	0	0	0	#_FracFemale_GP_1                   
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; 2=Ricker (2 parms); 3=std_B-H(2); 4=SCAA(2);5=Hockey(3); 6=B-H_flattop(2); 7=Survival(3);8=Shepard(3);9=Ricker_Power(3);10=B-H_a,b(4)
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn # parm_name
  5	18	9.46556	 14	  5	0	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
0.2	 1	    0.8	0.8	0.2	0	 -5	0	0	0	0	0	0	0	#_SR_BH_steep
 -2	 2	    0.5	0.8	0.5	0	 -3	0	0	0	0	0	0	0	#_SR_sigmaR  
 -5	 5	      0	  0	  1	0	 -4	0	0	0	0	0	0	0	#_SR_regime  
  0	 2	      0	  1	 50	0	-50	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1995 # first year of main recr_devs; early devs can preceed this era
2021 # last year of main recr_devs; forecast devs start in following year
4 #_recdev phase
1 # (0/1) to read 13 advanced options
0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
1 #_recdev_early_phase
0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1966.3 #_last_yr_nobias_adj_in_MPD; begin of ramp
1996 #_first_yr_fullbias_adj_in_MPD; begin of plateau
2017 #_last_yr_fullbias_adj_in_MPD
2028.1 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.998 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-5 #min rec_dev
5 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.2 # F ballpark
2001 # F ballpark year (neg value to disable)
4 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
2.9 # max F or harvest rate, depends on F_Method
#_fleet	start_F	first_parm_phase
    1	0.1	99	#_F_4_Fleet_Parms1 
    2	0.1	99	#_F_4_Fleet_Parms2 
    3	0.1	99	#_F_4_Fleet_Parms3 
    4	0.1	99	#_F_4_Fleet_Parms4 
    5	0.1	99	#_F_4_Fleet_Parms5 
    6	0.1	99	#_F_4_Fleet_Parms6 
    7	0.1	99	#_F_4_Fleet_Parms7 
    8	0.1	99	#_F_4_Fleet_Parms8 
    9	0.1	99	#_F_4_Fleet_Parms9 
   10	0.1	99	#_F_4_Fleet_Parms10
   11	0.1	99	#_F_4_Fleet_Parms11
   12	0.1	99	#_F_4_Fleet_Parms12
   13	0.1	99	#_F_4_Fleet_Parms13
   14	0.1	99	#_F_4_Fleet_Parms14
   15	0.1	99	#_F_4_Fleet_Parms15
   16	0.1	99	#_F_4_Fleet_Parms16
   17	0.1	99	#_F_4_Fleet_Parms17
   18	0.1	99	#_F_4_Fleet_Parms18
   19	0.1	99	#_F_4_Fleet_Parms19
-9999	  0	 0	#_terminator       
4 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#
#_Q_setup for fleets with cpue or survey data
#_fleet	link	link_info	extra_se	biasadj	float  #  fleetname
   20	1	0	1	0	1	#_20.IDX.AU.1
   21	1	0	1	0	1	#_21.IDX.NZ.2
-9999	0	0	0	0	0	#_terminator 
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
 -20	 15	  -10.5671	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_20.IDX.AU.1(20) 
-0.2	0.5	-0.0534666	0	1	0	 4	0	0	0	0	0	0	0	#_Q_extraSD_20.IDX.AU.1(20)
 -20	 15	  -14.4344	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_21.IDX.NZ.2(21) 
-0.2	0.5	 0.0583826	0	1	0	 4	0	0	0	0	0	0	0	#_Q_extraSD_21.IDX.NZ.2(21)
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
24	0	0	 0	#_1 01.DW.1N    
24	0	0	 0	#_2 02.DW.1C    
 1	0	0	 0	#_3 03.DW.1S    
24	0	0	 0	#_4 04.AU.1N    
15	0	0	 2	#_5 05.AU.1C    
15	0	0	 3	#_6 06.AU.1S    
15	0	0	 2	#_7 07.EU.1C    
15	0	0	 1	#_8 08.PICT.1N  
15	0	0	 2	#_9 09.PICT.1C  
15	0	0	 1	#_10 10.DW.2N   
24	0	0	 0	#_11 11.DW.2C   
 1	0	0	 0	#_12 12.DW.2S   
24	0	0	 0	#_13 13.NZ.2C   
15	0	0	12	#_14 14.NZ.2S   
15	0	0	 1	#_15 15.EU.2N   
15	0	0	11	#_16 16.EU.2C   
15	0	0	12	#_17 17.EU.2S   
15	0	0	 1	#_18 18.PICT.2N 
15	0	0	11	#_19 19.PICT.2C 
15	0	0	 5	#_20 20.IDX.AU.1
15	0	0	13	#_21 21.IDX.NZ.2
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
0	0	0	0	#_1 01.DW.1N    
0	0	0	0	#_2 02.DW.1C    
0	0	0	0	#_3 03.DW.1S    
0	0	0	0	#_4 04.AU.1N    
0	0	0	0	#_5 05.AU.1C    
0	0	0	0	#_6 06.AU.1S    
0	0	0	0	#_7 07.EU.1C    
0	0	0	0	#_8 08.PICT.1N  
0	0	0	0	#_9 09.PICT.1C  
0	0	0	0	#_10 10.DW.2N   
0	0	0	0	#_11 11.DW.2C   
0	0	0	0	#_12 12.DW.2S   
0	0	0	0	#_13 13.NZ.2C   
0	0	0	0	#_14 14.NZ.2S   
0	0	0	0	#_15 15.EU.2N   
0	0	0	0	#_16 16.EU.2C   
0	0	0	0	#_17 17.EU.2S   
0	0	0	0	#_18 18.PICT.2N 
0	0	0	0	#_19 19.PICT.2C 
0	0	0	0	#_20 20.IDX.AU.1
0	0	0	0	#_21 21.IDX.NZ.2
#
#_SizeSelex
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
  40	300	  144.327	 100	99	0	 2	0	0	0	0	  0	0	0	#_SizeSel_P_1_01.DW.1N(1) 
 -10	 10	 -1.24687	 -10	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_01.DW.1N(1) 
 -10	 10	  7.29677	   0	99	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_3_01.DW.1N(1) 
 -10	 10	  7.38285	   0	99	0	 5	0	0	0	0	  0	0	0	#_SizeSel_P_4_01.DW.1N(1) 
-999	  9	     -999	-999	99	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_01.DW.1N(1) 
-999	  9	 -1.10639	   0	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_6_01.DW.1N(1) 
  40	300	  125.472	 100	99	0	 2	0	0	0	0	  0	0	0	#_SizeSel_P_1_02.DW.1C(2) 
 -10	 10	-0.209581	 -10	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_02.DW.1C(2) 
 -10	 10	  6.70844	   0	99	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_3_02.DW.1C(2) 
 -10	 10	  9.99585	   0	99	0	 5	0	0	0	0	  0	0	0	#_SizeSel_P_4_02.DW.1C(2) 
-999	  9	     -999	-999	99	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_02.DW.1C(2) 
-999	  9	 -495.013	   0	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_6_02.DW.1C(2) 
  40	300	  150.098	 100	99	0	 3	0	0	0	0	0.5	0	0	#_SizeSel_P_1_03.DW.1S(3) 
   1	100	  99.8585	  10	99	0	 3	0	0	0	0	0.5	0	0	#_SizeSel_P_2_03.DW.1S(3) 
  40	300	  144.327	 100	99	0	 2	0	0	0	0	  0	0	0	#_SizeSel_P_1_04.AU.1N(4) 
 -10	 10	 -1.24687	 -10	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_04.AU.1N(4) 
 -10	 10	  7.29677	   0	99	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_3_04.AU.1N(4) 
 -10	 10	  7.38285	   0	99	0	 5	0	0	0	0	  0	0	0	#_SizeSel_P_4_04.AU.1N(4) 
-999	  9	     -999	-999	99	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_04.AU.1N(4) 
-999	  9	 -1.10639	   0	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_6_04.AU.1N(4) 
  40	300	  160.834	 100	99	0	 2	0	0	0	0	  0	0	0	#_SizeSel_P_1_11.DW.2C(11)
 -10	 10	-0.719876	 -10	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_11.DW.2C(11)
 -10	 10	  7.29806	   0	99	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_3_11.DW.2C(11)
 -10	 10	 -1.83732	   0	99	0	 5	0	0	0	0	  0	0	0	#_SizeSel_P_4_11.DW.2C(11)
-999	  9	     -999	-999	99	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_11.DW.2C(11)
-999	  9	  2.25754	   0	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_6_11.DW.2C(11)
  40	300	  295.584	 100	99	0	 3	0	0	0	0	0.5	0	0	#_SizeSel_P_1_12.DW.2S(12)
   1	100	  92.1615	  10	99	0	 3	0	0	0	0	0.5	0	0	#_SizeSel_P_2_12.DW.2S(12)
  40	300	  121.147	 100	99	0	 2	0	0	0	0	  0	0	0	#_SizeSel_P_1_13.NZ.2C(13)
 -10	 10	 -2.33481	 -10	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_13.NZ.2C(13)
 -10	 10	  6.29561	   0	99	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_3_13.NZ.2C(13)
 -10	 10	  7.92573	   0	99	0	 5	0	0	0	0	  0	0	0	#_SizeSel_P_4_13.NZ.2C(13)
-999	  9	     -999	-999	99	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_13.NZ.2C(13)
-999	  9	 -0.57496	   0	99	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_6_13.NZ.2C(13)
#_AgeSelex
#_No age_selex_parm
#_no timevary selex parameters
#
0 #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
# Tag loss and Tag reporting parameters go next
0 # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# Input variance adjustments factors: 
#_factor	fleet	value
    4	 1	    0	#_Variance_adjustment_list1 
    4	 2	0.005	#_Variance_adjustment_list2 
    4	 3	 0.05	#_Variance_adjustment_list3 
    4	 6	 0.05	#_Variance_adjustment_list4 
    4	 7	0.005	#_Variance_adjustment_list5 
    4	 8	 0.01	#_Variance_adjustment_list6 
    4	10	1e-04	#_Variance_adjustment_list7 
    4	11	 0.01	#_Variance_adjustment_list8 
    4	12	    0	#_Variance_adjustment_list9 
    4	13	 0.01	#_Variance_adjustment_list10
    4	14	 0.01	#_Variance_adjustment_list11
    4	15	    0	#_Variance_adjustment_list12
    4	16	 0.05	#_Variance_adjustment_list13
    4	17	    0	#_Variance_adjustment_list14
    4	18	    0	#_Variance_adjustment_list15
    4	19	 0.01	#_Variance_adjustment_list16
    5	 5	  0.6	#_Variance_adjustment_list17
    7	 4	 0.02	#_Variance_adjustment_list18
    7	 5	 0.02	#_Variance_adjustment_list19
    7	13	 0.02	#_Variance_adjustment_list20
-9999	 0	    0	#_terminator                
#
6 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 1 changes to default Lambdas (default value is 1.0)
#_like_comp	fleet	phase	value	sizefreq_method
    1	1	1	1	1	#_Surv_01.DW.1N_Phz1
-9999	0	0	0	0	#_terminator        
#
0 # 0/1 read specs for more stddev reporting
#
999
