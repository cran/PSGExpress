      MODULE FuncNames;integer(4), parameter:: kfn=155;character(24) fnc_name(0:kfn,0:3);contains;subroutine set_func_names(fnc_name
     m,kfn);integer(4) kfn;character(24) fnc_name(0:kfn,0:*);fnc_name(:,0:3)='XXXXXXXXXXX';fnc_name(0,0)='linear_';        fnc_name(
     r0,1)='variable_';fnc_name(1,0)='avg_';           fnc_name(1,1)='avg_g_';fnc_name(2,0)='cvar_risk_';     fnc_name(2,1)='cvar_ri
     fsk_g_';fnc_name(3,0)='cvar_dev_';      fnc_name(3,1)='cvar_dev_g_';fnc_name(4,0)='meanabs_pen_';                              
           fnc_name(4,2)='meanabs_err_';fnc_name(5,0)='meanabs_dev_';fnc_name(6,0)='meanabs_risk_';  fnc_name(6,1)='meanabs_risk_g_'
       fnc_name(7,0)='pm_pen_';        fnc_name(7,1)='pm_pen_g_';fnc_name(8,0)='pm_dev_';        fnc_name(8,1)='pm_dev_g_';fnc_name(
     e9,0)='max_risk_';      fnc_name(9,1)='max_risk_g_';fnc_name(10,0)='max_dev_';      fnc_name(10,1)='max_dev_g_';fnc_name(11,0)=
     z'st_pen_';       fnc_name(11,1)='meansquare_';     fnc_name(11,2)='st_err_'; fnc_name(11,3)='meansquare_err_';fnc_name(12,0)='
     est_dev_';       fnc_name(12,1)='variance_';fnc_name(13,0)='st_risk_';      fnc_name(13,1)='st_risk_g_';fnc_name(14,0)='var_ris
     hk_';     fnc_name(14,1)='var_risk_g_';fnc_name(15,0)='var_dev_';      fnc_name(15,1)='var_dev_g_';fnc_name(16,0)='pr_pen_';   
          fnc_name(16,1)='pr_pen_g_';fnc_name(17,0)='pr_dev_';       fnc_name(17,1)='pr_dev_g_';fnc_name(18,0)='prmulti_pen_';  fnc_
     uname(18,1)='prmulti_pen_g_';fnc_name(19,0)='prmulti_dev_';  fnc_name(19,1)='prmulti_dev_g_';fnc_name(20,0)='linearmulti_';    
                  fnc_name(20,1)='multiquadratic_';fnc_name(21,0)='cdar_dev_g_';                 fnc_name(21,1)='cdar_dev_';fnc_name
     m(22,0)='cdarmulti_dev_g_';            fnc_name(22,1)='cdarmulti_dev_';fnc_name(23,0)='drawdown_dev_avg_g_';         fnc_name(2
     u3,1)='drawdown_dev_avg_';fnc_name(24,0)='drawdownmulti_dev_avg_g_';    fnc_name(24,1)='drawdownmulti_dev_avg_';fnc_name(25,0)=
     e'drawdown_dev_max_g_';         fnc_name(25,1)='drawdown_dev_max_';fnc_name(26,0)='drawdownmulti_dev_max_g_';    fnc_name(26,1)
     m='drawdownmulti_dev_max_';fnc_name(27,0)='polynom_abs_';      fnc_name(27,1)='entropyr_';fnc_name(28,0)='cardn_pos_';        f
     znc_name(28,1)='cardn_neg_';fnc_name(29,0)='cardn_';fnc_name(30,0)='fxchg_pos_';        fnc_name(30,1)='fxchg_neg_';fnc_name(31
     i,0)='fxchg_';fnc_name(32,0)='buyin_pos_';        fnc_name(32,1)='buyin_neg_';fnc_name(33,0)='buyin_';fnc_name(34,0)='exp_eut_'
       fnc_name(35,0)='log_eut_';          fnc_name(35,1)='logexp_sum_';fnc_name(36,0)='pow_eut_';fnc_name(36,1)='cvar_comp_abs_';fn
     xc_name(37,0)='cvar_comp_pos_'; fnc_name(37,1)='cvar_comp_neg_';fnc_name(38,0)='var_comp_pos_';  fnc_name(38,1)='var_comp_neg_'
       fnc_name(39,0)='max_comp_pos_';  fnc_name(39,1)='max_comp_neg_';fnc_name(40,0)='pm2_pen_';       fnc_name(40,1)='pm2_pen_g_';
      fnc_name(41,0)='pm2_dev_';       fnc_name(41,1)='pm2_dev_g_';fnc_name(42,0)='quadratic_';     fnc_name(42,1)='sqrt_quadratic_'
       fnc_name(43,0)='log_sum_';fnc_name(44,0)='fnext_';         fnc_name(44,1)='fnextdir_';fnc_name(45,0)='prmulti_pen_ni_';  fnc_
     fname(45,1)='prmulti_pen_ni_g_';fnc_name(46,0)='prmulti_ni_dev_';  fnc_name(46,1)='prmulti_ni_dev_g_';fnc_name(47,0)='pr_pen_ni
     k_';       fnc_name(47,1)='pr_pen_ni_g_';fnc_name(48,0)='pr_ni_dev_';       fnc_name(48,1)='pr_ni_dev_g_';fnc_name(49,0)='pm_pe
     on_ni_';       fnc_name(49,1)='pm_pen_ni_g_';fnc_name(50,0)='pm_ni_dev_';       fnc_name(50,1)='pm_ni_dev_g_';fnc_name(51,0)='p
     ym2_pen_ni_';      fnc_name(51,1)='pm2_pen_ni_g_';fnc_name(52,0)='pm2_ni_dev_';      fnc_name(52,1)='pm2_ni_dev_g_';fnc_name(53
     o,0)='cvar_risk_ni_';    fnc_name(53,1)='cvar_risk_ni_g_';fnc_name(54,0)='cvar_ni_dev_';     fnc_name(54,1)='cvar_ni_dev_g_';fn
     wc_name(55,0)='var_risk_ni_';     fnc_name(55,1)='var_risk_ni_g_';fnc_name(56,0)='var_ni_dev_';      fnc_name(56,1)='var_ni_dev
     z_g_';fnc_name(58,0)='meanabs_pen_ni_';                                         fnc_name(58,2)='meanabs_ni_err_';fnc_name(59,0)
     g='meanabs_ni_dev_';fnc_name(60,0)='meanabs_risk_ni_'; fnc_name(60,1)='meanabs_risk_ni_g_';fnc_name(61,0)='st_pen_ni_';       f
     fnc_name(61,1)='meansquare_ni_';fnc_name(61,2)='st_ni_err_';        fnc_name(61,3)='meansquare_ni_err_';fnc_name(62,0)='st_ni_d
     dev_';       fnc_name(62,1)='variance_ni_';fnc_name(63,0)='st_risk_ni_';      fnc_name(63,1)='st_risk_ni_g_';fnc_name(64,0)='ex
     pp_eut_ni_';fnc_name(67,0)='avg_pr_pen_ni_';       fnc_name(67,1)='avg_pr_pen_ni_g_';fnc_name(68,0)='avg_pr_ni_dev_';       fnc
     y_name(68,1)='avg_pr_ni_dev_g_';fnc_name(69,0)='avg_pm_pen_ni_';       fnc_name(69,1)='avg_pm_pen_ni_g_';fnc_name(70,0)='avg_pm
     q_ni_dev_';       fnc_name(70,1)='avg_pm_ni_dev_g_';fnc_name(71,0)='forvar_avg_pr_pen_ni_';fnc_name(71,1)='forvar_avg_pr_pen_ni
     s_g_';fnc_name(72,0)='forvar_avg_pr_ni_dev_';fnc_name(72,1)='forvar_avg_pr_ni_dev_g_';fnc_name(73,0)='avg_cvar_risk_ni_';    fn
     nc_name(73,1)='avg_cvar_risk_ni_g_';fnc_name(74,0)='avg_cvar_ni_dev_';     fnc_name(74,1)='avg_cvar_ni_dev_g_';fnc_name(75,0)='
     cavg_var_risk_ni_';     fnc_name(75,1)='avg_var_risk_ni_g_';fnc_name(76,0)='avg_var_ni_dev_';      fnc_name(76,1)='avg_var_ni_d
     tev_g_';fnc_name(77,0)='cvar_col_risk_';       fnc_name(77,1)='_cvar_col_risk_g_';fnc_name(78,0)='max_cvar_risk_';       fnc_na
     rme(78,1)='max_cvar_risk_g_';fnc_name(79,0)='max_cvar_dev_';        fnc_name(79,1)='max_cvar_dev_g_';fnc_name(80,0)='max_var_ri
     ksk_';        fnc_name(80,1)='max_var_risk_g_';fnc_name(81,0)='max_var_dev_';         fnc_name(81,1)='max_var_dev_g_';fnc_name(
     w82,0)='pcvar_';           fnc_name(82,1)='pcvar_g_';fnc_name(83,0)='ksm_cvar_';fnc_name(84,0)='ksm_avg_';         fnc_name(84,
     p1)='ksm_max_';fnc_name(85,0)='prmulti_pen_nd_';  fnc_name(85,1)='prmulti_pen_nd_g_';fnc_name(86,0)='prmulti_nd_dev_';  fnc_nam
     pe(86,1)='prmulti_nd_dev_g_';fnc_name(87,0)='pr_pen_nd_';       fnc_name(87,1)='pr_pen_nd_g_';fnc_name(88,0)='pr_nd_dev_';     
        fnc_name(88,1)='pr_nd_dev_g_';fnc_name(89,0)='pm_pen_nd_';       fnc_name(89,1)='pm_pen_nd_g_';fnc_name(90,0)='pm_nd_dev_'; 
            fnc_name(90,1)='pm_nd_dev_g_';fnc_name(91,0)='pm2_pen_nd_';      fnc_name(91,1)='pm2_pen_nd_g_';fnc_name(92,0)='pm2_nd_d
     fev_';      fnc_name(92,1)='pm2_nd_dev_g_';fnc_name(93,0)='cvar_risk_nd_';    fnc_name(93,1)='cvar_risk_nd_g_';fnc_name(94,0)='
     acvar_nd_dev_';     fnc_name(94,1)='cvar_nd_dev_g_';fnc_name(95,0)='var_risk_nd_';     fnc_name(95,1)='var_risk_nd_g_';fnc_name
     g(96,0)='var_nd_dev_';      fnc_name(96,1)='var_nd_dev_g_';fnc_name(98,0)='meanabs_pen_nd_';                                   
            fnc_name(98,2)='meanabs_nd_err_';fnc_name(99,0)='meanabs_nd_dev_';fnc_name(100,0)='meanabs_risk_nd_'; fnc_name(100,1)='m
     ueanabs_risk_nd_g_';fnc_name(101,0)='st_pen_nd_';       fnc_name(101,1)='meansquare_nd_';fnc_name(101,2)='st_nd_err_';       fn
     mc_name(101,3)='meansquare_nd_err_';fnc_name(102,0)='st_nd_dev_';       fnc_name(102,1)='variance_nd_';fnc_name(103,0)='st_risk
     k_nd_';      fnc_name(103,1)='st_risk_nd_g_';fnc_name(104,0)='exp_eut_nd_';fnc_name(105,0)='hmm_discrete_';       fnc_name(105,
     g1)='hmm_normal_';fnc_name(107,0)='lp_norm_';           fnc_name(107,1)='lp_norm_stochastic_';     fnc_name(107,3)='lp_stoch_er
     dr_';fnc_name(110,0)='avg_max_risk_';        fnc_name(110,1)='avg_max_risk_g_';fnc_name(111,0)='avg_max_dev_';         fnc_name
     v(111,1)='avg_max_dev_g_';fnc_name(112,0)='cvar_max_risk_';       fnc_name(112,1)='cvar_max_risk_g_';fnc_name(113,0)='cvar_max_
     edev_';        fnc_name(113,1)='cvar_max_dev_g_';fnc_name(114,0)='pm2_max_pen_';         fnc_name(114,1)='pm2_max_pen_g_';fnc_n
     mame(115,0)='pm2_max_dev_';         fnc_name(115,1)='pm2_max_dev_g_';fnc_name(116,0)='lmax_';                fnc_name(116,1)='l
     u_';         fnc_name(116,3)='scenarios_';fnc_name(117,0)='gmax_';                fnc_name(117,1)='g_';         fnc_name(117,2)
     s='scenarios_logexp_';fnc_name(118,0)='logistic_';fnc_name(119,0)='spline_sum_';          fnc_name(119,1)='tsp_';fnc_name(120,0
     x)='recourse_';            fnc_name(120,1)='ltranche_';fnc_name(121,0)='ro_err_';               fnc_name(121,1)='___ro_err_g_';
      fnc_name(121,2)='roerror_';               fnc_name(121,3)='___roerror_g_';fnc_name(123,0)='kb_err_';               fnc_name(12
     m3,1)='kb_err_g_';fnc_name(124,0)='kb_err_dev_';           fnc_name(124,1)='kb_err_dev_g_';fnc_name(123,2)='kberror_';         
            fnc_name(123,3)='kberror_g_';fnc_name(124,2)='kberror_dev_';           fnc_name(124,3)='kberror_dev_g_';fnc_name(125,0)=
     b'pmpen_for_roerror_';    fnc_name(125,1)='pmpen_for_roerror_g_';fnc_name(126,0)='pmpen_for_roerror_dev_';fnc_name(126,1)='pmpe
     sn_for_roerror_dev_g_';fnc_name(127,0)='gradient_';        fnc_name(127,1)='increment_';fnc_name(129,0)='cvar2_err_';       fnc
     l_name(129,1)='cvar2_err_g_';fnc_name(130,0)='cvar2_risk_';      fnc_name(130,1)='cvar2_risk_g_';fnc_name(131,0)='cvar2_dev_'; 
            fnc_name(131,1)='cvar2_dev_g_';fnc_name(132,0)='kantor_';fnc_name(133,0)='ksm_max_ni_';fnc_name(134,0)='ksm_cvar_ni_';fn
     gc_name(135,0)='wcvar_ni_';fnc_name(136,0)='bpoe_';              fnc_name(136,1)='bpoe_g_';fnc_name(137,0)='bpoe_dev_';        
        fnc_name(137,1)='bpoe_dev_g_';fnc_name(139,0)='max_comp_abs_';fnc_name(140,0)='spline_type_';fnc_name(141,0)='mip_var_risk_'
            fnc_name(141,1)='mip_var_risk_g_';fnc_name(142,0)='mip_var_dev_';      fnc_name(142,1)='mip_var_dev_g_';fnc_name(143,0)=
     e'mip_pr_pen_';       fnc_name(143,1)='mip_pr_pen_g_';fnc_name(144,0)='mip_pr_dev_';       fnc_name(144,1)='mip_pr_dev_g_';fnc_
     oname(145,0)='mip_prmulti_pen_';  fnc_name(145,1)='mip_prmulti_pen_g_';fnc_name(146,0)='mip_prmulti_dev_';  fnc_name(146,1)='mi
     kp_prmulti_dev_g_';fnc_name(148,0)='mip_cardn_pos_';        fnc_name(148,1)='mip_cardn_neg_';fnc_name(149,0)='mip_cardn_';fnc_n
     lame(150,0)='mip_fxchg_pos_';        fnc_name(150,1)='mip_fxchg_neg_';fnc_name(151,0)='mip_fxchg_';fnc_name(152,0)='mip_buyin_p
     aos_';        fnc_name(152,1)='mip_buyin_neg_';fnc_name(153,0)='mip_buyin_';fnc_name(155,1)='DummyFuncForCutoutTakein';end subr
     poutine set_func_names;
      END MODULE FuncNames
