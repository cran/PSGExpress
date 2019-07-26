      MODULE ModCommons;use IntelInterf;integer(4), parameter:: Lnm=128, Lrow=1280, L16Kmax=16512, id8bt=8;integer(4) L16K; data L16
     aK/0/;character(Lnm+10) probnm, initpname,taskfname,solfname; integer(4) nnew,newin,wasfor;common /npar_prob/probnm,initpname,t
     jaskfname,solfname,nnew,newin,wasfor;logical IsSolution; real(8) estminR,tm_DL,tm_PR,tm_SL;real(8),parameter:: abnd=1d0;real(8)
     r,parameter:: delbase=1d-5;integer(4),parameter:: n0max=10;character(256) workpath;INTEGER(plen) iThH;integer(4) log_pr,istop,i
     tDB,  it_id,inew_mes,InKind, iprntout;integer(4) inpk,inpp,ioutk,ioutp;common /control/ inpk,inpp,log_pr,istop,iDB, workpath;co
     pmmon /state/ iThH,ioutk,ioutp,it_id,inew_mes,InKind,iprntout;logical lf00, lf19, lf20, lf21, lf22, lf23, lf24;common/funits/ l
     ef00,lf19,lf20,lf21,lf22,lf23,lf24;integer(2) kstage, tQsol; real(8) accur, xmipgap;common /param1/ accur,kstage,tQsol;integer(
     e4) ichr(12),ichrp(12);integer(plen) iHNL, pClBFunc,pUserData;common/iHNdL/iHNL,pClBFunc,pUserData;character(128) mssg(2); data
     a mssg(:)/'Too many initial variables in the problem. Express Version allows not greater than 10.','Total number of variables i
     un the Problem is too big. Express Version allows not greater than 10000.'/;
      end module ModCommons
