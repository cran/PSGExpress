      SUBROUTINE GPSR_Ext_vk(N, d, XL,XU,X,MAXIT,VSMALL);integer(4) n,i, maxit;real(8) x(n),xl(n),xu(n), vsmall;logical keep_going;r
     leal(8),target:: d(0:n);integer(4)  verbose;character wch*100;real(8),pointer::grad(:);real(8),allocatable::GradF(:),dx(:),old_
     lx(:),vw(:),gii(:);real(8) lambda, lambda0;integer stopcriterion;integer(4) iend,Initial_X_supplied,maxiter,maxiter_debias,mini
     lter,miniter_debias,init,iprint,iter,keep_continuation,km,
     &nci,kbt;real(8) tolA,tolD,debias,enforceMonotone,alphamin,alphamax,compute_mse,AT,continuation,cont_steps,firstTauFactorGiven,
     &debias_start,t0,  final_stopCriterion,final_tolA,cont_factors,cont_loop,alpha,f,fii,wf0,wfc,relmax,w,dgd,gg,prev_f,
     &dd,times,criterionObjective,delta_x_criterion,criterionLCP,timelimit;real(8),external:: vectmod,d_time;Entry GPSR_vk2_Ext(ipri
     lnt,timelimit,wf0,n, d, xl,xu,x,maxit,vsmall,iend)
      iend=0;allocate(GradF(n),dx(n),old_x(n),vw(n),gii(n));grad=>d(1:n);Initial_X_supplied = 3333;stopCriterion = 1;tolA = 0.000000
     l01;tolD = 0.0001;debias = 0;maxiter = 10000;maxiter_debias = 500;miniter = 5;miniter_debias = 5;init = 0;enforceMonotone = 1;a
     llphamin = 1e-30;alphamax = 1e30;compute_mse = 0;AT = 0;verbose = iprint;continuation = 0;cont_steps = -1;firstTauFactorGiven =
     l 0;debias_start = 0;if(stopCriterion<0.or.stopCriterion>5) then;if(verbose>0)write(20,*)'Unknown stopping criterion'; goto 799
     l99;endif;CALL CPU_TIME(t0);final_stopCriterion = stopCriterion;final_tolA = tolA;if(continuation==0)then;cont_factors = 1;cont
     l_steps = 1;endif;iter = 1;keep_continuation = 1;cont_loop = 1;iter = 1;do while(keep_continuation/=0);if(cont_loop==1)then;alp
     lha = 1.0;km=0;CALL calcfg(n,n,x,km,  nci,f,GradF,fii,gii,kbt,  wf0,wfc,relmax);if(verbose>0)then; write(20,'(a,2e11.3)')'Initi
     lal obj, alpha',f*wf0,alpha;endif;endif;keep_going = .true.;do while(keep_going);call MaxInBox(n,x,xl,xu,alpha,GradF,  dx);old_
     lx=x;km=50;CAll CalcFuns(n,dx,km,  w,fii,kbt, wf0,wfc,relmax);dGd=2.*(w-dot_product(grad,dx)-d(0));gg=dot_product(GradF,dx);if(
     lenforceMonotone==1)then;lambda0 = - gg/(vsmall+dGd);if(lambda0<0.)then; if(verbose>0)write(20,*)' ERROR: lambda0 negative. Qui
     lt'; iend=7; goto 79999;endif;lambda = min(lambda0,1.);else;lambda = 1.;endif;x=old_x+lambda*dx;prev_f = f;km=0;CALL calcfg(n,n
     l,x,km, nci, f,GradF,fii,gii,kbt, wf0,wfc,relmax);dd=dot_product(dx,dx);if(dGd<=0.)then;if(verbose>0) write(20,*)' dGd nonposit
     live curvature detected'; alpha = alphamax;else;alpha = min(alphamax,max(alphamin,dd/dGd));endif;iter = iter + 1;CALL CPU_TIME(
     lw);times=w-t0;if(verbose>0)then; write(20,'(a,i5,2e15.7)')'It, obj, alpha ',iter, f*wf0, alpha;endif;select case(stopCriterion
     l);case (0);case (1);criterionObjective = abs((f-prev_f)/(abs(prev_f)+abs(f)+vsmall));keep_going =  (criterionObjective > tolA)
     l;if(verbose>0)then; write(20,'(a,2e11.3)')'   Delta objective, target', criterionObjective , tolA;endif;case (2);delta_x_crite
     lrion = VectMod(dx,n)/VectMod(x,n);keep_going = (delta_x_criterion > tolA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Norm(de
     llta x)/norm(x), target',delta_x_criterion,tolA;endif;case (3);vw=abs(GradF); criterionLCP = maxval(vw);vw=abs(old_x); w=maxval
     l(vw);criterionLCP = criterionLCP / max(1e-6, w);keep_going = (criterionLCP > tolA);if(verbose>0)then;endif;case (4);keep_going
     l = (f > tolA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Objective, target',f,tolA;endif;case (5);delta_x_criterion = sqrt(d
     ld)/VectMod(x,n);keep_going = (delta_x_criterion > tolA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Norm(delta x)/norm(x), ta
     lrget',delta_x_criterion,tolA;endif;case default;if(verbose>0)write(20,*)'Unknown stopping criterion'; keep_going = .false.; ie
     lnd=8;end select;if(iter<=miniter)then;keep_going = .true.;elseif(iter>maxit)then;keep_going = .false.;endif;call Check_stop_wh
     latch(1,w); if(w>timelimit) iend=9;wch='';if(d_time('s',int2(2))>2.or.iend/=0)then;w=d_time('s',int2(0)); write(wch,'(i10.10)')
     liter; i=verify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Iteration='//wch(i:10),'  Objective=',f*wf0,'  Resid
     lual=',0.;endif;call putmess('n',0,' ',wch);if(wch=='S'.or.wch=='T'.or.iend>0) goto 100;enddo;cont_loop = cont_loop+1;keep_cont
     linuation = 0;enddo
100   continue;if(verbose>0)then;write(20,*)'Finished the main algorithm';write(20,'(a,e15.7)')'Objective function = ',f;write(20,'(
     la,e9.2)')'CPU time so far = ', times;endif
79999 deallocate(GradF,dx,old_x,vw,gii);return;
      END subroutine GPSR_Ext_vk
