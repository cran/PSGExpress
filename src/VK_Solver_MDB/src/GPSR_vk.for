      SUBROUTINE GPSR_vk(N,d,G,
     &XL,XU,X,MAXIT,VSMALL);use CiFort;integer(4) I,N,MAXIT;DIMENSION G(N,N),X(N),XL(N),XU(N);DOUBLE PRECISION VSMALL,G,X,XL,XU;real
     l(8),target:: d(0:n);integer(4) verbose;character wch*100;logical keep_going;real(8),pointer:: grad(:);real(8),allocatable::Gx(
     l:),GradF(:),dx(:),old_x(:),Gdx(:),vw(:);real(8) lambda, lambda0;integer(4) stopcriterion;integer(4) iend,Initial_X_supplied,ma
     lxiter,maxiter_debias,miniter,miniter_debias,init,iprint,iter,keep_continuation;real(8) tolA,tolD,debias,enforceMonotone,alpham
     lin,alphamax,compute_mse,AT,continuation,cont_steps,firstTauFactorGiven,
     +debias_start,t0,     final_stopCriterion,final_tolA,cont_factors,cont_loop,alpha,f,wf0,w,dgd,gg,prev_f,
     +dd,times,criterionObjective,delta_x_criterion,criterionLCP,timelimit;real(8),external:: vectmod,d_time;Entry GPSR_vk2_Iter(ipr
     lint,timelimit,wf0,n,
     &d,G, xl,xu,x,maxit,vsmall,iend);iend=0;allocate(Gx(n),GradF(n),dx(n),old_x(n),Gdx(n),vw(n));grad=>d(1:n);Initial_X_supplied = 
     l3333;stopCriterion = 1;tolA = 0.00000001;tolD = 0.0001;debias = 0;maxiter = 10000;maxiter_debias = 500;miniter = 5;miniter_deb
     lias = 5;init = 0;enforceMonotone = 1;alphamin = 1e-30;alphamax = 1e30;compute_mse = 0;AT = 0;verbose = iprint;continuation = 0
     l;cont_steps = -1;firstTauFactorGiven = 0;debias_start = 0;if(stopCriterion<0.or.stopCriterion>5) then;if(verbose>0)write(20,*)
     l'Unknown stopping criterion'; goto 79999;endif;CALL CPU_TIME(t0);final_stopCriterion = stopCriterion;final_tolA = tolA;if(cont
     linuation==0)then;cont_factors = 1;cont_steps = 1;endif;iter = 1;keep_continuation = 1;cont_loop = 1;iter = 1;do while(keep_con
     ltinuation/=0);if(cont_loop==1)then;alpha = 1.0;call MatrMultVect(Gx,G,x,n,n);f = 0.5*dot_product(x,Gx) + dot_product(x,grad);i
     lf(verbose>0)then; write(20,'(a,2e11.3)')'Initial obj, alpha',f,alpha;endif;endif;keep_going = .true.;do while(keep_going);Grad
     lF=Gx+grad;call MaxInBox(n,x,xl,xu,alpha,GradF,  dx);old_x=x;call MatrMultVect(Gdx,G,dx,n,n); dGd=dot_product(dx,Gdx);gg=dot_pr
     loduct(GradF,dx);if(enforceMonotone==1)then;lambda0 = - gg/(vsmall+dGd);if(lambda0<0.)then; if(verbose>0)write(20,*)'Internal e
     lrror: lambda0 negative. Quit'; iend=7; goto 79999;endif;lambda = min(lambda0,1.);else;lambda = 1.;endif;x=old_x+lambda*dx;prev
     l_f = f;f=prev_f+lambda*(0.5*lambda*dGd+gg);Gx=Gx+lambda*Gdx;if(mod(iter,20)==0)then;call MatrMultVect(Gx,G,x,n,n);f = 0.5*dot_
     lproduct(x,Gx) + dot_product(x,grad);endif;dd=dot_product(dx,dx);if(dGd<=0.)then;if(verbose>0) write(20,*)' dGd nonpositive cur
     lvature detected'; alpha = alphamax;else;alpha = min(alphamax,max(alphamin,dd/dGd));endif;iter = iter + 1;CALL CPU_TIME(w);time
     ls=w-t0;if(verbose>0)then; write(20,'(a,i5,2e15.7)')'It, obj, alpha ',iter, f, alpha;endif;select case(stopCriterion);case (0);
      case (1);criterionObjective = abs((f-prev_f)/(abs(prev_f)+abs(f)+vsmall));keep_going =  (criterionObjective > tolA);if(verbose
     l>0)then; write(20,'(a,2e11.3)')'   Delta objective, target', criterionObjective , tolA;endif;case (2);delta_x_criterion = Vect
     lMod(dx,n)/VectMod(x,n);keep_going = (delta_x_criterion > tolA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Norm(delta x)/norm
     l(x), target',delta_x_criterion,tolA;endif;case (3);vw=abs(GradF); criterionLCP = maxval(vw);vw=abs(old_x); w=maxval(vw);criter
     lionLCP = criterionLCP / max(1e-6, w);keep_going = (criterionLCP > tolA);if(verbose>0)then;endif;case (4);keep_going = (f > tol
     lA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Objective, target',f,tolA;endif;case (5);delta_x_criterion = sqrt(dd)/VectMod(
     lx,n);keep_going = (delta_x_criterion > tolA);if(verbose>0)then; write(20,'(a,2e11.3)')'   Norm(delta x)/norm(x), target',delta
     l_x_criterion,tolA;endif;case default;if(verbose>0)write(20,*)'Unknown stopping criterion'; keep_going = .false.; iend=8;end se
     llect;if(iter<=miniter)then;keep_going = .true.;elseif(iter>maxit)then;keep_going = .false.;endif;call Check_stop_whatch(1,w); 
      if(w>timelimit) iend=9;wch='';if(d_time('s',int2(2))>2.or.iend/=0)then;w=d_time('s',int2(0)); write(wch,'(i10.10)')iter; i=ver
     lify(wch,'0'); if(i<1.or.i>10)i=10;write(wch,'(a,2(a,e18.12))')'Iteration='//wch(i:10),'  Objective=',f*wf0,'  Residual=',0.;en
     ldif;call putmess('n',0,' ',wch);if(wch=='S'.or.wch=='T'.or.iend>0) goto 100;enddo;cont_loop = cont_loop+1;keep_continuation = 
     l0;enddo
100   continue;if(verbose>0)then;write(20,*)'Finished the main algorithm';write(20,'(a,e15.7)')'Objective function = ',f;write(20,'(
     la,e9.2)')'CPU time so far = ', times;endif
79999 deallocate(Gx,GradF,dx,old_x,Gdx,vw);return;END subroutine GPSR_vk;subroutine MaxInBox(n,x,xl,xu,alpha,GradF,  dx);integer(4) 
     ln,i;real(8) x(n),xl(n),xu(n),GradF(n),alpha,dx(n),w;do i=1,n; w=x(i)-alpha*GradF(i); if(w<xl(i))then; w=xl(i); elseif(w>xu(i))
     lthen; w=xu(i); endif;dx(i)=w-x(i);enddo;
      end subroutine MaxInBox
