/*
 * ALL CONTENT OF crbond_quadpack.cpp AND crbond_quadpack.h IS TAKEN FROM crbond.com
 * THE AUTHOR PROVIDED THE FILES "as is"
 * THE ORIGINAL FILES ARE ESSENTIALLY A C PORT OF THE FORTRAN LIBRARY QUADPACK 
 *
 *
 *  the content of different files was merged in a single source and single header file
 *  on line 434 & 712 parentheses were clarified to adhere to the original quadpack source 
 *  
 *  a simple wraper function integrate was added
 */


#include <limits>
#include <cmath>
#include <algorithm>

#include "crbond_quadpack.h"

const double epmach = std::numeric_limits<double>::epsilon();
const int LIMIT = 500;

const double uflow = std::numeric_limits<double>::min();
const double oflow = std::numeric_limits<double>::max();


/*
 * 
 * PROTOTYPES,
 * 
 */
  double G_K21(double (*f)(double) ,double a,double b,double *abserr,
               double *resabs,double *resasc);
  
  // /* Gauss-Kronrod for integration over infinite range. */
  double G_K15I(double (*f)(double),double boun,int inf,double a,double b,
                double *abserr,double *resabs,double *resasc);
  
  double dqext(int *n,double epstab[],double *abserr,
               double res3la[],int *nres);
  void dqsort(int limit,int last,int *maxerr,double *ermax,
              double elist[],int iord[],int *nrmax);







namespace quadpack{
// 
//   namespace internal{
    
    double G_K15I(double (*f)(double),double boun,int inf,double a,double b,
                  double *abserr,double *resabs,double *resasc)
    {
      static long double XGK15[8] = {
        0.99145537112081263921,
        0.94910791234275852453,
        0.86486442335976907279,
        0.74153118559939443986,
        0.58608723546769113029,
        0.40584515137739716691,
        0.20778495500789846760,
        0.00000000000000000000};
      static long double WGK15[8] = {
        0.02293532201052922496,
        0.06309209262997855329,
        0.10479001032225018384,
        0.14065325971552591875,
        0.16900472663926790283,
        0.19035057806478540991,
        0.20443294007529889241,
        0.20948214108472782801};
      static long double WG7[4] = {
        0.12948496616886969327,
        0.27970539148927666790, 
        0.38183005050511894495,
        0.41795918367346938776};
      double fv1[8],fv2[8];
      double absc,absc1,absc2,centr,dinf;
      double fc,fsum,fval1,fval2,hlgth,resg,resk;
      double reskh,result,tabsc1,tabsc2;
      int j;
      
      dinf = std::min((double)(1.0),(double)inf);
      centr = 0.5 * (a + b);
      hlgth = 0.5 * (b - a);
      tabsc1 = boun + dinf * (1.0 - centr)/centr;
      fval1 = (*f)(tabsc1);
      if (inf == 2)
        fval1 += (*f)(-tabsc1);
      fc=(fval1/centr)/centr;
      resg = fc * WG7[3];
      resk = fc * WGK15[7];
      *resabs = std::abs(resk);
      for (j = 0; j < 7; j++) {
        absc = hlgth * XGK15[j];
        absc1 = centr - absc;
        absc2 = centr + absc;
        tabsc1 = boun + dinf * (1.0 - absc1)/absc1;
        tabsc2 = boun + dinf * (1.0 - absc2)/absc2;
        fval1 = (*f)(tabsc1);
        fval2 = (*f)(tabsc2);
        if (inf == 2) {
          fval1 += (*f)(-tabsc1);
          fval2 += (*f)(-tabsc2);
        }
        fval1 = (fval1/absc1)/absc1;
        fval2 = (fval2/absc2)/absc2;
        fv1[j] = fval1;
        fv2[j] = fval2;
        fsum = fval1 + fval2;
        if (j & 1) resg += WG7[j/2] * fsum; /* odd 'j's are truncated */
    resk += WGK15[j] * fsum;
    *resabs = (*resabs) + WGK15[j] * (std::abs(fval1) + std::abs(fval2));
      }
      reskh = resk * 0.5;
      *resasc = WGK15[7] * std::abs(fc - reskh);
      for (j = 0; j < 7; j++ )
        *resasc = (*resasc) + WGK15[j] * (std::abs(fv1[j] - reskh) +
          std::abs(fv2[j] - reskh));
      result = resk * hlgth;
      *resabs = (*resabs) * hlgth;
      *resasc = (*resasc) * hlgth; 
      *abserr = std::abs((resk - resg) * hlgth);
      if ((*resasc != 0.0) && (*abserr != 0.0))
        *abserr = (*resasc) * std::min(1.0,pow((200.0 * (*abserr)/(*resasc)),1.5));
      if (*resabs > uflow/(50.0 * epmach))
        *abserr = std::max(epmach * 50.0 * (*resabs),(*abserr)); 	
      return result;
    }
    
    
    
    double G_K21(double (*f)(double) ,double a,double b,double *abserr,
                 double *resabs,double *resasc)
    {
      static long double XGK21[11] = {
        0.99565716302580808074,
        0.97390652851717172008,
        0.93015749135570822600,
        0.86506336668898451073,
        0.78081772658641689706,
        0.67940956829902440623,
        0.56275713466860468334,
        0.43339539412924719080,
        0.29439286270146019813,
        0.14887433898163121088,
        0.00000000000000000000};
      static long double WGK21[11] = {
        0.01169463886737187428,
        0.03255816230796472748,
        0.05475589657435199603,
        0.07503967481091995277,
        0.09312545458369760554,
        0.10938715880229764190,
        0.12349197626206585108,
        0.13470921731147332593,
        0.14277593857706008080,
        0.14773910490133849137,
        0.14944555400291690566};
      static long double WG10[5] = {
        0.06667134430868813759,
        0.14945134915058059315,
        0.21908636251598204400,
        0.26926671930999635509,
        0.29552422471475287017};
      double fv1[10],fv2[10];
      double absc,centr,dhlgth;
      double fc,fsum,fval1,fval2,hlgth;
      double resg,resk,reskh,result;
      int j,jtw,jtwm1;
      
      centr = 0.5 * (a + b);
      hlgth = 0.5 * (b - a);
      dhlgth = fabs(hlgth);
      
      resg = 0.0;
      fc=(*f)(centr);
      resk = fc * WGK21[10];
      *resabs = fabs(resk);
      for (j = 0; j < 5; j++) {
        jtw = 2 * j + 1;
        absc = hlgth * XGK21[jtw];
        fval1 = (*f)(centr-absc);
        fval2 = (*f)(centr+absc);
        fv1[jtw] = fval1;
        fv2[jtw] = fval2;
        fsum = fval1 + fval2;
        resg += WG10[j] * fsum;
        resk += WGK21[jtw] * fsum;
        *resabs = *resabs + WGK21[jtw] * (fabs(fval1) + fabs(fval2));
      }
      for (j = 0; j < 5; j++) {
        jtwm1 = j * 2;
        absc = hlgth * XGK21[jtwm1];
        fval1 = (*f)(centr-absc);
        fval2 = (*f)(centr+absc);
        fv1[jtwm1] = fval1;
        fv2[jtwm1] = fval2;
        fsum = fval1 + fval2;
        resk = resk + WGK21[jtwm1] * fsum;
        *resabs = (*resabs) + WGK21[jtwm1] * (fabs(fval1) + fabs(fval2));
      }
      reskh = resk * 0.5;
      *resasc = WGK21[10] * fabs(fc - reskh);
      for (j = 0; j < 10; j++ )
        *resasc = (*resasc) + WGK21[j] * (fabs(fv1[j] - reskh) +
          fabs(fv2[j] - reskh));
      result = resk * hlgth;
      *resabs = (*resabs) * dhlgth;
      *resasc = (*resasc) * dhlgth;
      *abserr = fabs((resk - resg) * hlgth);
      if ((*resasc != 0.0) && (*abserr != 0.0))
        *abserr = (*resasc) * std::min(1.0,pow((200.0 * (*abserr)/(*resasc)),1.5));
      if (*resabs > uflow/(50.0 * epmach))
        *abserr = std::max(epmach * 50.0 * (*resabs),(*abserr)); 	
      return result;
    }
    
    
    
  
    void dqsort(int limit,int last,int *maxerr,double *ermax,double elist[],
                int iord[],int *nrmax)
    {
      double errmax,errmin;
      int i,ibeg,ido,isucc,j,jbnd,jupbn,k;
      
      if (last > 1) goto _10;
      iord[0] = 0;
      iord[1] = 1;
      goto _90;
      _10:
        errmax = elist[*maxerr];
      if (*nrmax == 0) goto _30;
      ido = (*nrmax) - 1;
      for (i = 0;i <= ido; i++) {
        isucc = iord[*nrmax-1];
        if (errmax <= elist[isucc]) goto _30;
        iord[*nrmax] = isucc;
        (*nrmax)--;
      }
      _30:
        jupbn = last;
      if (last > (limit/2 + 2))
        jupbn = limit + 3 - last;
      errmin = elist[last];
      jbnd = jupbn - 1;
      ibeg = *nrmax + 1;
      if (ibeg > jbnd) goto _50;
      for (i = ibeg; i <= jbnd; i++) {
        isucc = iord[i];
        if (errmax >= elist[isucc]) goto _60;
        iord[i-1] = isucc;
      }
      _50: 
        iord[jbnd] = *maxerr;
      iord[jupbn] = last;
      goto _90;
      _60:
        iord[i-1] = *maxerr;
      k = jbnd;
      for (j = i;j <= jbnd; j++) {
        isucc = iord[k];
        if (errmin < elist[isucc]) goto _80;
        iord[k+1] = isucc;
        k--;
      }
      iord[i] = last;
      goto _90;
      _80:
        iord[k+1] = last;
      _90:
        *maxerr = iord[*nrmax];
      *ermax = elist[*maxerr];
      return;  
    }	
  
  
  
  
  double dqext(int *n,double epstab[],double *abserr,
               double res3la[],int *nres)
  {
    double delta1,delta2,delta3,epsinf;
    double error,err1,err2,err3,e0,e1,e1abs,e2,e3;
    double res,result,ss,tol1,tol2,tol3;
    int NN,i,ib,ib2,ie,indx,k1,k2,k3,limexp,newelm,num;
    
    (*nres)++;
    NN = *n;
    NN++;   /* make NN a FORTRAN array index */
    *abserr = oflow;
    result = epstab[*n];
    if (NN < 3) goto _100;		/* N < 3 */
    limexp = 50;			/* limexp = 50 */
    epstab[*n+2] = epstab[*n];
    newelm = (*n)/2;      /* (n-1)/2 */
    epstab[*n] = oflow;
    num = NN;
    k1 = NN;
    for (i = 1; i <= newelm; i++) {
      k2 = k1 - 1;
      k3 = k1 - 2;
      res = epstab[k1+1];
      e0 = epstab[k3-1];
      e1 = epstab[k2-1];
      e2 = res;
      e1abs = fabs(e1);
      delta2 = e2 - e1;
      err2 = fabs(delta2);
      tol2 = std::max(fabs(e2),e1abs) * epmach;
      delta3 = e1 - e0;
      err3 = fabs(delta3);
      tol3 = std::max(e1abs,fabs(e0)) * epmach;
      if ((err2 > tol2) || (err3 > tol3)) goto _10;
      result = res;
      *abserr = err2 + err3;
      goto _100;
      _10:
        e3 = epstab[k1-1];
      epstab[k1-1] = e1;
      delta1 = e1 - e3;
      err1 = fabs(delta1);
      tol1 = std::max(e1abs,fabs(e3)) * epmach;
      if ((err1 <= tol1) || (err2 <= tol2) || (err3 <= tol3)) goto _20;
      ss = 1.0/delta1 + 1.0/delta2 - 1.0/delta3;
      epsinf = fabs(ss*e1);
      if (epsinf > 1.0e-4) goto _30;
      _20:
        NN = i + i - 1;
      goto _50;
      _30:
        res = e1 + 1.0 / ss;
      epstab[k1-1] = res; 
      k1 -= 2;
      error = err2 + fabs(res - e2) + err3;
      if (error > (*abserr)) goto _40;
      *abserr = error;
      result = res;
      _40: 
        ;
    } //continue
    _50:
      if (NN == limexp) NN = 2 * (limexp/2) - 1;
      ib = 1;						/* ib = 1 */
      if (((num/2) * 2 ) == num) ib = 2;		/* ib = 2 */
      ie = newelm + 1;
      for (i = 1;i <= ie; i++) {
        ib2 = ib + 2;
        epstab[ib-1] = epstab[ib2-1];
        ib = ib2;
      }
      if (num == NN) goto _80;
      indx = num - NN + 1;
      for (i = 1;i <= NN; i++) {
        epstab[i-1] = epstab[indx-1];
        indx++;
      }
      _80:
        if (*nres > 3) goto _90;       /* nres >= 4 */
        res3la[(*nres)-1] = result;
        *abserr = oflow;
        goto _100;
        _90:
          *abserr = fabs(result - res3la[2]) + fabs(result - res3la[1]) +
            fabs(result - res3la[0]);
        res3la[0] = res3la[1];
        res3la[1] = res3la[2];
        res3la[2] = result;
        _100:
          *abserr = std::max(*abserr,5.0 * epmach * fabs(result));
        *n = NN - 1;
        return result;
  }
  
  // }//internal


/* DQAGS - Integration over finite intervals. (From QUADPACK)
*
*	Adaptive integration routine which handles functions
*	to be integrated between two finite bounds.
*
*	The adaptive strategy compares results of integration
*	over the given interval with the sum of results obtained
*	from integration over a bisected interval. Since error
*	estimates are available from each regional integration, the
*	region with the largest error is bisected and new results
*	are computed. This bisection process is continued until the
*	error is less than the prescribed limit or convergence
*	failure is determined.
*
* PARAMETERS:
*
*	f() - double precision function to be integrated.
*
*	a - lower limit of integration.
*
*	b - upper limit of integration.
*
*	epsabs - absolute accuracy requested.
*
*	epsrel - relative accuracy requested.
*/ 	
double dqags(double (*f)(double),double a,double b,double epsabs,double epsrel,double *abserr,int *neval,int *ier)
{
  double abseps,alist[LIMIT],area,area1,area12,area2;
  double a1,a2,blist[LIMIT],b1,b2,correc,defabs,defab1;
  double defab2,dres,elist[LIMIT],erlarg,erlast,errbnd;
  double errmax,error1,error2,erro12,errsum,ertest;
  double resabs,reseps,result,res3la[3],rlist[LIMIT];
  double rlist2[52],small;
  
  int id,ierro,iord[LIMIT],iroff1,iroff2,iroff3,jupbnd,k,ksgn;
  int ktmin,last,maxerr,nres,nrmax,numrl2;
  int limit;
  bool extrap,noext;
  
  limit = LIMIT -1;
  /* Test validity of parameters. */	
  *ier = 0;
  *neval = 0;
  result = 0.0;
  *abserr = 0.0;
  alist[0] = a;
  blist[0] = b;
  rlist[0] = 0.0;
  elist[0] = 0.0;
  if ((epsabs < 0.0) && (epsrel < 0.0)) *ier = 6;
  if (*ier == 6) return result;
  
  /* First approximation to the integral. */
  ierro = 0;
  result = G_K21(f,a,b,abserr,&defabs,&resabs);
  
  /* Test on accuracy. */
  dres = fabs(result);
  errbnd = std::max(epsabs,epsrel*dres);
  last = 1;
  rlist[0] = result;
  elist[0] = *abserr;	
  iord[0] = 0;
  if ((*abserr <= 100.0 * epmach * defabs) && (*abserr > errbnd))
    *ier = 2;
  if (limit == 0) *ier = 1;
  if ((*ier != 0) || ((*abserr <= errbnd) && (*abserr != resabs)) ||
      (*abserr == 0.0)) goto _140;
  
  /* Initialization. */
  rlist2[0] = result;
  errmax = *abserr;
  maxerr = 0; 			/* maxerr = 1 */
  area = result;
  errsum = *abserr;
  *abserr = oflow;
  nrmax = 0;
  nres = 0;          /* nres = 0 */
  numrl2 = 1;			/* numrl2 = 2 */
  ktmin = 0;
  extrap = false;
  noext = false;
  ierro = 0;
  iroff1 = 0;
  iroff2 = 0;
  iroff3 = 0;
  ksgn = -1;
  if (dres > (1.0 - 50.0 * epmach) * defabs)
    ksgn = 1;
  
  /* Main loop. */
  for (last = 1; last <= limit; last++) {
    
    /* Bisect the interval with the nrmax-th largest error estimate. */
    a1 = alist[maxerr];
    b1 = 0.5 * (alist[maxerr] + blist[maxerr]);
    a2 = b1;
    b2 = blist[maxerr];
    erlast = errmax;
    area1 = G_K21(f,a1,b1,&error1,&resabs,&defab1);
    area2 = G_K21(f,a2,b2,&error2,&resabs,&defab2);
    
    /* Improve previous approxminations to integral and error
    and test for accuracy. */
    area12 = area1 + area2;
    erro12 = error1 + error2;
    errsum = errsum + erro12 - errmax;
    area = area + area12 - rlist[maxerr];
    if ((defab1 == error1) || (defab2 == error2)) goto _15; 
    if ((fabs(rlist[maxerr] - area12) > 1.0e-5 * fabs(area12))
          || (erro12 < .99 * errmax)) goto _10;
    if (extrap) iroff2++;
    else iroff1++;
    _10:
      if ((last > 9) && (erro12 > errmax))    /* last > 10 */
    iroff3++;
      _15:
        rlist[maxerr] = area1;
      rlist[last] = area2;
      errbnd = std::max(epsabs,epsrel * fabs(area));
      
      /* Test for roundoff error and eventually set error flag. */
      if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20))
        *ier = 2;
      if (iroff2 > 5)
        *ier = 3;
      
      /* Set error flag in the case that the number of subintervals
      equals limit. */
      if (last == limit)	/* last == limit */
      *ier = 1;
      
      /* Set error flag in the case of bad integrand behavior at some
      points in the integration range. */
      if (std::max(fabs(a1),fabs(b2)) <= (1.0 +1000.0 * epmach) *
          (fabs(a2) + 1000.0*uflow)) 
        *ier = 4;
      
      /* Append the newly-created intervals to the list. */
      if (error2 > error1) goto _20;
      alist[last] = a2;
      blist[maxerr] = b1;
      blist[last] = b2;
      elist[maxerr] = error1;
      elist[last] = error2;
      goto _30;
      _20:
        alist[maxerr] = a2;
      alist[last] = a1;
      blist[last] = b1;
      rlist[maxerr] = area2;
      rlist[last] = area1;
      elist[maxerr] = error2;
      elist[last] = error1;
      
      /* Call dqsort to maintain the descending ordering in the list of error
      estimates and select the subinterval with nrmax-th largest
      error estimate (to be bisected next). */
      _30:
        dqsort(limit,last,&maxerr,&errmax,elist,iord,&nrmax);
      if (errsum <= errbnd) goto _115;
      if (*ier != 0) goto _100;
      if (last == 1) goto _80;	/* last == 2 */
      if (noext) goto _90;		/* goto 90 */
      erlarg -= erlast;
      if (fabs(b1-a1) > small)
        erlarg += erro12;
      if (extrap) goto _40;
      
      /* Test whether the interval to be bisected next is the smallest interval. */
      if ((fabs(blist[maxerr] - alist[maxerr])) > small) 
        goto _90;	/* goto 90 */
      extrap = true;
      nrmax = 1;		/* nrmax = 2 */		
      _40:
        if ((ierro == 3) || (erlarg <= ertest)) goto _60;
        
        /* The smallest interval has the largest error. Before bisecting, decrease
        the sum of the erors over the larger intervals (erlarg) and
        perform extrapolation.) */
        id = nrmax;
        jupbnd = last;
        if (last > (2 + limit/2))
          jupbnd = limit + 3 - last;
        for (k = id;k <= jupbnd; k++) {
          maxerr = iord[nrmax];
          errmax = elist[maxerr];
          if (fabs(blist[maxerr] - alist[maxerr]) > small)
            goto _90;	/* goto 90 */
        nrmax++;
        }
        
        /* Perform extrapolation. */
        _60:
          numrl2++;
        rlist2[numrl2] = area;
        reseps=dqext(&numrl2,rlist2,&abseps,res3la,&nres);
        ktmin++;
        if ((ktmin > 5) && (*abserr < 1.0e-3 * errsum)) *ier = 5;
        if (abseps >= *abserr) goto _70;
        ktmin = 0;
        *abserr = abseps;
        result = reseps;
        correc = erlarg;
        ertest = std::max(epsabs,epsrel * fabs(reseps));
        if (*abserr <= ertest) goto _100;
        
        /* Prepare bisection of the smallest interval. */
        _70:
          if (numrl2 == 0) noext = true;
          if (*ier == 5) goto _100;
          maxerr = iord[0];
          errmax = elist[maxerr];
          nrmax = 0;
          extrap = false;
          small = small * 0.5;
          erlarg = errsum;
          goto _90;		/* goto 90 */
        _80:
          small = fabs(b-a)*0.375;
        erlarg = errsum;
        ertest = errbnd;
        rlist2[1] = area;
        _90:/* 90: */ 
        ; //continue - as in dqagse.f
  }					
  
  _100:
    if (*abserr == oflow) goto _115;
    if ((*ier + ierro) == 0) goto _110;
    if (ierro == 3) *abserr += correc;
    if (*ier == 0) *ier = 3;
    if ((result != 0.0) && (area != 0.0)) goto _105;
    if (*abserr > errsum) goto _115;
    if (area == 0.0) goto _130;
    goto _110;
    _105:
      if (*abserr/fabs(result) > errsum/fabs(area)) goto _115;
      
      /* Test on divergence. */
      _110:
        if ((ksgn == -1) && (std::max(fabs(result),fabs(area)) <= defabs * .01))
          goto _130;
        if ((0.01 > result/area) || (result/area > 100.0) ||
            (errsum > fabs(area))) *ier = 6;
        goto _130;
        
        /* Compute global integral. */
        _115:
          result = 0.0;
        for (k = 0; k <= last; k++)
          result += rlist[k];
        *abserr = errsum;
        _130:
          if (*ier > 2) (*ier)--;
          _140:
            *neval = 42 * last - 21;
          return result;	
  }	



/* DQAGI - Integration over (semi-) infinite intervals. (From QUADPACK)
*
*	Adaptive integration routine which handles functions
*	to be integrated between -infinity to +infinity, or
*	between either of those limits and some finite,
*	real boundary. 
*
*	The adaptive strategy compares results of integration
*	over the interval with the sum of results obtained from
*	integration of bisected interval. Since error estimates
*	are available from each regional integration, the interval
*	with the largest error is bisected and new results are
*	computed. This bisection process is continued until the
*	error is less than the prescribed limit or convergence
*	failure is determined.
*
*	Note that bisection, in the sense used above, refers to
*	bisection of the transformed interval.
*
* PARAMETERS:
*
*	f() - double precision function to be integrated.
*
*	bound - optional finite bound on integral.
*
*	inf - specifies range of integration as follows:
*		inf = -1 -- range is from -infinity to bound, 
*		inf =  1 -- range is from bound to +infinity,
*		inf =  2 -- range is from -infinity to +infinity,
*			    (bound is immaterial in this case).	
*
*	epsabs - absolute accuracy requested.
*
*	epsrel - relative accuracy requested.
*/ 	
double dqagi(double (*f)(double),double bound,int inf,double epsabs,
             double epsrel,double *abserr,int *neval,int *ier)
{
  double abseps,area,area1,area12,area2,a1,a2,b1,b2;
  double boun,correc,defabs,defab1,defab2,dres,erlarg;
  double erlast,errbnd,errmax,error1,error2,erro12;
  double errsum,ertest,resabs,reseps,result,res3la[3];
  double alist[LIMIT],blist[LIMIT],elist[LIMIT],rlist[LIMIT];
  double rlist2[52],small;
  
  int id,ierro,iord[LIMIT],iroff1,iroff2,iroff3,jupbnd,k,ksgn;
  int ktmin,last,maxerr,nres,nrmax,numrl2;
  int limit;
  bool extrap, noext;
  
  limit = LIMIT - 1;
  /* Test validity of parameters. */
  *ier = 0;
  *neval = 0;
  last = 0;
  result = 0.0;
  *abserr = 0.0;
  alist[0] = 0.0;
  blist[0] = 1.0;
  rlist[0] = 0.0;
  elist[0] = 0.0;
  iord[0] = 0;
  if ((epsabs < 0.0) && (epsrel < 0.0)) *ier = 6;
  if ((inf != 1) && (inf != -1) && (inf != 2)) *ier = 6;
  if (*ier == 6) return result;
  
  /* First approximation to the integral. */
  boun = bound;
  if (inf == 2) boun = 0.0;
  
  result = G_K15I(f,boun,inf,0.0,1.0,abserr,&defabs,&resabs);
  
  /* Test on accuracy. */
  last = 0;
  rlist[0] = result;
  elist[0] = *abserr;	
  iord[0] = 0;
  dres = fabs(result);
  errbnd = std::max(epsabs,epsrel*dres);
  if ((*abserr <= 100.0 * epmach * defabs) && (*abserr > errbnd))
    *ier = 2;
  if (limit == 0) *ier = 1;
  if ((*ier != 0) || ((*abserr <= errbnd) && (*abserr != resabs)) ||
      (*abserr == 0.0)) goto _130;
  
  /* Initialization for main loop. */
  rlist2[0] = result;
  errmax = *abserr;
  maxerr = 0; 			/* maxerr = 1 */
  area = result;
  errsum = *abserr;
  *abserr = oflow;
  nrmax = 0;
  nres = 0;          /* nres = 0 */
  ktmin = 0;
  numrl2 = 1;			/* numrl2 = 2 */
  extrap = false;
  noext = false;
  ierro = 0;
  iroff1 = 0;
  iroff2 = 0;
  iroff3 = 0;
  ksgn = -1;
  if (dres > (1.0 - 50.0 * epmach) * defabs)
    ksgn = 1;
  
  /* Main loop. */
  for (last = 1; last <= limit; last++) {
    a1 = alist[maxerr];
    b1 = 0.5 * (alist[maxerr] + blist[maxerr]);
    a2 = b1;
    b2 = blist[maxerr];
    erlast = errmax;
    area1 = G_K15I(f,boun,inf,a1,b1,&error1,&resabs,&defab1);
    area2 = G_K15I(f,boun,inf,a2,b2,&error2,&resabs,&defab2);
    
    /* Improve previous approxminations to integral and error
    and test for accuracy. */
    area12 = area1 + area2;
    erro12 = error1 + error2;
    errsum = errsum + erro12 - errmax;
    area = area + area12 - rlist[maxerr];
    if ((defab1 == error1) || (defab2 == error2)) goto _15; 
    if ((fabs(rlist[maxerr] - area12) > 1.0e-5 * fabs(area12))
          || (erro12 < .99 * errmax)) goto _10;
    if (extrap) iroff2++;
    else iroff1++;
    _10:
      if ((last > 9) && (erro12 > errmax))    /* last > 10 */
    iroff3++;
      _15:
        rlist[maxerr] = area1;
      rlist[last] = area2;
      errbnd = std::max(epsabs,epsrel * fabs(area));
      
      /* Test for roundoff error and eventually set error flag. */
      if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20))
        *ier = 2;
      if (iroff2 > 5)
        *ier = 3;
      
      /* Set error flag in the case that the number of subintervals
      equals limit. */
      if (last == limit)	/* last == limit */
      *ier = 1;
      
      /* Set error flag in the case of bad integrand behavior at some
      points in the integration range. */
      if (std::max(fabs(a1),fabs(b2)) <= (1.0 +1000.0 * epmach) *
          (fabs(a2) + 1000.0*uflow)) 
        *ier = 4;
      
      /* Append the newly-created intervals to the list. */
      if (error2 <= error1) {
        alist[last] = a2;
        blist[maxerr] = b1;
        blist[last] = b2;
        elist[maxerr] = error1;
        elist[last] = error2;
      }
      else {
        alist[maxerr] = a2;
        alist[last] = a1;
        blist[last] = b1;
        rlist[maxerr] = area2;
        rlist[last] = area1;
        elist[maxerr] = error2;
        elist[last] = error1;
      }
      /* Call dqsort to maintain the descending ordering in the list of error
      estimates and select the subinterval with nrmax-th largest
      error estimate (to be bisected next). */
      
      dqsort(limit,last,&maxerr,&errmax,elist,iord,&nrmax);
      if (errsum <= errbnd) goto _115;
      if (*ier != 0) goto _100;
      if (last == 1) goto _80;	/* last == 2 */
      if (noext) continue;  //goto _90;
      erlarg -= erlast;
      if (fabs(b1-a1) > small)
        erlarg += erro12;
      if (extrap) goto _40;
      
      /* Test whether the interval to be bisected next is the smallest interval. */
      if ((fabs(blist[maxerr] - alist[maxerr])) > small) 
        goto _90;
      extrap = true;
      nrmax = 1;		/* nrmax = 2 */		
      _40:
        if ((ierro == 3) || (erlarg <= ertest)) goto _60;
        
        /* The smallest interval has the largest error. Before bisecting, decrease
        the sum of the erors over the larger intervals (erlarg) and
        perform extrapolation.) */
        id = nrmax;
        jupbnd = last;
        if (last > (2 + limit/2))
          jupbnd = limit + 3 - last;
        for (k = id;k <= jupbnd; k++) {
          maxerr = iord[nrmax];
          errmax = elist[maxerr];
          if (fabs(blist[maxerr] - alist[maxerr]) > small)
            goto _90;
          nrmax++;
        }
        
        /* Perform extrapolation. */
        _60:
          numrl2++;
        rlist2[numrl2] = area;
        reseps=dqext(&numrl2,rlist2,&abseps,res3la,&nres);
        ktmin++;
        if ((ktmin > 5) && (*abserr < 1.0e-3 * errsum)) *ier = 5;
        if (abseps >= *abserr) goto _70;
        ktmin = 0;
        *abserr = abseps;
        result = reseps;
        correc = erlarg;
        ertest = std::max(epsabs,epsrel * fabs(reseps));
        if (*abserr <= ertest) goto _100;
        
        /* Prepare bisection of the smallest interval. */
        _70:
          if (numrl2 == 0) noext = true;
          if (*ier == 5) goto _100;
          maxerr = iord[0];
          errmax = elist[maxerr];
          nrmax = 0;
          extrap = false;
          small = small * 0.5;
          erlarg = errsum;
          continue;
          _80:
            small = .375;
          erlarg = errsum;
          ertest = errbnd;
          rlist2[0] = area;
          _90:/* 90: */
        ; //Continue
  }					
  _100:
    if (*abserr == oflow) goto _115;
    if ((*ier + ierro) == 0) goto _110;
    if (ierro == 3) *abserr += correc;
    if (*ier == 0) *ier = 3;
    if ((result != 0.0) && (area != 0.0)) goto _105;
    if (*abserr > errsum) goto _115;
    if (area == 0.0) goto _130;
    goto _110;
    _105:
      if (*abserr/fabs(result) > errsum/fabs(area)) goto _115;
      
      /* Test on divergence. */
      _110:
        if ((ksgn == -1) && (std::max(fabs(result),fabs(area)) <= defabs * .01))
          goto _130;
        if ((0.01 > result/area) || (result/area > 100.0) ||
            (errsum > fabs(area))) *ier = 6;
        goto _130;
        
        /* Compute global integral. */
        _115:
          result = 0.0;
        for (k = 0; k <= last; k++)
          result += rlist[k];
        *abserr = errsum;
        _130:
          *neval = 30 * last + 15;
        if (inf == 2) *neval *= 2;
        if (*ier > 2) (*ier)--;
        return result;	
  }	


  double integrate(double (*f)(double), double lower_value, double upper_value, int * ier)
  {
    //rel and abs tol are the same as in R
    double reltol = pow(std::numeric_limits<double>::epsilon(), 0.25);
    //to pass as pointer
    int tmp1;
    double tmp2;
    return dqags(f, lower_value, upper_value, reltol, reltol , &tmp2, &tmp1, ier); 
  }


}//quadpack




