#ifndef CLV_QUADP_HPP
#define CLV_QUADP_HPP


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



namespace quadpack{
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
  double dqags(double (*f)(double),double a,double b,double epsabs,double epsrel,double *abserr,int *neval,int *ier);







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
               double epsrel,double *abserr,int *neval,int *ier);


  /*
   * INTEGRATE over finite intervals
   *
   * wrapper around dqags
   * with absolut and relative accuracy = machine_epsilon ^ 0.25;
   */
  double integrate(double (*f)(double), double lower_value, double upper_value, int * ier);



}

#endif
