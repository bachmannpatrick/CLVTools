
#ifndef HYP2F1_HPP
#define HYP2F1_HPP

namespace cephes{

    //Prototypes

    //polevl.c
    double polevl ( double, double *, int );

    //psi.c
    double psi ( double );

    //hyp2f1.c
    double hypergeom2F1(double a , double b , double c, double x);

}

#endif
