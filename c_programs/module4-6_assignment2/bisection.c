#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double a1 = -4.0; // static memory variable
double b1 = -2.0; // static memory variable
double a2 = 2.0;  // static memory variable
double b2 = 4.0;  // static memory variable

int bisect_hist(double *hist, double a, double b, double tol);

// define the function exp(-x)*(3.2*sin(x)- 0.5*cos(x)) = 3
double f(double x0){
    double x;
    x = exp(-x0)*(3.2*sin(x0)- 0.5*cos(x0)) - 3;
    return x;
}

int bisect_hist(double *hist, double a, double b, double tol) {
    int it = 0;

    printf("Execution for interval: [a,b] = [%f, %f] \n", a, b);

    if (f(a)*f(b) >= 0 ) {
        printf("f(a)f(b)<0 not satisfied! f has no root in [a,b] = [%f, %f]\n", a, b);
        return -1;
    }

    while (fabs(b-a) > tol) {
        double mid = (a + b)/2.0;
        hist[it] = mid;
        if (f(a) * f(mid) < 0) {
            b = mid;
        }
        else {
            a = mid;
        }
        it++;
    }

    return it;
}

int main(void){
    double tol = 1e-4; // stack memory variable

    double *hist_xstar1; // stack memory variable
    hist_xstar1 = malloc(100*sizeof(double)); // heap memory allocated, and hist_xstar points to it

    double *hist_xstar2; // stack memory variable
    hist_xstar2 = malloc(100*sizeof(double)); // heap memory allocated, and hist_xstar2 points to it

    // call the bisection method by reference
    int num_it1 = bisect_hist(hist_xstar1, a1, b1, tol);

    // Is it a root?
    if (num_it1 >= 0) {
        printf("We found xstar1: %f, and f(xstar1) is: %e \n", hist_xstar1[num_it1-1], f(hist_xstar1[num_it1-1]));
        printf("The bisection method has found the root %f at the %d iteration \n", hist_xstar1[num_it1-1], num_it1);
    }

    int num_it2 = bisect_hist(hist_xstar2, a2, b2, tol);

    // is this a root?
    if (num_it2 >= 0) {
        printf("We found xstar2: %f, and f(xstar2) is: %f \n", hist_xstar2[num_it2-1], f(hist_xstar1[num_it2-1]));
        printf("The bisection method has found the root %f at the %d iteration \n", hist_xstar2[num_it2-1], num_it2);
    }

    // free the allocated heap memory
    free(hist_xstar1);
    free(hist_xstar2);
}
