#include <stdio.h>
#include <math.h>

int main() {
	double c = 30.403243784;
	int i;
	double x,xo;
	x = -1;
	for (i = 0; i < 10000; i++) {
		xo = x;
		x = floor(pow(2.0,c-x*x))*1e-9;
		printf("%5.9f\n",x);
	}
	printf("%5.9f\n",xo+x);
}
