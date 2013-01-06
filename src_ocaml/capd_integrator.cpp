/**
 * An integrator using CAPD
 */

#include <iostream>
#include <iomanip>
#include <string>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"

#include "../src_cpp/MapEx.h"

#include "capd_integrator.h"

using namespace std;
using namespace capd;

int test(const char *input) {

	cout.precision(17);
	cout.setf(ios::fixed,ios::floatfield);

	//cout << input << endl;
	//IMap vectorField(input);
	//IMap vectorField("var:t,x,v; fun:1,v,-10.0;");
	DerMap vectorField;

	IVector x(3);
	x[0]=0.0;
	x[1]=1.0;
	x[2]=1.0;

	ITaylor solver(vectorField, 20, 0.1);
	ITimeMap timeMap(solver);
	C0Rect2Set s(x);

	double T=1;
	//double T=0.1;
	timeMap.stopAfterStep(true);
	interval prevTime(0.);

	cout << '{' << endl;

	try {
		do 
		{
			//IVector v = timeMap(T,s);
			timeMap.moveSet(T, s);
			cout << s.show();
			//dumpPped(cout, s);
			//dumpPipe(cout, timeMap.getCurrentTime(), s);
			cout << endl;

			interval stepMade = solver.getStep();
			//cout << endl << "step made: " << stepMade << endl;
			const ITaylor::CurveType& curve = solver.getCurve();

			//int grid=5;
			int grid=1;
			for(int i=0;i<grid;++i)
			{
				//interval subsetOfDomain = interval(i,i+1)*stepMade/grid;
				//intersection(domain,subsetOfDomain,subsetOfDomain);
				//interval subsetOfDomain = stepMade;
				interval subsetOfDomain = interval(0,1)*stepMade;

				IVector v = curve(subsetOfDomain);
				//std::cout << "\nenclosure for t=" << prevTime + subsetOfDomain << ":  " << v;
				//std::cout << "\ndiam(enclosure): " << diam(v);
				//dumpPipe1(cout, prevTime+subsetOfDomain, v);
			}
			prevTime = timeMap.getCurrentTime();
			//cout << endl << "current time: " << prevTime << endl << endl;

		} while(!timeMap.completed());
	} catch(exception& e)
	{
		cout << "\n\nException caught!\n" << e.what() << endl << endl;
	}

	cout << "{} }" << endl;

	//return EXIT_FAILURE;
	return EXIT_SUCCESS;
}
