#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"

#include "MapEx.h"
#include "NodeVisitor.h"
#include "NodeVisitorDiff.h"
#include "util.h"

using namespace capd;
//using namespace capd::dynset;
using namespace std;

//typedef capd::dynset::C1PpedSet<IMatrix> Pped;
//typedef capd::dynset::C1Pped2Set<IMatrix> Pped;
//typedef capd::vectalg::Vector<Pped, 4> PpedVector;

int main()
{
	cout.precision(17);
	cout.setf(ios::fixed,ios::floatfield);

	//// This is vector field for the Rossler system
	//IMap vectorField("par:a,b;var:x,y,z;fun:-(y+z),x+b*y,b+z*(x-a);");
	//
	//// set chaotic parameter values
	//vectorField.setParameter("a",interval(57.)/10.); // a=5.7
	//vectorField.setParameter("b",interval(2.)/10.); // b=0.2

	//IVector x(3);
	//x[0] = 0.;
	//x[1] = -8.3809417428298;
	//x[2] = 0.029590060630665;
	//// take some box around c
	//x += interval(-1,1)*1e-10; 


	// the Planar Restricted Circular 3 Body Problem
	//IMap vectorField("par:mu,mj; var:x,y,dx,dy; fun:dx, dy, x - mj*(x+mu)*sqrt((x+mu)^2+y^2)^(-3) - mu*(x-mj)*sqrt((x-mj)^2+y^2)^(-3) + 2*dy, y*(1-mj*sqrt((x+mu)^2+y^2)^(-3) - mu*sqrt((x-mj)^2+y^2)^(-3)) - 2*dx;");
	//
	//// mass ratio
	//vectorField.setParameter("mu", interval(123.0)/interval(10000.0));
	//vectorField.setParameter("mj", 1.0-mu);

	//IVector x(4);
	//x[0]=0.9928634178;
	//x[1]=0.0;
	//x[2]=0.0;
	//x[3]=2.129213043;

	try{

	IMap vectorField("var:t,x,v; fun:1,v,-sin(x);");
	//IMap vectorField("var:t,x,v; fun:1,v,-10.0;");
	//MyIMap vectorField;
	//vectorField.setup();

	//init(3);
	//putVariable("t");
	//putVariable("x");
	//putVariable("v");
	//putScalarNode(1.0);
	//putTree();
	//putVarNode(2);
	//putTree();
	//putScalarNode(0.);
	//putVarNode(1);
	//putSinNode();
	//putDifNode();
	//putTree();
	//// compute deriv
	//getIMap()->compDiff();

	IVector x(3);
	x[0]=0.0;
	x[1]=1.0;
	x[2]=1.0;

	// Van der Pol
	//IMap vectorField("par:mu; var:t,x,y; fun:1,y,mu*(1-x^2)*y-x;");
	//vectorField.setParameter("mu", 2);
	//
	//IVector x(3);
	//x[0]=0.0;
	//x[1]=2.0;
	//x[2]=0.0;


	//IMap vectorField("var:x,y,dx,dy; fun:dx,dy,-x/(x^2+y^2)^(3/2),-y/(x^2+y^2)^(3/2);");
	//
	//IVector x(4);
	//x[0]=0.5;
	//x[1]=0.0;
	//x[2]=0.0;
	//x[3]=sqrt(3.0);


	// The solver uses high order enclosure method to verify the existence of the solution. 
	// The order will be set to 20.
	// The time step (when step control is turned off) will be 0.1.
	// The time step control is turned on by default but the solver must know if we want to 
	// integrate forwards or backwards (then put negative number).
	ITaylor solver(vectorField, 20, 0.1);
	//ITaylor solver(*getIMap(), 20, 0.1);
	ITimeMap timeMap(solver);

	// define a doubleton representation of the interval vector x
	C0Rect2Set s(x);
	//C0PpedSet s(x);
	//IEuclNorm r;
	//C1Rect2Set s(x, r);
	//Pped s(x, r);

	// Here we start to integrate. The time of integration is set to T=10. 
	double T=5;
	//double T=0.1;
	timeMap.stopAfterStep(true);
	interval prevTime(0.);

	cout << '{' << endl;

	//try{
		do 
		{
			//IVector v = timeMap(T,s);
			timeMap.moveSet(T, s);
			//cout << s.show();
			//dumpPped(cout, s);
			//dumpPipe(cout, timeMap.getCurrentTime(), s);
			//cout << endl;

			interval stepMade = solver.getStep();
			//cout << endl << "step made: " << stepMade << endl;
			const ITaylor::CurveType& curve = solver.getCurve();

			int grid=5;
			//int grid=1;
			for(int i=0;i<grid;++i)
			{
				interval subsetOfDomain = interval(i,i+1)*stepMade/grid;
				//intersection(domain,subsetOfDomain,subsetOfDomain);
				//interval subsetOfDomain = stepMade;
				//interval subsetOfDomain = interval(0,1)*stepMade;

				IVector v = curve(subsetOfDomain);
				//std::cout << "\nenclosure for t=" << prevTime + subsetOfDomain << ":  " << v;
				//std::cout << "\ndiam(enclosure): " << diam(v);
				dumpPipe1(cout, prevTime+subsetOfDomain, v);
			}
			prevTime = timeMap.getCurrentTime();
			//cout << endl << "current time: " << prevTime << endl << endl;

		} while(!timeMap.completed());
	} catch(exception& e)
	{
		cout << "\n\nException caught!\n" << e.what() << endl << endl;
	}

	cout << "{} }" << endl;
}  // END
