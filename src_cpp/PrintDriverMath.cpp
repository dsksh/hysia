#include <iostream>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"
#include "capd/geomset/AffineSet.h"

#include "Parallelepiped.h"
#include "util.h"

namespace capd { 

/// printing functions
void PrintDriverMath::printBegin(std::ostream& out)
{
	out << '{' << std::endl;
}

void PrintDriverMath::printEnd(std::ostream& out)
{
	out << '}' << std::endl;
}

/// printing functions
void PrintDriverMath::printStep(std::ostream& out, const int stepId, const char *lid, 
								const double sim_time)
{
	out << "(* step " << stepId << " at " << lid << ", time: (" << sim_time << ", " << (getTime()/1000.) << ") *)" << std::endl;
	// FIXME
	//std::cout << "step " << stepId << " at " << lid << ", time: " << sim_time << " (sim), " << (getTime()/1000.) << " (exec)" << std::endl;
}

void PrintDriverMath::printInterval(std::ostream& out, const capd::interval& value)
{
	out << '{' << value.leftBound() << ',' << value.rightBound() << '}';
	out.flush();
}


void PrintDriverMath::printBox(std::ostream& out, const capd::IVector& value) 
{
	out << "box[{" << std::endl;

	// each axis
	for (int i(0); i < value.size(); ++i) {
		if (i>0) out << ", " << std::endl;
		printInterval(out, value[i]);
	}

	out << "}]" << std::endl;
	out.flush();
}

void PrintDriverMath::printPped(std::ostream& out, 
		const capd::IVector& x, const capd::IMatrix& B, const capd::IVector& r)
{
	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(B);
	/*for (int i(1); i <= B.numberOfRows(); ++i) {
		it = B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}*/
	for (int i(1); i <= B.numberOfRows(); ++i) {
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			out << B(i,j).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << r[i].leftBound() << ',' << r[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]" << std::endl;
	out.flush();
}


void PrintDriverMath::prologuePipe(std::ostream& out, const capd::interval& time)
{
	out << "{ " << std::endl;

	// time
	printInterval(out, time);

	out << "," << std::endl;
}

void PrintDriverMath::epiloguePipe(std::ostream& out)
{
	out << "}" << std::endl;
	out.flush();
}

} // the end of the namespace capd
