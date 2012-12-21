#include <iostream>

#include "capd/capdlib.h"
#include "capd/dynset/C1PpedSet.h"
#include "capd/dynset/C1Pped2Set.h"

inline void dumpInterval(std::ostream& out, const interval& value)
{
	out << '[' << value.leftBound() << ',' << value.rightBound() << ']';
	out.flush();
}


inline void dumpPipe(std::ostream& out,
					 const interval& time, const capd::IVector& value)
{
	out << "{" << std::endl;
	out << "  \"plot\":\t0," << std::endl;

	// time
	out << "  \"time\":\t";
	out << '[' << time.leftBound() << ',' << time.rightBound() << ']';

	// each axis
	for (int i(0); i < value.size(); ++i) {
		out << ',' << std::endl;

		// name
		out << "  \"x" << i << "\":\t";
		// value
		out << '[' << value[i].leftBound() << ',' << value[i].rightBound() << ']';
	}

	out << std::endl << "}" << std::endl << std::endl;
	out.flush();
}

inline void dumpPipe1(std::ostream& out,
					  const interval& time, const capd::IVector& value,
					  bool print_comma = true)
{
	out << "{ " << std::endl;

	// time
	out << '{' << time.leftBound() << ',' << time.rightBound() << '}';

	// each axis
	for (int i(0); i < value.size(); ++i) {
		out << ", " << std::endl;
		out << '{' << value[i].leftBound() << ',' << value[i].rightBound() << '}';
	}

	out << std::endl << (print_comma ? " }," : " }") << std::endl;
	out.flush();
}


template<typename Piped>
inline void dumpPped(std::ostream& out, const Piped& value);

template<>
inline void dumpPped(std::ostream& out, 
					 const capd::C0Rect2Set& value)
{
	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]," << std::endl;
	out.flush();
}

template<>
inline void dumpPped(std::ostream& out, 
					 const capd::dynset::C1PpedSet<capd::IMatrix>& value)
{
	out << "pped[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]," << std::endl;
	out.flush();
}

template<>
inline void dumpPped(std::ostream& out, 
					 const capd::dynset::C1Pped2Set<capd::IMatrix>& value)
{
	out << "pped2[" << std::endl;

	// x
	out << "{ " << std::endl;
	bool first(true);
	for (int i(0); i < value.m_x.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		//out << '{' << value.m_x[i].leftBound() << ',' << value.m_x[i].rightBound() << '}';
		out << value.m_x[i].leftBound();
	}
	out << std::endl << "}," << std::endl;

	// B
	out << "{ " << std::endl;
	const_MatrixIterator<capd::IMatrix> it(value.m_B);
	for (int i(1); i <= value.m_B.numberOfRows(); ++i) {
		it = value.m_B.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_B.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r[i].leftBound() << ',' << value.m_r[i].rightBound() << '}';
	}
	out << std::endl << "}," << std::endl;

	// C
	out << "{ " << std::endl;
	it = value.m_C;
	for (int i(1); i <= value.m_C.numberOfRows(); ++i) {
		it = value.m_C.beginOfRow(i);
		if (i > 1) out << ',' << std::endl;
		out << '{';
		for (int j(1); j <= value.m_C.numberOfColumns(); ++j) {
			it.moveToNextColumn();
			if (j > 1) out << ", ";
			//out << '{' << (*it).leftBound() << ',' << (*it).rightBound() << '}';
			out << (*it).leftBound();
		}
		out << '}';
	}
	out << std::endl << "}," << std::endl;

	// r0
	out << "{" << std::endl;
	first = true;
	for (int i(0); i < value.m_r0.size(); ++i) {
		if (!first)
			out << ", " << std::endl;
		else
			first = false;

		out << '{' << value.m_r0[i].leftBound() << ',' << value.m_r0[i].rightBound() << '}';
	}
	out << std::endl << "}" << std::endl;

	out << std::endl << "]," << std::endl;
	out.flush();
}


template<typename Piped>
inline void dumpPipe(std::ostream& out, const interval& time, const Piped& value);

inline void dumpPipe(std::ostream& out,
					 const interval& time, const capd::C0Rect2Set& value)
{
	out << "{ " << std::endl;

	// time
	dumpInterval(out, time);

	out << "," << std::endl;

	// each axis
	dumpPped(out, value);

	out << std::endl << " }," << std::endl;
	out.flush();
}
