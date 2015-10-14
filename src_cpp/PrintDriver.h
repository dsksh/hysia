#ifndef _PRINT_DVR_H_
#define _PRINT_DVR_H_ 

#include <iostream>
#include <memory>

#include "Parallelepiped.h"

namespace capd { 

class PrintDriver
{
public:
	/// printing functions
	virtual void printBegin(std::ostream&) = 0;
	virtual void printEnd(std::ostream&) = 0;
	virtual void printStep(std::ostream&, const int, const char *, const double) = 0;
	virtual void printInterval(std::ostream&, const capd::interval&) = 0;

	virtual void printBox(std::ostream&, const capd::IVector&) = 0;
	virtual void printPped(std::ostream&, 
		const capd::IVector&, const capd::IMatrix&, const capd::IVector&) = 0;

	virtual void prologuePipe(std::ostream&, const capd::interval&) = 0;
	virtual void epiloguePipe(std::ostream&) = 0;
};

class PrintDriverJson : public PrintDriver
{
public:
	PrintDriverJson() {}

	/// printing functions
	virtual void printBegin(std::ostream&);
	virtual void printEnd(std::ostream&);
	virtual void printStep(std::ostream&, const int, const char *, const double);
	virtual void printInterval(std::ostream&, const capd::interval&);

	virtual void printBox(std::ostream&, const capd::IVector&);
	virtual void printPped(std::ostream&, 
		const capd::IVector&, const capd::IMatrix&, const capd::IVector&);

	virtual void prologuePipe(std::ostream&, const capd::interval&);
	virtual void epiloguePipe(std::ostream&);
};

class PrintDriverMath : public PrintDriver
{
public:
	PrintDriverMath() {}

	/// printing functions
	virtual void printBegin(std::ostream&);
	virtual void printEnd(std::ostream&);
	virtual void printStep(std::ostream&, const int, const char *, const double);
	virtual void printInterval(std::ostream&, const capd::interval&);

	virtual void printBox(std::ostream&, const capd::IVector&);
	virtual void printPped(std::ostream&, 
		const capd::IVector&, const capd::IMatrix&, const capd::IVector&);

	virtual void prologuePipe(std::ostream&, const capd::interval&);
	virtual void epiloguePipe(std::ostream&);
};

typedef std::auto_ptr<PrintDriver> PrintDriverPtr;

} // the end of the namespace capd

extern capd::PrintDriverPtr g_print_dvr;

#endif // _PRINT_DVR_H_
