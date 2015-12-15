#ifndef _CAPD_MAP_EX_H_ 
#define _CAPD_MAP_EX_H_ 

#include <string>
#include <stdexcept>
#include <sstream>
#include <vector>

#include "capd/capdlib.h"

//#include "capd/map/BasicFunction.h"
//#include "capd/map/Function.h"
//#include "capd/map/C2Coeff.h"
//#include "capd/map/CnCoeff.h"
//#include "capd/map/SeriesContainer.h"

#include "capd/map/CnCoeff.hpp"
#include "capd/map/C2Coeff.hpp"
#include "capd/map/BasicFunction.hpp"
#include "capd/map/CnMap.h"
#include "capd/map/Node.h"

namespace capd{ 

class DerMap : public capd::map::CnMap<capd::IMatrix,1>
{
public:
	typedef IMatrix MatrixType;
	typedef IMatrix::RowVectorType VectorType;
	typedef VectorType::ScalarType ScalarType;
	typedef capd::map::Node<ScalarType> NodeType;
	typedef capd::map::CnContainer<NodeType*> TreesContainer;

	DerMap();
	DerMap(int, int, int = 0);
	DerMap(const char *);
	DerMap(const DerMap&);
	~DerMap();

	int numParams() { return m_dim-m_indexOfFirstParam; }

	int putVariable(const char *);
	int putParameter(const char *);
	int putAndSetParameter(const char *, const interval&);
	void setValue(const VectorType&);
	ScalarType *getValue() const { return m_val; }
	void putTree(const int, capd::map::Node<ScalarType> *node);
	void putDTree(const int, const int, capd::map::Node<ScalarType> *node);
	void doneTree();

	NodeType *createVarNode(int);

	void compDiff();

	//template<typename T>
	//NodeType *compDiffNode(T& node, const int index);

	//const int getDim() { return m_dim; }

private:
	int m_trees_idx;
	int m_dtrees_idx;
};

class AuxMap : protected capd::map::BasicFunction<IMatrix::ScalarType>
{
public:
	typedef DerMap::MatrixType MatrixType;
	typedef DerMap::VectorType VectorType;
	typedef DerMap::ScalarType ScalarType;
	typedef DerMap::NodeType NodeType;
	typedef DerMap::TreesContainer TreesContainer;

    AuxMap(DerMap&, int);
	AuxMap(const AuxMap&);
	~AuxMap();

	void putParameter(const char *);
	void putTree(const int, NodeType *node);
	void putDTree(const int, const int, NodeType *node);

	void reset();
	virtual VectorType operator()();
	virtual VectorType operator()(const VectorType& val);
	virtual MatrixType der();
	virtual MatrixType operator[](const VectorType& val);

	void setParameter(const char *name, const interval& val);

	const int getDim() { return m_dim; }

protected:
	DerMap& m_dmap;
	TreesContainer m_trees;
	int m_dim2;
	int m_dim_f;
	const std::string& currentFormula()
   	{ static const std::string s(""); return s; }

	using capd::map::BasicFunction<ScalarType>::m_dim;
	using capd::map::BasicFunction<ScalarType>::m_order;
	//using capd::map::BasicFunction<ScalarType>::m_size;
};

class DiffMap : public AuxMap
{
public:
	typedef DerMap::VectorType VectorType;
	typedef DerMap::ScalarType ScalarType;

    DiffMap(bool, bool, AuxMap&, AuxMap&);
	//DiffMap(const DiffMap&);
	~DiffMap();

	virtual VectorType operator()();
	virtual VectorType operator()(const VectorType& val);
	virtual MatrixType der();
	virtual MatrixType operator[](const VectorType& val);

protected:
	bool m_neg1;
	bool m_neg2;
	AuxMap& m_amap1;
	AuxMap& m_amap2;
};

} // the end of the namespace capd

#endif // _CAPD_MAP_EX_H_ 
