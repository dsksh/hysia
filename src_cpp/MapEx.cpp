#include <string>
#include <stdexcept>
#include <sstream>
#include <vector>

#include "capd/capdlib.h"

#include "capd/map/CnCoeff.hpp"
#include "capd/map/C2Coeff.hpp"
#include "capd/map/BasicFunction.hpp"
#include "capd/map/CnMap.h"
#include "capd/map/Parser.h"

#include "NodeVisitor.h"
#include "NodeVisitorDiff.h"
#include "NodeEx.h"
#include "MapEx.h"

namespace capd{ 

using namespace std;
using namespace map;

/* DerMap implementation. */

DerMap::DerMap() 
  : capd::map::CnMap<capd::IMatrix,1>(),
	m_trees_idx(0), m_dtrees_idx(0)
{ }

DerMap::DerMap(int dim, int order, int n_param) 
  : capd::map::CnMap<capd::IMatrix,1>(),
	m_trees_idx(0), m_dtrees_idx(0)
{
	m_dim = dim + n_param; 
	m_dim2 = dim;
	m_indexOfFirstParam = dim;
	m_order = order;
	m_size = m_dim*m_order;
	m_val = new ScalarType[m_size];
	std::fill(m_val, m_val+m_size, ScalarType(0.));
	m_trees = TreesContainer(dim, 1);
//cout << "DM init: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;

	m_var.clear();
}

DerMap::DerMap(const DerMap& rhs)
	: capd::map::CnMap<capd::IMatrix,1>(),
	  m_trees_idx(0), m_dtrees_idx(0)
{
    m_dim = rhs.m_dim; 
	m_dim2 = rhs.m_dim2;
    m_indexOfFirstParam = rhs.m_indexOfFirstParam;
    m_order = rhs.m_order; 
	m_size = rhs.m_size;
	m_var = rhs.m_var;
    //m_val = rhs.m_val;
	m_val = new ScalarType[m_size];
//cout << "DM init: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;
	for (int i(0); i < m_size; ++i)
		m_val[i] = rhs.m_val[i];
	//std::fill(m_val, m_val+m_size, ScalarType(0.));
	m_trees = TreesContainer(m_dim2, 1);
}

DerMap::DerMap(const char *s)
	: capd::map::CnMap<capd::IMatrix,1>(s),
	  m_trees_idx(0), m_dtrees_idx(0)
{
//cout << "DM init: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;
}

DerMap::~DerMap() 
{
	//cout << "dismiss DerMap: " << m_trees_idx << endl;
}

int DerMap::putVariable(const char *name) 
{
	m_var.push_back(name);
	return m_var.size()-1; // TODO
}

int DerMap::putParameter(const char *name) 
{
	return putVariable(name);
}

int DerMap::putAndSetParameter(const char *name, const interval& val) 
{
	m_var.push_back(name);
	int i(m_var.size()-1); // TODO
	m_val[i] = val;
	return i;
}

void DerMap::setValue(const VectorType& val) 
{
  	for (int i(0); i < m_indexOfFirstParam; ++i)
	    m_val[m_order*i] = val[i];
}

void DerMap::putTree(const int i, Node<ScalarType> *node) 
{
	m_trees(i) = node;
	++(m_trees(i)->m_links);
}

DerMap::NodeType *DerMap::createVarNode(int index) 
{
	return new VarNodeEx<ScalarType>(m_order, &m_val[m_order*index], index);
}


void DerMap::compDiff() 
{
	for (int i(0); i<m_dim2; ++i) {
		NodeEx<ScalarType> *t = dynamic_cast<NodeEx<ScalarType> *>(m_trees(i));
		for (int j(0); j<m_dim2; ++j) {
			DiffVisitor<ScalarType> visitor(m_order, j);
			t->accept(visitor);
			m_trees(i,j) = visitor.getNode();
			++(m_trees(i,j)->m_links);
		}
	}
}

void DerMap::putDTree(const int i, const int j, Node<ScalarType> *node) 
{
	m_trees(i,j) = node;
	++(m_trees(i,j)->m_links);
	++m_dtrees_idx;
}

void DerMap::doneTree()
{
	++m_trees_idx;
	m_dtrees_idx = 0;
}


/* AuxMap implementation. */

AuxMap::AuxMap(DerMap& dmap, int dim_f) 
	: BasicFunction<ScalarType>(), m_dmap(dmap), m_trees(dmap.dimension(),1)
{
	m_dim = dmap.dimension() + dmap.numParams();
	m_dim2 = dmap.dimension();
	m_dim_f = dim_f;
	m_indexOfFirstParam = m_dim2;
	m_order = dmap.getOrder();

	m_size = m_dim*m_order;
	m_val = new ScalarType[m_size];
	std::fill(m_val, m_val+m_size, ScalarType(0.));
//cout << "AM init: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;
}

inline AuxMap::AuxMap(const AuxMap& rhs)
	: BasicFunction<ScalarType>(), m_dmap(rhs.m_dmap), 
	  //m_trees(rhs.m_trees),
	  m_trees(rhs.m_dim2,1) // TODO
	  //m_trees(0,0) // TODO
{
	m_dim = rhs.m_dim;
	m_dim2 = rhs.m_dmap.dimension();
	m_dim_f = rhs.m_dim_f;
	m_indexOfFirstParam = rhs.m_indexOfFirstParam;
	m_order = rhs.m_order; 
	m_size = rhs.m_size; 
	m_val = new ScalarType[m_size];
	std::fill(m_val, m_val+m_size, ScalarType(0.));
//cout << "AM init: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;
}

AuxMap::~AuxMap() 
{
	//cout << "dismiss CapdFun" << endl;
}

void AuxMap::putTree(const int i, Node<ScalarType> *node) 
{
	m_trees(i) = node;
	++(m_trees(i)->m_links);
}

void AuxMap::putDTree(const int i, const int j, Node<ScalarType> *node) 
{
	m_trees(i,j) = node;
	++(m_trees(i,j)->m_links);
}

void AuxMap::reset()
{
	// propagate the current order and variable values.
	int o(m_dmap.getOrder());
	setOrder(o);
	for (int i(0); i < m_dim_f; ++i) 
		m_trees(i)->setOrder(o, m_dmap.getValue());
	for (int i(0); i < m_dim_f; ++i)
		for (int j(0); j < m_dim2; ++j)
			m_trees(i,j)->setOrder(o, m_dmap.getValue());

	for (int i(0); i < m_dim_f; ++i) 
		m_trees(i)->reset();
	for (int i(0); i < m_dim_f; ++i)
		for (int j(0); j < m_dim2; ++j)
			m_trees(i,j)->reset();
}

AuxMap::VectorType AuxMap::operator()()
{
	// propagate the current order and variable values.
	int o(m_dmap.getOrder());
	setOrder(o);
	for (int i(0); i < m_dim_f; ++i) 
		m_trees(i)->setOrder(o, m_dmap.getValue());

	// reset
	for (int i(0); i < m_dim_f; ++i) 
		m_trees(i)->reset();

	VectorType result(m_dim_f);
	//typename VectorType::iterator b=result.begin(), e=result.end();
	for (int i(0); i < m_dim_f; ++i) 
		result[i] = (*m_trees(i))(0);

	return result;
}

AuxMap::MatrixType AuxMap::der()
{
	// propagate the current order and variable values.
	int o(m_dmap.getOrder());
//cout << "o: " << o << endl;
	setOrder(o);

//cout << "AM stat: " << m_dim << "," << m_indexOfFirstParam << "," << m_dim2 << "," << m_order << "," << m_size << endl;

	for (int i(0); i < m_dim_f; ++i)
		for (int j(0); j < m_dim2; ++j)
			m_trees(i,j)->setOrder(o, m_dmap.getValue());

	// reset
	for (int i(0); i < m_dim_f; ++i)
		for (int j(0); j < m_dim2; ++j)
			m_trees(i,j)->reset();

	MatrixType result(m_dim_f,m_dim2);
	for (int i(0); i < m_dim_f; ++i)
		for (int j(0); j < m_dim2; ++j)
			result(i+1,j+1) = (*m_trees(i,j))(0);

	return result;
}

AuxMap::VectorType AuxMap::operator()(const AuxMap::VectorType& val)
{
	m_dmap.setValue(val);
	return (*this)();
}

AuxMap::MatrixType AuxMap::operator[](const AuxMap::VectorType& val)
{
	m_dmap.setValue(val);
	return this->der();
}

void AuxMap::setParameter(const char *name, const interval& val) 
{
	m_dmap.setParameter(name, val);
}


/* DiffMap implementation. */

DiffMap::DiffMap(AuxMap& amap1, AuxMap& amap2) 
	: AuxMap(amap1), m_amap1(amap1), m_amap2(amap2)
{ }

/*inline DiffMap::DiffMap(const DiffMap& rhs)
	: AuxMap(rhs.m_amap1), m_amap1(rhs.m_amap1), m_amap2(rhs.m_amap2)
{ }
*/

DiffMap::~DiffMap() 
{
	//cout << "dismiss CapdFun" << endl;
}

DiffMap::VectorType DiffMap::operator()()
{
	return m_amap1() - m_amap2();
}

DiffMap::MatrixType DiffMap::der()
{
std::cout << "dmd" << std::endl;
std::cout << m_amap1.der() << std::endl;
	return m_amap1.der() - m_amap2.der();
}

DiffMap::VectorType DiffMap::operator()(const DiffMap::VectorType& val)
{
	return m_amap1(val) - m_amap2(val);
}

DiffMap::MatrixType DiffMap::operator[](const DiffMap::VectorType& val)
{
	return m_amap1[val] - m_amap2[val];
}

} // the end of the namespace capd
