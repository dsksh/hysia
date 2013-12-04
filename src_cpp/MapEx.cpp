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

DerMap::DerMap(int dim, int order) 
  : capd::map::CnMap<capd::IMatrix,1>(),
	m_trees_idx(0), m_dtrees_idx(0)
{
	m_dim = dim; m_dim2 = dim;
	m_indexOfFirstParam = dim;
	m_order = order;
	m_size = m_dim*m_order;
	m_val = new ScalarType[m_size];
	std::fill(m_val, m_val+m_size, ScalarType(0.));
	m_trees = TreesContainer(dim, 1);
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
    //m_val = rhs.m_val;
	m_val = new ScalarType[m_size];
	for (int i(0); i < m_size; ++i)
		m_val[i] = m_val[i];
	//std::fill(m_val, m_val+m_size, ScalarType(0.));
	m_trees = TreesContainer(m_dim, 1);
}

DerMap::~DerMap() 
{
	//cout << "dismiss DerMap: " << m_trees_idx << endl;
}

//void DerMap::setup()
//{
//}

//void DerMap::setup1() 
//{
//}

int DerMap::putVariable(const char *name) 
{
	m_var.push_back(name);
	return m_var.size() -2; // TODO
}

int DerMap::setParam(const char *name, const interval& val) 
{
	m_var.push_back(name);
	int i(m_var.size() -2); // TODO
	m_val[i] = val;
	return i;
}

void DerMap::setValue(const VectorType& val) 
{
  	for (int i(0); i < m_dim; ++i)
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
		for (int j(0); j<m_dim; ++j) {
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
	: BasicFunction<ScalarType>(), m_dmap(dmap), m_trees(dim_f,dmap.dimension())
{
	m_dim = dmap.dimension();
	m_dim2 = dim_f;
	m_indexOfFirstParam = m_dim;
	m_order = 1;
	//m_size = m_dim;
	//m_val = new ScalarType[m_size];
	//std::fill(m_val, m_val+m_size, ScalarType(0.));
}

inline AuxMap::AuxMap(const AuxMap& rhs)
	: BasicFunction<ScalarType>(rhs), m_dmap(rhs.m_dmap), m_trees(rhs.m_trees)
{}

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
	for (int i(0); i < m_dim2; ++i) 
		m_trees(i)->setOrder(o, m_dmap.getValue());
	for (int i(0); i < m_dim2; ++i)
		for (int j(0); j < m_dim; ++j)
			m_trees(i,j)->setOrder(o, m_dmap.getValue());

	for (int i(0); i < m_dim2; ++i) 
		m_trees(i)->reset();
	for (int i(0); i < m_dim2; ++i)
		for (int j(0); j < m_dim; ++j)
			m_trees(i,j)->reset();
}

AuxMap::VectorType AuxMap::operator()()
{
	// propagate the current order and variable values.
	int o(m_dmap.getOrder());
	setOrder(o);
	for (int i(0); i < m_dim2; ++i) 
		m_trees(i)->setOrder(o, m_dmap.getValue());

	// reset
	for (int i(0); i < m_dim2; ++i) 
		m_trees(i)->reset();

	VectorType result(m_dim2);
	//typename VectorType::iterator b=result.begin(), e=result.end();
	for (int i(0); i < m_dim2; ++i) 
		result[i] = (*m_trees(i))(0);

	return result;
}

AuxMap::MatrixType AuxMap::der()
{
	// propagate the current order and variable values.
	int o(m_dmap.getOrder());
	setOrder(o);
	for (int i(0); i < m_dim2; ++i)
		for (int j(0); j < m_dim; ++j)
			m_trees(i,j)->setOrder(o, m_dmap.getValue());

	// reset
	for (int i(0); i < m_dim2; ++i)
		for (int j(0); j < m_dim; ++j)
			m_trees(i,j)->reset();

	MatrixType result(m_dim2,m_dim);
	for (int i(0); i < m_dim2; ++i)
		for (int j(0); j < m_dim; ++j)
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

} // the end of the namespace capd
