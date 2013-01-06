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

DerMap::DerMap() 
  : capd::map::CnMap<capd::IMatrix,1>(),
	m_trees_idx(0), m_dtrees_idx(0)
{}

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

inline DerMap::DerMap(const DerMap& rhs)
	: capd::map::CnMap<capd::IMatrix,1>(rhs)
{}

DerMap::~DerMap() 
{
	//cout << "dismiss DerMap: " << m_trees_idx << endl;
}

void DerMap::setup()
{
	try{
	std::string s("var:t,x,v; fun:1,v,-sin(x);");

	/* BasicFunction::create_from_text */

	//map::Parser::removeWhiteSpaces(s);
	m_order = 1;
	//map::Parser::splitVariables("var:",s,m_var);  /* we read variables and dimension */
	m_var.push_back("t");
	m_var.push_back("x");
	m_var.push_back("v");
	m_indexOfFirstParam = m_var.size();

	//size_t parPos = s.find("par");
	//if(parPos!=std::string::npos)
	//	map::Parser::splitVariables("par:",s,m_var);  /* we read variables and dimension */

	m_dim = m_var.size();
	m_size = m_dim*m_order;
	m_val = new ScalarType[m_size];
	std::fill(m_val, m_val+m_size, ScalarType(0.));

	/* from CnMap::create_from_text */

	m_dim2 = 3;
	m_trees = TreesContainer(m_dim2, getRank());

/*	//map::Parser::removeWhiteSpaces(s);
	int i, j;
	std::vector<std::string> fun;
	//map::Parser::splitVariables("fun:",s,fun);
	fun.push_back("1");
	fun.push_back("v");
	fun.push_back("-sin(x)");
	m_dim2 = fun.size();
	m_trees = TreesContainer(m_dim2, getRank());

	m_formulas = map::CnContainer<std::string>(m_dim2, getRank());

	for(i=0; i<m_dim2; ++i)
	{
		m_formulas(i) = fun[i];
		m_currentFormula = &(m_formulas(i));
		//eqnanal(fun[i],& (m_trees(i)));
		//(m_trees(i)->m_links)++;
	}
*/

    m_trees(0) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(0)->m_links)++;
	m_trees(1) = new VarNode<ScalarType>(m_order, &m_val[2], 2);
	(m_trees(1)->m_links)++;
	m_trees(2) = new DifNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2)->left  = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	m_trees(2)->right = new SinNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2)->right->left  = new VarNode<ScalarType>(m_order, &m_val[1], 1);
	m_trees(2)->right->right = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2)->m_links)++;

	// now we compute the partial derivatives up to the order 'rank'
	// for k=1
/*	if(getRank()>0)
	{
		for(i=0;i<m_dim2;++i)
		{
			m_currentFormula = & (m_formulas(i));
			for(j=0;j<m_dim2;++j)
			{
				std::string temp = *m_currentFormula;
				m_formulas(i,j) = diffanal(temp,j);
				if(m_formulas(i,j)=="") m_formulas(i,j)="0";
				eqnanal(m_formulas(i,j), &(m_trees(i,j)));
				(m_trees(i,j)->m_links)++;
			}
		}
	}
*/

    m_trees(0,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	//m_trees(0,0) = m_trees(0,0)->operator()(m_order);
	(m_trees(0,0)->m_links)++;
    m_trees(0,1) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(0,1)->m_links)++;
    m_trees(0,2) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(0,2)->m_links)++;
    m_trees(1,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(1,0)->m_links)++;
    m_trees(1,1) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(1,1)->m_links)++;
    m_trees(1,2) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(1,2)->m_links)++;
    m_trees(2,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(2,0)->m_links)++;
    m_trees(2,1) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2,1)->m_links)++;
	m_trees(2,2) = new DifNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2,2)->left  = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	m_trees(2,2)->right = new CosNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2,2)->right->left  = new VarNode<ScalarType>(m_order, &m_val[1], 1);
	m_trees(2,2)->right->right = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2,2)->m_links)++;
/*	m_trees(0)->computeDerivatives(1);
	m_trees(1)->computeDerivatives(1);
	m_trees(2)->computeDerivatives(1);
*/
/*	IVector x(3);
	x[0]=0.0;
	x[1]=1.0;
	x[2]=1.0;
	computeDerivatives(x, m_trees, m_order, 0, 2);
*/

/*	// for k>1
	for(i=2;i<=getRank();++i)
	{
		TreesContainer::Multipointer mp = m_trees.first(i);
		do{
			TreesContainer::Multipointer mp2(i-1,mp.begin());
			for(j=0;j<m_dim2;++j)
			{
				std::string temp = m_formulas(j,mp2);
				m_formulas(j,mp) = diffanal(temp,mp[mp.dimension()-1]);
				if(m_formulas(j,mp)=="") m_formulas(j,mp)="0";
				m_currentFormula = &(m_formulas(j,mp));
				eqnanal(m_formulas(j,mp),&(m_trees(j,mp)));
				(m_trees(j,mp)->m_links)++;
			}
		}while(m_trees.next(mp));
	}
*/

	} catch(std::runtime_error &r)
	{
		std::string re = "exception in constructor Map::Map(const char *, int)\n";
		re += r.what();
		throw std::runtime_error(re);
	}
}

void DerMap::setup1() {
/*	m_var.push_back("t");
	m_var.push_back("x");
	m_var.push_back("v");

    m_trees(0) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(0)->m_links)++;
    m_trees(1) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(1)->m_links)++;
    m_trees(2) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2)->m_links)++;
*/
    m_trees(0,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(0,0)->m_links)++;
    m_trees(0,1) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(0,1)->m_links)++;
    m_trees(0,2) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(0,2)->m_links)++;

    m_trees(1,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(1,0)->m_links)++;
    m_trees(1,1) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(1,1)->m_links)++;
    m_trees(1,2) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(1,2)->m_links)++;

    m_trees(2,0) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(2,0)->m_links)++;
    m_trees(2,1) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(2,1)->m_links)++;
    m_trees(2,2) = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	(m_trees(2,2)->m_links)++;

/*    m_trees(2,1) = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2,1)->m_links)++;
	m_trees(2,2) = new DifNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2,2)->left  = new ConsNode<ScalarType>(m_order, ScalarType(0.));
	m_trees(2,2)->right = new CosNode<ScalarType>(m_order, NULL, NULL);
	m_trees(2,2)->right->left  = new VarNode<ScalarType>(m_order, &m_val[1], 1);
	m_trees(2,2)->right->right = new ConsNode<ScalarType>(m_order, ScalarType(1.));
	(m_trees(2,2)->m_links)++;
*/
}

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

void DerMap::putTree(const int i, Node<ScalarType> *node) 
{
	m_trees(i) = node;
	++(m_trees(i)->m_links);
}

DerMap::NodeType *DerMap::createVarNode(int index) 
{
	return new VarNodeEx<ScalarType>(m_order, &m_val[index], index);
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


AuxMap::AuxMap(int dim_v, int dim_f) 
	: BasicFunction<ScalarType>(), m_trees(dim_f,dim_v)
{
	m_dim = dim_v;
	m_dim2 = dim_f;
	m_indexOfFirstParam = m_dim;
	m_order = 1;
	//m_size = m_dim;
	//m_val = new ScalarType[m_size];
	//std::fill(m_val, m_val+m_size, ScalarType(0.));
}

inline AuxMap::AuxMap(const AuxMap& rhs)
	: BasicFunction<ScalarType>(rhs), m_trees(rhs.m_trees)
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

AuxMap::VectorType AuxMap::operator()() const
{
	VectorType result(m_dim);
	//typename VectorType::iterator b=result.begin(), e=result.end();
	for (int i(0); i < m_dim2; ++i) 
		result[i] = (*m_trees(i))(0);

	return result;
}

AuxMap::MatrixType AuxMap::der() const
{
	MatrixType result(m_dim,m_dim);
	for (int i(0); i < m_dim; ++i)
		for (int j(0); j < m_dim2; ++j)
			result(i+1,j+1) = (*m_trees(j,i))(0);

	return result;
}

} // the end of the namespace capd
