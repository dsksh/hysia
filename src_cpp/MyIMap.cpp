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

#include "MyIMap.h"

namespace capd{ 

using namespace std;
using namespace map;

MyIMap::MyIMap() : capd::map::CnMap<capd::IMatrix,1>()
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

inline MyIMap::MyIMap(const MyIMap&)
	: capd::map::CnMap<capd::IMatrix,1>()
{}

MyIMap::~MyIMap() 
{}

} // the end of the namespace capd
