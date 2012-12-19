#ifndef _CAPD_NODE_VISITOR_DIFF_H_ 
#define _CAPD_NODE_VISITOR_DIFF_H_ 

#include <list>

#include "capd/capdlib.h"
#include "capd/map/Node.h"

#include "NodeEx.h"

namespace capd{

template<typename ScalarType>
class DiffVisitor : public NodeVisitor<ScalarType>
{
	typedef std::list<capd::map::Node<ScalarType> *> NodeList;
public:
	DiffVisitor(const int order, const int index)
	: m_order(order), m_index(index)
	{
	}

	void visitUnaryNode(capd::map::UnaryNode<ScalarType> *n) 
	{
		m_list.push_front(new ConsNodeEx<ScalarType>(m_order, ScalarType(0.)));
	}
	void visitConsNode(capd::map::ConsNode<ScalarType> *n) 
	{
		m_list.push_front(new ConsNodeEx<ScalarType>(m_order, ScalarType(0.)));
	}
	void visitVarNode(VarNodeEx<ScalarType> *n) 
	{
		double v;
		if (n->m_numberOfVariables == m_index) v = 1.; else v = 0.;
		m_list.push_front(new ConsNodeEx<ScalarType>(m_order, ScalarType(v)));
	}
	void visitSinNode(capd::map::SinNode<ScalarType> *n) 
	{
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		capd::map::Node<ScalarType> *darg = m_list.front(); m_list.pop_front();
		capd::map::Node<ScalarType> *r = 
			new ConsNodeEx<MyIMap::ScalarType>(m_order, ScalarType(1.));

		NodeEx<ScalarType> *l = new CosNodeEx<ScalarType>(m_order, arg, r);
		m_list.push_front(new capd::map::MulNode<ScalarType>(m_order, l, darg));
	}
	void visitDifNode(capd::map::DifNode<ScalarType> *n) 
	{
		NodeEx<ScalarType> *le = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		le->accept(*this);
		NodeEx<ScalarType> *re = dynamic_cast<NodeEx<ScalarType> *>(n->right);
		re->accept(*this);
		capd::map::Node<ScalarType> *r = m_list.front(); m_list.pop_front();
		capd::map::Node<ScalarType> *l = m_list.front(); m_list.pop_front();
		m_list.push_front(new DifNodeEx<ScalarType>(m_order, l, r));
	}

	capd::map::Node<ScalarType> *getNode()
	{
		return m_list.front();
	}

private:
	const int m_order;
	const int m_index;
	NodeList m_list;
};

} // the end of the namespace capd

#endif // _CAPD_NODE_VISITOR_DIFF_H_
