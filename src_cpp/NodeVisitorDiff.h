#ifndef _CAPD_NODE_VISITOR_DIFF_H_ 
#define _CAPD_NODE_VISITOR_DIFF_H_ 

#include <list>

#include "capd/capdlib.h"
#include "capd/map/Node.h"

#include "NodeVisitor.h"
#include "NodeEx.h"

namespace capd{

template<typename ScalarType>
class DiffVisitor : public NodeVisitor<ScalarType>
{
	typedef capd::map::Node<ScalarType> NodeType;
	typedef std::list<NodeType *>       NodeList;
public:
	DiffVisitor(const int order, const int index)
	: m_order(order), m_index(index)
	{}

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

	void visitSqrNode(capd::map::SqrNode<ScalarType> *n) 
	{
		// sqr(f(x))' = 2*f(x)*sqr(f'(x))
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new ConsNodeEx<ScalarType>(m_order, ScalarType(2.));
		NodeType *n2 = new MulNodeEx<ScalarType>(m_order, n1, n->left);
		NodeType *n3 = new SqrNodeEx<ScalarType>(m_order, darg);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n2, n3));
	}
	void visitSqrtNode(capd::map::SqrtNode<ScalarType> *n) 
	{
		// sqrt(f(x))' = f'(x)/2*sqrt(f(x))
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new ConsNodeEx<ScalarType>(m_order, ScalarType(2.));
		NodeType *n2 = new SqrtNodeEx<ScalarType>(m_order, n->left);
		NodeType *n3 = new MulNodeEx<ScalarType>(m_order, n1, n2);
		m_list.push_front(new DivNodeEx<ScalarType>(m_order, darg, n3));
	}
	void visitExpNode(capd::map::ExpNode<ScalarType> *n) 
	{
		// exp(x)' = exp(x)  or  exp(f(x))' = exp(f(x))*(f'(x))
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new ExpNodeEx<ScalarType>(m_order, n->left);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n1, darg));
	}
	void visitLogNode(capd::map::LogNode<ScalarType> *n) 
	{
		// log(f(x))' = f'(x)/f(x)
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		m_list.push_front(new DivNodeEx<ScalarType>(m_order, darg, n->left));
	}
	void visitSinNode(capd::map::SinNode<ScalarType> *n) 
	{
		// sin(x)' = cos(x)  or  sin(f(x))' = cos(f(x))*(f'(x))
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new CosNodeEx<ScalarType>(m_order, n->left);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n1, darg));
	}
	void visitCosNode(capd::map::CosNode<ScalarType> *n) 
	{
		// cos(x)' = (-sin(x))  or  cos(f(x))' = (-sin(f(x)))*(f'(x))
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new ConsNodeEx<ScalarType>(m_order, ScalarType(0.));
		NodeType *n2 = new SinNodeEx<ScalarType>(m_order, n->left);
		NodeType *n3 = new DifNodeEx<ScalarType>(m_order, n1, n2);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n3, darg));
	}
	void visitAtanNode(capd::map::AtanNode<ScalarType> *n) 
	{
		// atan'(u) = 1/(1+u^2) * u'
		NodeEx<ScalarType> *arg = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		arg->accept(*this);
		NodeType *darg = m_list.front(); m_list.pop_front();
		NodeType *n1 = new ConsNodeEx<ScalarType>(m_order, ScalarType(1.));
		NodeType *n2 = new SqrNodeEx<ScalarType>(m_order, n->left);
		NodeType *n3 = new SumNodeEx<ScalarType>(m_order, n1, n2);
		NodeType *n4 = new DivNodeEx<ScalarType>(m_order, n1, n3);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n4, darg));
	}

	void visitSumNode(capd::map::SumNode<ScalarType> *n) 
	{
		// (f+g)' = f'+g'
		NodeEx<ScalarType> *le = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		le->accept(*this);
		NodeEx<ScalarType> *re = dynamic_cast<NodeEx<ScalarType> *>(n->right);
		re->accept(*this);
		NodeType *r = m_list.front(); m_list.pop_front();
		NodeType *l = m_list.front(); m_list.pop_front();
		m_list.push_front(new SumNodeEx<ScalarType>(m_order, l, r));
	}
	void visitDifNode(capd::map::DifNode<ScalarType> *n) 
	{
		// (f-g)' = f'-g'
		NodeEx<ScalarType> *le = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		le->accept(*this);
		NodeEx<ScalarType> *re = dynamic_cast<NodeEx<ScalarType> *>(n->right);
		re->accept(*this);
		NodeType *r = m_list.front(); m_list.pop_front();
		NodeType *l = m_list.front(); m_list.pop_front();
		m_list.push_front(new DifNodeEx<ScalarType>(m_order, l, r));
	}
	void visitMulNode(capd::map::MulNode<ScalarType> *n) 
	{
		// (f*g)' = f'*g+f*g'
		NodeEx<ScalarType> *le = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		le->accept(*this);
		NodeEx<ScalarType> *re = dynamic_cast<NodeEx<ScalarType> *>(n->right);
		re->accept(*this);
		NodeType *r1 = m_list.front(); m_list.pop_front();
		NodeType *r2 = new MulNodeEx<ScalarType>(m_order, n->left, r1);
		NodeType *l1 = m_list.front(); m_list.pop_front();
		NodeType *l2 = new MulNodeEx<ScalarType>(m_order, l1, n->right);
		m_list.push_front(new SumNodeEx<ScalarType>(m_order, l2, r2));
	}
	void visitDivNode(capd::map::DivNode<ScalarType> *n) 
	{
		// (f/g)' = (f'*g-f*g')/g^2
		NodeEx<ScalarType> *le = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		le->accept(*this);
		NodeEx<ScalarType> *re = dynamic_cast<NodeEx<ScalarType> *>(n->right);
		re->accept(*this);
		NodeType *r1 = m_list.front(); m_list.pop_front();
		NodeType *r2 = new MulNodeEx<ScalarType>(m_order, n->left, r1);
		NodeType *l1 = m_list.front(); m_list.pop_front();
		NodeType *l2 = new MulNodeEx<ScalarType>(m_order, l1, n->right);
		NodeType *n1 = new DifNodeEx<ScalarType>(m_order, l2, r2);
		NodeType *n2 = new SqrNodeEx<ScalarType>(m_order, n->right);
		m_list.push_front(new DifNodeEx<ScalarType>(m_order, n1, n2));
	}
	void visitPowNode(capd::map::PowNode<ScalarType> *n) 
	{
		// (f^n)' = (f')*(n)*f^(n-1)
		NodeEx<ScalarType> *f = dynamic_cast<NodeEx<ScalarType> *>(n->left);
		f->accept(*this);
		NodeType *df = m_list.front(); m_list.pop_front();
		NodeType *n1 = new MulNodeEx<ScalarType>(m_order, df, n->right);
		NodeType *n2 = new ConsNodeEx<ScalarType>(m_order, ScalarType(1.));
		NodeType *n3 = new DifNodeEx<ScalarType>(m_order, n->right, n2);
		NodeType *n4 = new PowNodeEx<ScalarType>(m_order, n->left, n3);
		m_list.push_front(new MulNodeEx<ScalarType>(m_order, n1, n4));
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
