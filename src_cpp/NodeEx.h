#ifndef _CAPD_NODE_EX_H_ 
#define _CAPD_NODE_EX_H_ 

#include "capd/capdlib.h"
#include "capd/map/Node.h"

#include "NodeVisitor.h"

namespace capd{

template<typename ScalarType>
class NodeEx
{
public:
	virtual void accept(NodeVisitor<ScalarType>&) = 0;
};

template<typename ScalarType>
class ConsNodeEx : public NodeEx<ScalarType>, 
				   public capd::map::ConsNode<ScalarType>
{
public:
	ConsNodeEx(int a_order, ScalarType d)
		: capd::map::ConsNode<ScalarType>(a_order, d)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitConsNode(this);
	}

/*	ScalarType& operator()(int i)
	{
		this->capd::map::UnaryNode<ScalarType>::operator()(i);
	}
	void setOrder(int i, ScalarType *v)
	{
		capd::map::UnaryNode<ScalarType>::setOrder(i, v);
	}
	bool isConst() const { return true; }
*/
};

template<typename ScalarType>
class VarNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::VarNode<ScalarType>
{
public:
	VarNodeEx(int a_order, ScalarType *d, int index)
	: capd::map::VarNode<ScalarType>(a_order, d, index),
	  m_numberOfVariables(index)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitVarNode(this);
	}

	const int m_numberOfVariables;
};

template<typename ScalarType>
class SqrNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::SqrNode<ScalarType>
{
public:
	SqrNodeEx(int a_order, capd::map::Node<ScalarType> *n)
	: capd::map::SqrNode<ScalarType>(a_order, n)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitSqrNode(this);
	}
};

template<typename ScalarType>
class SqrtNodeEx : public NodeEx<ScalarType>, 
				   public capd::map::SqrtNode<ScalarType>
{
public:
	SqrtNodeEx(int a_order, capd::map::Node<ScalarType> *n)
	: capd::map::SqrtNode<ScalarType>(a_order, n)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitSqrtNode(this);
	}
};

template<typename ScalarType>
class ExpNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::ExpNode<ScalarType>
{
public:
	ExpNodeEx(int a_order, capd::map::Node<ScalarType> *n)
	: capd::map::ExpNode<ScalarType>(a_order, n)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitExpNode(this);
	}
};

template<typename ScalarType>
class LogNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::LogNode<ScalarType>
{
public:
	LogNodeEx(int a_order, capd::map::Node<ScalarType> *n)
	: capd::map::LogNode<ScalarType>(a_order, n)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitLogNode(this);
	}
};

template<typename ScalarType>
class SinNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::SinNode<ScalarType>
{
public:
	SinNodeEx(int a_order, capd::map::Node<ScalarType> *l)
	: capd::map::SinNode<ScalarType>(a_order, l, new ConsNodeEx<ScalarType>(a_order, ScalarType(1.)))
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitSinNode(this);
	}
};

template<typename ScalarType>
class CosNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::CosNode<ScalarType>
{
public:
	CosNodeEx(int a_order, capd::map::Node<ScalarType> *l)
	: capd::map::CosNode<ScalarType>(a_order, l, new ConsNodeEx<ScalarType>(a_order, ScalarType(1.)))
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitCosNode(this);
	}
};

template<typename ScalarType>
class AtanNodeEx : public NodeEx<ScalarType>, 
				   public capd::map::AtanNode<ScalarType>
{
public:
	AtanNodeEx(int a_order, capd::map::Node<ScalarType> *l)
	: capd::map::AtanNode<ScalarType>(a_order, l, new ConsNodeEx<ScalarType>(a_order, ScalarType(1.)))
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitAtanNode(this);
	}
};

template<typename ScalarType>
class AsinNodeEx : public NodeEx<ScalarType>, 
				   public capd::map::AsinNode<ScalarType>
{
public:
	AsinNodeEx(int a_order, capd::map::Node<ScalarType> *l)
	: capd::map::AsinNode<ScalarType>(a_order, l, new ConsNodeEx<ScalarType>(a_order, ScalarType(1.)))
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		// TODO v.visitAsinNode(this);
	}
};

template<typename ScalarType>
class AcosNodeEx : public NodeEx<ScalarType>, 
				   public capd::map::AcosNode<ScalarType>
{
public:
	AcosNodeEx(int a_order, capd::map::Node<ScalarType> *l)
	: capd::map::AcosNode<ScalarType>(a_order, l, new ConsNodeEx<ScalarType>(a_order, ScalarType(1.)))
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		// TODO v.visitAcosNode(this);
	}
};

template<typename ScalarType>
class SumNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::SumNode<ScalarType>
{
public:
	SumNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::SumNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitSumNode(this);
	}
};

template<typename ScalarType>
class DifNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::DifNode<ScalarType>
{
public:
	DifNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::DifNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitDifNode(this);
	}
};

template<typename ScalarType>
class MulNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::MulNode<ScalarType>
{
public:
	MulNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::MulNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitMulNode(this);
	}
};

template<typename ScalarType>
class DivNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::DivNode<ScalarType>
{
public:
	DivNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::DivNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitDivNode(this);
	}
};

template<typename ScalarType>
class PowNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::PowNode<ScalarType>
{
public:
	PowNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::PowNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		v.visitPowNode(this);
	}
};

} // the end of the namespace capd

#endif // _CAPD_NODE_EX_H_

