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
class SinNodeEx : public NodeEx<ScalarType>, 
				  public capd::map::SinNode<ScalarType>
{
public:
	SinNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::SinNode<ScalarType>(a_order, l, r)
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
	CosNodeEx(int a_order, capd::map::Node<ScalarType> *l, capd::map::Node<ScalarType> *r)
	: capd::map::CosNode<ScalarType>(a_order, l, r)
	{}

	void accept(NodeVisitor<ScalarType>& v)
	{
		//v.visitCosNode(this);
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

} // the end of the namespace capd

#endif // _CAPD_NODE_EX_H_

