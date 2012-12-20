#ifndef _CAPD_NODE_VISITOR_H_ 
#define _CAPD_NODE_VISITOR_H_ 

#include <list>

#include "capd/capdlib.h"
#include "capd/map/Node.h"

namespace capd{

template<typename ScalarType>
class VarNodeEx;

template<typename ScalarType>
class NodeVisitor
{
public:
	virtual void visitUnaryNode(capd::map::UnaryNode<ScalarType> *) = 0;
	virtual void visitConsNode(capd::map::ConsNode<ScalarType> *) = 0;
	virtual void visitVarNode(VarNodeEx<ScalarType> *) = 0;

	virtual void visitSqrNode (capd::map::SqrNode<ScalarType> *)  = 0;
	virtual void visitSqrtNode(capd::map::SqrtNode<ScalarType> *) = 0;
	virtual void visitExpNode (capd::map::ExpNode<ScalarType> *)  = 0;
	virtual void visitLogNode (capd::map::LogNode<ScalarType> *)  = 0;
	virtual void visitSinNode (capd::map::SinNode<ScalarType> *)  = 0;
	virtual void visitCosNode (capd::map::CosNode<ScalarType> *)  = 0;
	virtual void visitAtanNode(capd::map::AtanNode<ScalarType> *) = 0;
	//virtual void visitAsinNode(capd::map::AsinNode<ScalarType> *) = 0;
	//virtual void visitAcosNode(capd::map::AcosNode<ScalarType> *) = 0;

	virtual void visitSumNode(capd::map::SumNode<ScalarType> *) = 0;
	virtual void visitDifNode(capd::map::DifNode<ScalarType> *) = 0;
	virtual void visitMulNode(capd::map::MulNode<ScalarType> *) = 0;
	virtual void visitDivNode(capd::map::DivNode<ScalarType> *) = 0;
	virtual void visitPowNode(capd::map::PowNode<ScalarType> *) = 0;
};

} // the end of the namespace capd

#endif // _CAPD_NODE_VISITOR_H_
