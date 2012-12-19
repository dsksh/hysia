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
	virtual void visitSinNode(capd::map::SinNode<ScalarType> *) = 0;
	virtual void visitDifNode(capd::map::DifNode<ScalarType> *) = 0;
};

} // the end of the namespace capd

#endif // _CAPD_NODE_VISITOR_H_
