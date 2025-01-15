package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.{globalFromLiteral, identifierToFirstUsages}
import io.joern.dataflowengineoss.queryengine.AccessPathUsage.toTrackedBaseAndAccessPathSimple
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.semanticcpg.accesspath.MatchResult
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import scala.collection.{Set, mutable}

/** 基于ReachingDefProblem的解决方案创建数据依赖边。
 */
class DdgGenerator(semantics: Semantics) {

  implicit val s: Semantics = semantics

  /** 一旦计算出到达定义，我们通过检查哪些到达定义是相关的来创建数据依赖图，
   * 这意味着符号被传播并且被目标节点使用。
   *
   * @param dstGraph
   *   要添加边的差异图
   * @param problem
   *   到达定义问题
   * @param solution
   *   问题的解决方案
   */
  def addReachingDefEdges(
                           dstGraph: DiffGraphBuilder,
                           method: Method,
                           problem: DataFlowProblem[StoredNode, mutable.BitSet],
                           solution: Solution[StoredNode, mutable.BitSet]
                         ): Unit = {
    implicit val implicitDst: DiffGraphBuilder = dstGraph

    val numberToNode = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val in           = solution.in
    val gen          = solution.problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen

    val allNodes      = in.keys.toList
    val usageAnalyzer = new UsageAnalyzer(problem, in)

    /** 从入口节点到每个没有其他传入定义的节点添加边。
     */
    def addEdgesFromEntryNode(): Unit = {
      // 从入口节点添加边
      allNodes
        .filter(n => isDdgNode(n) && usageAnalyzer.usedIncomingDefs(n).isEmpty)
        .foreach { node =>
          addEdge(method, node)
        }
    }

    // 处理 `foo(new Bar()) or return new Bar()`
    def addEdgeForBlock(block: Block, towards: StoredNode): Unit = {
      block.astChildren.lastOption match {
        case None => // 不做处理
        case Some(node: Identifier) =>
          val edgesToAdd = in(node).toList
            .flatMap(numberToNode.get)
            .filter(inDef => usageAnalyzer.isUsing(node, inDef))
            .collect {
              case identifier: Identifier => identifier
              case call: Call             => call
            }
          edgesToAdd.foreach { inNode =>
            addEdge(inNode, block, nodeToEdgeLabel(inNode))
          }
          if (edgesToAdd.nonEmpty) {
            addEdge(block, towards)
          }
        case Some(node: Call) =>
          addEdge(node, block, nodeToEdgeLabel(node))
          addEdge(block, towards)
        case _ => // 不做处理
      }
    }

    /** 添加传入边到调用站点的参数，包括同一调用站点的参数之间的边。
     */
    def addEdgesToCallSite(call: Call): Unit = {
      // 调用站点参数之间的边
      usageAnalyzer.usedIncomingDefs(call).foreach { case (use, ins) =>
        ins.foreach { in =>
          val inNode = numberToNode(in)
          if (inNode != use) {
            addEdge(inNode, use, nodeToEdgeLabel(inNode))
          }
        }
      }

      // 对于所有调用，假设输入参数
      // 污染相应的输出参数
      // 和返回值。我们在查询时根据给定的语义过滤无效的边。
      usageAnalyzer.uses(call).foreach { use =>
        gen(call).foreach { g =>
          val genNode = numberToNode(g)
          if (use != genNode && isDdgNode(use)) {
            addEdge(use, genNode, nodeToEdgeLabel(use))
          }
        }
      }

      // 处理 `foo(new Bar())`，被降低为
      // `foo({Bar tmp = Bar.alloc(); tmp.init(); tmp})`
      call.argument.isBlock.foreach { block => addEdgeForBlock(block, call) }
    }

    def addEdgesToReturn(ret: Return): Unit = {
      // 处理 `return new Bar()`，被降低为
      // `return {Bar tmp = Bar.alloc(); tmp.init(); tmp}`
      usageAnalyzer.uses(ret).collectAll[Block].foreach(block => addEdgeForBlock(block, ret))
      usageAnalyzer.usedIncomingDefs(ret).foreach { case (use: CfgNode, inElements) =>
        addEdge(use, ret, use.code)
        inElements
          .filterNot(x => numberToNode.get(x).contains(use))
          .flatMap(numberToNode.get)
          .foreach { inElemNode =>
            addEdge(inElemNode, use, nodeToEdgeLabel(inElemNode))
          }
        if (inElements.isEmpty) {
          addEdge(method, ret)
        }
      }
      addEdge(ret, method.methodReturn, "<RET>")
    }

    def addEdgesToMethodParameterOut(paramOut: MethodParameterOut): Unit = {
      // 总是有一条边从方法输入参数到相应的方法输出参数，
      // 因为输入参数的修改只影响副本。
      paramOut.start.paramIn.foreach { paramIn =>
        addEdge(paramIn, paramOut, paramIn.name)
      }
      usageAnalyzer.usedIncomingDefs(paramOut).foreach { case (_, inElements) =>
        inElements.foreach { inElement =>
          val inElemNode = numberToNode(inElement)
          val edgeLabel  = nodeToEdgeLabel(inElemNode)
          addEdge(inElemNode, paramOut, edgeLabel)
        }
      }
    }

    def addEdgesToExitNode(exitNode: MethodReturn): Unit = {
      in(exitNode).foreach { i =>
        val iNode = numberToNode(i)
        addEdge(iNode, exitNode, nodeToEdgeLabel(iNode))
      }
    }

    /** 这是孤立标识符优化的一部分：由于我们从 `gen` 集合中删除了孤立标识符，
     * 我们现在必须检索它们并创建从每个孤立标识符到出口节点的边。
     */
    def addEdgesFromLoneIdentifiersToExit(method: Method): Unit = {
      val numberToNode     = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
      val exitNode         = method.methodReturn
      val transferFunction = solution.problem.transferFunction.asInstanceOf[OptimizedReachingDefTransferFunction]
      val genOnce          = transferFunction.loneIdentifiers
      genOnce.foreach { case (_, defs) =>
        defs.foreach { d =>
          val dNode = numberToNode(d)
          addEdge(dNode, exitNode, nodeToEdgeLabel(dNode))
        }
      }
    }

    def addEdgesToCapturedIdentifiersAndParameters(): Unit = {
      val identifierDestPairs =
        method._identifierViaContainsOut
          .flatMap { identifier =>
            identifierToFirstUsages(identifier).map(usage => (identifier, usage))
          }
          .l
          .distinctBy(_._2.method)

      identifierDestPairs
        .foreach { case (src, dst) =>
          addEdge(src, dst, nodeToEdgeLabel(src))
        }
      method.parameter.foreach { param =>
        param.capturedByMethodRef.referencedMethod.ast.isIdentifier.foreach { identifier =>
          addEdge(param, identifier, nodeToEdgeLabel(param))
        }
      }

      // 注意：以下连接闭包的方法边界之间的REACHING_DEF边。在PARENT -> CHILD的情况下，
      // 这不会带来不一致的流，但从CHILD -> PARENT我们观察到了不一致。这种数据流建模是不健全的，
      // 因为引擎假设REACHING_DEF边是过程内的。
      // 详情请参见Joern的PR #3735
      val globalIdentifiers =
        (method._callViaContainsOut ++ method._returnViaContainsOut).ast.isLiteral
          .flatMap(globalFromLiteral(_))
          .collectAll[Identifier]
          .l
      globalIdentifiers
        .foreach { global =>
          identifierToFirstUsages(global).map { identifier =>
            addEdge(global, identifier, nodeToEdgeLabel(global))
          }
        }
    }

    // 添加从入口节点的边
    addEdgesFromEntryNode()
    allNodes.foreach {
      case call: Call                   => addEdgesToCallSite(call)
      case ret: Return                  => addEdgesToReturn(ret)
      case paramOut: MethodParameterOut => addEdgesToMethodParameterOut(paramOut)
      case _                            =>
    }

    // 添加到捕获的标识符和参数的边
    addEdgesToCapturedIdentifiersAndParameters()
    // 添加到出口节点的边
    addEdgesToExitNode(method.methodReturn)
    // 添加从孤立标识符到出口的边
    addEdgesFromLoneIdentifiersToExit(method)
  }

  private def addEdge(fromNode: StoredNode, toNode: StoredNode, variable: String = "")(implicit
                                                                                       dstGraph: DiffGraphBuilder
  ): Unit = {
    // 如果起始节点或目标节点是未知类型，则直接返回
    if (fromNode.isInstanceOf[Unknown] || toNode.isInstanceOf[Unknown])
      return

    (fromNode, toNode) match {
      // 如果子节点和父节点之间的边是有效的，则添加边
      case (parentNode: CfgNode, childNode: CfgNode) if EdgeValidator.isValidEdge(childNode, parentNode) =>
        dstGraph.addEdge(fromNode, toNode, EdgeTypes.REACHING_DEF, variable)
      case _ =>

    }
  }

  /** 有一些节点类型 (a) 不应被视为DDG的一部分，或 (b) 不是独立的DDG节点，或
   * (c) 在DDG中有特殊意义。此函数指示给定节点是否为常规DDG节点。
   */
  private def isDdgNode(x: StoredNode): Boolean = {
    x match {
      case _: Method           => false
      case _: ControlStructure => false
      case _: FieldIdentifier  => false
      case _: JumpTarget       => false
      case _: MethodReturn     => false
      case _                   => true
    }
  }

  private def nodeToEdgeLabel(node: StoredNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: CfgNode           => n.code
      case _                    => ""
    }
  }
}

/** 在计算到达定义之后，我们会得到每个流图节点 `n` 的传入定义集合 `in(n)`。
 * 这个组件确定那些传入定义是相关的，因为它们定义的值实际上被 `n` 使用。
 */
private class UsageAnalyzer(
                             problem: DataFlowProblem[StoredNode, mutable.BitSet],
                             in: Map[StoredNode, Set[Definition]]
                           ) {

  val numberToNode: Map[Definition, StoredNode] = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode

  private val allNodes = in.keys.toList
  private val containerSet =
    Set(Operators.fieldAccess, Operators.indexAccess, Operators.indirectIndexAccess, Operators.indirectFieldAccess)
  private val indirectionAccessSet = Set(Operators.addressOf, Operators.indirection)
  val usedIncomingDefs: Map[StoredNode, Map[StoredNode, Set[Definition]]] = initUsedIncomingDefs()

  def initUsedIncomingDefs(): Map[StoredNode, Map[StoredNode, Set[Definition]]] = {
    allNodes.map { node =>
      node -> usedIncomingDefsForNode(node)
    }.toMap
  }

  private def usedIncomingDefsForNode(node: StoredNode): Map[StoredNode, Set[Definition]] = {
    uses(node).map { use =>
      use -> in(node).filter { inElement =>
        val inElemNode = numberToNode(inElement)
        isUsing(use, inElemNode)
      }
    }.toMap
  }

  def isUsing(use: StoredNode, inElemNode: StoredNode): Boolean =
    sameVariable(use, inElemNode) || isContainer(use, inElemNode) || isPart(use, inElemNode) || isAlias(use, inElemNode)

  /** 确定节点 `use` 是否描述 `inElement` 的容器，例如，use = `ptr` 而 inElement = `ptr->foo`。
   */
  private def isContainer(use: StoredNode, inElement: StoredNode): Boolean = {
    inElement match {
      case call: Call if containerSet.contains(call.name) =>
        call.argument.headOption.exists { base =>
          nodeToString(use) == nodeToString(base)
        }
      case _ => false
    }
  }

  /** 确定 `use` 是否是 `inElement` 的一部分，例如，use = `argv[0]` 而 inElement = `argv`
   */
  private def isPart(use: StoredNode, inElement: StoredNode): Boolean = {
    use match {
      case call: Call if containerSet.contains(call.name) =>
        inElement match {
          case param: MethodParameterIn =>
            call.argument.headOption.exists { base =>
              nodeToString(base).contains(param.name)
            }
          case identifier: Identifier =>
            call.argument.headOption.exists { base =>
              nodeToString(base).contains(identifier.name)
            }
          case _ => false
        }
      case _ => false
    }
  }

  private def isAlias(use: StoredNode, inElement: StoredNode): Boolean = {
    use match {
      case useCall: Call =>
        inElement match {
          case inCall: Call =>
            val (useBase, useAccessPath) = toTrackedBaseAndAccessPathSimple(useCall)
            val (inBase, inAccessPath)   = toTrackedBaseAndAccessPathSimple(inCall)
            useBase == inBase && useAccessPath.matchAndDiff(inAccessPath.elements)._1 == MatchResult.EXACT_MATCH
          case _ => false
        }
      case _ => false
    }
  }

  def uses(node: StoredNode): Set[StoredNode] = {
    val n: Set[StoredNode] = node match {
      case ret: Return                  => ret.astChildren.collect { case x: Expression => x }.toSet
      case call: Call                   => call.argument.toSet
      case paramOut: MethodParameterOut => Set(paramOut)
      case _                            => Set()
    }
    n.filterNot(_.isInstanceOf[FieldIdentifier])
  }

  /** 将调用的参数与传入定义进行比较，以查看它们是否引用同一个变量
   */
  private def sameVariable(use: StoredNode, inElement: StoredNode): Boolean = {
    inElement match {
      case param: MethodParameterIn =>
        nodeToString(use).contains(param.name)
      case call: Call if indirectionAccessSet.contains(call.name) =>
        call.argumentOption(1).exists(x => nodeToString(use).contains(x.code))
      case call: Call =>
        nodeToString(use).contains(call.code)
      case identifier: Identifier => nodeToString(use).contains(identifier.name)
      case _                      => false
    }
  }

  private def nodeToString(node: StoredNode): Option[String] = {
    node match {
      case ident: Identifier     => Some(ident.name)
      case exp: Expression       => Some(exp.code)
      case p: MethodParameterIn  => Some(p.name)
      case p: MethodParameterOut => Some(p.name)
      case _                     => None
    }
  }
}
