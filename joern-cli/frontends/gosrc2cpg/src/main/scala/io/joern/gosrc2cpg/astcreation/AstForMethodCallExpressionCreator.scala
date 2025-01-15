package io.joern.gosrc2cpg.astcreation

// 导入所需的包和模块
import io.joern.gosrc2cpg.datastructures.MethodCacheMetaData
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import ujson.Value

import scala.util.{Success, Try}

// 定义一个trait用于创建方法调用表达式的AST节点
trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  // 为函数调用表达式生成AST
  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {
    // 获取方法名称、签名、完整名称、类型完整名称和接收者AST
    val (methodName, signature, fullName, typeFullName, receiverAst) =
      preReqForCallNode(createParserNodeInfo(expr.json(ParserKeys.Fun)))
    // 创建一个对应的调用节点
    val cpgCall = callNode(
      expr,
      expr.code,
      methodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(typeFullName)
    )
    // 返回调用AST，包含参数和接收者AST
    Seq(callAst(cpgCall, astForArgs(expr.json(ParserKeys.Args)), receiverAst.headOption))
  }

  // 为构造函数调用生成AST
  protected def astForConstructorCall(compositeLit: ParserNodeInfo): Seq[Ast] = {
    val (methodName, signature, fullName, _, _) = preReqForCallNode(
      createParserNodeInfo(compositeLit.json(ParserKeys.Type))
    )
    val cpgCall = callNode(
      compositeLit,
      compositeLit.code,
      methodName,
      fullName + "." + XDefines.ConstructorMethodName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(fullName)
    )
    Seq(callAst(cpgCall, astForStructureDeclarationArgument(compositeLit.json(ParserKeys.Elts))))
  }

  // 获取调用节点的必要信息，如方法名、签名等
  private def preReqForCallNode(funcDetails: ParserNodeInfo): (String, String, String, String, Seq[Ast]) = {
    val (aliasOpt, methodName) = funcDetails.node match
      case Ident =>
        (None, funcDetails.json(ParserKeys.Name).str)  // 处理标识符节点
      case SelectorExpr =>
        val xNode = createParserNodeInfo(funcDetails.json(ParserKeys.X))
        (Some(xNode), funcDetails.json(ParserKeys.Sel)(ParserKeys.Name).str)  // 处理选择表达式
      case x =>
        logger.warn(
          s"Unhandled class ${x.getClass} under astForCallExpression! file -> ${parserResult.fullPath} -> Line no -> ${funcDetails.lineNumber.get}"
        )
        (None, "")
    callMethodFullNameTypeFullNameAndSignature(methodName, aliasOpt)
  }

  // 为结构体声明的参数生成AST
  private def astForStructureDeclarationArgument(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argument = createParserNodeInfo(x)
        argument.node match
          case BasicLit => astForNode(argument)  // 处理基本字面量
          case Ident    => astForNode(argument)  // 处理标识符
          case _        => astForNode(argument)  // 默认处理
      })
      .toSeq
  }

  // 为函数调用的参数生成AST
  private def astForArgs(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argNode = createParserNodeInfo(x)
        argNode.node match
          case MapType  => Seq(Ast(literalNode(argNode, argNode.code, Defines.map)))  // 处理映射类型
          case ChanType => Seq(Ast(literalNode(argNode, argNode.code, Defines.chan)))  // 处理通道类型
          case _        => astForNode(argNode)  // 默认处理
      })
      .toSeq
  }

  // 获取方法的完整名称、类型完整名称和签名
  private def callMethodFullNameTypeFullNameAndSignature(
                                                          methodName: String,
                                                          aliasName: Option[ParserNodeInfo] = None
                                                        ): (String, String, String, String, Seq[Ast]) = {
    // 假设导入节点已经被处理，别名到其命名空间的映射已经完成
    aliasName match
      case None =>
        val methodFullName = s"$fullyQualifiedPackage.$methodName"
        val methodInfo = goGlobal
          .getMethodMetadata(fullyQualifiedPackage, methodName)
          .getOrElse(MethodCacheMetaData(Defines.anyTypeName, s"$methodFullName()"))
        val (signature, fullName, returnTypeFullName) =
          Defines.builtinFunctions.getOrElse(methodName, (methodInfo.signature, methodFullName, methodInfo.returnType))
        val probableLambdaTypeFullName = scope.lookupVariable(methodName) match
          case Some((_, lambdaTypeFullName)) => Some(lambdaTypeFullName)
          case _ =>
            goGlobal.getStructTypeMemberType(fullyQualifiedPackage, methodName)
        val (postLambdaFullname, postLambdaSignature, postLambdaReturnTypeFullName) = probableLambdaTypeFullName match
          case Some(lambdaTypeFullName) =>
            val (nameSpaceName, lambdaName) = goGlobal.splitNamespaceFromMember(lambdaTypeFullName)
            goGlobal.getMethodMetadata(nameSpaceName, lambdaName) match {
              case Some(metaData) => (lambdaTypeFullName, metaData.signature, metaData.returnType)
              case _              => (fullName, signature, returnTypeFullName)
            }
          case _ =>
            (fullName, signature, returnTypeFullName)
        (methodName, postLambdaSignature, postLambdaFullname, postLambdaReturnTypeFullName, Seq.empty)
      case Some(xnode) =>
        xnode.node match
          case Ident =>
            Try(xnode.json(ParserKeys.Obj)) match
              case Success(_) =>
                processReceiverAst(methodName, xnode)  // 处理接收者AST
              case _ =>
                val alias              = xnode.json(ParserKeys.Name).str
                val fullNamespace      = resolveAliasToFullName(alias)
                val callMethodFullName = s"$fullNamespace.$methodName"
                val lambdaFullName =
                  goGlobal.getStructTypeMemberType(fullNamespace, methodName).getOrElse(callMethodFullName)
                val (nameSpace, memberName) = goGlobal.splitNamespaceFromMember(lambdaFullName)
                val MethodCacheMetaData(returnTypeFullNameCache, signatureCache) =
                  goGlobal
                    .getMethodMetadata(nameSpace, memberName)
                    .getOrElse(
                      MethodCacheMetaData(
                        s"$callMethodFullName.${Defines.ReturnType}.${XDefines.Unknown}",
                        s"$callMethodFullName()"
                      )
                    )
                (methodName, signatureCache, lambdaFullName, returnTypeFullNameCache, Seq.empty)
          case _ =>
            processReceiverAst(methodName, xnode)  // 处理链式方法调用
  }

  // 处理接收者AST
  private def processReceiverAst(
                                  methodName: String,
                                  xnode: ParserNodeInfo
                                ): (String, String, String, String, Seq[Ast]) = {
    val receiverAst = astForNode(xnode)
    val receiverTypeFullName =
      receiverAst.headOption
        .flatMap(_.root)
        .map(_.properties.get(PropertyNames.TYPE_FULL_NAME).get.toString)
        .getOrElse(Defines.anyTypeName)
        .stripPrefix("*")
    val callMethodFullName = s"$receiverTypeFullName.$methodName"
    val MethodCacheMetaData(returnTypeFullNameCache, signatureCache) = goGlobal
      .getMethodMetadata(receiverTypeFullName, methodName)
      .getOrElse(
        MethodCacheMetaData(
          s"$receiverTypeFullName.$methodName.${Defines.ReturnType}.${XDefines.Unknown}",
          s"$callMethodFullName()"
        )
      )
    (methodName, signatureCache, callMethodFullName, returnTypeFullNameCache, receiverAst)
  }
}
