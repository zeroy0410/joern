package io.joern.gosrc2cpg

import better.files.File
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.passes.*
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.gosrc2cpg.utils.AstGenRunner.GoAstGenRunnerResult
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}

import java.nio.file.Paths
import scala.util.Try

// `GoSrc2Cpg`类定义，继承自`X2CpgFrontend[Config]`，用于将Go语言源代码转换为代码属性图（Cpg）相关操作
class GoSrc2Cpg(goGlobalOption: Option[GoGlobal] = Option(GoGlobal())) extends X2CpgFrontend[Config] {
  // 创建一个`Report`实例，用于记录和报告相关信息（可能是转换过程中的各种情况等）
  private val report: Report = new Report()

  // 用于存储`GoModHelper`实例的可选值，初始化为`None`，可能后续用于处理Go模块相关信息
  private var goMod: Option[GoModHelper] = None

  // 创建代码属性图（Cpg）的方法，接收一个`Config`配置对象，返回一个`Try[Cpg]`类型，表示可能成功或失败的操作结果
  def createCpg(config: Config): Try[Cpg] = {
    // 在一个新的空的`Cpg`环境下执行操作，传入输出路径和配置对象
    withNewEmptyCpg(config.outputPath, config) ({ (cpg, config) =>
      // 使用临时目录（名为`gosrc2cpgOut`）来执行后续操作，确保临时目录使用完毕后能正确清理
      File.usingTemporaryDirectory("gosrc2cpgOut") { tmpDir =>
        // 执行元数据传递（MetaDataPass）操作，传入代码属性图（Cpg）、语言类型（这里是Go语言）以及输入路径，创建并应用相关元数据
        MetaDataPass(cpg, Languages.GOLANG, config.inputPath).createAndApply()
        // 创建`AstGenRunner`实例并调用`executeForGo`方法，获取AST生成的结果列表，这些结果与Go语言代码的抽象语法树生成相关
        // 调用外部工具，前端为go-parser package，将Go源代码转换为json
        val astGenResults = new AstGenRunner(config).executeForGo(tmpDir)
        // 遍历每个AST生成结果
        astGenResults.foreach(astGenResult => {
          // 如果`goGlobalOption`有值则使用其值，否则使用默认创建的`GoGlobal`实例
          goGlobalOption
            .orElse(Option(GoGlobal()))
            .foreach(goGlobal => {
              // 创建`GoModHelper`实例，传入模块路径（可选）以及解析后的模块文件信息（经过读取和映射操作），并将其赋值给`goMod`
              goMod = Some(
                GoModHelper(
                  Some(astGenResult.modulePath),
                  astGenResult.parsedModFile
                    .flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
                )
              )
              // 设置`goGlobal`中的主模块名称，通过`goMod`获取模块元数据中的模块名称（如果存在）
              goGlobal.mainModule = goMod.flatMap(modHelper => modHelper.getModMetaData().map(mod => mod.module.name))
              // 创建并应用`InitialMainSrcPass`，传入代码属性图（Cpg）、解析后的文件列表、配置对象、`goMod`实例、`goGlobal`实例以及临时目录等信息，
              // 可能用于处理主源文件相关的初始操作
              InitialMainSrcPass(cpg, astGenResult.parsedFiles, config, goMod.get, goGlobal, tmpDir).createAndApply()
              // 如果`goGlobal`中包级别变量和常量的抽象语法树映射大小大于0，则创建并应用`PackageCtorCreationPass`，
              // 可能用于创建包构造相关的操作
              if goGlobal.pkgLevelVarAndConstantAstMap.size() > 0 then
                PackageCtorCreationPass(cpg, config, goGlobal).createAndApply()
              // 如果配置中要求获取依赖（`fetchDependencies`为`true`）
              if (config.fetchDependencies) {
                // 设置`goGlobal`中正在处理依赖的标识为`true`
                goGlobal.processingDependencies = true
                // 执行下载依赖相关的操作（`DownloadDependenciesPass`），传入代码属性图（Cpg）、`goMod`实例、`goGlobal`实例以及配置对象等信息
                DownloadDependenciesPass(cpg, goMod.get, goGlobal, config).process()
                // 设置`goGlobal`中正在处理依赖的标识为`false`，表示依赖处理完成
                goGlobal.processingDependencies = false
              }
              // 创建并应用`AstCreationPass`，传入代码属性图（Cpg）、解析后的文件列表、配置对象、`goMod`实例、`goGlobal`实例、临时目录以及`report`实例等信息，
              // 用于创建抽象语法树相关操作，并最后打印`report`中的信息
              AstCreationPass(cpg, astGenResult.parsedFiles, config, goMod.get, goGlobal, tmpDir, report)
                .createAndApply()
              report.print()
            })
        })
      }
    })
  }

  // 获取`GoModHelper`实例的方法，如果`goMod`不为`None`则返回其中存储的`GoModHelper`实例
  def getGoModHelper: GoModHelper = goMod.get
}