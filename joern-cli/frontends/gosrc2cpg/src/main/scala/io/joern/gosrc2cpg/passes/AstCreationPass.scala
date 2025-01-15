package io.joern.gosrc2cpg.passes

// 导入相关的类和工具，用于文件操作、配置处理、抽象语法树创建等功能
import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.ParserResult
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success
import scala.util.Try

// AstCreationPass类，用于创建抽象语法树相关的处理流程，继承自BasePassForAstProcessing类
class AstCreationPass(
                       // 代码属性图（Code Property Graph，CPG）对象，用于表示代码的结构和关系等信息
                       cpg: Cpg,
                       // 抽象语法树文件的列表，包含要处理的文件路径字符串
                       astFiles: List[String],
                       // 配置对象，包含相关的配置参数
                       config: Config,
                       // Go模块帮助类对象，辅助处理Go模块相关的操作
                       goMod: GoModHelper,
                       // 包含Go语言全局相关数据结构的对象
                       goGlobal: GoGlobal,
                       // 临时目录对象，用于存放临时文件等
                       tmpDir: File,
                       // 报告对象，用于记录处理过程中的相关信息，如处理情况、耗时等
                       report: Report
                     ) extends BasePassForAstProcessing(cpg, astFiles, config, goMod, goGlobal, tmpDir) {

  // 获取日志记录器对象，用于记录处理过程中的各种信息，方便调试和查看运行情况
  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  // 重写父类中的processAst方法，用于处理抽象语法树的具体逻辑
  override def processAst(diffGraph: DiffGraphBuilder, astCreator: AstCreator): Unit = {
    // 使用TimeUtils的time方法来计算一段代码执行的耗时，并将结果解构赋值给相应变量
    val ((gotCpg, filename), duration) = TimeUtils.time {
      // 通过IOUtils读取指定文件（由astCreator.parserResult.fullPath确定路径）的行数，以获取文件的代码行数
      val fileLOC = IOUtils.readLinesInFile(Paths.get(astCreator.parserResult.fullPath)).size
      // 向报告对象中添加关于当前处理文件的报告信息，包括相对路径文件名、代码行数以及是否已解析标记为已解析（parsed = true）
      report.addReportInfo(astCreator.relPathFileName, fileLOC, parsed = true)
      Try {
        // 调用astCreator的createAst方法来创建抽象语法树，并获取局部差异信息（可能用于后续合并等操作）
        val localDiff = astCreator.createAst()
        // 将局部差异信息合并到差异图（diffGraph）中
        diffGraph.absorb(localDiff)
      } match {
        // 如果创建抽象语法树过程中出现异常
        case Failure(exception) =>
          // 在日志中记录警告信息，包含无法为哪个文件生成CPG以及对应的异常信息
          logger.warn(s"Failed to generate a CPG for: '${astCreator.parserResult.fullPath}'", exception)
          // 返回一个包含处理结果（false表示失败）和相对路径文件名的元组
          (false, astCreator.relPathFileName)
        // 如果创建抽象语法树成功
        case Success(_) =>
          // 在日志中记录信息，表示已为指定文件生成了CPG
          logger.info(s"Generated a CPG for: '${astCreator.parserResult.fullPath}'")
          // 返回一个包含处理结果（true表示成功）和相对路径文件名的元组
          (true, astCreator.relPathFileName)
      }
    }
    // 根据处理结果（gotCpg）、文件名（filename）以及耗时（duration）来更新报告对象中的相关信息
    report.updateReport(filename, cpg = gotCpg, duration)
  }
}