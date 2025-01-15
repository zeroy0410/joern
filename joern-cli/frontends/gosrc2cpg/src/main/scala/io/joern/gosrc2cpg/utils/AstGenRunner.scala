package io.joern.gosrc2cpg.utils

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.Environment.ArchitectureType.ArchitectureType
import io.joern.x2cpg.utils.Environment.OperatingSystemType.OperatingSystemType
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

// `AstGenRunner`对象相关定义
object AstGenRunner {
  // 获取日志记录器实例，用于记录相关日志信息
  private val logger = LoggerFactory.getLogger(getClass)
  // 定义`GoAstGenRunnerResult`样例类，用于表示Go语言相关的AST生成运行结果
  // 包含模块路径、解析的模块文件（可选）、解析的文件列表以及跳过的文件列表等信息
  case class GoAstGenRunnerResult(
                                   modulePath: String = "",
                                   parsedModFile: Option[String] = None,
                                   parsedFiles: List[String] = List.empty,
                                   skippedFiles: List[String] = List.empty
                                 ) extends AstGenRunnerResult
}

// `AstGenRunner`类定义，继承自`AstGenRunnerBase`，用于处理Go语言代码的AST生成相关操作
class AstGenRunner(config: Config, includeFileRegex: String = "") extends AstGenRunnerBase(config) {
  import io.joern.gosrc2cpg.utils.AstGenRunner.*

  // Windows x86平台对应的可执行文件名后缀
  override val WinX86 = "windows.exe"
  // Linux ARM64平台对应的可执行文件名后缀
  override val LinuxArm = "linux-arm64"
  // Mac ARM64平台对应的可执行文件名后缀
  override val MacArm = "macos-arm64"

  // 支持的二进制文件对应的操作系统和架构类型组合集合
  override val SupportedBinaries: Set[(OperatingSystemType, ArchitectureType)] = Set(
    Environment.OperatingSystemType.Windows -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Linux -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Linux -> Environment.ArchitectureType.ARMv8,
    Environment.OperatingSystemType.Mac -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Mac -> Environment.ArchitectureType.ARMv8
  )

  // 确定哪些文件在AST生成过程中被跳过，根据`astGenOut`中的输出信息来判断
  override def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      // 如果输出行不以`Converted`开头，说明解析失败，提取文件名和失败原因并记录警告日志，返回文件名的`Option`类型
      case out if!out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
        Option(filename)
      // 如果输出行以`Converted`开头，记录调试日志，返回`None`表示未被跳过
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    // 过滤掉`None`值，返回被跳过文件的文件名列表
    skipped.flatten
  }

  // 根据用户配置和文件后缀等条件判断是否过滤文件，决定是否对该文件进行后续处理
  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      // 如果文件路径被用户配置忽略，返回`false`，表示要过滤该文件
      case filePath if isIgnoredByUserConfig(filePath) => false
      // 如果文件后缀是`.mod`，返回`false`，表示要过滤该文件（通常不处理模块文件）
      case filePath if filePath.endsWith(".mod") => false
      // 其他情况返回`true`，表示不过滤该文件
      case _ => true
    }
  }

  // 从给定的文件列表中过滤出模块文件（以`.mod`结尾的文件）
  private def filterModFile(files: List[String], out: File): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
        case filePath if filePath.endsWith(".mod") => true
        case _ => false
      }
    }
  }

  // 运行原生的AST生成命令，根据给定的输入、输出路径、排除和包含的相关配置执行外部命令
  override def runAstGenNative(in: String, out: File, exclude: String, include: String)(implicit
                                                                                        metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    // 如果排除配置为空，生成空的排除命令参数序列；否则生成包含`-exclude`及对应排除内容的命令参数序列
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-exclude", exclude)
    // 如果包含配置为空，生成空的包含命令参数序列；否则生成包含`-include-packages`及对应包含内容的命令参数序列
    val includeCommand = if (include.isEmpty) Seq.empty else Seq("-include-packages", include)
    // 构建完整的外部命令参数序列并运行外部命令，将结果转换为`Try`类型
    ExternalCommand
      .run((astGenCommand +: excludeCommand) ++ includeCommand ++ Seq("-out", out.toString(), in), ".")
      .toTry
  }

  // 针对Go语言执行AST生成相关操作并返回结果列表
  def executeForGo(out: File): List[GoAstGenRunnerResult] = {
    // 隐式获取`AstGenProgramMetaData`实例，用于传递相关元数据信息
    implicit val metaData: AstGenProgramMetaData = config.astGenMetaData
    // 获取输入文件对应的`File`实例，根据配置中的输入路径创建
    val in = File(config.inputPath)
    logger.info(s"Running goastgen in '$config.inputPath'...")
    // 运行原生的AST生成命令，根据运行结果进行不同处理
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), includeFileRegex) match {
      case Success(result) =>
        // 确定源文件列表，根据输出路径、文件后缀以及忽略文件相关配置来筛选
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        // 过滤出模块文件
        val parsedModFile = filterModFile(srcFiles, out)
        // 过滤出普通的解析文件（非模块文件）
        val parsed = filterFiles(srcFiles, out)
        // 确定被跳过的文件列表
        val skipped = skippedFiles(in, result.toList)
        // 根据模块对解析文件、模块文件和跳过文件进行分类整理
        segregateByModule(config.inputPath, out.toString, parsedModFile, parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        List()
    }
  }

  /**
   * 将所有解析的文件（包括`go.mod`文件）按照不同模块进行分类隔离。
   * 也会对定义在其他模块内部的模块进行隔离处理。
   */
  private def segregateByModule(
                                 inputPath: String,
                                 outPath: String,
                                 parsedModFiles: List[String],
                                 parsedFiles: List[String],
                                 skippedFiles: List[String]
                               ): List[GoAstGenRunnerResult] = {
    // 创建`ModuleMeta`实例，用于表示模块相关的元数据信息，初始时包含模块路径、输出模块路径等信息
    val moduleMeta: ModuleMeta =
      ModuleMeta(inputPath, outPath, None, ListBuffer[String](), ListBuffer[String](), ListBuffer[ModuleMeta]())
    // 如果有解析的模块文件
    if (parsedModFiles.nonEmpty) {
      // 按照文件路径分割后的长度对模块文件进行排序，然后逐个添加模块文件到`ModuleMeta`实例中
      parsedModFiles
        .sortBy(_.split(UtilityConstants.fileSeparateorPattern).length)
        .foreach(modFile => {
          moduleMeta.addModFile(modFile, inputPath, outPath)
        })
      // 添加普通解析文件到`ModuleMeta`实例中
      parsedFiles.foreach(moduleMeta.addParsedFile)
      // 添加被跳过的文件到`ModuleMeta`实例中
      skippedFiles.foreach(moduleMeta.addSkippedFile)
      // 获取所有子模块对应的`GoAstGenRunnerResult`列表
      moduleMeta.getOnlyChildren
    } else {
      // 如果没有解析的模块文件，添加普通解析文件到`ModuleMeta`实例中
      parsedFiles.foreach(moduleMeta.addParsedFile)
      // 添加被跳过的文件到`ModuleMeta`实例中
      skippedFiles.foreach(moduleMeta.addSkippedFile)
      // 获取所有子模块及自身对应的`GoAstGenRunnerResult`列表
      moduleMeta.getAllChildren
    }
  }

  // 获取给定路径的父文件夹路径，如果没有父文件夹则返回空字符串
  private def getParentFolder(path: String): String = {
    val parent = Paths.get(path).getParent
    if (parent!= null) parent.toString else ""
  }

  // `ModuleMeta`样例类定义，用于表示模块的元数据信息，包含模块路径、输出模块路径、模块文件路径（可选）、
  // 解析文件列表、跳过文件列表以及子模块列表等属性
  case class ModuleMeta(
                         modulePath: String,
                         outputModulePath: String,
                         modFilePath: Option[String],
                         parsedFiles: ListBuffer[String],
                         skippedFiles: ListBuffer[String],
                         childModules: ListBuffer[ModuleMeta]
                       ) {
    // 将模块文件添加到合适的子模块中，如果不存在合适的子模块则创建新的子模块并添加
    def addModFile(modFile: String, inputPath: String, outPath: String): Unit = {
      childModules.collectFirst {
        case childMod if modFile.startsWith(childMod.outputModulePath) =>
          childMod.addModFile(modFile, inputPath, outPath)
      } match {
        case None =>
          val outmodpath = getParentFolder(modFile)
          childModules.addOne(
            ModuleMeta(
              outmodpath.replace(outPath, inputPath),
              outmodpath,
              Some(modFile),
              ListBuffer[String](),
              ListBuffer[String](),
              ListBuffer[ModuleMeta]()
            )
          )
        case _ =>
      }
    }

    // 将解析文件添加到合适的子模块中，如果不存在合适的子模块则添加到自身的解析文件列表中
    def addParsedFile(parsedFile: String): Unit = {
      childModules.collectFirst {
        case childMod if parsedFile.startsWith(childMod.outputModulePath) =>
          childMod.addParsedFile(parsedFile)
      } match {
        case None => parsedFiles.addOne(parsedFile)
        case _ =>
      }
    }

    // 将被跳过的文件添加到合适的子模块中，如果不存在合适的子模块则添加到自身的跳过文件列表中
    def addSkippedFile(skippedFile: String): Unit = {
      childModules.collectFirst {
        case childMod if skippedFile.startsWith(childMod.outputModulePath) =>
          childMod.addSkippedFile(skippedFile)
      } match {
        case None => skippedFiles.addOne(skippedFile)
        case _ =>
      }
    }

    // 获取所有子模块对应的`GoAstGenRunnerResult`列表（不包含自身）
    def getOnlyChildren: List[GoAstGenRunnerResult] = {
      childModules.flatMap(_.getAllChildren).toList
    }

    // 获取所有子模块及自身对应的`GoAstGenRunnerResult`列表
    def getAllChildren: List[GoAstGenRunnerResult] = {
      getOnlyChildren ++ List(
        GoAstGenRunnerResult(
          modulePath = modulePath,
          parsedModFile = modFilePath,
          parsedFiles = parsedFiles.toList,
          skippedFiles = skippedFiles.toList
        )
      )
    }
  }
}