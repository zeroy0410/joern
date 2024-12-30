package io.joern.x2cpg

import better.files.File
import io.joern.x2cpg.X2Cpg.{applyDefaultOverlays, withErrorsToConsole}
import io.joern.x2cpg.frontendspecific.FrontendArgsDelimitor
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

// 定义默认配置
object X2CpgConfig {
  def defaultInputPath: String  = ""
  def defaultOutputPath: String = "cpg.bin"
}

// 配置的基础特征，定义了输入输出路径、服务器模式等
trait X2CpgConfig[R <: X2CpgConfig[R]] {
  var inputPath: String   = X2CpgConfig.defaultInputPath
  var outputPath: String  = X2CpgConfig.defaultOutputPath
  var serverMode: Boolean = false

  // 设置输入路径
  def withInputPath(inputPath: String): R = {
    this.inputPath = Paths.get(inputPath).toAbsolutePath.normalize().toString
    this.asInstanceOf[R]
  }

  // 设置输出路径
  def withOutputPath(x: String): R = {
    this.outputPath = x
    this.asInstanceOf[R]
  }

  // 设置服务器模式
  def withServerMode(x: Boolean): R = {
    this.serverMode = x
    this.asInstanceOf[R]
  }

  var defaultIgnoredFilesRegex: Seq[Regex] = Seq.empty
  var ignoredFilesRegex: Regex             = "".r
  var ignoredFiles: Seq[String]            = Seq.empty

  // 设置默认忽略文件的正则表达式
  def withDefaultIgnoredFilesRegex(x: Seq[Regex]): R = {
    this.defaultIgnoredFilesRegex = x
    this.asInstanceOf[R]
  }

  // 设置忽略文件的正则表达式
  def withIgnoredFilesRegex(x: String): R = {
    this.ignoredFilesRegex = x.r
    this.asInstanceOf[R]
  }

  // 设置忽略的文件
  def withIgnoredFiles(x: Seq[String]): R = {
    this.ignoredFiles = x.map(createPathForIgnore)
    this.asInstanceOf[R]
  }

  // 创建用于忽略的文件路径
  def createPathForIgnore(ignore: String): String = {
    val path = Paths.get(ignore)
    if (path.isAbsolute) { path.toString }
    else { Paths.get(inputPath, ignore).toAbsolutePath.normalize().toString }
  }

  var schemaValidation: ValidationMode = ValidationMode.Disabled

  // 设置模式验证
  def withSchemaValidation(value: ValidationMode): R = {
    this.schemaValidation = value
    this.asInstanceOf[R]
  }

  var disableFileContent: Boolean = true

  // 设置是否禁用文件内容
  def withDisableFileContent(value: Boolean): R = {
    this.disableFileContent = value
    this.asInstanceOf[R]
  }

  // 继承字段配置
  def withInheritedFields(config: R): R = {
    this.inputPath = config.inputPath
    this.outputPath = config.outputPath
    this.serverMode = config.serverMode
    this.defaultIgnoredFilesRegex = config.defaultIgnoredFilesRegex
    this.ignoredFilesRegex = config.ignoredFilesRegex
    this.ignoredFiles = config.ignoredFiles
    this.disableFileContent = config.disableFileContent
    this.asInstanceOf[R]
  }
}

// 允许配置是否下载依赖项以获取额外的符号信息
trait DependencyDownloadConfig[R <: X2CpgConfig[R]] { this: R =>

  def withDownloadDependencies(value: Boolean): R

}

object DependencyDownloadConfig {
  def parserOptions[R <: X2CpgConfig[R] & DependencyDownloadConfig[R]]: OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence( // 命令行解析库，当download-dependencies存在时，执行操作。
      opt[Unit]("download-dependencies")
        .text("Download the dependencies of the target project and use their symbols to resolve types.")
        .action((_, c) => c.withDownloadDependencies(true))
    )
  }
}

// CPG前端的Main类基类
abstract class X2CpgMain[T <: X2CpgConfig[T], X <: X2CpgFrontend[T]](
                                                                      val cmdLineParser: OParser[Unit, T],
                                                                      val frontend: X
                                                                    )(implicit defaultConfig: T) {

  private val logger = LoggerFactory.getLogger(classOf[X2CpgMain[T, X]])

  // 记录版本和参数
  private def logVersionAndArgs(args: Array[String]): Unit = {
    val frontendName = frontend.getClass.getSimpleName.stripSuffix("$")
    val joernVersion =
      // 只有在使用sbt assembly构建时才有适当的版本，否则可能为null
      Option(frontend.getClass.getPackage.getImplementationVersion).map(v => s"v$v").getOrElse("local build")
    val logText = s"Executing $frontendName ($joernVersion) with arguments: ${args.mkString(" ")}"
    logger.debug(logText)
  }

  // 记录输出路径
  private def logOutputPath(outputPath: String): Unit = {
    if (X2CpgConfig.defaultOutputPath == outputPath) {
      // 仅在用户未给定显式路径时记录输出路径
      logger.info(s"The resulting CPG will be stored at ${File(outputPath)}")
    }
  }

  // 运行前端和配置的方法
  def run(config: T, frontend: X): Unit

  def main(args: Array[String]): Unit = {
    logVersionAndArgs(args)
    X2Cpg.parseCommandLine(args, cmdLineParser, defaultConfig) match {
      case Some(config) =>
        try {
          logOutputPath(config.outputPath)
          run(config, frontend)
        } catch {
          case ex: Throwable =>
            ex.printStackTrace()
            System.exit(1)
        }
      case None =>
        println("Error parsing the command line")
        System.exit(1)
    }
  }
}

// 表示CPG生成器的特征，T是前端配置类
trait X2CpgFrontend[T <: X2CpgConfig[T]] {

  // 根据给定配置创建CPG，返回Try以检测异常
  def createCpg(config: T): Try[Cpg]

  // 根据给定配置创建CPG，并在控制台打印错误（如果有）
  @throws[Throwable]("if createCpg throws any Throwable")
  def run(config: T): Unit = {
    withErrorsToConsole(config) { _ =>
      createCpg(config) match {
        case Success(cpg) =>
          cpg.close() // 持久化到磁盘
          Success(cpg)
        case Failure(exception) =>
          Failure(exception)
      }
    } match {
      case Failure(exception) =>
        // 显式抛出异常以确保每个前端在createCpg中出现异常时终止
        throw exception
      case Success(_) => // 正常情况
    }
  }

  // 根据给定配置创建带有默认覆盖层的CPG
  def createCpgWithOverlays(config: T): Try[Cpg] = {
    val maybeCpg = createCpg(config)
    maybeCpg.map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }
  }

  // 为给定代码创建默认覆盖层的CPG
  def createCpgWithOverlays(inputName: String)(implicit defaultConfig: T): Try[Cpg] = {
    val maybeCpg = createCpg(inputName)
    maybeCpg.map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }
  }

  // 为单个位置的代码创建CPG，带有默认前端配置
  def createCpg(inputName: String, outputName: Option[String])(implicit defaultConfig: T): Try[Cpg] = {
    val defaultWithInputPath = defaultConfig.withInputPath(inputName)
    val config = if (!outputName.contains(X2CpgConfig.defaultOutputPath)) {
      if (outputName.isEmpty) {
        defaultWithInputPath.withOutputPath("")
      } else {
        defaultWithInputPath.withOutputPath(outputName.get)
      }
    } else {
      defaultWithInputPath
    }
    createCpg(config)
  }

  // 为文件创建内存中的CPG，使用默认配置
  def createCpg(inputName: String)(implicit defaultConfig: T): Try[Cpg] = createCpg(inputName, None)(defaultConfig)

}

object X2Cpg {

  private val logger = LoggerFactory.getLogger(X2Cpg.getClass)

  // 使用X2Cpg命令行解析器解析命令行参数
  def parseCommandLine[R <: X2CpgConfig[R]](
                                             args: Array[String],
                                             frontendSpecific: OParser[?, R],
                                             initialConf: R
                                           ): Option[R] = {
    val parser = commandLineParser(frontendSpecific)
    OParser.parse(parser, args, initialConf)
  }

  // 创建可扩展的命令行解析器
  private def commandLineParser[R <: X2CpgConfig[R]](frontendSpecific: OParser[?, R]): OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence(
      arg[String]("input-dir")
        .text("source directory")
        .action((x, c) => c.withInputPath(x)),
      opt[String]("output")
        .abbr("o")
        .text("output filename")
        .action { (x, c) =>
          c.withOutputPath(x)
        },

      // 以前是用`,`作为分隔符，现在可以多次提供此参数
      opt[Seq[String]]("exclude")
        .valueName("<file1>")
        .unbounded()
        .action { (x, c) =>
          c.ignoredFiles = c.ignoredFiles ++ x.map(c.createPathForIgnore)
          c
        }
        .text("files or folders to exclude during CPG generation (paths relative to <input-dir> or absolute paths)"),
      opt[String]("exclude-regex")
        .action { (x, c) =>
          c.ignoredFilesRegex = x.r
          c
        }
        .text("a regex specifying files to exclude during CPG generation (paths relative to <input-dir> are matched)"),
      opt[Unit]("enable-early-schema-checking")
        .action((_, c) => c.withSchemaValidation(ValidationMode.Enabled))
        .text("enables early schema validation during AST creation (disabled by default)"),
      opt[Unit]("enable-file-content")
        .action((_, c) => c.withDisableFileContent(false))
        .text(
          "add the raw source code to the content field of FILE nodes to allow for method source retrieval via offset fields (disabled by default)"
        ),
      opt[Unit]("server")
        .action((_, c) => c.withServerMode(true))
        .hidden()
        .text("runs this frontend in server mode (disabled by default)"),
      opt[Unit]("disable-file-content")
        .action((_, c) => c.withDisableFileContent(true))
        .hidden()
        .text("currently unused option that will replace enable-file-content"),
      help("help").text("display this help message"),
      frontendSpecific
    )
  }

  // 创建一个新的空CPG，可以在指定路径上存储或在内存中
  def newEmptyCpg(optionalOutputPath: Option[String] = None): Cpg = {
    optionalOutputPath match {
      case Some(outputPath) =>
        lazy val outFile = File(outputPath)
        if (outputPath != "" && outFile.exists) {
          logger.info("Output file exists, removing: " + outputPath)
          outFile.delete()
        }
        Cpg.withStorage(outFile.path)
      case None => Cpg.empty
    }
  }

  // 对新创建的CPG应用函数applyPasses
  def withNewEmptyCpg[T <: X2CpgConfig[?]](outPath: String, config: T)(applyPasses: (Cpg, T) => Unit): Try[Cpg] = {
    val outputPath = if (outPath != "") Some(outPath) else None
    Try {
      val cpg = newEmptyCpg(outputPath)
      Try {
        applyPasses(cpg, config)
      } match {
        case Success(_) => cpg
        case Failure(exception) =>
          cpg.close()
          throw exception
      }
    }
  }

  // 评估函数f并在控制台打印错误
  def withErrorsToConsole[T <: X2CpgConfig[?]](config: T)(f: T => Try[?]): Try[?] = {
    f(config) match {
      case Failure(exception) =>
        exception.printStackTrace()
        Failure(exception)
      case Success(v) =>
        Success(v)
    }
  }

  // 为前端生成的CPG运行默认的通过
  def applyDefaultOverlays(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    defaultOverlayCreators().foreach { creator =>
      creator.run(context)
    }
  }

  // 定义默认覆盖层的唯一位置
  def defaultOverlayCreators(): List[LayerCreator] = {
    List(new Base(), new ControlFlow(), new TypeRelations(), new CallGraph())
  }

  // 将sourceCode写入临时文件
  def writeCodeToFile(sourceCode: String, tmpDirPrefix: String, suffix: String): java.io.File = {
    val tmpDir = Files.createTempDirectory(tmpDirPrefix).toFile
    tmpDir.deleteOnExit()
    val codeFile = java.io.File.createTempFile("Test", suffix, tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(sourceCode); close() }
    tmpDir
  }

  // 去除字符串的引号
  def stripQuotes(str: String): String = str.replaceAll("^(\"|')|(\"|')$", "")

}
