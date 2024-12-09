using log4net;
using log4net.Appender;
using log4net.Config;
using log4net.Core;
using log4net.Layout;

/// <summary>
/// 简单封装log4net基本功能
/// 1 无论log4net.config文件是否存在，都可以正常使用log4net.dll记录日志
/// 2 支持两种类型的logger：SystemLogger和DatabaseLogger，还可以添加更多的logger对象
/// </summary>
public class MyLogger
{
    private static readonly Type declaringType = typeof(MyLogger);

    protected static ILog SystemLog
    {
        get;
        private set;
    }

    protected static ILog DatabaseLog
    {
        get;
        private set;
    }

    static MyLogger()
    {
        string filePath = "ConfigFile/log4net.config";

        if (File.Exists(filePath))
        {
            XmlConfigurator.ConfigureAndWatch(new FileInfo(filePath));
        }
        else
        {
            //默认设置（log4net 按装饰器模式实现，支持：ConsoleLog、FileLog、EventLog等不同方式）
            RollingFileAppender appender = new RollingFileAppender
            {
                Name = "root",
                File = "logs\\log.txt",
                AppendToFile = true,
                RollingStyle = RollingFileAppender.RollingMode.Composite, //日期发生变化或日志文件大小超过10M，自动生成新的日志文件
                DatePattern = "yyyyMMdd\".txt\"", //如果日期发了变化，自动保存历史日志文件，并生成新的日志文件
                MaxSizeRollBackups = 10, //保留几个历史日志文件
                Layout = new PatternLayout("[%d{yyyy-MM-dd HH:mm:ss.fff}] [%t] %-5level %logger property:[%property{NDC}] - %message%newline") //%m表示日志内容
            };

            BasicConfigurator.Configure(appender);
            appender.ActivateOptions();
        }

        //为log4net添加不同类型的参数logger，用于配置选择哪种方式输出日志
        //SystemLog = LogManager.GetLogger("RollingFile"); //返回或新建一个命名的logger对象
        //DatabaseLog = LogManager.GetLogger("DatabaseLogger");
    }

    private static void WriteLog(ILog log, Level level, Exception exception, string message)
    {
        log.Logger.Log(declaringType, level, message, exception);
    }

    #region 装饰器>>系统日志

    public static void Debug(string loggerType, string message, Exception exception = null)
    {
        WriteLog(LogManager.GetLogger(loggerType), Level.Debug, exception, message);
    }

    public static void Info(string loggerType, string message, Exception exception = null)
    {
        WriteLog(LogManager.GetLogger(loggerType), Level.Info, exception, message);
    }

    public static void Warn(string loggerType, string message, Exception exception = null)
    {
        WriteLog(LogManager.GetLogger(loggerType), Level.Warn, exception, message);
    }

    public static void Error(string loggerType, string message, Exception exception = null)
    {
        WriteLog(LogManager.GetLogger(loggerType), Level.Error, exception, message);
    }

    public static void Fatal(string loggerType, string message, Exception exception = null)
    {
        WriteLog(LogManager.GetLogger(loggerType), Level.Fatal, exception, message);
    }

    #endregion

    #region 装饰器>>数据库日志       

    //public static void DatabaseInfo(string message, Exception exception = null)
    //{
    //    WriteLog(DatabaseLog, Level.Info, exception, message);
    //}

    //public static void DatabaseWarn(string message, Exception exception = null)
    //{
    //    WriteLog(DatabaseLog, Level.Warn, exception, message);
    //}

    //public static void DatabaseError(string message, Exception exception = null)
    //{
    //    WriteLog(DatabaseLog, Level.Error, exception, message);
    //}

    #endregion
}
