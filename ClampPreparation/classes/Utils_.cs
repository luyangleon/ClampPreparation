using Microsoft.VisualBasic;
using Newtonsoft.Json.Linq;
using System.Text;
using System.Data;
using System.Reflection;
using System.Diagnostics;
using System.Net.Security;
using System.Net.Sockets;
using System.Net;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace ClampPreparation.classes
{
    public static class Utils
    {
        #region 字串简繁转换，引用：Microsoft.VisualBasic，依赖：Microsoft.VisualBasic;
        /// <summary>
        /// 字符串简体转繁体
        /// </summary>
        /// <param name="strSimple"></param>
        /// <returns></returns>
        public static string ToTraditionalChinese(string strSimple)
        {
            string strTraditional = Strings.StrConv(strSimple, VbStrConv.TraditionalChinese, 0);
            return strTraditional;
        }

        /// <summary>
        /// 字符串繁体转简体
        /// </summary>
        /// <param name="strTraditional"></param>
        /// <returns></returns>
        public static string ToSimplifiedChinese(string strTraditional)
        {
            string strSimple = Strings.StrConv(strTraditional, VbStrConv.SimplifiedChinese, 0);
            return strSimple;
        }
        #endregion

        #region MD5值取得
        /// <summary>
        /// 取得字节流对应的MD5值
        /// </summary>
        /// <param name="content"></param>
        /// <returns></returns>
        public static string GetMD5(byte[] content)
        {
            byte[] result;
            if (content == null) return "";
            StringBuilder sb = new StringBuilder();
            using (MD5CryptoServiceProvider md5 = new MD5CryptoServiceProvider())
            {
                result = md5.ComputeHash(content);
                for (int i = 0; i < result.Length; i++)
                {
                    sb.Append(result[i].ToString("X2"));
                }
            }
            return sb.ToString();
        }
        #endregion

        #region 取得app.config中参数值并建立缓存
        /// <summary>
        /// app.config参数字典
        /// </summary>
        private static Dictionary<string, string> _Parameters = new Dictionary<string, string>();
        /// <summary>
        /// 系统日志文件路径
        /// </summary>
        private static string _SystemLogFile = GetParameterValue("SystemLogFile") ?? $"C:/{Process.GetCurrentProcess().ProcessName}.Log";
        /// <summary>
        /// 系统日志文件(必须在app.config中以key=SystemLogFile来指定，未指定则默认为：C:\[进程名].log)
        /// </summary>
        public static string SystemLogFile { get => _SystemLogFile; set => _SystemLogFile = value; }

        /// <summary>
        /// 环境参数写入字典，如果存在则更新
        /// </summary>
        /// <param name="key">键</param>
        /// <param name="value">值</param>
        public static void SetParameterValue(string key, string value)
        {
            if (key == null || (key = key.Trim()).Length == 0) return;
            if (value == null || (value = value.Trim()).Length == 0) return;
            if (_Parameters.ContainsKey(key)) _Parameters[key] = value;
            else _Parameters.Add(key, value);
        }

        /// <summary>
        /// 取得app.config中参数值
        /// </summary>
        /// <param name="key">参数名</param>
        /// <returns>返回参数的值，若无此参数返回null</returns>
        //public static string GetParameterValue(string key)
        //{
        //    string result = null;
        //    if (key != null || !"".Equals(key.Trim()))
        //    {
        //        key = key.Trim();
        //        if (_Parameters.ContainsKey(key))
        //        {   //包含参数，直接返回值
        //            result = _Parameters[key];
        //        }
        //        else
        //        {   //不包含参数，尝试从app.config中读参
        //            result = ConfigurationManager.AppSettings[key];
        //            //如果有读到参数，记录到字典以便下次使用
        //            if (result != null) _Parameters.Add(key, result);
        //        }
        //    }
        //    return result;
        //}
        public static string GetParameterValue(string key)
        {
            string result = null;
            if (key != null || !"".Equals(key.Trim()))
            {
                key = key.Trim();
                if (_Parameters.ContainsKey(key))
                {   //包含参数，直接返回值
                    result = _Parameters[key];
                }
                else
                {   //不包含参数，尝试从app.config中读参
                    try
                    {
                        JObject obj = new JObject();
                        obj = JObject.Parse(File.ReadAllText("appsettings.json"));
                        if (obj != null)
                        {
                            if (obj.ContainsKey(key)) result = obj[key].ToString().Trim();
                        }
                    }
                    catch { }
                    //如果有读到参数，记录到字典以便下次使用
                    if (result != null) _Parameters.Add(key, result);
                }
            }
            return result;
        }
        #endregion

        /// <summary>
        /// 返回本机IP地址，需引用System.Net.Socket
        /// </summary>
        /// <returns></returns>
        public static string GetLocalIp()
        {
            string localIp = "";
            try
            {
                using (Socket socket = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp))
                {
                    socket.Connect("8.8.8.8", 65535);
                    localIp = (socket.LocalEndPoint as IPEndPoint).Address.ToString();
                }
            }
            catch { }
            return localIp;
        }

        #region 拼接字串方法GetJoinStr():用指定分隔符拼接字串，GetSqlInStr():拼接SQL中IN条件的条件字串
        /// <summary>
        /// 拼接SQL中IN条件的条件字串
        /// </summary>
        /// <param name="list">可迭代对象(数组，列表，集合等)</param>
        /// <returns>返回SQL WHERE IN条件的字串，(EX:'s1','s2','s3','s4')</returns>
        public static string GetSqlInStr(IEnumerable<string> list)
        {
            string result = "";
            if (list.Count() == 0) return result;
            foreach (string s in list)
            {
                if (s.Trim().Length > 0) result += $"'{s}',";
            }
            result = result.TrimEnd(',');
            return result;
        }

        /// <summary>
        /// 使用指定分隔符拼接字串
        /// </summary>
        /// <param name="list">可迭代对象(数组，列表，集合等)</param>
        /// <returns>返回将对象所有元素用分隔符拼接起来的长字串,对象为空返回空字串</returns>
        public static string GetJoinStr(IEnumerable<string> list, char splitChar)
        {
            string result = "";
            if (list.Count() == 0) return result;
            foreach (string s in list)
            {
                result += s + splitChar;
            }
            result = result.TrimEnd(splitChar);
            return result;
        }
        #endregion

        #region 获取目录下的文件名
        /// <summary>
        /// 获取本地文件夹下的文件名
        /// </summary>
        /// <param name="filePath">资料夹路径</param>
        /// <param name="fileNamePattern">文件名，支持通配符</param>
        /// <returns>返回匹配的文件名列表</returns>
        public static List<string> GetFileNames(string filePath, string fileNamePattern)
        {
            List<string> result = new List<string>();
            DirectoryInfo di = new DirectoryInfo(filePath);
            if (di.Exists)
            {
                foreach (FileInfo fi in di.GetFiles(fileNamePattern))
                {
                    result.Add(fi.Name);
                }
            }
            return result;
        }

        /// <summary>
        /// 获取本地文件夹下的文件对象(FileInfo)
        /// </summary>
        /// <param name="filePath">资料夹路径</param>
        /// <param name="fileNamePattern">文件名，支持通配符</param>
        /// <returns>返回匹配的文件对象列表</returns>
        public static List<FileInfo> GetFileInfos(string filePath, string fileNamePattern)
        {
            List<FileInfo> result = new List<FileInfo>();
            DirectoryInfo di = new DirectoryInfo(filePath);
            if (di.Exists)
            {
                foreach (FileInfo fi in di.GetFiles(fileNamePattern))
                {
                    result.Add(fi);
                }
            }
            return result;
        }
        #endregion

        /// <summary>
        /// 判断文件是否被占用
        /// </summary>
        /// <param name="filePath">文件路径</param>
        /// <returns>True:被占用 False:未占用</returns>
        public static bool IsOccupied(string filePath)
        {
            FileStream fs = null;
            try
            {
                fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None);
                return false;
            }
            catch { return true; }
            finally { if (fs != null) fs.Close(); }
        }

        #region 判断进程是否已存在
        /// <summary>
        /// 判断进程是否已存在(放在FormLoad事件中使用)
        /// </summary>
        /// <param name="processName">进程名，当前进程名取得方式(Process.GetCurrentProcess().ProcessName)</param>
        /// <returns>True:进程已存在，False:进程不存在或进程名为空</returns>
        public static bool IsProcessExist(string processName)
        {
            int result = 0;
            //进程名无效时返回False
            if (processName == null || "".Equals(processName.Trim())) return false;
            processName = processName.Trim();
            try
            {
                //获取当前进程数组
                Process[] processes = Process.GetProcesses();
                foreach (Process p in processes)
                {
                    if (p.ProcessName.Equals(processName))
                    {   //进程名相同，返回结果
                        result++;
                        if (result > 1) break;  //进程超过1个
                    }
                }
            }
            catch (Exception ex)
            {
                AppendToFile(_SystemLogFile, $"Utils.ProcessExist({processName}) fail:{ex.Message}", true);
            }
            return result > 1;
        }
        #endregion

        #region 检查参数是否未定义或定义为空
        /// <summary>
        /// 检查参数是否未定义或定义为空
        /// </summary>
        /// <param name="lPara">参数key列表(app.config中的参数key)</param>
        /// <returns>True:参数未定义或为空值，False:参数有定义</returns>
        public static bool ParaNotExist(List<string> lPara)
        {
            if (lPara == null) return false;
            int iErrorCnt = 0;
            string sVal;
            foreach (string sPara in lPara)
            {
                sVal = Utils.GetParameterValue(sPara);
                if (sVal == null || sVal.Trim().Length == 0)
                {
                    AppendToFile(SystemLogFile, $"Utils.ParaNotExist，系统参数[{sPara}]未设定或为空.", true);
                    iErrorCnt++;
                }
            }
            return iErrorCnt > 0;
        }
        #endregion

        #region 将对象转换为定长字符串返回，长度不足补满
        /// <summary>
        /// 取指定长度含中文字串
        /// </summary>
        /// <param name="str">要处理的字串(含中文)</param>
        /// <param name="length">要返回的字串长度(中文算一个字符)</param>
        /// <param name="encoding">编码类型</param>
        /// <returns></returns>
        public static string GetLenChTxt(string str, int iLength, Encoding encoding)
        {
            if (iLength <= 0) return string.Empty;
            if (str == null) return "".PadRight(iLength, ' ');
            byte[] strByte = encoding.GetBytes(str);
            string s = "";
            if (strByte.Length <= iLength)
                return str + s.PadRight(iLength - strByte.Length);
            else
            {
                string[] letters = str.Select(c => c.ToString()).ToArray();
                int i, t = 0;
                string word = "";
                foreach (string ss in letters)
                {
                    byte[] ssByte = encoding.GetBytes(ss);
                    i = ssByte.Length;
                    if (t + i <= iLength)
                    {
                        word = word + ss;
                        t = t + i;
                    }
                }
                return word;
            }
        }

        /// <summary>
        /// 将对象转换为定长的字符串，不足用空格左补位
        /// </summary>
        /// <param name="obj">转换对象</param>
        /// <param name="iLength">结果字串长度</param>
        /// <returns>定长的字符串，传入对象为空时，转出定长补位字串</returns>
        public static string GetPadString(object obj, int iLength)
        {
            return GetPadString(obj, iLength, false, ' ');
        }

        /// <summary>
        /// 将对象转换为定长的字符串，不足用空格补位
        /// </summary>
        /// <param name="obj">转换对象</param>
        /// <param name="iLength">结果字串长度</param>
        /// <param name="bPadRight">True:右补位，False:左补位</param>
        /// <returns>定长的字符串，传入对象为空时，转出定长补位字串</returns>
        public static string GetPadString(object obj, int iLength, bool bPadRight)
        {
            return GetPadString(obj, iLength, bPadRight, ' ');
        }

        /// <summary>
        /// 将对象转换为定长的字符串，不足则左补位
        /// </summary>
        /// <param name="obj">转换对象</param>
        /// <param name="iLength">结果字串长度</param>
        /// <param name="cFillChar">字符串长度不足时，补位的字符</param>
        /// <returns>定长的字符串，传入对象为空时，转出定长补位字串</returns>
        public static string GetPadString(object obj, int iLength, char cFillChar)
        {
            return GetPadString(obj, iLength, false, cFillChar);
        }

        /// <summary>
        /// 将对象转换为定长的字符串
        /// </summary>
        /// <param name="obj">转换对象</param>
        /// <param name="iLength">结果字串长度</param>
        /// <param name="bPadRight">True:右补位，False:左补位</param>
        /// <param name="cFillChar">字符串长度不足时，补位的字符</param>
        /// <returns>定长的字符串，传入对象为空时，转出定长补位字串</returns>
        public static string GetPadString(object obj, int iLength, bool bPadRight, char cFillChar)
        {
            string result = "";
            try
            {
                result = obj == null ? "" : Convert.ToString(obj).Trim();
                if (result.Length > iLength)
                {
                    //字符超出长度，PadRight从右边截取，PadLeft从左边截取
                    result = bPadRight ? result.Substring(result.Length - iLength, iLength) : result.Substring(0, iLength);
                }
                else if (result.Length < iLength)
                {
                    result = bPadRight ? result.PadRight(iLength, cFillChar) : result.PadLeft(iLength, cFillChar);
                }
            }
            catch (Exception ex)
            {
                AppendToFile(SystemLogFile, "Utils.GetPadString执行错误，" + ex.Message, true);
            }
            return result;
        }
        #endregion

        #region 写字串至本地TXT文件，依赖：using System.IO
        /// <summary>
        /// 将内容追加到文件中
        /// </summary>
        /// <param name="filepath">文件路径及文件名称</param>
        /// <param name="content">写入内容</param>
        /// <param name="encoding">文件编码</param>
        /// <param name="needtime">True:在内容前加入系统时间，False:不加入系统时间</param>
        /// <returns>成功返回True，失败返回False</returns>
        public static bool AppendToFile(string filepath, List<string> content, Encoding encoding, bool marktime)
        {
            bool result = false;
            //传入参数错误，忽略
            if (content == null || content.Count == 0 || filepath == null || "".Equals(filepath.Trim())) return result;
            try
            {
                //内容前加入系统时间
                if (marktime) { content[0] = $"{DateTime.Now.ToString("yyyy/MM/dd HH:mm:ss")} {content[0]}"; }
                //若前置文件夹不存在，尝试建立
                FileInfo fileInfo = new FileInfo(filepath);
                if (!fileInfo.Directory.Exists) fileInfo.Directory.Create();
                //执行文本追加
                File.AppendAllLines(filepath, content, encoding);
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"{DateTime.Now.ToString("yyyy/MM/dd HH:mm:ss")} Utils.AppendToFile fail:{ex.Message}");
            }
            return result;
        }

        /// <summary>
        /// 将内容追加到文件中(Encoding.UTF8)
        /// </summary>
        /// <param name="filepath">文件路径及文件名称</param>
        /// <param name="content">写入内容</param>
        /// <param name="marktime">True:在内容前加入系统时间，False:不加入系统时间</param>
        /// <returns>成功返回True，失败返回False</returns>
        public static bool AppendToFile(string filepath, List<string> content, bool marktime)
        {
            return AppendToFile(filepath, content, Encoding.UTF8, marktime);
        }

        /// <summary>
        /// 将内容追加到文件中(Encoding.UTF8)
        /// </summary>
        /// <param name="filepath">文件路径及文件名称</param>
        /// <param name="content">写入内容</param>
        /// <param name="marktime">True:在内容前加入系统时间，False:不加入系统时间</param>
        /// <returns>成功返回True，失败返回False</returns>
        public static bool AppendToFile(string filepath, string content, bool marktime)
        {
            return AppendToFile(filepath, new List<string>() { content }, marktime);
        }

        /// <summary>
        /// 将内容追加到文件中(Encoding.UTF8)
        /// </summary>
        /// <param name="filepath">文件路径及文件名称</param>
        /// <param name="content">写入内容</param>
        /// <param name="encoding">指定写入内容的编码(System.Text.Encoding)</param>
        /// <returns>成功返回True，失败返回False</returns>
        public static bool AppendToFile(string filepath, string content, Encoding encoding)
        {
            return AppendToFile(filepath, new List<string>() { content }, encoding, false);
        }
        #endregion
        /// <summary>
        /// get请求
        /// </summary>
        /// <param name="url"></param>
        /// <returns></returns>
        public static string SendGetRequest(string url)
        {
            SetCertificatePolicy();
            string responseContent = string.Empty;

            try
            {
                HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
                request.Method = "GET";
                request.ContentType = "application/json";

                using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
                {
                    using (Stream stream = response.GetResponseStream())
                    {
                        using (StreamReader reader = new StreamReader(stream))
                        {
                            responseContent = reader.ReadToEnd();
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("An error occurred: " + ex.Message);
            }

            return responseContent;
        }
        /// <summary>
        /// post请求
        /// </summary>
        /// <param name="url"></param>
        /// <param name="postData"></param>
        /// <returns></returns>

        public static string SendPostRequest(string url, string postData)
        {
            SetCertificatePolicy();
            string responseContent = string.Empty;

            try
            {
                byte[] data = Encoding.UTF8.GetBytes(postData);

                HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
                request.Method = "POST";
                request.ContentType = "application/json";
                request.ContentLength = data.Length;

                using (Stream stream = request.GetRequestStream())
                {
                    stream.Write(data, 0, data.Length);
                }

                using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
                {
                    using (Stream stream = response.GetResponseStream())
                    {
                        using (StreamReader reader = new StreamReader(stream))
                        {
                            responseContent = reader.ReadToEnd();
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("An error occurred: " + ex.Message);
            }

            return responseContent;
        }

        /// <summary>
        /// Sets the cert policy.
        /// </summary>
        public static void SetCertificatePolicy()
        {
            ServicePointManager.ServerCertificateValidationCallback
                       += RemoteCertificateValidate;
        }

        /// <summary>
        /// Remotes the certificate validate.
        /// </summary>
        private static bool RemoteCertificateValidate(
           object sender, X509Certificate cert,
            X509Chain chain, SslPolicyErrors error)
        {
            return true;
        }

        #region 扩展方法

        public static IList<T> ToList<T>(this DataTable table) where T : new()
        {
            IList<PropertyInfo> properties = typeof(T).GetProperties().ToList();
            IList<T> result = new List<T>();

            foreach (var row in table.Rows)
            {
                var item = CreateItemFromRow<T>((DataRow)row, properties);
                result.Add(item);
            }

            return result;
        }

        private static T CreateItemFromRow<T>(DataRow row, IList<PropertyInfo> properties) where T : new()
        {
            T item = new T();
            foreach (var property in properties)
            {
                //property.SetValue(item, row[property.Name], null);
                // 跳过扩展字段
                //if (property.Name.StartsWith("Extend_")) continue;
                // 获取属性的数据类型
                Type propertyType = property.PropertyType;
                // 获取 DataRow 中的值
                object? rowValue = row[property.Name];
                // 进行非空验证
                if (rowValue == DBNull.Value) continue;
                if (rowValue is string) rowValue = rowValue?.ToString()?.Trim();
                // 进行数据类型验证
                if (!propertyType.IsInstanceOfType(rowValue)) continue;
                // 如果属性是可写的，则设置属性的值
                if (property.CanWrite)
                {
                    property.SetValue(item, rowValue, null);
                }
            }
            return item;
        }

        public static async Task<string> SendPostRequestAsync(string url, string postData)
        {
            using var client = new HttpClient();
            var request = new HttpRequestMessage(HttpMethod.Post, url);
            request.Content = new StringContent(postData, Encoding.UTF8, "application/json");

            using var response = await client.SendAsync(request);

            if (response.IsSuccessStatusCode)
            {
                return await response.Content.ReadAsStringAsync();
            }
            else
            {
                throw new Exception($"API request failed: {response.StatusCode}");
            }
        }

        #endregion
    }
}
