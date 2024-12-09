using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using StackExchange.Redis;
using System.Data;
using System.DirectoryServices;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

public static class Utils
{
    #region 取得app.config中参数值并建立缓存
    /// <summary>
    /// app.config参数字典
    /// </summary>
    private static Dictionary<string, string> _Parameters = new Dictionary<string, string>();

    // 定义简繁对照字串
    const string SIMP = "标别皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙闭边编贬变辩辫鳖瘪濒滨宾摈饼拨钵铂驳卜补参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔点垫电淀钓调迭谍叠钉顶锭订东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞国爱赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛伞丧骚扫涩杀纱筛晒闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽绥岁孙损笋缩琐锁獭挞抬摊贪瘫滩坛谭谈叹汤烫涛绦腾誊锑题体屉条贴铁厅听烃铜统头图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝呜钨乌诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮樱婴鹰应缨莹萤营荧蝇颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰帧郑证织职执纸挚掷帜质钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆浊兹资渍踪综总纵邹诅组钻致钟么为只凶准启板里雳余链泄";
    const string TRAD = "標別皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃閉邊編貶變辯辮鼈癟瀕濱賓擯餅撥缽鉑駁蔔補參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締點墊電澱釣調叠諜疊釘頂錠訂東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐國愛賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽傘喪騷掃澀殺紗篩曬閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖綏歲孫損筍縮瑣鎖獺撻擡攤貪癱灘壇譚談歎湯燙濤縧騰謄銻題體屜條貼鐵廳聽烴銅統頭圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問甕撾蝸渦窩嗚鎢烏誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲櫻嬰鷹應纓瑩螢營熒蠅穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙幀鄭證織職執紙摯擲幟質鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄濁茲資漬蹤綜總縱鄒詛組鑽緻鐘麼為隻兇準啟闆裡靂餘鍊洩";

    /// <summary>
    /// 配置文件对象
    /// </summary>
    private static ConfigurationManager _Config;

    /// <summary>
    /// 配置文件对象引入，需在Program.cs中带入对象
    /// Utils.InitConfigrationManager(builder.Configuration);
    /// </summary>
    /// <param name="configurationManager"></param>
    public static void InitConfigrationManager(ConfigurationManager configurationManager)
    {
        _Config = configurationManager;
    }

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
                result = _Config[key];
                //如果有读到参数，记录到字典以便下次使用
                if (result != null) _Parameters.Add(key, result);
            }
        }
        return result;
    }
    #endregion

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
            result += $"'{s}',";
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

    #region 验证员工帐号密码(AD认证)，依赖：using System.DirectoryServices;
    /// <summary>
    /// 验证员工帐号密码(AD认证)
    /// </summary>
    /// <param name="userId">帐号</param>
    /// <param name="password">密码</param>
    /// <returns>验证通过返回True，否则为False</returns>
    public static bool checkAD(string userId, string password)
    {
        bool result = false;
        string ldap = "LDAP://192.168.60.9";
        try
        {
            DirectoryEntry entry = new DirectoryEntry(ldap, @"YUEMA\" + userId, password);
            object nativeObject = entry.NativeObject;
            result = true;
        }
        catch (Exception ex)
        {
            MyLogger.Error("Utils.checkAD", $"验证域账户失败，UserId='{userId}',Pass='{password}'", ex);
        }
        return result;
    }

    /// <summary>
    /// 验证员工帐号密码(AD认证)
    /// </summary>
    /// <param name="userId">帐号</param>
    /// <param name="password">密码</param>
    /// <returns>验证通过返回True，否则为False</returns>
    public static bool checkADb(string userId, string password)
    {
        bool result = false;
        JObject jresult = null;
        string url = "https://cloud.longchengreentech.com:8443/Utils/CheckAD";
        try
        {
            string sendContent = JsonConvert.SerializeObject(new { UserId = userId, Password = password });
            using (HttpClient client = new HttpClient())
            {
                using (HttpContent content = new StringContent(sendContent))
                {
                    HttpResponseMessage msg = client.PostAsync(url, content).Result;
                    jresult = JObject.Parse(msg.Content.ReadAsStringAsync().Result);
                }
            }
            result = Convert.ToBoolean(jresult["success"]);
        }
        catch (Exception ex)
        {
            MyLogger.Error("Utils.checkAD", $"验证域账户失败，UserId='{userId}',Pass='{password}'", ex);
        }
        return result;
    }
    #endregion

    #region 字串简繁转换
    /// <summary>
    /// 字符串简体转繁体
    /// </summary>
    /// <param name="strSimple"></param>
    /// <returns></returns>
    public static string ToTraditionalChinese(string strSimple)
    {
        string result = "";
        int position;
        for (int i = 0; i < strSimple.Length; i++)
        {
            position = SIMP.IndexOf(strSimple[i]);
            if (position < 0)
                result += strSimple[i];
            else
                result += TRAD[position];
        }
        return result;
    }

    /// <summary>
    /// 字符串繁体转简体
    /// </summary>
    /// <param name="strTraditional"></param>
    /// <returns></returns>
    public static string ToSimplifiedChinese(string strTraditional)
    {
        string result = "";
        int position;
        for (int i = 0; i < strTraditional.Length; i++)
        {
            position = TRAD.IndexOf(strTraditional[i]);
            if (position < 0)
                result += strTraditional[i];
            else
                result += SIMP[position];
        }
        return result;
    }
    #endregion

    /// <summary>
    /// 取字符串最后几位字符（不去空格）
    /// </summary>
    /// <param name="str">原始字符串</param>
    /// <param name="len">截取长度</param>
    /// <returns></returns>
    public static string Right(string str, int len)
    {
        if (str == null || str.Length <= len) return str;
        else
        {
            str = str.Remove(0, str.Length - len);
            return str;
        }
    }

    // List转DataTable
    public static DataTable listToDT<T>(List<T> entitys)
    {
        DataTable dt = new DataTable();
        //检查实体集合不能为空
        if (entitys == null || entitys.Count < 1)
        {
            return dt;
        }

        //取出第一个实体的所有Propertie
        Type entityType = entitys[0].GetType();
        PropertyInfo[] entityProperties = entityType.GetProperties();

        //生成DataTable的structure
        //生产代码中，应将生成的DataTable结构Cache起来，此处略
        for (int i = 0; i < entityProperties.Length; i++)
        {
            //dt.Columns.Add(entityProperties[i].Name, entityProperties[i].PropertyType);
            dt.Columns.Add(entityProperties[i].Name);
        }

        //将所有entity添加到DataTable中
        foreach (object entity in entitys)
        {
            //检查所有的的实体都为同一类型
            if (entity.GetType() != entityType)
            {
                throw new Exception("要转换的集合元素类型不一致");
            }
            object[] entityValues = new object[entityProperties.Length];
            for (int i = 0; i < entityProperties.Length; i++)
            {
                entityValues[i] = entityProperties[i].GetValue(entity, null);

            }
            dt.Rows.Add(entityValues);
        }
        return dt;
    }

    // DataTable转List
    public static List<T> dtToList<T>(DataTable dt) where T : new()
    {
        // 定义集合    
        List<T> ts = new List<T>();

        // 获得此模型的类型   
        Type type = typeof(T);
        string tempName = "";

        foreach (DataRow dr in dt.Rows)
        {
            T t = new T();
            // 获得此模型的公共属性      
            PropertyInfo[] propertys = t.GetType().GetProperties();
            foreach (PropertyInfo pi in propertys)
            {
                tempName = pi.Name;  // 检查DataTable是否包含此列    
                if (dt.Columns.Contains(tempName))
                {
                    // 判断此属性是否有Setter      
                    if (!pi.CanWrite) continue;
                    object value = dr[tempName];
                    if (value != DBNull.Value)
                        pi.SetValue(t, value, null);
                }
            }
            ts.Add(t);
        }
        return ts;
    }
    #region 写字串至本地TXT文件，依赖：using System.IO
    /// <summary>
    /// 将内容追加到文件中
    /// </summary>
    /// <param name="filepath">文件路径及文件名称</param>
    /// <param name="content">写入内容</param>
    /// <param name="encoding">文件编码</param>
    /// <param name="needtime">True:在内容前加入系统时间，False:不加入系统时间</param>
    /// <returns>成功返回True，失败返回False</returns>
    private static bool AppendToFile(string filepath, List<string> content, Encoding encoding, bool marktime)
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
    #region 身份证验证
    /// <summary>
    /// 传入身份证号，返回是否为合法
    /// </summary>
    /// <param name="CardId">身份证号</param>
    /// <returns></returns>
    public static bool ValidateIDCard(string CardId)
    {
        string pattern = @"^\d{17}(?:\d|X)$";
        if (!Regex.IsMatch(CardId, pattern)) // 18位格式检查  
        {
            return false;
        }

        string id = CardId;
        string birth = id.Substring(6, 8);
        birth = birth.Insert(6, "-").Insert(4, "-"); // 插入出生日期中的分隔符  
        DateTime birthDate;
        if (!DateTime.TryParse(birth, out birthDate)) // 出生日期检查  
        {
            return false;
        }

        int[] arr_weight = { 7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2 };//加权数组  
        char[] id_last = { '1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2' };//效验数组  
        int sum = 0;
        for (int i = 0; i < 17; i++)
        {
            sum += arr_weight[i] * int.Parse(id[i].ToString()); // 检查身份证号的校验位是否正确  
        }
        int result = sum % 11; // 实际效验的值  
        if (id_last[result] == id[17]) // 验位检查  
        {
            Console.WriteLine("合法");
            return true;
        }
        else
        {
            Console.WriteLine("不合法");
            return false;
        }
    }
    #endregion

    #region base64
    /// <summary>
    /// base64加密
    /// </summary>
    /// <param name="str"></param>
    /// <returns></returns>
    public static string Base64EncodeString(string str)
    {
        if (!string.IsNullOrWhiteSpace(str))
        {
            byte[] bytes = Encoding.UTF8.GetBytes(str);
            return Convert.ToBase64String(bytes);
        }
        return str;
    }

    /// <summary>
    /// base64解密
    /// </summary>
    /// <param name="str"></param>
    /// <returns></returns>
    public static string Base64DecodeString(string str)
    {
        if (!string.IsNullOrWhiteSpace(str))
        {
            byte[] fromBase64String = Convert.FromBase64String(str);

            return Encoding.UTF8.GetString(fromBase64String);

        }
        return str;
    }
    #endregion

    #region 取连接字串及参数
    /// <summary>
    /// 取DB连接字串
    /// 依赖配置文件项："W3WebApiUrl": "https://cloud.longchengreentech.com:8443/",
    /// </summary>
    /// <param name="dbName">资料库名</param>
    /// <param name="connStr">连接字串结果</param>
    /// <returns>null为失败</returns>
    public static string GetDbConnectionString(string dbName)
    {
        string res = null;

        if (!string.IsNullOrWhiteSpace(dbName))
        {
            try
            {
                string url = GetParameterValue("W3WebApiUrl") + "Utils/GetDBConnectionString?dbname=" + dbName;
                using (HttpClient client = new())
                {
                    HttpResponseMessage resp = client.PostAsync(url, null).Result;
                    string result = resp.Content.ReadAsStringAsync().Result;
                    if (resp.StatusCode == System.Net.HttpStatusCode.OK && result.Contains("success") && result.Contains("data"))
                    {
                        HttpResponseInfo jres = JsonConvert.DeserializeObject<HttpResponseInfo>(result);
                        res = jres.success ? jres.data.ToString() : null;
                    }
                }
            }
            catch (Exception ex)
            {
                MyLogger.Error("InformationPlatform.Utils", $"GetDBConnectionString('{dbName}')执行失败", ex);
            }
        }
        else
        {
            MyLogger.Error("InformationPlatform.Utils", $"GetDBConnectionString('{dbName}')，参数无效");
        }
        return res;
    }

    /// <summary>
    /// 取DB连接字串
    /// 依赖配置文件项："W3WebApiUrl": "https://cloud.longchengreentech.com:8443/",
    /// </summary>
    /// <param name="dbName">资料库名</param>
    /// <param name="connStr">连接字串结果</param>
    /// <returns>true:成功 / false:失败</returns>
    public static bool GetDbConnectionString(string dbName, out string connStr)
    {
        bool success = false;
        connStr = string.Empty;

        if (!string.IsNullOrWhiteSpace(dbName))
        {
            try
            {
                string url = GetParameterValue("W3WebApiUrl") + "Utils/GetDBConnectionString?dbname=" + dbName;
                using (HttpClient client = new())
                {
                    HttpResponseMessage resp = client.PostAsync(url, null).Result;
                    string result = resp.Content.ReadAsStringAsync().Result;
                    if (resp.StatusCode == System.Net.HttpStatusCode.OK && result.Contains("success") && result.Contains("data"))
                    {
                        HttpResponseInfo jres = JsonConvert.DeserializeObject<HttpResponseInfo>(result);
                        success = jres.success;
                        connStr = jres.data.ToString();
                    }
                }
            }
            catch (Exception ex)
            {
                MyLogger.Error("InformationPlatform.Utils", $"GetDBConnectionString('{dbName}')执行失败", ex);
            }
        }
        else
        {
            MyLogger.Error("InformationPlatform.Utils", $"GetDBConnectionString('{dbName}')，参数无效");
        }
        return success;
    }

    /// <summary>
    /// 取全局参数值
    /// 依赖配置文件项："W3WebApiUrl": "https://cloud.longchengreentech.com:8443/",
    /// </summary>
    /// <param name="key">参数key</param>
    /// <param name="value">返回参数key对应值</param>
    /// <returns>true:成功 / false:失败</returns>
    public static bool GetGlobalParameter(string key, out string value)
    {
        bool success = false;
        value = string.Empty;

        if (!string.IsNullOrWhiteSpace(key))
        {
            try
            {
                string url = GetParameterValue("W3WebApiUrl") + "Utils/GetGlobalParameter?key=" + key;
                using (HttpClient client = new())
                {
                    HttpResponseMessage resp = client.PostAsync(url, null).Result;
                    string result = resp.Content.ReadAsStringAsync().Result;
                    if (resp.StatusCode == System.Net.HttpStatusCode.OK && result.Contains("success") && result.Contains("data"))
                    {
                        HttpResponseInfo jres = JsonConvert.DeserializeObject<HttpResponseInfo>(result);
                        success = jres.success;
                        value = jres.data.ToString();
                    }
                }
            }
            catch (Exception ex)
            {
                MyLogger.Error("InformationPlatform.Utils", $"GetGlobalParameter('{key}')执行失败", ex);
            }
        }
        else
        {
            MyLogger.Error("InformationPlatform.Utils", $"GetGlobalParameter('{key}')，参数无效");
        }
        return success;
    }
    #endregion

    #region Redis操作
    /// <summary>
    /// 取Redis连接字串
    /// 依赖配置文件项："W3DBSecretConnStr": "UkdGMFlTQlRiM1Z5WTJVOU1Ua3lMakUyT0M0Mk1DNDRNanRKYm1sMGFXRnNJRU5oZEdGc2IyYzlSR0pUWldOeVpYUTdVR1Z5YzJsemRDQlRaV04xY21sMGVTQkpibVp2UFZSeWRXVTdWWE5sY2lCSlJEMXpaV055WlhSMWMyVnlPMUJoYzNOM2IzSmtQVFZ5UjJZMlNYVlFOa2t5YWpWdmFWRTFORFoyTlV3clpEVTJaVkkxYjNGQk5YQjVTalphYlZFMVdWZHpOVmtyTkE9PQ==",
    /// </summary>
    /// <returns>失败返回null</returns>
    public static string GetRedisConnStr()
    {
        string result = GetParameterValue("W3Redis");

        // 首次使用Redis，向DB取得
        if (string.IsNullOrWhiteSpace(result))
        {
            SQLDBHelper SA = new();
            string sql = "SELECT Account FROM tb_DbSecret WHERE ConnName='W3Redis';";
            try
            {
                object obj = SA.ExecuteScalar(sql, GetParameterValue("W3DBSecretConnStr"));
                if (obj != null)
                {
                    result = Base64DecodeString(Base64DecodeString(obj.ToString()!));
                    if (!string.IsNullOrWhiteSpace(result))
                    {
                        // 成功取到值时，写本地缓存
                        SetParameterValue("W3Redis", result!.Trim());
                    }
                }
            }
            catch (Exception ex)
            {
                MyLogger.Error("InformationPlatform.Utils", $"GetRedisConnStr()执行失败", ex);
            }
        }
        return result;
    }

    /// <summary>
    /// 读Redis键值
    /// </summary>
    /// <param name="key">键</param>
    /// <param name="value"></param>
    /// <returns>true:成功/false:失败</returns>
    public static bool RedisStringGet(string key, out string value)
    {
        ConnectionMultiplexer RedisConn = null;
        IDatabase redis = null;
        bool success = false;
        string redisConnStr = GetRedisConnStr();
        value = "";

        // 连接Redis
        try
        {
            RedisConn = ConnectionMultiplexer.Connect(redisConnStr!);
            redis = RedisConn.GetDatabase();
            RedisValue v = redis.StringGetAsync(key).Result;
            if (v.HasValue)
            {
                value = v.ToString();
                success = true;
            }
            RedisConn?.Close();
        }
        catch (Exception ex)
        {
            MyLogger.Error("InformationPlatform.Utils", $"RedisStringGet('{key}')，Redis访问失败", ex);
        }
        return success;
    }

    /// <summary>
    /// 写Redis字串(简单值)
    /// </summary>
    /// <param name="key">键</param>
    /// <param name="value">值</param>
    /// <param name="expire">过期时间(秒)</param>
    /// <returns>true:成功/false:失败</returns>
    public static bool RedisStringSet(string key, string value, int expire = 0)
    {
        ConnectionMultiplexer RedisConn = null;
        IDatabase redis = null;
        bool success = false;
        string redisConnStr = GetRedisConnStr();

        // 连接Redis
        try
        {
            RedisConn = ConnectionMultiplexer.Connect(redisConnStr!);
            redis = RedisConn.GetDatabase();
            redis.StringSet(key, value);
            // 设置过期时间
            if (expire > 0) redis.StringGetSetExpiry(key, new TimeSpan(0, 0, expire));
            success = true;
            RedisConn?.Close();
        }
        catch (Exception ex)
        {
            MyLogger.Error("InformationPlatform.Utils", $"RedisStringSet('{key}':'{value}')，Redis访问失败", ex);
        }
        return success;
    }

    /// <summary>
    /// 写Redis字串(单key，多键值对)
    /// </summary>
    /// <param name="key">键</param>
    /// <param name="jvalue">JsonObject多键值对</param>
    /// <param name="expire">过期时间(秒)</param>
    /// <returns>true:成功/false:失败</returns>
    public static bool RedisStringSet(string key, JObject jvalue, int expire = 0)
    {
        bool success = false;
        try
        {
            if (jvalue.Count > 0)
            {
                string value = JsonConvert.SerializeObject(jvalue);
                success = RedisStringSet(key, value, expire);
                success = true;
            }
        }
        catch (Exception ex)
        {
            MyLogger.Error("InformationPlatform.Utils", $"RedisStringSet('{key}':'{jvalue.ToString()}')，Redis访问失败", ex);
        }
        return success;
    }

    /// <summary>
    /// 写Redis字串(单key多值串接，默认用逗号间隔)
    /// </summary>
    /// <param name="key">键</param>
    /// <param name="arr">字串List或array</param>
    /// <param name="expire">过期时间(秒)</param>
    /// <param name="separator">间隔符</param>
    /// <returns>true:成功/false:失败</returns>
    public static bool RedisStringSet(string key, IEnumerable<string> arr, int expire = 0, string separator = ",")
    {
        bool success = false;
        string value = string.Empty;
        try
        {
            if (arr.Count() > 0)
            {
                value = string.Join(separator, arr);
                success = RedisStringSet(key, value, expire);
            }
        }
        catch (Exception ex)
        {
            MyLogger.Error("InformationPlatform.Utils", $"RedisStringSet('{key}':'{value}')，Redis访问失败", ex);
        }
        return success;
    }

    /// <summary>
    /// 移除存在的RedisKey
    /// </summary>
    /// <param name="key">键</param>
    /// <returns>true:成功/false:失败</returns>
    public static bool RedisKeyRemove(string key)
    {
        ConnectionMultiplexer RedisConn = null;
        IDatabase redis = null;
        bool success = false;
        string redisConnStr = GetRedisConnStr();
        // 连接Redis
        try
        {
            RedisConn = ConnectionMultiplexer.Connect(redisConnStr!);
            redis = RedisConn.GetDatabase();
            if (redis.KeyExists(key))
            {
                if (redis.KeyDelete(key))
                {
                    success = true;
                }
            }
            RedisConn?.Close();
        }
        catch (Exception ex)
        {
            MyLogger.Error("InformationPlatform.Utils", $"RedisKeyRemove('{key}')，RedisKey删除失败", ex);
        }
        return success;
    }

    /// <summary>
    /// WebApi返回结果构造
    /// </summary>
    struct HttpResponseInfo
    {
        public bool success { get; set; }
        public string message { get; set; }
        public object data { get; set; }
    }
    #endregion
}
