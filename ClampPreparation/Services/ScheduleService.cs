using Microsoft.AspNetCore.Mvc;
using System.Data;
using System.Text;
using Newtonsoft.Json;
using ClampPreparation.Models;
using System;
using System.Data.SqlClient;
using AutoMapper;
using ClampPreparation.Dto;
using static DevExpress.XtraPrinting.Native.PageSizeInfo;
using static DevExpress.Xpo.Helpers.AssociatedCollectionCriteriaHelper;

namespace ClampPreparation.Services
{
    public class ScheduleService : IScheduleService
    {

        public ScheduleService()
        {
        }

        #region 废弃 TODO

        /// <summary>
        /// 查询排程信息
        /// </summary>
        /// <returns></returns>
        public List<ScheduleDto> GetSchedules_old(string corrugatorId = "1")
        {
            List<ScheduleDto> dtoList = new();
            StringBuilder sbSql = new();
            try
            {
                SQLHelper sqlConn = new();

                #region 查询SQL server中的盘点数据

                #region 原SQL TODO

                sbSql.Append(
$"""
DECLARE @LINE varchar(1),@AFluteRate float,@BFluteRate float,@CFluteRate float,@EFluteRate float
SELECT @LINE ='{corrugatorId}';
select @AFluteRate={Utils.GetParameterValue("AFluteRate")};
select @BFluteRate={Utils.GetParameterValue("BFluteRate")};
select @CFluteRate={Utils.GetParameterValue("CFluteRate")};
select @EFluteRate={Utils.GetParameterValue("EFluteRate")};

SELECT 
	SchDT,
    SchDay,
    SchHour,
    GradeIDMount,
    Width,
    MAX(CASE WHEN RollStand = '贴合' THEN Desc1 ELSE '' END) AS DBGrade,
    MAX(CASE WHEN RollStand = '贴合' THEN PaperLen ELSE 0 END) AS DBLen,
    MAX(CASE WHEN RollStand = 'B面' THEN Desc1 ELSE '' END) AS BLGrade,
    MAX(CASE WHEN RollStand = 'B芯' THEN PaperLen ELSE 0 END) AS BMLen,
    MAX(CASE WHEN RollStand = 'B芯' THEN Desc1 ELSE '' END) AS BMGrade,
    MAX(CASE WHEN RollStand = 'B面' THEN PaperLen ELSE 0 END) AS BLLen,
    MAX(CASE WHEN RollStand = 'A面' THEN Desc1 ELSE '' END) AS ALGrade,
    MAX(CASE WHEN RollStand = 'A芯' THEN PaperLen ELSE 0 END) AS AMLen,
    MAX(CASE WHEN RollStand = 'A芯' THEN Desc1 ELSE '' END) AS AMGrade,
    MAX(CASE WHEN RollStand = 'A面' THEN PaperLen ELSE 0 END) AS ALLen
FROM (
    -- 以下为协理抓取的数据来源
    SELECT 
        RollStand,GradeIDMount,SchDT,SchDay,SchHour,Width,Pid1,Desc1,ROUND(SUM(clineal), 0) AS PaperLen
    FROM (
        SELECT '贴合' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, a.clineal, a.width AS Width,
			DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
			DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
			DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
			ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=0 and a.RollStandIndex=1
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
        UNION ALL 

      		SELECT 'A芯' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, 
         		case 
               		when c.Flute='A' then a.clineal * @AFluteRate
               		when c.Flute='B' then a.clineal * @BFluteRate 
               		when c.Flute='C' then a.clineal * @CFluteRate 
               		when c.Flute='AB' then a.clineal * @AFluteRate 
               		when c.Flute='BA' then a.clineal * @AFluteRate 
               		when c.Flute='CB' then a.clineal * @CFluteRate 
               		when c.Flute='BC' then a.clineal * @CFluteRate 
               		when c.Flute='BE' then a.clineal * @BFluteRate 
               		when c.Flute='EB' then a.clineal * @BFluteRate 
               		else a.clineal end as clineal,
         		a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=1
                     							    and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.RollStandIndex=d.AreaLink
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
        UNION ALL 
      		SELECT 'A面' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, a.clineal, a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=2
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
        UNION ALL 
      		SELECT 'B芯' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, 
         		case 
               			when c.Flute='B' then a.clineal * @BFluteRate 
               			when c.Flute='E' then a.clineal * @EFluteRate 
               			when c.Flute='AB' then a.clineal * @BFluteRate 
               			when c.Flute='BA' then a.clineal * @BFluteRate 
               			when c.Flute='CB' then a.clineal * @BFluteRate 
               			when c.Flute='BC' then a.clineal * @BFluteRate 
               			when c.Flute='BE' then a.clineal * @EFluteRate 
               			when c.Flute='EB' then a.clineal * @EFluteRate 
               			else a.clineal end as clineal,
         		a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=1
                     							    and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.RollStandIndex=d.AreaLink
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
        UNION ALL 
      		SELECT 'B面' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, a.clineal, a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=2
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 

      		UNION ALL 
      		SELECT 'C芯' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, 
         		case 
               			when c.Flute='A' then a.clineal * @AFluteRate
               			when c.Flute='B' then a.clineal * @BFluteRate 
               			when c.Flute='C' then a.clineal * @CFluteRate 
               			when c.Flute='E' then a.clineal * @EFluteRate 
               			when c.Flute='AB' then a.clineal * @BFluteRate 
               			when c.Flute='BA' then a.clineal * @BFluteRate 
               			when c.Flute='CB' then a.clineal * @BFluteRate 
               			when c.Flute='BC' then a.clineal * @BFluteRate 
               			when c.Flute='BE' then a.clineal * @EFluteRate 
               			when c.Flute='EB' then a.clineal * @EFluteRate 
               			else a.clineal end as clineal,
         		a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=1
                     							    and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.RollStandIndex=d.AreaLink
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
        UNION ALL 
      		SELECT 'C面' AS RollStand, LEFT(a.RAOCriteria, LEN(c.GradeID)) AS GradeIDMount, a.clineal, a.width AS Width,
				DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME)) AS SchDT,
				DATEPART(DAY, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchDay,
				DATEPART(HOUR, DATEADD(DAY, -2, CAST(b.EstimatedStopTime AS DATETIME))) AS SchHour,
				ISNULL(d.PaperGradeID, '') AS PID1, ISNULL(d.Description, '') AS Desc1
        from combination a  with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                   left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
            				   left join (select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description
                              from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
                     							  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=2
                     							  ) d on a.combinationLink=d.ParentOpLink
where a.cbmachinelink=@LINE and a.cblineupindex >= (select min(cblineupindex) from combination  with (nolock) where cbmachinelink=@LINE and cbstate <=50) 
    ) x
    WHERE Pid1 <> ''  -- 除去无效记录
    GROUP BY RollStand, GradeIDMount,SchDT, SchDay, SchHour, Width, Pid1, Desc1
) src
GROUP BY SchDT,SchDay, SchHour, GradeIDMount, Width
ORDER BY SchDT,SchDay, SchHour, Width desc, GradeIDMount
""");
                #endregion

                DataTable dt = new();
                if (Utils.GetDbConnectionString(Utils.GetParameterValue("ePS_DBName"), out string connStr))
                {
                    dt = sqlConn.GetDataTable(sbSql.ToString(), connStr);
                }
                #region DataTable 格式化
                DataTable dtFormat = new();
                string[] colNames =
                [
                    "SchDT", "SchDay", "SchHour", "GradeIDMount", "Width", "DBGrade", "DBLen", "BMGrade", "BMLen",
                    "BLGrade", "BLLen", "AMGrade", "AMLen", "ALGrade", "ALLen"
                ];
                foreach (var colStr in colNames)
                {
                    dtFormat.Columns.Add(colStr, typeof(string));
                }
                // 遍历赋值到格式化的DataTable
                foreach (DataRow dr in dt.Rows)
                {
                    DataRow drFormat = dtFormat.NewRow();
                    foreach (var colName in colNames)
                    {
                        drFormat[colName] = dr[colName];
                    }
                    dtFormat.Rows.Add(drFormat);
                }
                #endregion
                dtFormat = MergeDataTable(dtFormat);
                var jsonStr = JsonConvert.SerializeObject(dtFormat);
                dtoList = JsonConvert.DeserializeObject<List<ScheduleDto>>(jsonStr) ?? new List<ScheduleDto>();
                #endregion

                // 产生Json字符串并返回
                Utils.AppendToFile(Utils.GetParameterValue("SysLogFile"), "ScheduleService.GetSchedules()调用完成", true);
            }
            catch (Exception ex)
            {
                Utils.AppendToFile(Utils.GetParameterValue("SysLogFile"), $"ScheduleService.GetSchedules()执行失败\nSQL：{sbSql}，{ex}", true);
            }

            return dtoList;
        }
        private DataTable MergeDataTable_old(DataTable dt)
        {
            DataTable resDt = dt.Clone();
            foreach (var colStr in new[] { "DBLen", "BMLen", "BLLen", "AMLen", "ALLen" })
            {
                resDt.Columns[colStr].DataType = typeof(string);
            }
            double? widthCur, widthPre;
            int startDBIdx = 0, startBMIdx = 0, startBLIdx = 0, startAMIdx = 0, startALIdx = 0;

            for (int i = 0; i < dt.Rows.Count; i++)
            {
                widthCur = Convert.ToDouble(dt.Rows[i]["Width"]);
                widthPre = i > 0 ? Convert.ToDouble(dt.Rows[i - 1]["Width"]) : null;
                // 添加新行
                DataRow resRow = resDt.NewRow();
                foreach (var column in new[] { "SchDay", "SchHour", "GradeIDMount", "Width", "DBGrade", "BMGrade", "BLGrade", "AMGrade", "ALGrade" })
                {
                    resRow[column] = dt.Rows[i][column];
                }
                foreach (var column in new[] { "DBLen", "BMLen", "BLLen", "AMLen", "ALLen" })
                {
                    resRow[column] = dt.Rows[i][column]?.ToString();
                }
                // 判断是否相同的门幅
                if (widthCur != widthPre)
                {
                    // 门幅不同，需要重新计算
                    startDBIdx = startBMIdx = startBLIdx = startAMIdx = startALIdx = i;
                }
                else
                { // 门幅相同，需要累加
                    if (resRow["DBGrade"].ToString() == resDt.Rows[startDBIdx]["DBGrade"].ToString())
                    {
                        // 对比贴合机上一个材配，纸种相同就累加
                        resDt.Rows[startDBIdx]["DBLen"] = Convert.ToInt32(resDt.Rows[startDBIdx]["DBLen"]) +
                                                          Convert.ToInt32(dt.Rows[i]["DBLen"]);
                        resRow["DBLen"] = "-";
                    }
                    else
                    {
                        // 对比贴合机上一个材配，纸种不同
                        startDBIdx = i;
                    }
                    // 对比B芯上一个材配
                    if (resRow["BMGrade"].ToString() == resDt.Rows[startBMIdx]["BMGrade"].ToString())
                    {
                        resDt.Rows[startBMIdx]["BMLen"] = Convert.ToInt32(resDt.Rows[startBMIdx]["BMLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["BMLen"]);
                        resRow["BMLen"] = "-";
                    }
                    else
                    {
                        // 对比B芯上一个材配，纸种不同
                        startBMIdx = i;
                    }
                    // 对比B面上一个材配
                    if (resRow["BLGrade"].ToString() == resDt.Rows[startBLIdx]["BLGrade"].ToString())
                    {
                        resDt.Rows[startBLIdx]["BLLen"] = Convert.ToInt32(resDt.Rows[startBLIdx]["BLLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["BLLen"]);
                        resRow["BLLen"] = "-";
                    }
                    else
                    {
                        // 对比B面上一个材配，纸种不同
                        startBLIdx = i;
                    }
                    // 对比A芯上一个材配
                    if (resRow["AMGrade"].ToString() == resDt.Rows[startAMIdx]["AMGrade"].ToString())
                    {
                        resDt.Rows[startAMIdx]["AMLen"] = Convert.ToInt32(resDt.Rows[startAMIdx]["AMLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["AMLen"]);
                        resRow["AMLen"] = "-";
                    }
                    else
                    {
                        // 对比A芯上一个材配，纸种不同
                        startAMIdx = i;
                    }
                    // 对比A面上一个材配
                    if (resRow["ALGrade"].ToString() == resDt.Rows[startALIdx]["ALGrade"].ToString())
                    {
                        resDt.Rows[startALIdx]["ALLen"] = Convert.ToInt32(resDt.Rows[startALIdx]["ALLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["ALLen"]);
                        resRow["ALLen"] = "-";
                    }
                    else
                    {
                        // 对比A面上一个材配，纸种不同
                        startALIdx = i;
                    }

                    //UpdateOrResetIndex(resDt, resRow, "DBGrade", "DBLen", ref startDBIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "BMGrade", "BMLen", ref startBMIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "BLGrade", "BLLen", ref startBLIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "AMGrade", "AMLen", ref startAMIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "ALGrade", "ALLen", ref startALIdx, i, dt);
                }

                resDt.Rows.Add(resRow);
            }

            return resDt;
        }

        private DataTable MergeDataTable(DataTable dt)
        {
            DataTable resDt = dt.Clone();
            foreach (var colStr in new[] { "DBLen", "BMLen", "BLLen", "AMLen", "ALLen", "CMLen", "CLLen" })
            {
                resDt.Columns[colStr].DataType = typeof(string);
            }
            double? widthCur, widthPre;
            int startDBIdx = 0, startBMIdx = 0, startBLIdx = 0, startAMIdx = 0, startALIdx = 0, startCMIdx = 0, startCLIdx = 0;
            int acumulateDBLen = 0, acumulateBMLen = 0, acumulateBLLen = 0, acumulateAMLen = 0, acumulateALLen = 0, acumulateCMLen = 0, acumulateCLLen = 0;

            for (int i = 0; i < dt.Rows.Count; i++)
            {
                widthCur = Convert.ToDouble(dt.Rows[i]["Width"]);
                widthPre = i > 0 ? Convert.ToDouble(dt.Rows[i - 1]["Width"]) : null;
                // 添加新行
                DataRow resRow = resDt.NewRow();
                foreach (var column in new[] { "SchDay", "SchHour", "GradeIDMount", "Width", "DBGrade", "BMGrade", "BLGrade", "AMGrade", "ALGrade", "CMGrade", "CLGrade" })
                {
                    resRow[column] = dt.Rows[i][column];
                }
                foreach (var column in new[] { "DBLen", "BMLen", "BLLen", "AMLen", "ALLen", "CMLen", "CLLen" })
                {
                    resRow[column] = dt.Rows[i][column]?.ToString();
                }
                // 判断是否相同的门幅
                if (widthCur != widthPre)
                {
                    // 门幅不同，需要重新计算
                    startDBIdx = startBMIdx = startBLIdx = startAMIdx = startALIdx = startCMIdx = startCLIdx = i;
                    acumulateDBLen = Convert.ToInt32(dt.Rows[i]["DBLen"]);
                    acumulateBMLen = Convert.ToInt32(dt.Rows[i]["BMLen"]);
                    acumulateBLLen = Convert.ToInt32(dt.Rows[i]["BLLen"]);
                    acumulateAMLen = Convert.ToInt32(dt.Rows[i]["AMLen"]);
                    acumulateALLen = Convert.ToInt32(dt.Rows[i]["ALLen"]);
                    acumulateCMLen = Convert.ToInt32(dt.Rows[i]["CMLen"]);
                    acumulateCLLen = Convert.ToInt32(dt.Rows[i]["CLLen"]);
                }
                else
                {
                    // 门幅相同的前提下，材质相同，小时不相同，累加第一个
                    if (resRow["DBGrade"].ToString() == resDt.Rows[startDBIdx]["DBGrade"].ToString() &&
                        resRow["SchHour"].ToString() != resDt.Rows[startDBIdx]["SchHour"].ToString())
                    {
                        // 对比贴合机上一个材配，纸种相同就累加
                        resDt.Rows[startDBIdx]["DBLen"] = Convert.ToInt32(resDt.Rows[startDBIdx]["DBLen"]) + Convert.ToInt32(dt.Rows[i]["DBLen"]);
                        resRow["DBLen"] = "-";
                        startDBIdx = i;
                    }
                    else if (resRow["DBGrade"].ToString() == resDt.Rows[startDBIdx]["DBGrade"].ToString() &&
                             resRow["SchHour"].ToString() == resDt.Rows[startDBIdx]["SchHour"].ToString())
                    {
                        resRow["DBLen"] = "-";
                    }
                    else
                    {
                        // 对比贴合机上一个材配，纸种不同
                        startDBIdx = i;
                    }

                    // 对比B芯上一个材配
                    if (resRow["BMGrade"].ToString() == resDt.Rows[startBMIdx]["BMGrade"].ToString())
                    {
                        resDt.Rows[startBMIdx]["BMLen"] = Convert.ToInt32(resDt.Rows[startBMIdx]["BMLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["BMLen"]);
                        resRow["BMLen"] = "-";
                    }
                    else
                    {
                        // 对比B芯上一个材配，纸种不同
                        startBMIdx = i;
                    }
                    // 对比B面上一个材配
                    if (resRow["BLGrade"].ToString() == resDt.Rows[startBLIdx]["BLGrade"].ToString())
                    {
                        resDt.Rows[startBLIdx]["BLLen"] = Convert.ToInt32(resDt.Rows[startBLIdx]["BLLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["BLLen"]);
                        resRow["BLLen"] = "-";
                    }
                    else
                    {
                        // 对比B面上一个材配，纸种不同
                        startBLIdx = i;
                    }
                    // 对比A芯上一个材配
                    if (resRow["AMGrade"].ToString() == resDt.Rows[startAMIdx]["AMGrade"].ToString())
                    {
                        resDt.Rows[startAMIdx]["AMLen"] = Convert.ToInt32(resDt.Rows[startAMIdx]["AMLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["AMLen"]);
                        resRow["AMLen"] = "-";
                    }
                    else
                    {
                        // 对比A芯上一个材配，纸种不同
                        startAMIdx = i;
                    }
                    // 对比A面上一个材配
                    if (resRow["ALGrade"].ToString() == resDt.Rows[startALIdx]["ALGrade"].ToString())
                    {
                        resDt.Rows[startALIdx]["ALLen"] = Convert.ToInt32(resDt.Rows[startALIdx]["ALLen"]) +
                                                        Convert.ToInt32(dt.Rows[i]["ALLen"]);
                        resRow["ALLen"] = "-";
                    }
                    else
                    {
                        // 对比A面上一个材配，纸种不同
                        startALIdx = i;
                    }

                    //UpdateOrResetIndex(resDt, resRow, "DBGrade", "DBLen", ref startDBIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "BMGrade", "BMLen", ref startBMIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "BLGrade", "BLLen", ref startBLIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "AMGrade", "AMLen", ref startAMIdx, i, dt);
                    //UpdateOrResetIndex(resDt, resRow, "ALGrade", "ALLen", ref startALIdx, i, dt);
                }

                resDt.Rows.Add(resRow);
            }

            return resDt;
        }

        private void UpdateOrResetIndex(DataTable resDt, DataRow resRow, string columnGrade, string columnLen, ref int startIdx, int currentIdx, DataTable dt)
        {
            if (resRow[columnGrade].ToString() == resDt.Rows[startIdx][columnGrade].ToString())
            {
                resDt.Rows[startIdx][columnLen] = Convert.ToInt32(resDt.Rows[startIdx][columnLen]) +
                                                  Convert.ToInt32(dt.Rows[currentIdx][columnLen]);
                resRow[columnLen] = "-";
            }
            else
            {
                startIdx = currentIdx;
            }
        }

        /// <summary>
        /// 调整排程列表
        /// </summary>
        /// <param name="scheduleList"></param>
        /// <returns></returns>
        private List<ScheduleDto> AdjuestScheduleList(List<ScheduleDto> scheduleList)
        {
            ScheduleDto acumulateFirst = scheduleList[0];

            for (int i = 1; i < scheduleList.Count; i++) // 从第二个对象开始遍历
            {
                var current = scheduleList[i];
                //var previous = scheduleList[i - 1];

                if (current.Width != acumulateFirst.Width)
                {
                    acumulateFirst = current;
                }
                // 判断 Width 是否相同
                if (current.Width == acumulateFirst.Width)
                {
                    // 比较 贴合
                    if (current.DBGrade == acumulateFirst.DBGrade)
                    {
                        // Width和DBGrade都相同，但SchHour不同，需要相加
                        if (current.SchHour != acumulateFirst.SchHour)
                        {
                            acumulateFirst.DBLen = (Convert.ToInt32(acumulateFirst.DBLen) + Convert.ToInt32(current.DBLen)).ToString();
                        }
                        else
                        {
                            current.DBLen = "-"; // 替换 DBLen
                        }
                    }
                    // 比较 B芯
                    if (current.BMGrade == acumulateFirst.BMGrade)
                    {
                        // Width和DBGrade都相同，但SchHour不同，需要相加
                        if (current.SchHour != acumulateFirst.SchHour)
                        {
                            acumulateFirst.BMLen = (Convert.ToInt32(acumulateFirst.BMLen) + Convert.ToInt32(current.BMLen)).ToString();
                        }
                        else
                        {
                            current.BMLen = "-"; // 替换 BMLen
                        }
                    }
                    // 比较 B面
                    if (current.BLGrade == acumulateFirst.BLGrade)
                    {
                        // Width和DBGrade都相同，但SchHour不同，需要相加
                        if (current.SchHour != acumulateFirst.SchHour)
                        {
                            acumulateFirst.BLLen = (Convert.ToInt32(acumulateFirst.BLLen) + Convert.ToInt32(current.BLLen)).ToString();
                        }
                        else
                        {
                            current.BLLen = "-"; // 替换 BLLen
                        }
                    }
                    // 比较 A芯
                    if (current.AMGrade == acumulateFirst.AMGrade)
                    {
                        // Width和DBGrade都相同，但SchHour不同，需要相加
                        if (current.SchHour != acumulateFirst.SchHour)
                        {
                            acumulateFirst.AMLen = (Convert.ToInt32(acumulateFirst.AMLen) + Convert.ToInt32(current.AMLen)).ToString();
                        }
                        else
                        {
                            current.AMLen = "-"; // 替换 AMLen
                        }
                    }
                    // 比较 A面
                    if (current.ALGrade == acumulateFirst.ALGrade)
                    {
                        // Width和DBGrade都相同，但SchHour不同，需要相加
                        if (current.SchHour != acumulateFirst.SchHour)
                        {
                            acumulateFirst.ALLen = (Convert.ToInt32(acumulateFirst.ALLen) + Convert.ToInt32(current.ALLen)).ToString();
                        }
                        else
                        {
                            current.ALLen = "-"; // 替换 ALLen
                        }
                    }
                    // TODO C芯
                    // TODO C面
                }
            }
            return scheduleList;
        }

        #endregion

        /// <summary>
        /// 查询排程信息
        /// </summary>
        /// <returns></returns>
        public List<ScheduleDto> GetSchedules(string plantDbName = "W6ctidb_main", string corrugatorId = "1")
        {
            List<ScheduleDto> dtoList = new();
            StringBuilder sbSql = new();
            try
            {
                SQLHelper sqlConn = new();

                #region 查询SQL server中的盘点数据

                sbSql.Append(
$"""
select left(a.RAOCriteria,len(c.GradeID)) GradeIDMount,
a.clineal,a.width
,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)) SchDT
,isnull(d.RollStand,'') DB,isnull(d.SchDay,'') SchDay,isnull(d.SchHour,'') SchHour,isnull(d.Width,'') Width,isnull(d.Pid1,'') Pid1,isnull(d.Desc1,'') DBGrade,isnull(d.PaperLen,'') DBLen
,isnull(e.RollStand,'') BM,isnull(e.Pid1,'') Pid1,isnull(e.Desc1,'') BMGrade,isnull(e.PaperLen,'') BMLen
,isnull(f.RollStand,'') BL,isnull(f.Pid1,'') Pid1,isnull(f.Desc1,'') BLGrade,isnull(f.PaperLen,'') BLLen
,isnull(g.RollStand,'') AM,isnull(g.Pid1,'') Pid1,isnull(g.Desc1,'') AMGrade,isnull(g.PaperLen,'') AMLen
,isnull(h.RollStand,'') AL,isnull(h.Pid1,'') Pid1,isnull(h.Desc1,'') ALGrade,isnull(h.PaperLen,'') ALLen
,isnull(i.RollStand,'') CM,isnull(i.Pid1,'') Pid1,isnull(i.Desc1,'') CMGrade,isnull(i.PaperLen,'') CMLen
,isnull(j.RollStand,'') CL,isnull(j.Pid1,'') Pid1,isnull(j.Desc1,'') CLGrade,isnull(j.PaperLen,'') CLLen
from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
                      left join Grade c with (nolock) on a.GradeLinkUploadedToHost=c.GradeLink
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=0 and a.RollStandIndex=1
								) d1 on a.combinationLink=d1.ParentOpLink 
					  left join (
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from ( 
								select '貼合' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
								,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
								from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
																 left join (  
																		   select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,a.PaperLink
																			 from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
																			where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=0 and a.RollStandIndex=1
																) c on a.CombinationLink=c.ParentOpLink
								where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
														) d on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=d.SchDay
														   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=d.SchHour
														   and d1.PaperLink=d.PaperLink
														   and a.width=d.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=2
								) e1 on a.combinationLink=e1.ParentOpLink 
					  left join (
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from (
							select 'B芯' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
							,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal*c.TakeupFactor clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
							from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
												  left join (
															 select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor,a.PaperLink
															   from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
															  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=2
																and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.UsageType=1
															) c on a.CombinationLink=c.ParentOpLink
							where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
							) e on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=e.SchDay
							   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=e.SchHour
                               and e1.PaperLink=e.PaperLink
							   and a.width=e.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=1
								) f1 on a.combinationLink=f1.ParentOpLink 
					  left join (
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from ( 
								select 'B面' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
								,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
								from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
																 left join (  
																		   select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,a.PaperLink
																			 from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
																			where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=1 and a.RollStandIndex=1
																) c on a.CombinationLink=c.ParentOpLink
								where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
														) f on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=f.SchDay
														   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=f.SchHour
														   and f1.PaperLink=f.PaperLink
														   and a.width=f.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=2
								) g1 on a.combinationLink=g1.ParentOpLink 
					  left join (
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from (
							select 'A芯' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
							,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal*c.TakeupFactor clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
							from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
												  left join (
															 select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor,a.PaperLink
															   from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
															  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=2
																and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.UsageType=1
															) c on a.CombinationLink=c.ParentOpLink
							where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
							) g on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=g.SchDay
							   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=g.SchHour
                               and g1.PaperLink=g.PaperLink
							   and a.width=g.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=1
								) h1 on a.combinationLink=h1.ParentOpLink 
					  left join (
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from ( 
								select 'A面' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
								,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
								from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
																 left join (  
																		   select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,a.PaperLink
																			 from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
																			where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=2 and a.RollStandIndex=1
																) c on a.CombinationLink=c.ParentOpLink
								where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
														) h on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=h.SchDay
														   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=h.SchHour
														   and h1.PaperLink=h.PaperLink
														   and a.width=h.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=2
								) i1 on a.combinationLink=i1.ParentOpLink 
					  left join (  --for 7 ply SG
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from (
							select 'C芯' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
							,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal*c.TakeupFactor clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
							from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
												  left join (
															 select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,d.TakeupFactor,a.PaperLink
															   from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock),Luweop d with (nolock) 
															  where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=2
																and c.BoardLink=d.BoardLink and a.WEStationIndex=d.WEStationIndex and a.UsageType=1
															) c on a.CombinationLink=c.ParentOpLink
							where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
							) i on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=i.SchDay
							   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=i.SchHour
                               and i1.PaperLink=i.PaperLink
							   and a.width=i.width
					  left join (select a.BoardLink,a.PaperLink,b.ParentOpLink 
								from LUPaper a with (nolock),LUBoard b with (nolock) 
								where a.BoardLink=b.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=1
								) j1 on a.combinationLink=j1.ParentOpLink 
					  left join (  --for 7 ply SG
							select RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink,round(sum(clineal),0) PaperLen
							from ( 
								select 'C面' RollStand,datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchDay
								,datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime))) SchHour,a.clineal,c.PaperWidth Width,c.PaperGradeID Pid1,c.Description Desc1,c.PaperLink
								from combination a with (nolock) left join [CombinationEstimatedTime] b with (nolock) on a.CombinationLink=b.CombinationLink
																 left join (  
																		   select c.ParentOpLink,a.BoardLink,a.WEStationIndex,a.RollStandIndex,a.PaperWidth,b.PaperGradeID,b.Description,a.PaperLink
																			 from LuPaper a with (nolock),PaperGrade b with (nolock),LuBoard c with (nolock) 
																			where a.PaperLink=b.PaperGradeLink and a.BoardLink=c.BoardLink and a.WEStationIndex=3 and a.RollStandIndex=1
																) c on a.CombinationLink=c.ParentOpLink
								where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
							) x
							group by RollStand,SchDay,SchHour,Width,Pid1,Desc1,PaperLink
														) j on datepart(day,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=j.SchDay
														   and datepart(hour,dateadd(day,-2,cast(b.EstimatedStopTime as datetime)))=j.SchHour
														   and j1.PaperLink=j.PaperLink
														   and a.width=j.width

where a.cbmachinelink='{corrugatorId}' and a.cblineupindex >= (select min(cblineupindex) from combination with (nolock) where cbmachinelink='{corrugatorId}' and cbstate <=50) 
order by a.cblineupindex

""");
                DataTable dt = new();
                //if (Utils.GetDbConnectionString(Utils.GetParameterValue("ePS_DBName"), out string connStr))
                if (Utils.GetDbConnectionString(plantDbName, out string connStr))
                {
                    dt = sqlConn.GetDataTable(sbSql.ToString(), connStr);
                }
                #endregion

                #region DataTable 格式化
                DataTable dtFormat = new();
                string[] colNames =
                [
                    "SchDT", "SchDay", "SchHour", "GradeIDMount", "Width", "DBGrade", "DBLen", "BMGrade", "BMLen",
                    "BLGrade", "BLLen", "AMGrade", "AMLen", "ALGrade", "ALLen","CMGrade", "CMLen", "CLGrade", "CLLen"
                ];
                foreach (var colStr in colNames)
                {
                    dtFormat.Columns.Add(colStr, typeof(string));
                }
                // 遍历赋值到格式化的DataTable
                foreach (DataRow dr in dt.Rows)
                {
                    DataRow drFormat = dtFormat.NewRow();
                    foreach (var colName in colNames)
                    {
                        drFormat[colName] = dr[colName];
                    }
                    dtFormat.Rows.Add(drFormat);
                }
                #endregion

                var jsonStr = JsonConvert.SerializeObject(dtFormat);
                dtoList = JsonConvert.DeserializeObject<List<ScheduleDto>>(jsonStr) ?? new List<ScheduleDto>();

                #region 调整排程数值
                // DB
                var dbGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.DBGrade }).ToList();
                foreach (var dbGroup in dbGroupList)
                {
                    foreach (var dbDto in dbGroup.Skip(1))
                    {
                        dbDto.DBLen = "-";
                    }
                }
                // BM
                var bmGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.BMGrade }).ToList();
                foreach (var bmGroup in bmGroupList)
                {
                    foreach (var bmDto in bmGroup.Skip(1))
                    {
                        bmDto.BMLen = "-";
                    }
                }
                // BL
                var blGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.BLGrade}).ToList();
                foreach (var blGroup in blGroupList)
                {
                    foreach (var blDto in blGroup.Skip(1))
                    {
                        blDto.BLLen = "-";
                    }
                }
                // AM
                var amGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.AMGrade }).ToList();
                foreach (var amGroup in amGroupList)
                {
                    foreach (var amDto in amGroup.Skip(1))
                    {
                        amDto.AMLen = "-";
                    }
                }
                // AL
                var alGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.ALGrade }).ToList();
                foreach (var alGroup in alGroupList)
                {
                    foreach (var alDto in alGroup.Skip(1))
                    {
                        alDto.ALLen = "-";
                    }
                }
                // CM
                var cmGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.CMGrade }).ToList();
                foreach (var cmGroup in cmGroupList)
                {
                    foreach (var cmDto in cmGroup.Skip(1))
                    {
                        cmDto.CMLen = "-";
                    }
                }
                // CL
                var clGroupList = dtoList.GroupBy(g => new { g.SchDay, g.SchHour, g.Width, g.CLGrade }).ToList();
                foreach (var clGroup in clGroupList)
                {
                    foreach (var clDto in clGroup.Skip(1))
                    {
                        clDto.CLLen = "-";
                    }
                }
                #endregion

                // double转int
                //dtoList = dtoList.Select(dto =>
                //{
                //    dto.DBLen = ConvertValueToString(dto.DBLen);
                //    dto.BMLen = ConvertValueToString(dto.BMLen);
                //    dto.BLLen = ConvertValueToString(dto.BLLen);
                //    dto.AMLen = ConvertValueToString(dto.AMLen);
                //    dto.ALLen = ConvertValueToString(dto.ALLen);
                //    return dto;
                //}).ToList();

                // 产生Json字符串并返回
                Utils.AppendToFile(Utils.GetParameterValue("SysLogFile"), "ScheduleService.GetSchedules()调用完成", true);
            }
            catch (Exception ex)
            {
                Utils.AppendToFile(Utils.GetParameterValue("SysLogFile"), $"ScheduleService.GetSchedules()执行失败\nSQL：{sbSql}，{ex}", true);
            }

            return dtoList;
        }

        public static string ConvertValueToString(string? value)
        {
            try
            {
                int intValue = Convert.ToInt32(Convert.ToDecimal(value));
                return intValue.ToString();
            }
            catch (Exception ex)
            {
                Console.WriteLine($@"转换出现错误: {ex.Message}");
                return string.Empty;
            }
        }

    }
}
