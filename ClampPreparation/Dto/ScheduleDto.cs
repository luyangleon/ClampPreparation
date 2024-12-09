using System.ComponentModel.DataAnnotations;

namespace ClampPreparation.Dto
{
    public class ScheduleDto
    {
        /// <summary>
        /// 日期
        /// </summary>
        public int? SchDay { get; set; }
        /// <summary>
        /// 小时
        /// </summary>
        public int? SchHour { get; set; }
        /// <summary>
        /// 材质
        /// </summary>
        public string? GradeIDMount { get; set; }
        /// <summary>
        /// 门幅
        /// </summary>
        public double? Width { get; set; }
        /// <summary>
        /// 贴合材质
        /// </summary>
        public string? DBGrade { get; set; }
        /// <summary>
        /// 贴合长度
        /// </summary>
        public string? DBLen { get; set; }
        /// <summary>
        /// B芯材质
        /// </summary>
        public string? BMGrade { get; set; }
        /// <summary>
        /// B芯长度
        /// </summary>
        public string? BMLen { get; set; }
        /// <summary>
        /// B面材质
        /// </summary>
        public string? BLGrade { get; set; }
        /// <summary>
        /// B面长度
        /// </summary>
        public string? BLLen { get; set; }
        /// <summary>
        /// A芯材质
        /// </summary>
        public string? AMGrade { get; set; }
        /// <summary>
        /// A芯长度
        /// </summary>
        public string? AMLen { get; set; }
        /// <summary>
        /// A面材质
        /// </summary>
        public string? ALGrade { get; set; }
        /// <summary>
        /// A面长度
        /// </summary>
        public string? ALLen { get; set; }

        /// <summary>
        /// C芯材质
        /// </summary>
        public string? CMGrade { get; set; }
        /// <summary>
        /// C芯长度
        /// </summary>
        public string? CMLen { get; set; }
        /// <summary>
        /// C面材质
        /// </summary>
        public string? CLGrade { get; set; }
        /// <summary>
        /// C面长度
        /// </summary>
        public string? CLLen { get; set; }

        #region 扩展字段



        #endregion
    }
}
