using Microsoft.AspNetCore.Mvc;
using ClampPreparation.Dto;

namespace ClampPreparation.Services
{
    public interface IScheduleService
    {
        /// <summary>
        /// 查询排程信息
        /// </summary>
        /// <returns></returns>
        List<ScheduleDto> GetSchedules(string plantDbName = "W6ctidb_main", string corrugatorId = "1");

    }
}
