using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ClampPreparation.Dto;
using ClampPreparation.Services;

namespace ClampPreparation.Controllers
{
    public class ScheduleController : Controller
    {
        private readonly ILogger<ScheduleController> _logger;
        private readonly IScheduleService _scheduleService;
        public ScheduleController(ILogger<ScheduleController> logger,
            IScheduleService scheduleService)
        {
            _logger = logger;
            _scheduleService = scheduleService;
        }

        public ActionResult Index()
        {
            return View();
        }

        public IActionResult GetSchedules(string plantDbName, string corrugatorId)
        {
            var res = _scheduleService.GetSchedules(plantDbName, corrugatorId);

            return Json(res);
        }

    }
}
