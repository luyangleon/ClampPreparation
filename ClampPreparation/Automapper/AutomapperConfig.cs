using System.Data;
using AutoMapper;
using ClampPreparation.Dto;
using ClampPreparation.Models;

namespace ClampPreparation.Automapper
{
    public class AutomapperConfig : Profile
    {
        public AutomapperConfig() {
            CreateMap<DataRow, ScheduleDto>()
                .ForMember(dest => dest.SchDay, o => o.MapFrom(s => s["SchDay"] is DBNull ? null : s["SchDay"]))
                .ForMember(dest => dest.SchHour, o => o.MapFrom(s => s["SchHour"] is DBNull ? null : s["SchHour"]))
                .ForMember(dest => dest.SchMinute,
                    o => o.MapFrom(s => s["SchMinute"] is DBNull ? null : s["SchMinute"]))
                .ForMember(dest => dest.SchSecond,
                    o => o.MapFrom(s => s["SchSecond"] is DBNull ? null : s["SchSecond"]))
                .ForMember(dest => dest.GradeIDMount,
                    o => o.MapFrom(s => s["GradeIDMount"] is DBNull ? null : s["GradeIDMount"]))
                .ForMember(dest => dest.Width, o => o.MapFrom(s => s["Width"] is DBNull ? null : s["Width"]))
                .ForMember(dest => dest.DBGrade, o => o.MapFrom(s => s["DBGrade"] is DBNull ? null : s["DBGrade"]))
                .ForMember(dest => dest.DBLen, o => o.MapFrom(s => s["DBLen"] is DBNull ? null : s["DBLen"]))
                .ForMember(dest => dest.BMGrade, o => o.MapFrom(s => s["BMGrade"] is DBNull ? null : s["BMGrade"]))
                .ForMember(dest => dest.BMLen, o => o.MapFrom(s => s["BMLen"] is DBNull ? null : s["BMLen"]))
                .ForMember(dest => dest.BLGrade, o => o.MapFrom(s => s["BLGrade"] is DBNull ? null : s["BLGrade"]))
                .ForMember(dest => dest.BLLen, o => o.MapFrom(s => s["BLLen"] is DBNull ? null : s["BLLen"]))
                .ForMember(dest => dest.AMGrade, o => o.MapFrom(s => s["AMGrade"] is DBNull ? null : s["AMGrade"]))
                .ForMember(dest => dest.AMLen, o => o.MapFrom(s => s["AMLen"] is DBNull ? null : s["AMLen"]))
                .ForMember(dest => dest.ALGrade, o => o.MapFrom(s => s["ALGrade"] is DBNull ? null : s["ALGrade"]))
                .ForMember(dest => dest.ALLen, o => o.MapFrom(s => s["ALLen"] is DBNull ? null : s["ALLen"]))

                ;
        }
    }
}
