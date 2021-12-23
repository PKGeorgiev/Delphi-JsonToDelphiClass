using Json2Delphi.Web.Extensions;
using Json2Delphi.Web.Models;
using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;
using System;
using System.Runtime.InteropServices;

namespace Json2Delphi.Web.Controllers
{
    public class ConvertController : Controller
    {
        [HttpGet]
        [Route("~/")]
        public IActionResult Index() => View(new MainViewModel());


        [DllImport("GeneratorLIB.dll", CharSet = CharSet.Unicode)]
        private static extern bool GenerateUnit(
                    [MarshalAs(UnmanagedType.BStr)] string Settings,
                    [MarshalAs(UnmanagedType.BStr)] string JSON,
                    [MarshalAs(UnmanagedType.BStr)] out string errstr
                  );

        [HttpPost]
        [Route("Convert/Delphi")]
        public IActionResult ConvertDelphi([FromForm] MainViewModel model)
        {
            var settings = JsonConvert.SerializeObject(new Settings(model));
            var json = model.SourceText;
            var delphiUnit = string.Empty;
            try
            {
                GenerateUnit(settings, json, out delphiUnit);
                model.ResultText = delphiUnit;

            }
            catch (System.Exception e)
            {


                model.ResultText = $"Exception: { e.GetType().Name}.{Environment.NewLine} Message {e.Message} ";
            }

            ViewData.SetResultText(model.ResultText);
            return View("Index", model);
        }
    }
}