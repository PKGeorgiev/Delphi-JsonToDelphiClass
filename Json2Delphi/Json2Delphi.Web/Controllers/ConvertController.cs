using Json2Delphi.Web.Extensions;
using Json2Delphi.Web.Models;
using Microsoft.AspNetCore.Mvc;
using MimeKit;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.IO;
using System.Runtime.InteropServices;

namespace Json2Delphi.Web.Controllers
{
    public class ConvertController : Controller
    {
        private static bool TryIsValidJson(string strInput, out string errorMessage)
        {
            errorMessage = string.Empty;
            if (string.IsNullOrWhiteSpace(strInput))
            {
                errorMessage = "JSON input can not be empty";
                return false;
            }
            strInput = strInput.Trim();
            if ((strInput.StartsWith("{") && strInput.EndsWith("}")) || //For object
                (strInput.StartsWith("[") && strInput.EndsWith("]"))) //For array
            {
                try
                {
                    var obj = JToken.Parse(strInput);
                    return true;
                }
                catch (JsonReaderException jex)
                {
                    //Exception in parsing json
                    errorMessage = jex.Message;
                    return false;
                }
                catch (Exception ex) //some other exception
                {
                    errorMessage = ex.ToString();
                    return false;
                }
            }
            else
            {
                errorMessage = "JSON must start with \"{ or [\" and end with \"] or ]\" ";
                return false;
            }
        }


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

            if (TryIsValidJson(json, out delphiUnit))
                try
                {
                    GenerateUnit(settings, json, out delphiUnit);
                }
                catch (System.Exception e)
                {
                    delphiUnit = $"Exception: { e.GetType().Name}.{Environment.NewLine} Message {e.Message} ";
                }


            model.ResultText = delphiUnit;
            ViewData.SetResultText(model.ResultText);
            return View("Index", model);
        }

        [HttpGet]
        [Route("Convert/DownloadDTO")]
        public ActionResult DownloadDTO()
        {
            var attachment = "Pkg.Json.DTO.pas";
            var path = Path.Combine(Directory.GetCurrentDirectory(), attachment);
            var fileBytes = System.IO.File.ReadAllBytes(path);
            return File(fileBytes, "application/x-msdownload", attachment);
        }
    }
}