using Microsoft.AspNetCore.Mvc.ViewFeatures;

namespace Json2Delphi.Web.Extensions
{
    public static class ViewDataDictionaryExtensions
    {
        private const string _titleIdent = "Title";
        private const string _resultTextIdent = "ResultText";

        private static void SetIdent(ViewDataDictionary viewData, string ident, object value)
        {
            if (viewData is null) 
                return;
            viewData[ident] = value;
        }

        private static T GetIdent<T>(ViewDataDictionary viewData, string ident)
        {
            if (viewData is null || viewData[ident] is null) 
                return default;
            return (T)viewData[ident];
        }

        public static void SetTitle(this ViewDataDictionary viewData, string title)
        {
            SetIdent(viewData, _titleIdent, title);
        }

        public static string GetTitle(this ViewDataDictionary viewData) => GetIdent<string>(viewData, _titleIdent);

        public static void SetResultText(this ViewDataDictionary viewData, string resultText)
        {
            SetIdent(viewData, _resultTextIdent, resultText);
        }

        public static string GetResultText(this ViewDataDictionary viewData) => GetIdent<string>(viewData, _resultTextIdent);
    }
}