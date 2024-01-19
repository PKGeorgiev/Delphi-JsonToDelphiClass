using Json2Delphi.Web.Interface;

namespace Json2Delphi.Web.Models
{
    public class MainViewModel : ISettings
    {
        public string SourceText { get; set; } = string.Empty;
        public string ResultText { get; set; } = string.Empty;
        public bool AddJsonPropertyAttributes { get; set; } = false;
        public bool PostFixClassNames { get; set; } = false;
        public string PostFix { get; set; } = string.Empty;
        public bool UsePascalCase { get; set; } = true;
        public bool SuppressZeroDate { get; set; } = true;
    }
}