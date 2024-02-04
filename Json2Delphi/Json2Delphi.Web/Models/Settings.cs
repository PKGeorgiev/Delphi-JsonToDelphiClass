using Json2Delphi.Web.Interface;

namespace Json2Delphi.Web.Models
{
    public class Settings : ISettings
    {
        public Settings() { }

        public Settings(ISettings settings)
        {
            AddJsonPropertyAttributes = settings.AddJsonPropertyAttributes;
            PostFixClassNames = settings.PostFixClassNames;
            PostFix = settings.PostFix;
            UsePascalCase = settings.UsePascalCase;
            SuppressZeroDate = settings.SuppressZeroDate;
        }

        public bool AddJsonPropertyAttributes { get; set; }
        public bool PostFixClassNames { get; set; }
        public string PostFix { get; set; }
        public bool UsePascalCase { get; set; }
        public bool SuppressZeroDate { get; set; }
    }
}