namespace Json2Delphi.Web.Interface
{
    public interface ISettings
    {
        bool AddJsonPropertyAttributes { get; set; }
        bool PostFixClassNames { get; set; }
        string PostFix { get; set; }
        bool UsePascalCase { get; set; }
        public bool SuppressZeroDate { get; set; }
    }
}