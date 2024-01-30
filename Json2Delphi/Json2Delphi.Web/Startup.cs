using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace Json2Delphi.Web
{
  public class Startup
  {
    public Startup(IConfiguration configuration) => Configuration = configuration;

    public IConfiguration Configuration { get; }

    // This method gets called by the runtime. Use this method to add services to the container.
    public void ConfigureServices(IServiceCollection services) {
      services.AddRazorPages();
      services.AddControllersWithViews();
      services.AddWebOptimizer(pipeline => {
        pipeline.AddCssBundle("/lib/highlight/highlight-bundle.css", "lib/highlight/default.css");
        pipeline.AddCssBundle("/css/json2delphi-bundle.css", "css/_globals.css", "css/json2delphi.css");
        pipeline.AddJavaScriptBundle("/js/json2delphi-bundle.js", "js/json2delphi.js");
      });
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IWebHostEnvironment env) {
      if (env.IsDevelopment()) app.UseDeveloperExceptionPage();

      app.UseWebOptimizer();
      app.UseHttpsRedirection();
      app.UseStaticFiles();
      app.UseRouting();
      app.UseEndpoints(endpoints => {
        endpoints.MapRazorPages();
        endpoints.MapControllers();
      });
    }
  }
}