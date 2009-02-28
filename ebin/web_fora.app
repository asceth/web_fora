%% This is the application resource file (.app file) for the web_fora,
%% application.
{application, web_fora,
  [{description, "Fora application to be used with web_router"},
   {vsn, "0.1.0"},
   {modules, [web_fora,
              web_fora_categories, web_fora_category
             ]},
   {registered,[]},
   {applications, [kernel, stdlib, web_router, inets]}
  ]}.
