let plugins =
  [
    (module Scopes.Plugin : PluginType.PluginType);
    (module Salsa_plugin.Plugin : PluginType.PluginType);
  ]
