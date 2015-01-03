Delphi-JsonToDelphiClass
========================

Generates Delphi Classes based on JSON string. Just like XML Data Binding, but for JSON.

Main features:

- Build entirely on the RTL (no external dependencies) so it's cross-platform;
- Accepts any valid JSON string, no matter how complex the object is;
- Visualizes the structure of the JSON objects in a treeview;
- Generates complete delphi unit (declaration and implementation), based on the JSON string input;
- Automatically prefixes reserved Delphi words with "&" (ampersand);
- * Blocks unit generation if the JSON string contains empty Array;
- Adds support code to automatically destroy complex sub types. So you don't have to manage subobject's lifetime manually;
- ** Uses TArray<T> to represent lists;
- Adds helper serialization/deserialization functions;
- Serialization and deserialization results in the same JSON structure!
- Automatically detects date/datetime parts and maps them to TDate/TDateTime (as long as dates are ISO8601 compliant);
- Maps all numbers to Double;
- Maps true/false values to Boolean;
- Allows you to change property names (keys);
- Allows you to change the names of the stub classes;
- Supports JSON pretty print to format the input string;
- Simple and responsive GUI;
- *** Automatic check for update, based on ITask (Parallel Programming Library)!
- It's open source! You can find the source code and binary releases on GitHub.
- The program uses MadExcept to report unhanded exceptions;

* If the JSON array is empty the contained type is unknown. Unit generation works only with known and supported types.

** This is because serialization of TList<T> adds "noise" i.e. includes internal properties that did not exist in the original JSON string.

*** The releases of JsonToDelphiClass (source and binaries) are public and reside on GitHub. The update unit uses GitHub's REST API to enumerate tags/releases.

Report any problems/suggestions using GitHub's facilities.

You can find more information here: http://www.pgeorgiev.com/?p=1832
