Delphi-JsonToDelphiClass
========================

## This is a fork of Petar Georgiev original work  ##

New feature:
* Support for objects with diffrents properties in an Array

Eg this JSON 
```
{
   "ArrayTest":[
      {
           "S1":"5102"
      },
      {
           "S2":"True"
      }      
   ]
}
```

Generates the following DTO:
```
  TArrayTestDTO = class
  private
    FS1: string;
    FS2: string;
  published
    property S1: string read FS1 write FS1;
    property S2: string read FS2 write FS2;
  end;
```

Fixes & Features: 22th November 2020
* Fixed Bug in Demo generator - It wasn't allways valid code
* Added support for non object arrays. They are now mapped into at TList
* Stopred the generastor from generating surplus classes. 
* Added a settings dialog and settings class
* New feature: Properties in PascalCase


Lots of changes in this version:
* Only floating point numbers are mapped to Double
* Numbers are mapped to Integer or Int64 depending on their size
* Generated code restructored, and simplified
* Generated classes inheriteds from TJsonDTO
* Socurce Code restructored
* Parser logic are seperated from GUI logic
* Fixed bug in the RegEx for recognizing an ISO8601 Date
* Serialization removed the "noise" of List<T> i.e. includes internal properties that did not exist in the original JSON string.
* Generated code uses TObjectList<T>
  
Generates Delphi Classes based on JSON string. Just like XML Data Binding, but for JSON.

Main features:

- Build entirely on the RTL (no external dependencies) so it's cross-platform;
- Accepts any valid JSON string, no matter how complex the object is;
- Visualizes the structure of the JSON objects in a treeview;
- Generates complete delphi unit (declaration and implementation), based on the JSON string input;
- Automatically prefixes reserved Delphi words with "&" (ampersand);
- ** Support for JSON string that contains empty Array;
- Adds support code to automatically destroy complex sub types. So you don't have to manage subobject's lifetime manually;
- ** Uses TObjectList<T> to represent lists;
- Adds helper serialization/deserialization functions;
- Serialization and deserialization results in the same JSON structure!
- Automatically detects date/datetime parts and maps them to TDate/TDateTime (as long as dates are ISO8601 compliant);
- Maps floating point numbers to Double
- Maps Number to Integer or Int64 depending on the number
- Maps true/false values to Boolean;
- Allows you to change property names (keys);
- Allows you to change the names of the stub classes;
- Supports JSON pretty print to format the input string;
- Simple and responsive GUI;
- Automatic check for update, based on ITask (Parallel Programming Library)!
- It's open source! You can find the source code and binary releases on GitHub.
- The program uses MadExcept to report unhanded exceptions;

* If the JSON array is empty the contained type is unknown. Unit generation works only with known and supported types.

*** The releases of JsonToDelphiClass (source and binaries) are public and reside on GitHub. The update unit uses GitHub's REST API to enumerate tags/releases.

Report any problems/suggestions using GitHub's facilities.

