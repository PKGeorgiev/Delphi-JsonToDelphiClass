Delphi-JsonToDelphiClass
========================
## Fixes & Features: 04th June 2021 ##

### Bugs: ###
* An error message occured when switching betewwn the diffrent demofiles
* Dates without timestamp wasn't recognized within  the RegEx
* Compile error in unit tests
* Updated elements in a list wasn't applied to the generated json.
* Issue #2 [Out of memory error and High CPU usage](https://github.com/JensBorrisholt/Delphi-JsonToDelphiClass/pull/2) - Thank You [MarkRSill](https://github.com/MarkRSill)

### Features ###
* Added unit tests for updating elements in lists.

## Fixes & Features: 26th Marts 2021 ##

### Bugs: ###
* The same class name could appear multiple times:

E.g this JSON generated faulty code:

```
{
    "/": {
        "readonly": true
    },
    "\\": {
        "readonly": true
    }
} 
```

## Fixes & Features: 22th December 2020 ##

### Bugs: ###

### Features ###

* New main form. Completly rewritten. 
* Support for BSON
* Support for Minify JSON
* Support for multiblt output formats
* JSON are now minifyed before posted to the validator. Means support for larger JSONs to be validated. 
* Version 3.0 released.

## Fixes & Features: 11th December 2020 ##

### Bugs: ###

* "id": "01010101" faulty generated a TDateTime property not string. 
* Settings.AddJsonPropertyAttributes didn't generate a Property Attribute 

### Features ###

* JSON are now posted directly to the validator
* Better property name generator
* More unit tests

## Fixes & Features: 24th November 2020 ##

### Bugs: ###

### Features ###

* Possibility to change the postfix of ClassNames, via Settings Dialog. Default: DTO
* Settings Dialog rewritten to use LiveBindings
* Create a Demo Project, using *your* Json Data

## Fixes & Features: 22th November 2020 ##

### Bugs: ###
* Demo generator didn't allways generate valid code
* Stopped the generator from generating surplus classes. 

### Features ###
* Non object arrays are now mapped into a TList<T> instead of TArray<T>
* Added a settings dialog and settings class
* Properties in PascalCase (Setting)
* Allways use JsonName property annotation  (Setting)
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


## Previous changes ##

* Only floating point numbers are mapped to Double
* Numbers are mapped to Integer or Int64 depending on their size
* Generated code restructored, and simplified
* Generated classes inheriteds from TJsonDTO
* Socurce Code restructored
* Parser logic seperated from GUI logic
* Fixed bug in the RegEx for recognizing an ISO8601 Date
* Serialization removed the "noise" of List<T> i.e. includes internal properties that did not exist in the original JSON string.
* Generated code uses TObjectList<T>
  
Generates Delphi Classes based on JSON string. Just like XML Data Binding, but for JSON.

## Main features ##

- Build entirely on the RTL (no external dependencies) so it's cross-platform;
- Accepts any valid JSON string, no matter how complex the object is;
- Visualizes the structure of the JSON objects in a treeview;
- Generates complete delphi unit (declaration and implementation), based on the JSON string input;
- Automatically prefixes reserved Delphi words with "&" (ampersand);
- Support for JSON string that contains empty Array;
- Adds support code to automatically destroy complex sub types. So you don't have to manage subobject's lifetime manually;
- Uses TObjectList<T> to represent lists;
- Adds helper serialization/deserialization functions;
- Serialization and deserialization results in the same JSON structure!
- Automatically detects date/datetime parts and maps them to TDate/TDateTime (as long as dates are ISO8601 compliant);
- Maps floating point numbers to Double
- Maps Number to Integer or Int64 depending on the number
- Maps true/false values to Boolean;
- Supports JSON pretty print to format the input string;
- Simple and responsive GUI;
- Automatic check for update, based on ITask (Parallel Programming Library)!
- It's open source! You can find the source code and binary releases on GitHub.

* If the JSON array is empty the contained type is unknown. Unit generation works only with known and supported types.

*** The releases of JsonToDelphiClass (source and binaries) are public and reside on GitHub. The update unit uses GitHub's REST API to enumerate tags/releases.

Report any problems/suggestions using GitHub's facilities.

